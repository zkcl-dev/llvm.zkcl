use std::ffi::{CStr, CString, c_void};
use std::io::{self, Write};
use std::mem::ManuallyDrop;
use std::{ptr, slice};

use inkwell::context::ContextRef;
use inkwell::llvm_sys::LLVMDiagnosticSeverity::{
    LLVMDSError, LLVMDSNote, LLVMDSRemark, LLVMDSWarning,
};
use inkwell::llvm_sys::core::{
    LLVMContextCreate, LLVMContextSetDiagnosticHandler, LLVMDisposeMemoryBuffer,
    LLVMDisposeMessage, LLVMGetBufferSize, LLVMGetBufferStart, LLVMGetDiagInfoDescription,
    LLVMGetDiagInfoSeverity, LLVMGetModuleContext, LLVMSetModuleIdentifier,
};
use inkwell::llvm_sys::error::{LLVMCreateStringError, LLVMErrorRef};
use inkwell::llvm_sys::orc2::lljit::{
    LLVMOrcLLJITAddLLVMIRModule, LLVMOrcLLJITAddLLVMIRModuleWithRT,
    LLVMOrcLLJITGetIRTransformLayer, LLVMOrcLLJITGetObjTransformLayer,
};
use inkwell::llvm_sys::orc2::{
    LLVMOrcCreateNewThreadSafeContextFromLLVMContext, LLVMOrcCreateNewThreadSafeModule,
    LLVMOrcDisposeCSymbolFlagsMap, LLVMOrcDisposeThreadSafeContext,
    LLVMOrcIRTransformLayerSetTransform, LLVMOrcMaterializationResponsibilityGetSymbols,
    LLVMOrcMaterializationResponsibilityRef, LLVMOrcObjectTransformLayerSetTransform,
    LLVMOrcResourceTrackerRef, LLVMOrcSymbolStringPoolEntryStr, LLVMOrcThreadSafeContextRef,
    LLVMOrcThreadSafeModuleRef, LLVMOrcThreadSafeModuleWithModuleDo,
};
use inkwell::llvm_sys::prelude::{
    LLVMContextRef, LLVMDiagnosticInfoRef, LLVMMemoryBufferRef, LLVMModuleRef,
};
use inkwell::module::Module as LlvmModule;

use super::{Dylib, Materialization, Session, Submission};
use crate::{Module, Result, llvm_result};

pub(super) struct IrContext {
    raw: LLVMOrcThreadSafeContextRef,
    pub(super) llvm: LLVMContextRef,
}

pub(super) struct CompilingIr {
    context: LLVMContextRef,
    pub(super) errors: Vec<String>,
}

impl IrContext {
    fn new() -> Self {
        // SAFETY: the thread-safe context takes ownership of this fresh context.
        unsafe {
            let llvm = LLVMContextCreate();
            let raw = LLVMOrcCreateNewThreadSafeContextFromLLVMContext(llvm);
            Self { raw, llvm }
        }
    }
}

impl Drop for IrContext {
    fn drop(&mut self) {
        // SAFETY: this wrapper owns one shared context reference.
        unsafe { LLVMOrcDisposeThreadSafeContext(self.raw) };
    }
}

impl Session {
    pub(super) fn install_ir_tracking(&self) {
        let context = ptr::from_ref(self).cast_mut().cast();
        // SAFETY: the session is pinned by its Rc allocation. LLVM 21's
        // default in-place dispatcher calls both layers on the lookup thread.
        // Its public IR layer runs after the built-in initializer transform.
        unsafe {
            LLVMOrcIRTransformLayerSetTransform(
                LLVMOrcLLJITGetIRTransformLayer(self.raw.as_ptr()),
                begin_compile,
                context,
            );
            LLVMOrcObjectTransformLayerSetTransform(
                LLVMOrcLLJITGetObjTransformLayer(self.raw.as_ptr()),
                record_compiled_object,
                context,
            );
        }
    }
}

impl Dylib {
    pub(super) fn add_ir_module(
        &self,
        module: &Module,
        tracker: Option<LLVMOrcResourceTrackerRef>,
    ) -> Result<()> {
        let bitcode = module.bitcode_buffer()?;
        let identifier = module.identifier()?;
        let context = IrContext::new();
        // SAFETY: this is a fresh context, and only the resulting module uses it.
        let snapshot = LlvmModule::parse_bitcode_from_buffer(&bitcode, unsafe {
            ContextRef::new(context.llvm)
        })?;
        self.session.configure_module(&snapshot)?;
        // SAFETY: the identifier bytes remain valid for the call. The TSM takes
        // the module and shares the context. LLJIT consumes the TSM on all paths.
        unsafe {
            let snapshot = ManuallyDrop::new(snapshot);
            LLVMSetModuleIdentifier(
                snapshot.as_mut_ptr(),
                identifier.as_ptr().cast(),
                identifier.len(),
            );
            let tsm = LLVMOrcCreateNewThreadSafeModule(snapshot.as_mut_ptr(), context.raw);
            let jit = self.session.raw.as_ptr();
            llvm_result(match tracker {
                Some(tracker) => LLVMOrcLLJITAddLLVMIRModuleWithRT(jit, tracker, tsm),
                None => LLVMOrcLLJITAddLLVMIRModule(jit, self.raw, tsm),
            })?;
        }
        self.session
            .tracking
            .borrow_mut()
            .submissions
            .push(Submission::ir(context, tracker));
        Ok(())
    }
}

extern "C" fn get_module_context(context: *mut c_void, module: LLVMModuleRef) -> LLVMErrorRef {
    // SAFETY: the caller provided one writable slot, and LLVM holds the module lock.
    unsafe { *context.cast::<LLVMContextRef>() = LLVMGetModuleContext(module) };
    ptr::null_mut()
}

extern "C" fn begin_compile(
    context: *mut c_void,
    module: *mut LLVMOrcThreadSafeModuleRef,
    responsibility: LLVMOrcMaterializationResponsibilityRef,
) -> LLVMErrorRef {
    // SAFETY: LLVM passes a live session context and an owned TSM slot. This
    // transform only reads the module and leaves the slot with LLVM.
    let session = unsafe { &*context.cast::<Session>() };
    let mut llvm: LLVMContextRef = ptr::null_mut();
    let error = unsafe {
        LLVMOrcThreadSafeModuleWithModuleDo(
            *module,
            get_module_context,
            ptr::from_mut(&mut llvm).cast(),
        )
    };
    if !error.is_null() {
        return error;
    }
    let mut tracking = session.tracking.borrow_mut();
    let Some(submission) = tracking.pending_ir(llvm) else {
        return ptr::null_mut();
    };
    session.compiling_ir.replace(Some(CompilingIr {
        context: llvm,
        errors: Vec::new(),
    }));
    // SAFETY: the backend is about to compile this module in place. Both ORC
    // and tracking release its context before the session allocation is freed.
    unsafe { LLVMContextSetDiagnosticHandler(llvm, Some(record_compile_diagnostic), context) };
    if submission.tracker.is_some() {
        // SAFETY: the symbol map and names are live here. Copy the ORC-mangled
        // names while the array is valid, then free the array.
        unsafe {
            let mut count = 0;
            let symbols =
                LLVMOrcMaterializationResponsibilityGetSymbols(responsibility, &mut count);
            if count != 0 {
                for symbol in slice::from_raw_parts(symbols, count) {
                    let name = CStr::from_ptr(LLVMOrcSymbolStringPoolEntryStr(symbol.Name));
                    submission.definitions.insert(name.to_bytes().to_vec());
                }
            }
            LLVMOrcDisposeCSymbolFlagsMap(symbols);
        }
    }
    ptr::null_mut()
}

extern "C" fn record_compile_diagnostic(diagnostic: LLVMDiagnosticInfoRef, context: *mut c_void) {
    // SAFETY: LLVM provides a live diagnostic and transfers ownership of its description.
    let (severity, message) = unsafe {
        let severity = LLVMGetDiagInfoSeverity(diagnostic);
        let description = LLVMGetDiagInfoDescription(diagnostic);
        let message = CStr::from_ptr(description).to_string_lossy().into_owned();
        LLVMDisposeMessage(description);
        (severity, message)
    };
    match severity {
        LLVMDSError => {
            // SAFETY: the compiling context never outlives this session.
            let session = unsafe { &*context.cast::<Session>() };
            if let Some(ir) = session.compiling_ir.borrow_mut().as_mut() {
                ir.errors.push(message);
            }
        }
        LLVMDSWarning => {
            let _ = writeln!(io::stderr().lock(), "warning: {message}");
        }
        LLVMDSNote => {
            let _ = writeln!(io::stderr().lock(), "note: {message}");
        }
        LLVMDSRemark => {}
    }
}

extern "C" fn record_compiled_object(
    context: *mut c_void,
    buffer: *mut LLVMMemoryBufferRef,
) -> LLVMErrorRef {
    // SAFETY: the session context is live and LLVM provides a writable buffer slot.
    let session = unsafe { &*context.cast::<Session>() };
    let mut tracking = session.tracking.borrow_mut();
    let Some(ir) = session.compiling_ir.take() else {
        // SAFETY: direct object submissions reach this layer with their original buffer.
        tracking.begin_object_link(unsafe { *buffer });
        return ptr::null_mut();
    };
    let Some(submission) = tracking.pending_ir(ir.context) else {
        return reject_object(buffer, "LLVM JIT lost the compiling IR submission");
    };
    submission.materialization = Materialization::Started;
    if !ir.errors.is_empty() {
        return reject_object(buffer, &ir.errors.join("\n"));
    }
    // SAFETY: the buffer and its bytes remain live until the transform returns.
    let bytes = unsafe {
        slice::from_raw_parts(
            LLVMGetBufferStart(*buffer).cast(),
            LLVMGetBufferSize(*buffer),
        )
    };
    match Submission::read(bytes, submission.tracker) {
        Ok(object) => {
            submission.definitions.extend(object.definitions);
            submission.imports = object.imports;
            ptr::null_mut()
        }
        Err(error) => reject_object(buffer, &error.to_string()),
    }
}

fn reject_object(buffer: *mut LLVMMemoryBufferRef, message: &str) -> LLVMErrorRef {
    let message = CString::new(message)
        .unwrap_or_else(|_| c"LLVM JIT could not materialize the compiled object".into());
    // SAFETY: the object transform owns the buffer and must clear the slot on error.
    unsafe {
        LLVMDisposeMemoryBuffer(*buffer);
        *buffer = ptr::null_mut();
        LLVMCreateStringError(message.as_ptr())
    }
}

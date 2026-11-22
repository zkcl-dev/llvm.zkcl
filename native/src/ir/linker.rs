use std::borrow::Cow;
use std::cell::RefCell;
use std::ffi::{CStr, CString, c_char, c_void};
use std::io::{self, Write};
use std::mem::ManuallyDrop;
use std::{ptr, slice};

use inkwell::context::AsContextRef;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module as LlvmModule;
use llvm_sys::bit_reader::LLVMParseBitcodeInContext2;
use llvm_sys::core::{
    LLVMContextGetDiagnosticContext, LLVMContextGetDiagnosticHandler,
    LLVMContextSetDiagnosticHandler, LLVMDisposeMessage, LLVMDisposeModule, LLVMGetDataLayoutStr,
    LLVMGetDiagInfoDescription, LLVMGetDiagInfoSeverity, LLVMGetSourceFileName, LLVMGetTarget,
    LLVMModuleCreateWithNameInContext, LLVMSetTarget,
};
use llvm_sys::linker::LLVMLinkModules2;
use llvm_sys::prelude::{LLVMDiagnosticInfoRef, LLVMModuleRef};
use llvm_sys::target::{LLVMGetModuleDataLayout, LLVMSetModuleDataLayout};
use llvm_sys::{LLVMDiagnosticHandler, LLVMDiagnosticSeverity};

use super::{Context, Module, ModuleStorage};
use crate::target::layouts_equal;
use crate::{Error, Result};

impl Module {
    /// Link verified modules in order into a fresh context. LLVM resolves
    /// declarations and linkage. The input modules and their handles stay live
    /// even if linking fails after merging some of the inputs.
    pub fn link(&self, inputs: &[Self]) -> Result<Self> {
        let primary = self.bitcode_buffer()?;
        let buffers = inputs
            .iter()
            .map(Self::bitcode_buffer)
            .collect::<Result<Vec<_>>>()?;
        let name = self.with_llvm(|module| module.get_name().to_string_lossy().into_owned());
        let storage = ModuleStorage::try_new(Context::default(), |context| {
            let messages = RefCell::default();
            let diagnostics = Diagnostics::install(context, &messages);
            let result = diagnostics.parse(&primary)?;
            for buffer in &buffers {
                let input = diagnostics.parse(buffer)?;
                result.link(input, &diagnostics)?;
            }
            // SAFETY: the result was parsed in this context and is now owned by
            // the wrapper. Construct it after linking so its cached layout
            // string reflects any layout assigned by the linker.
            let module = unsafe { LlvmModule::new(result.into_raw()) };
            module.set_name(&name);
            Ok::<_, Error>(module)
        })?;
        Self::verified(storage)
    }
}

struct OwnedModule<'ctx> {
    raw: LLVMModuleRef,
    context: &'ctx Context,
}

impl<'ctx> OwnedModule<'ctx> {
    fn with_triple(context: &'ctx Context, triple: &CStr) -> Self {
        // SAFETY: the module borrows the context and LLVM copies the triple.
        let raw = unsafe {
            let raw = LLVMModuleCreateWithNameInContext(c"".as_ptr(), context.llvm().as_ctx_ref());
            LLVMSetTarget(raw, triple.as_ptr());
            raw
        };
        Self { raw, context }
    }

    fn into_raw(self) -> LLVMModuleRef {
        ManuallyDrop::new(self).raw
    }

    fn check_triples(&self, input: &Self, diagnostics: &Diagnostics<'_>) -> Result<()> {
        // SAFETY: the triple strings are owned by the live modules.
        let (left, right) = unsafe {
            (
                CStr::from_ptr(LLVMGetTarget(self.raw)),
                CStr::from_ptr(LLVMGetTarget(input.raw)),
            )
        };
        if left.is_empty() || right.is_empty() || left == right {
            return Ok(());
        }
        // LLVM-C has no triple-compatibility query. IRMover checks it when
        // linking, so ask it with two empty modules before touching real IR.
        let left_probe = Self::with_triple(self.context, left);
        let right_probe = Self::with_triple(self.context, &probe_triple(right));
        // SAFETY: the modules share a context and LLVM consumes the source.
        let failed = unsafe { LLVMLinkModules2(left_probe.raw, right_probe.into_raw()) } != 0;
        let messages = diagnostics.take();
        if messages.iter().any(|(severity, message)| {
            *severity == LLVMDiagnosticSeverity::LLVMDSWarning
                && message.starts_with("Linking two modules of different target triples:")
        }) {
            return Err(Error::new(format!(
                "incompatible LLVM target triples '{}' and '{}'",
                left.to_string_lossy(),
                right.to_string_lossy(),
            )));
        }
        if failed {
            return Err(Error::new(error_messages(&messages)));
        }
        Ok(())
    }

    fn check_layout(&self, input: &Self) -> Result<()> {
        // SAFETY: the strings and data layouts are owned by the live modules.
        unsafe {
            let left = CStr::from_ptr(LLVMGetDataLayoutStr(self.raw)).to_string_lossy();
            let right = CStr::from_ptr(LLVMGetDataLayoutStr(input.raw)).to_string_lossy();
            if right.is_empty() {
                LLVMSetModuleDataLayout(input.raw, LLVMGetModuleDataLayout(self.raw));
            } else if !left.is_empty() && !layouts_equal(&left, &right) {
                return Err(Error::new(format!(
                    "incompatible LLVM data layouts '{left}' and '{right}'",
                )));
            }
        }
        Ok(())
    }

    fn link(&self, input: Self, diagnostics: &Diagnostics<'_>) -> Result<()> {
        self.check_triples(&input, diagnostics)?;
        self.check_layout(&input)?;
        // SAFETY: the source name is module-owned and copied before linking.
        let filename = unsafe {
            let mut length = 0;
            let name = LLVMGetSourceFileName(input.raw, &mut length);
            String::from_utf8_lossy(slice::from_raw_parts(name.cast(), length)).into_owned()
        };
        // SAFETY: both modules come from the same linker context. The linker
        // owns and destroys the source on both success and failure.
        let failed = unsafe { LLVMLinkModules2(self.raw, input.into_raw()) } != 0;
        let messages = diagnostics.take();
        if failed {
            return Err(Error::new(format!(
                "cannot link LLVM module '{filename}': {}",
                error_messages(&messages),
            )));
        }
        for (severity, message) in messages {
            if severity == LLVMDiagnosticSeverity::LLVMDSWarning {
                let _ = writeln!(io::stderr().lock(), "warning: {}", message.trim_end());
            }
        }
        Ok(())
    }
}

impl Drop for OwnedModule<'_> {
    fn drop(&mut self) {
        // SAFETY: a module transferred out of this wrapper skips Drop.
        unsafe { LLVMDisposeModule(self.raw) };
    }
}

fn probe_triple(triple: &CStr) -> Cow<'_, CStr> {
    // LLVM suppresses NVPTX warnings for libdevice spellings. Both OS names
    // parse as UnknownOS, so use a different UnknownOS spelling in the probe.
    let mut fields = triple.to_bytes().splitn(4, |&byte| byte == b'-');
    let (Some(arch @ (b"nvptx" | b"nvptx64")), Some(vendor), Some(os)) =
        (fields.next(), fields.next(), fields.next())
    else {
        return Cow::Borrowed(triple);
    };
    if !matches!(
        (vendor, os),
        (b"nvidia", b"gpulibs") | (b"unknown", b"unknown")
    ) {
        return Cow::Borrowed(triple);
    }
    let mut name = [arch, vendor, b"none"].join(&b'-');
    if let Some(environment) = fields.next() {
        name.push(b'-');
        name.extend_from_slice(environment);
    }
    Cow::Owned(CString::new(name).expect("triple components have no NULs"))
}

type Diagnostic = (LLVMDiagnosticSeverity, String);

struct Diagnostics<'a> {
    context: &'a Context,
    previous_handler: LLVMDiagnosticHandler,
    previous_context: *mut c_void,
    messages: &'a RefCell<Vec<Diagnostic>>,
}

impl<'a> Diagnostics<'a> {
    fn install(context: &'a Context, messages: &'a RefCell<Vec<Diagnostic>>) -> Self {
        let raw = context.llvm().as_ctx_ref();
        // SAFETY: the guard borrows the context and callback state for the
        // registration. LLVM calls this handler synchronously.
        unsafe {
            let previous_handler = LLVMContextGetDiagnosticHandler(raw);
            let previous_context = LLVMContextGetDiagnosticContext(raw);
            LLVMContextSetDiagnosticHandler(
                raw,
                Some(capture_diagnostic),
                ptr::from_ref(messages).cast_mut().cast(),
            );
            Self {
                context,
                previous_handler,
                previous_context,
                messages,
            }
        }
    }

    fn parse(&self, buffer: &MemoryBuffer<'_>) -> Result<OwnedModule<'a>> {
        let mut raw = ptr::null_mut();
        // SAFETY: LLVM borrows the buffer for parsing and returns an owned
        // module in this context on success.
        let failed = unsafe {
            LLVMParseBitcodeInContext2(
                self.context.llvm().as_ctx_ref(),
                buffer.as_mut_ptr(),
                &mut raw,
            )
        } != 0;
        let messages = self.take();
        if failed {
            return Err(Error::new(format!(
                "cannot read LLVM bitcode: {}",
                error_messages(&messages),
            )));
        }
        Ok(OwnedModule {
            raw,
            context: self.context,
        })
    }

    fn take(&self) -> Vec<Diagnostic> {
        self.messages.take()
    }
}

impl Drop for Diagnostics<'_> {
    fn drop(&mut self) {
        // SAFETY: restore the handler before the borrowed callback state goes
        // away or the result module is used again.
        unsafe {
            LLVMContextSetDiagnosticHandler(
                self.context.llvm().as_ctx_ref(),
                self.previous_handler,
                self.previous_context,
            );
        }
    }
}

extern "C" fn capture_diagnostic(diagnostic: LLVMDiagnosticInfoRef, data: *mut c_void) {
    // SAFETY: LLVM supplies a live diagnostic and the state registered by the
    // guard. The description is owned and must be released after copying it.
    unsafe {
        let severity = LLVMGetDiagInfoSeverity(diagnostic);
        let description = take_message(LLVMGetDiagInfoDescription(diagnostic));
        let messages = &*data.cast::<RefCell<Vec<Diagnostic>>>();
        messages.borrow_mut().push((severity, description));
    }
}

// SAFETY: `message` must be an owned LLVM message.
unsafe fn take_message(message: *mut c_char) -> String {
    unsafe {
        let text = CStr::from_ptr(message).to_string_lossy().into_owned();
        LLVMDisposeMessage(message);
        text
    }
}

fn error_messages(messages: &[Diagnostic]) -> String {
    let errors = messages
        .iter()
        .filter(|(severity, _)| *severity == LLVMDiagnosticSeverity::LLVMDSError)
        .map(|(_, message)| message.trim_end())
        .collect::<Vec<_>>()
        .join("\n");
    if errors.is_empty() {
        "LLVM reported no error text".to_owned()
    } else {
        errors
    }
}

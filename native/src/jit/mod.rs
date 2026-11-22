use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ffi::{CStr, CString, c_void};
use std::mem::ManuallyDrop;
use std::ptr::{self, NonNull};
use std::rc::Rc;
use std::slice;
use std::sync::{Mutex, PoisonError, mpsc};

use inkwell::llvm_sys::error::{LLVMCreateStringError, LLVMErrorRef};
use inkwell::llvm_sys::orc2::lljit::{
    LLVMOrcCreateLLJIT, LLVMOrcDisposeLLJIT, LLVMOrcLLJITAddObjectFile,
    LLVMOrcLLJITAddObjectFileWithRT, LLVMOrcLLJITGetDataLayoutStr, LLVMOrcLLJITGetExecutionSession,
    LLVMOrcLLJITGetMainJITDylib, LLVMOrcLLJITGetTripleString, LLVMOrcLLJITMangleAndIntern,
    LLVMOrcOpaqueLLJIT,
};
use inkwell::llvm_sys::orc2::{
    LLVMJITEvaluatedSymbol, LLVMJITSymbolFlags,
    LLVMJITSymbolGenericFlags::{
        LLVMJITSymbolGenericFlagsCallable, LLVMJITSymbolGenericFlagsExported,
    },
    LLVMOrcAbsoluteSymbols, LLVMOrcCJITDylibSearchOrderElement, LLVMOrcCLookupSet,
    LLVMOrcCLookupSetElement, LLVMOrcCSymbolMapPair, LLVMOrcCSymbolMapPairs,
    LLVMOrcCreateCustomCAPIDefinitionGenerator, LLVMOrcDefinitionGeneratorRef,
    LLVMOrcDisposeMaterializationUnit, LLVMOrcExecutionSessionGetJITDylibByName,
    LLVMOrcExecutionSessionIntern, LLVMOrcExecutionSessionLookup, LLVMOrcExecutionSessionRef,
    LLVMOrcExecutionSessionSetErrorReporter, LLVMOrcJITDylibAddGenerator, LLVMOrcJITDylibDefine,
    LLVMOrcJITDylibLookupFlags,
    LLVMOrcJITDylibLookupFlags::{
        LLVMOrcJITDylibLookupFlagsMatchAllSymbols,
        LLVMOrcJITDylibLookupFlagsMatchExportedSymbolsOnly,
    },
    LLVMOrcJITDylibRef, LLVMOrcLookupKind,
    LLVMOrcLookupKind::{LLVMOrcLookupKindDLSym, LLVMOrcLookupKindStatic},
    LLVMOrcLookupStateRef, LLVMOrcResourceTrackerRef, LLVMOrcRetainSymbolStringPoolEntry,
    LLVMOrcSymbolLookupFlags::{
        LLVMOrcSymbolLookupFlagsRequiredSymbol, LLVMOrcSymbolLookupFlagsWeaklyReferencedSymbol,
    },
    LLVMOrcSymbolStringPoolEntryRef, LLVMOrcSymbolStringPoolEntryStr,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::{TargetData, TargetTriple};

use crate::target::{configure_module_target, initialize_targets};
use crate::{Error, Module, OptimizationLevel, Result, TargetMachine, llvm_result};

mod ir;
mod tracker;
pub use tracker::ResourceTracker;
use tracker::{Materialization, Submission, Tracking};

struct Session {
    raw: NonNull<LLVMOrcOpaqueLLJIT>,
    diagnostics: Mutex<Vec<String>>,
    tracking: RefCell<Tracking>,
    compiling_ir: RefCell<Option<ir::CompilingIr>>,
}

extern "C" fn record_diagnostic(context: *mut c_void, error: LLVMErrorRef) {
    // SAFETY: LLVM transfers the error and keeps this session's context live.
    let session = unsafe { &*context.cast::<Session>() };
    // The compiler reports failure without passing an object to the next layer.
    let compiling_ir = session.compiling_ir.take();
    if let Err(error) = unsafe { llvm_result(error) } {
        let mut diagnostics = session
            .diagnostics
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        if let Some(ir) = compiling_ir {
            diagnostics.extend(ir.errors);
        }
        diagnostics.push(error.to_string());
    }
}

impl Session {
    fn new() -> Result<Rc<Self>> {
        let mut raw = ptr::null_mut();
        // SAFETY: LLVM writes the owned session to a valid output slot. A
        // null builder requests an in-process LLJIT without compile threads.
        unsafe { llvm_result(LLVMOrcCreateLLJIT(&mut raw, ptr::null_mut()))? };
        let session = Rc::new(Self {
            raw: NonNull::new(raw).expect("successful LLJIT creation"),
            diagnostics: Mutex::default(),
            tracking: RefCell::default(),
            compiling_ir: RefCell::default(),
        });
        // SAFETY: the Rc allocation keeps the reporter context live through disposal.
        unsafe {
            LLVMOrcExecutionSessionSetErrorReporter(
                session.execution_session(),
                record_diagnostic,
                Rc::as_ptr(&session).cast_mut().cast(),
            );
        }
        session.install_ir_tracking();
        Ok(session)
    }

    fn execution_session(&self) -> LLVMOrcExecutionSessionRef {
        // SAFETY: LLJIT owns the execution session.
        unsafe { LLVMOrcLLJITGetExecutionSession(self.raw.as_ptr()) }
    }

    fn triple(&self) -> &CStr {
        // SAFETY: the triple string is owned by the live session.
        unsafe { CStr::from_ptr(LLVMOrcLLJITGetTripleString(self.raw.as_ptr())) }
    }

    fn data_layout(&self) -> &CStr {
        // SAFETY: the layout string is owned by the live session.
        unsafe { CStr::from_ptr(LLVMOrcLLJITGetDataLayoutStr(self.raw.as_ptr())) }
    }

    fn configure_module(&self, module: &LlvmModule<'_>) -> Result<()> {
        let triple = TargetTriple::create(&self.triple().to_string_lossy());
        let data = TargetData::create(&self.data_layout().to_string_lossy());
        configure_module_target(module, &triple, &data)
    }

    fn lookup_result<T>(&self, result: Result<T>) -> Result<T> {
        let notes = std::mem::take(
            &mut *self
                .diagnostics
                .lock()
                .unwrap_or_else(PoisonError::into_inner),
        );
        result.map_err(|error| {
            if notes.is_empty() {
                error
            } else {
                Error::new(format!("{error}\n{}", notes.join("\n")))
            }
        })
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        // SAFETY: the last owner disposes the live LLJIT exactly once.
        let _ = unsafe { llvm_result(LLVMOrcDisposeLLJIT(self.raw.as_ptr())) };
    }
}

struct Dylib {
    raw: LLVMOrcJITDylibRef,
    session: Rc<Session>,
    target: Rc<TargetMachine>,
    names: Rc<RefCell<HashSet<String>>>,
    name: Option<String>,
    imports: Option<Rc<ParentImports>>,
}

impl Dylib {
    fn new(
        session: Rc<Session>,
        target: Rc<TargetMachine>,
        names: Rc<RefCell<HashSet<String>>>,
        name: Option<String>,
        imports: Option<Rc<ParentImports>>,
    ) -> Rc<Self> {
        // SAFETY: LLJIT owns the main dylib for the lifetime of the session.
        let raw = unsafe { LLVMOrcLLJITGetMainJITDylib(session.raw.as_ptr()) };
        Rc::new(Self {
            raw,
            session,
            target,
            names,
            name,
            imports,
        })
    }

    fn add_object(&self, bytes: &[u8], tracker: Option<LLVMOrcResourceTrackerRef>) -> Result<()> {
        let mut submission = Submission::read(bytes, tracker)?;
        let buffer =
            ManuallyDrop::new(MemoryBuffer::create_from_memory_range_copy(bytes, "object"));
        submission.materialization = Materialization::PendingObject(buffer.as_mut_ptr());
        // SAFETY: the tracker belongs to this live dylib. LLVM takes ownership
        // of the buffer in either call, including on error. It runs the object
        // transform with this same buffer when the unit materializes.
        unsafe {
            let jit = self.session.raw.as_ptr();
            llvm_result(match tracker {
                Some(tracker) => LLVMOrcLLJITAddObjectFileWithRT(jit, tracker, buffer.as_mut_ptr()),
                None => LLVMOrcLLJITAddObjectFile(jit, self.raw, buffer.as_mut_ptr()),
            })?;
        }
        self.session
            .tracking
            .borrow_mut()
            .submissions
            .push(submission);
        Ok(())
    }

    fn lookup(self: &Rc<Self>, name: &CStr) -> Result<JitSymbol> {
        // SAFETY: the session, name and returned interned entry are live.
        let name = unsafe { LLVMOrcLLJITMangleAndIntern(self.session.raw.as_ptr(), name.as_ptr()) };
        self.lookup_entry(name, LLVMOrcJITDylibLookupFlagsMatchAllSymbols, false)?
            .ok_or_else(|| Error::new("LLVM JIT lookup returned no symbol"))
    }

    fn lookup_import(self: &Rc<Self>, name: &CStr) -> Result<Option<JitSymbol>> {
        // SAFETY: the session, name and returned interned entry are live.
        let entry = unsafe {
            LLVMOrcExecutionSessionIntern(self.session.execution_session(), name.as_ptr())
        };
        let symbol = self.lookup_entry(
            entry,
            LLVMOrcJITDylibLookupFlagsMatchExportedSymbolsOnly,
            true,
        )?;
        match (symbol, &self.imports) {
            (None, Some(imports)) => imports.parent.lookup_import(name),
            (symbol, _) => Ok(symbol),
        }
    }

    fn lookup_entry(
        self: &Rc<Self>,
        name: LLVMOrcSymbolStringPoolEntryRef,
        flags: LLVMOrcJITDylibLookupFlags,
        optional: bool,
    ) -> Result<Option<JitSymbol>> {
        // SAFETY: the name remains owned by us until the lookup takes it.
        let bytes = unsafe { CStr::from_ptr(LLVMOrcSymbolStringPoolEntryStr(name)) }
            .to_bytes()
            .to_vec();
        let mut order = [LLVMOrcCJITDylibSearchOrderElement {
            JD: self.raw,
            JDLookupFlags: flags,
        }];
        let mut symbols = [LLVMOrcCLookupSetElement {
            Name: name,
            LookupFlags: if optional {
                LLVMOrcSymbolLookupFlagsWeaklyReferencedSymbol
            } else {
                LLVMOrcSymbolLookupFlagsRequiredSymbol
            },
        }];
        let (tx, rx) = mpsc::channel::<LookupResult>();
        // SAFETY: the callback takes ownership of the sender and ORC takes
        // the retained name and copies the two input arrays.
        unsafe {
            LLVMOrcExecutionSessionLookup(
                self.session.execution_session(),
                LLVMOrcLookupKindDLSym,
                order.as_mut_ptr(),
                order.len(),
                symbols.as_mut_ptr(),
                symbols.len(),
                lookup_complete,
                Box::into_raw(Box::new(tx)).cast(),
            );
        }
        let Some(symbol) = self
            .session
            .lookup_result(rx.recv().expect("ORC lookup result"))?
        else {
            return Ok(None);
        };
        let address = usize::try_from(symbol.Address)
            .ok()
            .and_then(|address| NonNull::new(ptr::with_exposed_provenance_mut(address)))
            .ok_or_else(|| Error::new("LLVM JIT symbol has no address in this process"))?;
        let leases = self.session.tracking.borrow().leases(&bytes);
        Ok(Some(JitSymbol {
            address,
            flags: symbol.Flags,
            compilation: None,
            leases,
            _dylib: self.clone(),
        }))
    }
}

impl Drop for Dylib {
    fn drop(&mut self) {
        if let Some(name) = &self.name {
            self.names.borrow_mut().remove(name);
        }
    }
}

type LookupResult = Result<Option<LLVMJITEvaluatedSymbol>>;

extern "C" fn lookup_complete(
    error: LLVMErrorRef,
    symbols: LLVMOrcCSymbolMapPairs,
    count: usize,
    context: *mut c_void,
) {
    // SAFETY: the callback owns the sender and the result array is valid
    // until we return. Copy the address and flags without retaining the array.
    unsafe {
        let sender = Box::from_raw(context.cast::<mpsc::Sender<LookupResult>>());
        let result = llvm_result(error).and_then(|()| {
            if count == 0 {
                return Ok(None);
            }
            if count != 1 {
                return Err(Error::new(
                    "LLVM JIT returned an unexpected number of symbols",
                ));
            }
            let symbol = &(*symbols).Sym;
            Ok(Some(LLVMJITEvaluatedSymbol {
                Address: symbol.Address,
                Flags: LLVMJITSymbolFlags {
                    GenericFlags: symbol.Flags.GenericFlags,
                    TargetFlags: symbol.Flags.TargetFlags,
                },
            }))
        });
        let _ = sender.send(result);
    }
}

struct ParentImports {
    parent: Rc<Dylib>,
    fallback: LLVMOrcJITDylibRef,
    symbols: RefCell<HashMap<Vec<u8>, JitSymbol>>,
}

extern "C" fn forward_parent_symbols(
    _generator: LLVMOrcDefinitionGeneratorRef,
    context: *mut c_void,
    _state: *mut LLVMOrcLookupStateRef,
    kind: LLVMOrcLookupKind,
    _dylib: LLVMOrcJITDylibRef,
    _flags: LLVMOrcJITDylibLookupFlags,
    names: LLVMOrcCLookupSet,
    count: usize,
) -> LLVMErrorRef {
    // SAFETY: this in-process LLJIT invokes the generator on its creating
    // thread. The generator context and lookup entries are live in the call.
    let imports = unsafe { &*context.cast::<ParentImports>() };
    if count == 0 || kind != LLVMOrcLookupKindStatic {
        return ptr::null_mut();
    }
    let names = unsafe { slice::from_raw_parts(names, count) };
    for entry in names {
        let name = unsafe { CStr::from_ptr(LLVMOrcSymbolStringPoolEntryStr(entry.Name)) };
        if imports.symbols.borrow().contains_key(name.to_bytes()) {
            continue;
        }
        let symbol = match imports.parent.lookup_import(name) {
            Ok(Some(symbol)) => symbol,
            Ok(None) => continue,
            Err(error) => {
                let message = CString::new(error.to_string())
                    .unwrap_or_else(|_| c"LLVM JIT parent import failed".into());
                // SAFETY: LLVM copies the message during the call.
                return unsafe { LLVMCreateStringError(message.as_ptr()) };
            }
        };
        // SAFETY: the child session owns this interned name. Define a retained
        // absolute symbol in the child's namespace and keep its owner live.
        let error = unsafe {
            LLVMOrcRetainSymbolStringPoolEntry(entry.Name);
            define_address(
                imports.fallback,
                entry.Name,
                symbol.address,
                LLVMJITSymbolFlags {
                    GenericFlags: symbol.flags.GenericFlags
                        | LLVMJITSymbolGenericFlagsExported as u8,
                    TargetFlags: symbol.flags.TargetFlags,
                },
            )
        };
        if !error.is_null() {
            return error;
        }
        imports
            .symbols
            .borrow_mut()
            .insert(name.to_bytes().to_vec(), symbol);
    }
    ptr::null_mut()
}

extern "C" fn dispose_parent_imports(context: *mut c_void) {
    // SAFETY: ORC owns one Rc and invokes the disposer once on destruction.
    drop(unsafe { Rc::from_raw(context.cast::<ParentImports>()) });
}

unsafe fn define_address(
    dylib: LLVMOrcJITDylibRef,
    name: LLVMOrcSymbolStringPoolEntryRef,
    address: NonNull<c_void>,
    flags: LLVMJITSymbolFlags,
) -> LLVMErrorRef {
    let mut symbol = LLVMOrcCSymbolMapPair {
        Name: name,
        Sym: LLVMJITEvaluatedSymbol {
            Address: address.as_ptr().expose_provenance() as u64,
            Flags: flags,
        },
    };
    // SAFETY: AbsoluteSymbols takes the retained name. A failed definition
    // leaves the unit with us.
    unsafe {
        let unit = LLVMOrcAbsoluteSymbols(&mut symbol, 1);
        let error = LLVMOrcJITDylibDefine(dylib, unit);
        if !error.is_null() {
            LLVMOrcDisposeMaterializationUnit(unit);
        }
        error
    }
}

/// An open in-process ORC JITDylib for the host architecture.
pub struct Jit(RefCell<Option<Rc<Dylib>>>);

/// The kind of an address imported from the host process.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostSymbolKind {
    Function,
    Data,
}

impl Jit {
    pub fn new() -> Result<Self> {
        initialize_targets();
        let session = Session::new()?;
        let target = Rc::new(TargetMachine::new(
            &session.triple().to_string_lossy(),
            OptimizationLevel::Default,
        )?);
        Ok(Self(RefCell::new(Some(Dylib::new(
            session,
            target,
            Rc::default(),
            None,
            None,
        )))))
    }

    fn dylib(&self) -> Result<Rc<Dylib>> {
        self.0
            .borrow()
            .clone()
            .ok_or_else(|| Error::new("LLVM JIT is closed"))
    }

    /// Create an isolated child JITDylib with its own definitions and parent imports.
    pub fn create_dylib(&self, name: &str) -> Result<Self> {
        let parent = self.dylib()?;
        if name.is_empty() {
            return Err(Error::new("a name is required for the ORC JITDylib"));
        }
        let native_name = CString::new(name)
            .map_err(|_| Error::new("ORC JITDylib name contains an embedded NUL"))?;
        // SAFETY: the session and name are live for this uniqueness check.
        let reserved = unsafe {
            !LLVMOrcExecutionSessionGetJITDylibByName(
                parent.session.execution_session(),
                native_name.as_ptr(),
            )
            .is_null()
        };
        if reserved || parent.names.borrow().contains(name) {
            return Err(Error::new("an ORC JITDylib with that name already exists"));
        }
        // The C API cannot configure link order or reclaim a JITDylib in an
        // existing session. A separate LLJIT gives the child both a private
        // namespace and deterministic teardown while a generator imports its parent.
        let session = Session::new()?;
        // LLJIT 21 puts this fallback after the main dylib in link order.
        // Keep imported addresses there so child definitions can shadow them.
        let fallback = unsafe {
            LLVMOrcExecutionSessionGetJITDylibByName(
                session.execution_session(),
                c"<Process Symbols>".as_ptr(),
            )
        };
        if fallback.is_null() {
            return Err(Error::new(
                "LLVM JIT could not find the process symbol namespace",
            ));
        }
        let imports = Rc::new(ParentImports {
            parent: parent.clone(),
            fallback,
            symbols: RefCell::default(),
        });
        let child = Dylib::new(
            session,
            parent.target.clone(),
            parent.names.clone(),
            Some(name.into()),
            Some(imports.clone()),
        );
        // SAFETY: the child generator takes ownership of the context on creation
        // and the live child's JITDylib takes ownership of the generator.
        unsafe {
            let generator = LLVMOrcCreateCustomCAPIDefinitionGenerator(
                forward_parent_symbols,
                Rc::into_raw(imports).cast_mut().cast(),
                dispose_parent_imports,
            );
            LLVMOrcJITDylibAddGenerator(child.raw, generator);
        }
        child.names.borrow_mut().insert(name.into());
        Ok(Self(RefCell::new(Some(child))))
    }

    /// Create a submission group in this JITDylib.
    pub fn create_resource_tracker(&self) -> Result<ResourceTracker> {
        Ok(ResourceTracker::new(self.dylib()?))
    }

    pub fn triple(&self) -> Result<String> {
        Ok(self
            .dylib()?
            .session
            .triple()
            .to_string_lossy()
            .into_owned())
    }

    pub fn data_layout(&self) -> Result<String> {
        Ok(self
            .dylib()?
            .session
            .data_layout()
            .to_string_lossy()
            .into_owned())
    }

    /// Assign the JIT's target before lowering or optimizing the module.
    pub fn configure_module(&self, module: &Module) -> Result<()> {
        let dylib = self.dylib()?;
        module.with_llvm(|module| dylib.session.configure_module(module))
    }

    /// Compile a verified snapshot and submit its object to the JIT linker.
    ///
    /// # Safety
    /// The caller must trust this module to run native code in the process.
    pub unsafe fn add_module(&self, module: &Module) -> Result<()> {
        self.add_module_in(module, None)
    }

    /// Compile and submit a verified module with a tracker from this JITDylib.
    ///
    /// # Safety
    /// The caller must trust this module to run native code in the process.
    pub unsafe fn add_module_with_tracker(
        &self,
        module: &Module,
        tracker: &ResourceTracker,
    ) -> Result<()> {
        self.add_module_in(module, Some(tracker))
    }

    fn add_module_in(&self, module: &Module, tracker: Option<&ResourceTracker>) -> Result<()> {
        let dylib = self.dylib()?;
        let tracker = tracker
            .map(|tracker| tracker.for_dylib(&dylib))
            .transpose()?;
        let object = dylib.target.object(module)?;
        dylib.add_object(&object, tracker)
    }

    /// Submit a verified IR snapshot for ORC to compile when a definition is needed.
    /// Compilation and link errors are reported by lookup.
    ///
    /// # Safety
    /// The caller must trust this module to run native code in the process.
    pub unsafe fn add_ir_module(&self, module: &Module) -> Result<()> {
        self.add_ir_module_in(module, None)
    }

    /// Submit a verified IR snapshot with a tracker from this JITDylib.
    /// The tracker can remove the snapshot before compilation or unload its code.
    ///
    /// # Safety
    /// The caller must trust this module to run native code in the process.
    pub unsafe fn add_ir_module_with_tracker(
        &self,
        module: &Module,
        tracker: &ResourceTracker,
    ) -> Result<()> {
        self.add_ir_module_in(module, Some(tracker))
    }

    fn add_ir_module_in(&self, module: &Module, tracker: Option<&ResourceTracker>) -> Result<()> {
        let dylib = self.dylib()?;
        let tracker = tracker
            .map(|tracker| tracker.for_dylib(&dylib))
            .transpose()?;
        dylib.add_ir_module(module, tracker)
    }

    /// Submit a verified module and resolve `name` in this JITDylib.
    /// If submission or lookup fails, remove the resources from this submission.
    /// The returned symbol retains the tracker. Dropping it leaves the module installed.
    ///
    /// # Safety
    /// The caller must trust this module to run native code in the process.
    pub unsafe fn add_module_and_lookup(&self, module: &Module, name: &str) -> Result<JitSymbol> {
        let tracker = self.create_resource_tracker()?;
        let mut symbol = self
            .add_module_in(module, Some(&tracker))
            .and_then(|()| self.lookup(name))
            .map_err(|error| match tracker.remove() {
                Ok(()) => error,
                Err(cleanup) => Error::new(format!(
                    "{error}\nCould not remove the JIT submission: {cleanup}"
                )),
            })?;
        symbol.compilation = Some(tracker);
        Ok(symbol)
    }

    /// Link a relocatable native object in this session's symbol namespace.
    ///
    /// # Safety
    /// The caller must trust the object to run native code in the process.
    pub unsafe fn add_object(&self, bytes: &[u8]) -> Result<()> {
        self.add_object_in(bytes, None)
    }

    /// Link a relocatable native object with a tracker from this JITDylib.
    ///
    /// # Safety
    /// The caller must trust the object to run native code in the process.
    pub unsafe fn add_object_with_tracker(
        &self,
        bytes: &[u8],
        tracker: &ResourceTracker,
    ) -> Result<()> {
        self.add_object_in(bytes, Some(tracker))
    }

    fn add_object_in(&self, bytes: &[u8], tracker: Option<&ResourceTracker>) -> Result<()> {
        let dylib = self.dylib()?;
        let tracker = tracker
            .map(|tracker| tracker.for_dylib(&dylib))
            .transpose()?;
        dylib.add_object(bytes, tracker)
    }

    /// Define an unmangled symbol at an address in this process. Children can
    /// resolve it as a parent import.
    ///
    /// # Safety
    /// The address must have the declared kind and match the ABI of its uses.
    /// Keep the code or storage alive until all dependent native code is gone,
    /// including code retained by children and looked-up symbols after close.
    pub unsafe fn define_symbol(
        &self,
        name: &str,
        address: NonNull<c_void>,
        kind: HostSymbolKind,
    ) -> Result<()> {
        let dylib = self.dylib()?;
        if name.is_empty() {
            return Err(Error::new("a name is required for the ORC symbol"));
        }
        let name = CString::new(name)
            .map_err(|_| Error::new("LLVM symbol name contains an embedded NUL"))?;
        let mut flags = LLVMJITSymbolGenericFlagsExported as u8;
        if kind == HostSymbolKind::Function {
            flags |= LLVMJITSymbolGenericFlagsCallable as u8;
        }
        // SAFETY: the live session returns a retained name for the definition.
        unsafe {
            let name = LLVMOrcLLJITMangleAndIntern(dylib.session.raw.as_ptr(), name.as_ptr());
            llvm_result(define_address(
                dylib.raw,
                name,
                address,
                LLVMJITSymbolFlags {
                    GenericFlags: flags,
                    TargetFlags: 0,
                },
            ))
        }
    }

    pub fn lookup(&self, name: &str) -> Result<JitSymbol> {
        let dylib = self.dylib()?;
        let name = CString::new(name)
            .map_err(|_| Error::new("LLVM symbol name contains an embedded NUL"))?;
        dylib.lookup(&name)
    }

    /// End submission and lookup. Outstanding symbols and children keep their code alive.
    pub fn close(&self) {
        self.0.take();
    }
}

/// An address which retains its JITDylib and its imports.
pub struct JitSymbol {
    address: NonNull<c_void>,
    flags: LLVMJITSymbolFlags,
    compilation: Option<ResourceTracker>,
    leases: Vec<Rc<()>>,
    _dylib: Rc<Dylib>,
}

impl JitSymbol {
    /// Borrow this address. Keep the symbol alive for every use of the pointer.
    pub fn address(&self) -> NonNull<c_void> {
        self.address
    }

    /// Unload the submission retained by this symbol.
    ///
    /// # Safety
    /// The symbol must be idle. An outer `Ok` invalidates it, including when
    /// ORC reports a cleanup error. The caller must release the handle then.
    pub(crate) unsafe fn remove_compilation(&self) -> Result<Result<()>> {
        let tracker = self
            .compilation
            .as_ref()
            .ok_or_else(|| Error::new("LLVM JIT symbol does not own a compilation"))?;
        tracker.remove_with_leases(&self.leases)
    }
}

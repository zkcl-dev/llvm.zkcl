mod types;
mod variables;

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    ffi::c_char,
    ptr, slice,
};

use inkwell::context::AsContextRef;
pub use inkwell::debug_info::DWARFSourceLanguage as DebugLanguage;
use inkwell::module::FlagBehavior;
use inkwell::module::Linkage;
use inkwell::values::AsValueRef;
use llvm_sys::core::LLVMIsNewDbgInfoFormat;
use llvm_sys::debuginfo::{
    LLVMCreateDIBuilder, LLVMDIBuilderCreateCompileUnit, LLVMDIBuilderCreateDebugLocation,
    LLVMDIBuilderCreateFile, LLVMDIBuilderCreateFunction, LLVMDIBuilderCreateLexicalBlock,
    LLVMDIBuilderCreateSubroutineType, LLVMDIBuilderFinalize, LLVMDIBuilderFinalizeSubprogram,
    LLVMDIFileGetDirectory, LLVMDIFileGetFilename, LLVMDIFlagZero, LLVMDILocationGetColumn,
    LLVMDILocationGetInlinedAt, LLVMDILocationGetLine, LLVMDILocationGetScope, LLVMDIScopeGetFile,
    LLVMDISubprogramGetLine, LLVMDWARFEmissionKind, LLVMDebugMetadataVersion, LLVMDisposeDIBuilder,
    LLVMGetModuleDebugMetadataVersion, LLVMGetSubprogram, LLVMInstructionGetDebugLoc,
    LLVMInstructionSetDebugLoc, LLVMSetSubprogram,
};
use llvm_sys::prelude::{LLVMDIBuilderRef, LLVMMetadataRef};

use crate::ir::BodyGeneration;
use crate::{Error, Function, Instruction, Module, Result, c_text};

pub use types::{DebugEncoding, DebugMember, DebugType};
pub use variables::DebugVariable;

const VERSION_FLAG: &str = "Debug Info Version";

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum DebugEmission {
    #[default]
    LineTables,
    Full,
}

/// Information LLVM records in a source compilation unit.
pub struct DebugOptions<'a> {
    pub directory: &'a str,
    pub producer: &'a str,
    pub language: DebugLanguage,
    pub optimized: bool,
    pub emission: DebugEmission,
}

impl Default for DebugOptions<'_> {
    fn default() -> Self {
        Self {
            directory: "",
            producer: "zkcl-llvm-api",
            language: DebugLanguage::C,
            optimized: false,
            emission: DebugEmission::LineTables,
        }
    }
}

/// Additional metadata recorded in the LLVM source compilation unit.
#[derive(Default)]
pub struct DebugCompileUnitOptions<'a> {
    /// Compiler command-line flags to embed in the debug information.
    pub flags: &'a str,
    /// Language runtime version, for languages whose debug info uses one.
    pub runtime_version: u32,
    /// Request debug information used by source-based profiling.
    pub debug_info_for_profiling: bool,
    /// Target sysroot path.
    pub sysroot: &'a str,
    /// Target SDK name.
    pub sdk: &'a str,
}

/// An open LLVM DIBuilder for a source compilation unit.
pub struct DebugInfo {
    builder: Cell<Option<LLVMDIBuilderRef>>,
    module: Module,
    file: LLVMMetadataRef,
    optimized: bool,
    emission: DebugEmission,
    // Each entry is a distinct subprogram created by this builder. The value
    // records whether LLVM has finalized its retained nodes.
    subprograms: RefCell<HashMap<LLVMMetadataRef, bool>>,
}

/// Source name, file and optional source-language signature of a function.
#[derive(Default)]
pub struct DebugFunctionOptions<'a> {
    pub name: Option<&'a str>,
    pub file: Option<&'a DebugFile>,
    pub result: Option<&'a DebugType>,
    pub parameters: &'a [DebugType],
    pub variadic: bool,
}

impl DebugInfo {
    pub fn new(module: &Module, filename: &str, options: DebugOptions<'_>) -> Result<Self> {
        Self::with_compile_unit_options(
            module,
            filename,
            options,
            DebugCompileUnitOptions::default(),
        )
    }

    /// Open a debug compilation unit with additional LLVM metadata settings.
    pub fn with_compile_unit_options(
        module: &Module,
        filename: &str,
        options: DebugOptions<'_>,
        unit: DebugCompileUnitOptions<'_>,
    ) -> Result<Self> {
        check_filename(filename)?;
        c_text(options.directory)?;
        c_text(options.producer)?;
        c_text(unit.flags)?;
        c_text(unit.sysroot)?;
        c_text(unit.sdk)?;
        // LLVM 21's DIBuilder asserts that standard tags are at most DW_LANG_Metal.
        if matches!(
            options.language,
            DebugLanguage::Ruby | DebugLanguage::Move | DebugLanguage::Hylo
        ) {
            return Err(Error::new(format!(
                "LLVM 21 cannot create a debug compilation unit for {:?}",
                options.language
            )));
        }
        module.with_llvm(|llvm| {
            // SAFETY: the format is read from a live module. zkcl-llvm-api leaves it fixed.
            if options.emission == DebugEmission::Full
                && unsafe { LLVMIsNewDbgInfoFormat(llvm.as_mut_ptr()) } == 0
            {
                return Err(Error::new(
                    "LLVM debug value records require the new debug format",
                ));
            }
            // SAFETY: LLVM owns the module, and the version is a constant from the linked library.
            let version = unsafe { LLVMDebugMetadataVersion() };
            let has_version = llvm.get_flag(VERSION_FLAG).is_some();
            if has_version
                && unsafe { LLVMGetModuleDebugMetadataVersion(llvm.as_mut_ptr()) } != version
            {
                return Err(Error::new(
                    "incompatible LLVM debug metadata version in the module",
                ));
            }
            module.begin_debug_info()?;
            if !has_version {
                let version = llvm
                    .get_context()
                    .i32_type()
                    .const_int(version.into(), false);
                llvm.add_basic_value_flag(VERSION_FLAG, FlagBehavior::Warning, version);
            }

            // SAFETY: filenames and annotations are borrowed for the call. The builder
            // records just one compile unit and will be finished before the module drops.
            let (builder, file) = unsafe {
                let builder = LLVMCreateDIBuilder(llvm.as_mut_ptr());
                let file = LLVMDIBuilderCreateFile(
                    builder,
                    filename.as_ptr().cast(),
                    filename.len(),
                    options.directory.as_ptr().cast(),
                    options.directory.len(),
                );
                let empty = c"".as_ptr();
                let emission = match options.emission {
                    DebugEmission::LineTables => {
                        LLVMDWARFEmissionKind::LLVMDWARFEmissionKindLineTablesOnly
                    }
                    DebugEmission::Full => LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
                };
                LLVMDIBuilderCreateCompileUnit(
                    builder,
                    options.language.into(),
                    file,
                    options.producer.as_ptr().cast(),
                    options.producer.len(),
                    options.optimized.into(),
                    unit.flags.as_ptr().cast(),
                    unit.flags.len(),
                    unit.runtime_version,
                    empty,
                    0,
                    emission,
                    0,
                    0,
                    unit.debug_info_for_profiling.into(),
                    unit.sysroot.as_ptr().cast(),
                    unit.sysroot.len(),
                    unit.sdk.as_ptr().cast(),
                    unit.sdk.len(),
                );
                (builder, file)
            };
            Ok(Self {
                builder: Cell::new(Some(builder)),
                module: module.clone(),
                file,
                optimized: options.optimized,
                emission: options.emission,
                subprograms: RefCell::default(),
            })
        })
    }

    fn builder(&self) -> Result<LLVMDIBuilderRef> {
        self.builder
            .get()
            .ok_or_else(|| Error::new("LLVM debug info is already finished"))
    }

    fn require_full(&self) -> Result<()> {
        if self.emission == DebugEmission::Full {
            Ok(())
        } else {
            Err(Error::new(
                "LLVM debug types and variables require full debug information",
            ))
        }
    }

    fn subprogram_finished(&self, subprogram: LLVMMetadataRef) -> Result<bool> {
        self.subprograms
            .borrow()
            .get(&subprogram)
            .copied()
            .ok_or_else(|| Error::new("LLVM debug objects belong to different compilation units"))
    }

    /// Describe another file that contributes code to this unit.
    pub fn file(&self, filename: &str, directory: &str) -> Result<DebugFile> {
        check_filename(filename)?;
        c_text(directory)?;
        let builder = self.builder()?;
        // SAFETY: the live DIBuilder and the two borrowed strings are valid for the call.
        let raw = unsafe {
            LLVMDIBuilderCreateFile(
                builder,
                filename.as_ptr().cast(),
                filename.len(),
                directory.as_ptr().cast(),
                directory.len(),
            )
        };
        Ok(DebugFile {
            raw,
            module: self.module.clone(),
        })
    }

    fn file_ref(
        &self,
        file: Option<&DebugFile>,
        inherited: LLVMMetadataRef,
    ) -> Result<LLVMMetadataRef> {
        if let Some(file) = file {
            self.module.check(&file.module)?;
            Ok(file.raw)
        } else {
            Ok(inherited)
        }
    }

    /// Attach a source definition to a function and return its debug scope.
    pub fn attach_function(
        &self,
        function: &Function,
        line: u32,
        options: DebugFunctionOptions<'_>,
    ) -> Result<DebugScope> {
        let builder = self.builder()?;
        self.module.check(&function.module)?;
        let file = self.file_ref(options.file, self.file)?;
        let llvm = function.llvm();
        let linkage = function.name()?;
        let name = c_text(options.name.unwrap_or(&linkage))?;
        let local = matches!(llvm.get_linkage(), Linkage::Internal | Linkage::Private);
        if function.subprogram().is_some() {
            return Err(Error::new("the LLVM function already has debug info"));
        }
        let mut signature = Vec::new();
        if options.result.is_some() || !options.parameters.is_empty() || options.variadic {
            self.require_full()?;
            signature.push(
                options
                    .result
                    .map(|result| result.in_module(&self.module))
                    .transpose()?
                    .unwrap_or(ptr::null_mut()),
            );
            for parameter in options.parameters {
                signature.push(parameter.in_module(&self.module)?);
            }
            if options.variadic {
                signature.push(ptr::null_mut());
            }
        }
        let count = u32::try_from(signature.len())
            .map_err(|_| Error::new("too many LLVM debug parameter types"))?;
        // SAFETY: all metadata operands and the function belong to the current module.
        let raw = unsafe {
            let signature = LLVMDIBuilderCreateSubroutineType(
                builder,
                file,
                signature.as_mut_ptr(),
                count,
                LLVMDIFlagZero,
            );
            let raw = LLVMDIBuilderCreateFunction(
                builder,
                file,
                name.as_ptr().cast(),
                name.len(),
                linkage.as_ptr().cast(),
                linkage.len(),
                file,
                line,
                signature,
                local.into(),
                1,
                line,
                LLVMDIFlagZero,
                self.optimized.into(),
            );
            LLVMSetSubprogram(llvm.as_value_ref(), raw);
            raw
        };
        self.subprograms.borrow_mut().insert(raw, false);
        Ok(DebugScope {
            raw,
            subprogram: raw,
            file,
            line,
            function: function.clone(),
            body: function.body_generation(),
        })
    }

    pub fn lexical_block(
        &self,
        scope: &DebugScope,
        line: u32,
        column: u16,
        file: Option<&DebugFile>,
    ) -> Result<DebugScope> {
        scope.check_live()?;
        let builder = self.builder()?;
        self.subprogram_finished(scope.subprogram)?;
        let file = self.file_ref(file, scope.file)?;
        // SAFETY: the scope and file are live metadata in the builder's module.
        let raw = unsafe {
            LLVMDIBuilderCreateLexicalBlock(builder, scope.raw, file, line, column.into())
        };
        Ok(DebugScope {
            raw,
            subprogram: scope.subprogram,
            file,
            line,
            function: scope.function.clone(),
            body: scope.body.clone(),
        })
    }

    /// Finalize the debug subprogram for a function created by this builder.
    /// Further variable declarations for the function are rejected.
    /// The function's existing variables can still be used in debug records.
    pub fn finish_function(&self, function: &Function) -> Result<()> {
        let builder = self.builder()?;
        self.module.check(&function.module)?;
        let subprogram = function
            .subprogram()
            .ok_or_else(|| Error::new("the LLVM function has no debug info"))?;
        if !self.subprogram_finished(subprogram)? {
            // SAFETY: this distinct subprogram was created by the live builder.
            unsafe { LLVMDIBuilderFinalizeSubprogram(builder, subprogram) };
            self.subprograms.borrow_mut().insert(subprogram, true);
        }
        Ok(())
    }

    /// Resolve pending nodes and close the builder. Repeated calls are harmless.
    pub fn finish(&self) {
        if let Some(builder) = self.builder.take() {
            // SAFETY: this is the sole owner and the module is still live.
            unsafe {
                LLVMDIBuilderFinalize(builder);
                LLVMDisposeDIBuilder(builder);
            }
            self.subprograms.take();
            self.module.finish_debug_info();
        }
    }
}

impl Drop for DebugInfo {
    fn drop(&mut self) {
        self.finish();
    }
}

fn check_filename(filename: &str) -> Result<()> {
    if filename.is_empty() {
        return Err(Error::new(
            "a source filename is required for LLVM debug info",
        ));
    }
    c_text(filename)?;
    Ok(())
}

pub struct DebugFile {
    raw: LLVMMetadataRef,
    module: Module,
}

impl DebugFile {
    fn from_raw(raw: LLVMMetadataRef, module: &Module) -> Option<Self> {
        (!raw.is_null()).then(|| Self {
            raw,
            module: module.clone(),
        })
    }

    pub fn filename(&self) -> Result<String> {
        self.text(LLVMDIFileGetFilename)
    }

    pub fn directory(&self) -> Result<String> {
        self.text(LLVMDIFileGetDirectory)
    }

    fn text(
        &self,
        get: unsafe extern "C" fn(LLVMMetadataRef, *mut u32) -> *const c_char,
    ) -> Result<String> {
        let mut len = 0;
        // SAFETY: the module keeps the file and its sized string live while copied.
        let bytes = unsafe {
            let ptr = get(self.raw, &mut len);
            if len == 0 {
                return Ok(String::new());
            }
            slice::from_raw_parts(ptr.cast::<u8>(), len as usize)
        };
        Ok(std::str::from_utf8(bytes)
            .map_err(|_| Error::new("LLVM debug file text is not UTF-8"))?
            .to_owned())
    }
}

pub struct DebugScope {
    raw: LLVMMetadataRef,
    subprogram: LLVMMetadataRef,
    file: LLVMMetadataRef,
    line: u32,
    function: Function,
    body: Option<BodyGeneration>,
}

impl DebugScope {
    pub(crate) fn check_live(&self) -> Result<()> {
        if let Some(body) = &self.body {
            body.check_live()?;
        }
        Ok(())
    }

    /// The source line of the subprogram or lexical block. Zero denotes an unknown line.
    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn file(&self) -> Option<DebugFile> {
        DebugFile::from_raw(self.file, &self.function.module)
    }

    /// Create a source location. Use column zero if the column is unknown.
    pub fn location(&self, line: u32, column: u16) -> DebugLocation {
        self.create_location(line, column, ptr::null_mut(), &self.function)
    }

    /// Describe code from this scope inlined at a source location in the same module.
    pub fn inlined_location(
        &self,
        line: u32,
        column: u16,
        inlined_at: &DebugLocation,
    ) -> Result<DebugLocation> {
        self.check_live()?;
        inlined_at.check_live()?;
        self.function.module.check(&inlined_at.function.module)?;
        let mut location = self.create_location(line, column, inlined_at.raw, &inlined_at.function);
        location.bodies.extend_from_slice(&inlined_at.bodies);
        Ok(location)
    }

    fn create_location(
        &self,
        line: u32,
        column: u16,
        inlined_at: LLVMMetadataRef,
        function: &Function,
    ) -> DebugLocation {
        let context = self.function.module.context().llvm();
        // SAFETY: scopes and locations refer to the same live module and context.
        let raw = unsafe {
            LLVMDIBuilderCreateDebugLocation(
                context.as_ctx_ref(),
                line,
                column.into(),
                self.raw,
                inlined_at,
            )
        };
        DebugLocation {
            raw,
            function: function.clone(),
            bodies: self.body.clone().into_iter().collect(),
        }
    }
}

impl Function {
    fn subprogram(&self) -> Option<LLVMMetadataRef> {
        // SAFETY: the retained module owns the function and its debug metadata.
        let raw = unsafe { LLVMGetSubprogram(self.llvm().as_value_ref()) };
        (!raw.is_null()).then_some(raw)
    }

    /// Return the source scope attached to this function, including in parsed IR.
    pub fn debug_scope(&self) -> Option<DebugScope> {
        let raw = self.subprogram()?;
        // SAFETY: `raw` is the function's live DISubprogram.
        let (file, line) = unsafe { (LLVMDIScopeGetFile(raw), LLVMDISubprogramGetLine(raw)) };
        Some(DebugScope {
            raw,
            subprogram: raw,
            file,
            line,
            function: self.clone(),
            body: self.body_generation(),
        })
    }
}

pub struct DebugLocation {
    pub(crate) raw: LLVMMetadataRef,
    // Outermost caller for inlined locations.
    pub(crate) function: Function,
    bodies: Vec<BodyGeneration>,
}

impl DebugLocation {
    pub(crate) fn from_raw(raw: LLVMMetadataRef, function: &Function) -> Option<Self> {
        (!raw.is_null()).then(|| Self {
            raw,
            function: function.clone(),
            bodies: function.body_generation().into_iter().collect(),
        })
    }

    pub(crate) fn check_live(&self) -> Result<()> {
        self.bodies.iter().try_for_each(BodyGeneration::check_live)
    }

    pub(crate) fn check_for_function(&self, function: &Function) -> Result<()> {
        self.check_live()?;
        function.check(&self.function)?;
        if self.bodies.iter().any(|body| !body.permits(function)) {
            return Err(Error::new(
                "a debug location from an unfinished LLVM function body cannot be used in another function",
            ));
        }
        Ok(())
    }

    pub fn line(&self) -> u32 {
        // SAFETY: the retained function keeps the location metadata live.
        unsafe { LLVMDILocationGetLine(self.raw) }
    }

    pub fn column(&self) -> u32 {
        // SAFETY: the retained function keeps the location metadata live.
        unsafe { LLVMDILocationGetColumn(self.raw) }
    }

    pub fn file(&self) -> Option<DebugFile> {
        // SAFETY: the location has a local scope in the retained module.
        let raw = unsafe { LLVMDIScopeGetFile(LLVMDILocationGetScope(self.raw)) };
        DebugFile::from_raw(raw, &self.function.module)
    }

    pub fn inlined_at(&self) -> Option<DebugLocation> {
        // SAFETY: the retained function keeps the location and its inline chain live.
        let raw = unsafe { LLVMDILocationGetInlinedAt(self.raw) };
        (!raw.is_null()).then(|| Self {
            raw,
            function: self.function.clone(),
            bodies: self.bodies.clone(),
        })
    }
}

impl Instruction {
    pub fn debug_location(&self) -> Option<DebugLocation> {
        // SAFETY: the retained block keeps this instruction and its metadata live.
        let raw = unsafe { LLVMInstructionGetDebugLoc(self.llvm().as_value_ref()) };
        DebugLocation::from_raw(raw, self.block().function())
    }

    /// Attach a location from the instruction's function, or clear its location.
    pub fn set_debug_location(&self, location: Option<&DebugLocation>) -> Result<()> {
        self.block().check_live()?;
        let raw = if let Some(location) = location {
            location.check_for_function(self.block().function())?;
            location.raw
        } else {
            ptr::null_mut()
        };
        // SAFETY: the location is absent or belongs to the same retained function.
        unsafe { LLVMInstructionSetDebugLoc(self.llvm().as_value_ref(), raw) };
        Ok(())
    }
}

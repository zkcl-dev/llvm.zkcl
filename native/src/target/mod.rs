use std::ffi::{CStr, CString};
use std::sync::Once;
use std::{ptr, slice};

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module as LlvmModule;
use inkwell::passes::PassBuilderOptions as LlvmPassBuilderOptions;
use inkwell::targets::{
    FileType, InitializationConfig, Target, TargetData, TargetMachine as LlvmMachine, TargetTriple,
};
use inkwell::values::{AsValueRef, FunctionValue};
use llvm_sys::core::{
    LLVMDisposeMessage, LLVMGetMDString, LLVMGetModuleContext, LLVMGetModuleFlag,
    LLVMGetNamedFunctionWithLength, LLVMGetValueName2, LLVMIsDeclaration, LLVMMetadataAsValue,
};
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::target_machine::{
    LLVMCodeModel, LLVMCreateTargetMachineOptions, LLVMCreateTargetMachineWithOptions,
    LLVMDisposeTargetMachineOptions, LLVMGetTargetMachineFeatureString, LLVMRelocMode,
    LLVMTargetMachineOptionsSetABI, LLVMTargetMachineOptionsSetCPU,
    LLVMTargetMachineOptionsSetCodeGenOptLevel, LLVMTargetMachineOptionsSetCodeModel,
    LLVMTargetMachineOptionsSetFeatures, LLVMTargetMachineOptionsSetRelocMode,
};
use llvm_sys::transforms::pass_builder::{
    LLVMPassBuilderOptionsSetAAPipeline, LLVMPassBuilderOptionsSetInlinerThreshold, LLVMRunPasses,
    LLVMRunPassesOnFunction,
};

pub use inkwell::OptimizationLevel;

use crate::{Error, Function, Module, Result, Type, c_text, llvm_result};

mod layout;

pub use layout::DataLayout;
pub(crate) use layout::layouts_equal;

/// The relocation assumptions used to generate machine code.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum RelocMode {
    #[default]
    Default,
    Static,
    PIC,
    DynamicNoPic,
    Ropi,
    Rwpi,
    RopiRwpi,
}

impl From<RelocMode> for LLVMRelocMode {
    fn from(mode: RelocMode) -> Self {
        match mode {
            RelocMode::Default => Self::LLVMRelocDefault,
            RelocMode::Static => Self::LLVMRelocStatic,
            RelocMode::PIC => Self::LLVMRelocPIC,
            RelocMode::DynamicNoPic => Self::LLVMRelocDynamicNoPic,
            RelocMode::Ropi => Self::LLVMRelocROPI,
            RelocMode::Rwpi => Self::LLVMRelocRWPI,
            RelocMode::RopiRwpi => Self::LLVMRelocROPI_RWPI,
        }
    }
}

/// The addressing assumptions used to generate machine code.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum CodeModel {
    #[default]
    Default,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
    JitDefault,
}

impl From<CodeModel> for LLVMCodeModel {
    fn from(model: CodeModel) -> Self {
        match model {
            CodeModel::Default => Self::LLVMCodeModelDefault,
            CodeModel::Tiny => Self::LLVMCodeModelTiny,
            CodeModel::Small => Self::LLVMCodeModelSmall,
            CodeModel::Kernel => Self::LLVMCodeModelKernel,
            CodeModel::Medium => Self::LLVMCodeModelMedium,
            CodeModel::Large => Self::LLVMCodeModelLarge,
            CodeModel::JitDefault => Self::LLVMCodeModelJITDefault,
        }
    }
}

/// Code generation settings. Empty strings select LLVM's defaults for the triple.
#[derive(Clone, Copy, Debug)]
pub struct TargetOptions<'a> {
    pub optimization: OptimizationLevel,
    /// CPU name passed to LLVM. LLVM-C cannot query the backend's CPU table.
    pub cpu: &'a str,
    /// Comma-separated LLVM subtarget features, each prefixed with `+` or `-`.
    /// LLVM resolves feature names. Select hard-float ABI features explicitly.
    pub features: &'a str,
    pub relocation: RelocMode,
    pub code_model: CodeModel,
    /// Explicit ELF ABI name supported by the selected backend.
    pub abi: &'a str,
}

impl Default for TargetOptions<'_> {
    fn default() -> Self {
        Self {
            optimization: OptimizationLevel::Default,
            cpu: "",
            features: "",
            relocation: RelocMode::PIC,
            code_model: CodeModel::Default,
            abi: "",
        }
    }
}

/// Tuning for LLVM's new pass manager. `None` preserves LLVM's default.
/// Tuning applies when the chosen pipeline uses it. The `default<O*>` and LTO
/// aliases choose loop and SLP vectorization according to their optimization
/// level. Explicitly named passes can take parameters in the pipeline text.
#[derive(Clone, Copy, Debug, Default)]
pub struct PassOptions<'a> {
    pub verify_each: bool,
    pub debug_logging: bool,
    /// A comma-separated alias-analysis pipeline. An empty string selects no analyses.
    pub aa_pipeline: Option<&'a str>,
    pub loop_interleaving: Option<bool>,
    pub loop_vectorization: Option<bool>,
    pub slp_vectorization: Option<bool>,
    pub loop_unrolling: Option<bool>,
    pub forget_all_scev_in_loop_unroll: Option<bool>,
    pub licm_mssa_opt_cap: Option<u32>,
    pub licm_mssa_no_acc_for_promotion_cap: Option<u32>,
    pub call_graph_profile: Option<bool>,
    pub merge_functions: Option<bool>,
    pub inliner_threshold: Option<i32>,
}

impl PassOptions<'_> {
    fn with_llvm<T>(self, run: impl FnOnce(&LlvmPassBuilderOptions) -> Result<T>) -> Result<T> {
        let aa_pipeline = self
            .aa_pipeline
            .map(CString::new)
            .transpose()
            .map_err(|_| Error::new("LLVM alias-analysis pipeline contains an embedded NUL"))?;
        let options = LlvmPassBuilderOptions::create();
        options.set_verify_each(self.verify_each);
        options.set_debug_logging(self.debug_logging);
        for (flag, setter) in [
            (
                self.loop_interleaving,
                LlvmPassBuilderOptions::set_loop_interleaving as fn(&LlvmPassBuilderOptions, bool),
            ),
            (
                self.loop_vectorization,
                LlvmPassBuilderOptions::set_loop_vectorization,
            ),
            (
                self.slp_vectorization,
                LlvmPassBuilderOptions::set_loop_slp_vectorization,
            ),
            (
                self.loop_unrolling,
                LlvmPassBuilderOptions::set_loop_unrolling,
            ),
            (
                self.forget_all_scev_in_loop_unroll,
                LlvmPassBuilderOptions::set_forget_all_scev_in_loop_unroll,
            ),
            (
                self.call_graph_profile,
                LlvmPassBuilderOptions::set_call_graph_profile,
            ),
            (
                self.merge_functions,
                LlvmPassBuilderOptions::set_merge_functions,
            ),
        ] {
            if let Some(flag) = flag {
                setter(&options, flag);
            }
        }
        if let Some(cap) = self.licm_mssa_opt_cap {
            options.set_licm_mssa_opt_cap(cap);
        }
        if let Some(cap) = self.licm_mssa_no_acc_for_promotion_cap {
            options.set_licm_mssa_no_acc_for_promotion_cap(cap);
        }
        // SAFETY: LLVM borrows the AA string. It and the options remain live
        // through `run`, which constructs and executes the pipeline synchronously.
        unsafe {
            if let Some(aa_pipeline) = &aa_pipeline {
                LLVMPassBuilderOptionsSetAAPipeline(options.as_mut_ptr(), aa_pipeline.as_ptr());
            }
            if let Some(threshold) = self.inliner_threshold {
                LLVMPassBuilderOptionsSetInlinerThreshold(options.as_mut_ptr(), threshold);
            }
        }
        run(&options)
    }
}

/// A code generation target and its ABI layout.
pub struct TargetMachine {
    machine: LlvmMachine,
    triple: TargetTriple,
    data: DataLayout,
    abi: Option<Box<str>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ObjectFormat {
    Elf,
    MachO,
    Other,
}

fn object_format(backend: &[u8], triple: &str) -> ObjectFormat {
    let mut components = triple.splitn(4, '-');
    let os = components.nth(2).unwrap_or_default();
    let environment = components.next().unwrap_or_default();
    // LLVM accepts a format suffix after, or appended to, a known environment.
    for (suffix, format) in [
        ("elf", ObjectFormat::Elf),
        ("macho", ObjectFormat::MachO),
        ("coff", ObjectFormat::Other),
        ("goff", ObjectFormat::Other),
        ("xcoff", ObjectFormat::Other),
        ("wasm", ObjectFormat::Other),
        ("spirv", ObjectFormat::Other),
        ("dxcontainer", ObjectFormat::Other),
    ] {
        if environment.ends_with(suffix) {
            return format;
        }
    }
    let darwin = [
        "darwin",
        "macos",
        "ios",
        "tvos",
        "watchos",
        "visionos",
        "xros",
        "driverkit",
    ]
    .iter()
    .any(|name| os.starts_with(name));
    match backend {
        b"aarch64" | b"aarch64_32" | b"arm" | b"armeb" | b"thumb" | b"thumbeb" | b"x86"
        | b"x86-64" => {
            if darwin {
                ObjectFormat::MachO
            } else if os.starts_with("windows") || os.starts_with("uefi") {
                ObjectFormat::Other
            } else {
                ObjectFormat::Elf
            }
        }
        b"ppc32" | b"ppc64" if darwin => ObjectFormat::MachO,
        b"mipsel" if os.starts_with("windows") => ObjectFormat::Other,
        b"aarch64_be" | b"riscv32" | b"riscv64" | b"loongarch64" | b"mips" | b"mipsel"
        | b"mips64" | b"mips64el" => ObjectFormat::Elf,
        _ => ObjectFormat::Other,
    }
}

fn supports_abi(backend: &[u8], abi: &str, feature: impl Fn(&str) -> Option<bool>) -> bool {
    let enabled = |name| feature(name) == Some(true);
    match (backend, abi) {
        (b"riscv32", "ilp32") | (b"riscv64", "lp64") => !enabled("e"),
        (b"riscv32", "ilp32e") | (b"riscv64", "lp64e") => enabled("e"),
        (b"riscv32", "ilp32f") | (b"riscv64", "lp64f") => !enabled("e") && enabled("f"),
        (b"riscv32", "ilp32d") | (b"riscv64", "lp64d") => !enabled("e") && enabled("d"),
        (b"loongarch64", "lp64f") => enabled("f"),
        (b"loongarch64", "lp64d") => enabled("d"),
        (b"loongarch64", "lp64s")
        | (b"mips64" | b"mips64el", "n32" | "n64")
        | (b"mips" | b"mipsel", "o32") => true,
        _ => false,
    }
}

fn valid_target_name(name: &[u8]) -> bool {
    !name.is_empty()
        && name
            .iter()
            .all(|ch| ch.is_ascii_alphanumeric() || b"-_.".contains(ch))
}

fn check_options(target: &Target, triple: &CStr, options: TargetOptions<'_>) -> Result<()> {
    let backend = target.get_name().to_bytes();
    let triple_name = triple.to_string_lossy();
    if !options.cpu.is_empty() && !valid_target_name(options.cpu.as_bytes()) {
        return Err(Error::new(format!(
            "unsupported LLVM target CPU '{}' for target '{triple_name}'",
            options.cpu,
        )));
    }
    if !options.features.is_empty() {
        for flag in options.features.split(',') {
            if !matches!(flag.as_bytes(), [b'+' | b'-', name @ ..] if valid_target_name(name)) {
                return Err(Error::new(format!(
                    "unsupported LLVM target feature '{flag}' for target '{triple_name}'",
                )));
            }
        }
    }
    let feature = |name: &str| {
        options.features.rsplit(',').find_map(|flag| {
            let (sign, entry) = flag.split_at_checked(1)?;
            match (name, entry, sign) {
                ("f", "d", "+") => Some(true),
                ("d", "f", "-") => Some(false),
                _ => (entry == name).then_some(sign == "+"),
            }
        })
    };
    // LLVM-C has no tables for CPU and feature names. Check known direct
    // conflicts before LLVM reaches backend paths that abort the process.
    let width_conflict = match backend {
        b"x86-64" => {
            feature("64bit") == Some(false)
                || feature("64bit-mode") == Some(false)
                || feature("32bit-mode") == Some(true)
                || feature("16bit-mode") == Some(true)
                || matches!(options.cpu, "i386" | "i486" | "i586" | "i686")
        }
        b"riscv64" | b"loongarch64" => {
            feature("64bit") == Some(false)
                || feature("32bit") == Some(true)
                || options.cpu == "generic-rv32"
        }
        b"riscv32" | b"loongarch32" => {
            feature("32bit") == Some(false)
                || feature("64bit") == Some(true)
                || options.cpu == "generic-rv64"
        }
        b"mips64" | b"mips64el" => feature("gp64") == Some(false),
        _ => false,
    };
    if width_conflict {
        return Err(Error::new(format!(
            "LLVM CPU or features conflict with target '{triple_name}'",
        )));
    }
    let format = || object_format(backend, &triple_name);
    let aarch64 = matches!(backend, b"aarch64" | b"aarch64_be" | b"aarch64_32");
    let supported_model = match options.code_model {
        CodeModel::Tiny => aarch64 && format() == ObjectFormat::Elf,
        CodeModel::Kernel => matches!(backend, b"x86" | b"x86-64"),
        CodeModel::Medium => !aarch64 && !matches!(backend, b"loongarch32" | b"xcore"),
        CodeModel::Large => backend != b"loongarch32",
        CodeModel::Default | CodeModel::Small | CodeModel::JitDefault => true,
    };
    if !supported_model {
        return Err(Error::new(format!(
            "unsupported LLVM code model for target '{triple_name}'",
        )));
    }
    if options.relocation == RelocMode::DynamicNoPic && format() != ObjectFormat::MachO {
        return Err(Error::new(format!(
            "LLVM dynamic-no-pic relocation requires a Mach-O target, got '{triple_name}'",
        )));
    }
    if matches!(
        options.relocation,
        RelocMode::Ropi | RelocMode::Rwpi | RelocMode::RopiRwpi
    ) && (!matches!(backend, b"arm" | b"armeb" | b"thumb" | b"thumbeb")
        || format() != ObjectFormat::Elf)
    {
        return Err(Error::new(format!(
            "LLVM ROPI and RWPI relocation require an ARM ELF target, got '{triple_name}'",
        )));
    }
    if !options.abi.is_empty()
        && (format() != ObjectFormat::Elf || !supports_abi(backend, options.abi, feature))
    {
        return Err(Error::new(format!(
            "unsupported LLVM target ABI '{}' for target '{triple_name}'",
            options.abi,
        )));
    }
    Ok(())
}

fn check_module_riscv_abi(module: &LlvmModule<'_>, abi: &str) -> Result<()> {
    let module = module.as_mut_ptr();
    let key = c"target-abi";
    let mut len = 0;
    // SAFETY: the context, flag and any string bytes remain owned by the live module.
    let value = unsafe {
        let flag = LLVMGetModuleFlag(module, key.as_ptr(), key.to_bytes().len());
        if flag.is_null() {
            return Ok(());
        }
        let value = LLVMMetadataAsValue(LLVMGetModuleContext(module), flag);
        let text = LLVMGetMDString(value, &mut len);
        if text.is_null() {
            return Err(Error::new("LLVM module target-abi must be a string"));
        }
        slice::from_raw_parts(text.cast::<u8>(), len as usize)
    };
    if value != abi.as_bytes() {
        return Err(Error::new(format!(
            "module target ABI '{}' does not match '{abi}'",
            String::from_utf8_lossy(value),
        )));
    }
    Ok(())
}

pub(crate) fn initialize_targets() {
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        Target::initialize_all(&InitializationConfig {
            disassembler: false,
            ..Default::default()
        })
    });
}

impl TargetMachine {
    pub fn host_triple() -> String {
        LlvmMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned()
    }

    pub fn host_cpu() -> String {
        LlvmMachine::get_host_cpu_name()
            .to_string_lossy()
            .into_owned()
    }

    pub fn host_features() -> String {
        LlvmMachine::get_host_cpu_features()
            .to_string_lossy()
            .into_owned()
    }

    pub fn new(triple: &str, opt: OptimizationLevel) -> Result<Self> {
        Self::with_options(
            triple,
            TargetOptions {
                optimization: opt,
                ..Default::default()
            },
        )
    }

    pub fn with_options(triple: &str, options: TargetOptions<'_>) -> Result<Self> {
        let triple = LlvmMachine::normalize_triple(&TargetTriple::create(c_text(triple)?));
        let cpu = CString::new(options.cpu)
            .map_err(|_| Error::new("LLVM target CPU contains an embedded NUL"))?;
        let features = CString::new(options.features)
            .map_err(|_| Error::new("LLVM target features contain an embedded NUL"))?;
        let abi = CString::new(options.abi)
            .map_err(|_| Error::new("LLVM target ABI contains an embedded NUL"))?;
        initialize_targets();
        let target = Target::from_triple(&triple)?;
        check_options(&target, triple.as_str(), options)?;
        let raw_target = target.as_mut_ptr();
        let raw_triple = triple.as_str().as_ptr();
        // SAFETY: the registered target and C strings remain live across all calls.
        // LLVM copies the option strings and the machine owns the resulting settings.
        let machine = unsafe {
            let raw_options = LLVMCreateTargetMachineOptions();
            LLVMTargetMachineOptionsSetCPU(raw_options, cpu.as_ptr());
            LLVMTargetMachineOptionsSetFeatures(raw_options, features.as_ptr());
            LLVMTargetMachineOptionsSetABI(raw_options, abi.as_ptr());
            LLVMTargetMachineOptionsSetCodeGenOptLevel(raw_options, options.optimization.into());
            LLVMTargetMachineOptionsSetRelocMode(raw_options, options.relocation.into());
            LLVMTargetMachineOptionsSetCodeModel(raw_options, options.code_model.into());
            let raw_machine =
                LLVMCreateTargetMachineWithOptions(raw_target, raw_triple, raw_options);
            LLVMDisposeTargetMachineOptions(raw_options);
            if raw_machine.is_null() {
                return Err(Error::new("LLVM could not create the target machine"));
            }
            LlvmMachine::new(raw_machine)
        };
        let triple = machine.get_triple();
        let data = DataLayout::from_llvm(machine.get_target_data());
        Ok(Self {
            machine,
            triple,
            data,
            abi: (!options.abi.is_empty()).then(|| options.abi.into()),
        })
    }

    /// Select a target from the module's triple, defaulting to the host for unassigned modules.
    pub fn for_module(module: &Module, opt: OptimizationLevel) -> Result<Self> {
        let triple = module.target_triple().unwrap_or_else(Self::host_triple);
        Self::new(&triple, opt)
    }

    pub fn triple(&self) -> String {
        self.triple.as_str().to_string_lossy().into_owned()
    }

    pub fn cpu(&self) -> String {
        self.machine.get_cpu().to_string_lossy().into_owned()
    }

    /// LLVM backend registered for this machine, such as `x86-64` or `riscv64`.
    pub fn backend_name(&self) -> String {
        self.machine
            .get_target()
            .get_name()
            .to_string_lossy()
            .into_owned()
    }

    pub fn backend_description(&self) -> String {
        self.machine
            .get_target()
            .get_description()
            .to_string_lossy()
            .into_owned()
    }

    /// The explicit ABI selected at creation. `None` means LLVM chooses the default.
    pub fn abi(&self) -> Option<&str> {
        self.abi.as_deref()
    }

    pub fn features(&self) -> String {
        // SAFETY: the machine is live and LLVM transfers ownership of the
        // returned string to this caller.
        unsafe {
            let raw = LLVMGetTargetMachineFeatureString(self.machine.as_mut_ptr());
            let text = CStr::from_ptr(raw).to_string_lossy().into_owned();
            LLVMDisposeMessage(raw);
            text
        }
    }

    pub fn data_layout(&self) -> String {
        self.data.text()
    }

    pub fn target_data(&self) -> DataLayout {
        self.data.clone()
    }

    /// Assign this target to a module before lowering target-dependent IR.
    /// A conflicting triple or layout leaves the module unchanged.
    pub fn configure_module(&self, module: &Module) -> Result<()> {
        module.with_llvm(|module| {
            // LLVM's RISC-V backend treats a conflicting module flag as fatal.
            if let Some(abi) = self.abi()
                && matches!(
                    self.machine.get_target().get_name().to_bytes(),
                    b"riscv32" | b"riscv64"
                )
            {
                check_module_riscv_abi(module, abi)?;
            }
            configure_module_target(module, &self.triple, self.data.llvm())
        })
    }

    /// The space occupied by one value in an array of this type, in bytes.
    pub fn abi_size(&self, ty: &Type) -> Result<u64> {
        self.data.alloc_size(ty)
    }

    /// The target ABI alignment of this type, in bytes.
    pub fn abi_alignment(&self, ty: &Type) -> Result<u32> {
        self.data.abi_alignment(ty)
    }

    /// The byte offset of a field in a fixed-size struct.
    pub fn field_offset(&self, ty: &Type, index: u32) -> Result<u64> {
        self.data.field_offset(ty, index)
    }

    fn emit(&self, module: &Module, kind: FileType) -> Result<MemoryBuffer<'static>> {
        let copy = module.snapshot()?;
        self.configure_module(&copy)?;
        Ok(copy.with_llvm(|module| self.machine.write_to_memory_buffer(module, kind))?)
    }

    pub fn object(&self, module: &Module) -> Result<Vec<u8>> {
        Ok(self.emit(module, FileType::Object)?.as_slice().to_vec())
    }

    pub fn assembly(&self, module: &Module) -> Result<String> {
        Ok(String::from_utf8_lossy(self.emit(module, FileType::Assembly)?.as_slice()).into_owned())
    }
}

pub(crate) fn configure_module_target(
    module: &LlvmModule<'_>,
    triple: &TargetTriple,
    data: &TargetData,
) -> Result<()> {
    let current = module.get_triple();
    let has_triple = !current.as_str().is_empty();
    if has_triple && LlvmMachine::normalize_triple(&current) != *triple {
        return Err(Error::new(format!(
            "module target {} does not match {}",
            current.as_str().to_string_lossy(),
            triple.as_str().to_string_lossy(),
        )));
    }
    let module_layout = module.get_data_layout();
    let current_layout = module_layout.as_str().to_string_lossy();
    let target_layout = data.get_data_layout();
    let has_layout = !current_layout.is_empty();
    if has_layout && !layouts_equal(&current_layout, &target_layout.as_str().to_string_lossy()) {
        return Err(Error::new(format!(
            "module data layout {current_layout} does not match {}",
            target_layout.as_str().to_string_lossy(),
        )));
    }
    // Release the read access to the current layout before replacing it.
    drop(module_layout);
    if !has_triple {
        module.set_triple(triple);
    }
    if !has_layout {
        module.set_data_layout(&target_layout);
    }
    Ok(())
}

impl Module {
    pub fn target_triple(&self) -> Option<String> {
        self.with_llvm(|module| {
            let triple = module.get_triple();
            (!triple.as_str().is_empty()).then(|| triple.as_str().to_string_lossy().into_owned())
        })
    }

    /// Set the normalized target triple before lowering target-dependent IR.
    /// An empty string clears the triple.
    pub fn set_target_triple(&self, triple: &str) -> Result<()> {
        let triple = TargetTriple::create(c_text(triple)?);
        let triple = if triple.as_str().is_empty() {
            triple
        } else {
            LlvmMachine::normalize_triple(&triple)
        };
        self.with_llvm(|module| module.set_triple(&triple));
        Ok(())
    }

    pub fn data_layout(&self) -> Option<String> {
        self.with_llvm(|module| {
            let layout = module.get_data_layout();
            (!layout.as_str().is_empty()).then(|| layout.as_str().to_string_lossy().into_owned())
        })
    }

    /// Snapshot the module's current layout, including LLVM's default if unset.
    pub fn target_data(&self) -> DataLayout {
        DataLayout::from_llvm(TargetData::create(
            self.data_layout().as_deref().unwrap_or(""),
        ))
    }

    /// Set the layout before lowering target-dependent IR.
    /// An empty layout clears the module's explicit layout.
    pub fn set_target_data(&self, layout: &DataLayout) {
        let layout = layout.llvm().get_data_layout();
        self.with_llvm(|module| module.set_data_layout(&layout));
    }

    pub fn emit_object(&self, target: Option<&TargetMachine>) -> Result<Vec<u8>> {
        self.with_target_machine(target, TargetMachine::object)
    }

    pub fn emit_assembly(&self, target: Option<&TargetMachine>) -> Result<String> {
        self.with_target_machine(target, TargetMachine::assembly)
    }

    fn with_target_machine<T>(
        &self,
        target: Option<&TargetMachine>,
        emit: impl FnOnce(&TargetMachine, &Module) -> Result<T>,
    ) -> Result<T> {
        match target {
            Some(target) => emit(target, self),
            None => emit(
                &TargetMachine::for_module(self, OptimizationLevel::Default)?,
                self,
            ),
        }
    }

    /// Run a new-pass-manager pipeline on a snapshot. A module without a layout
    /// gets the chosen target's layout before any target-dependent folding.
    pub fn optimize(&self, pipeline: &str, target: Option<&TargetMachine>) -> Result<Self> {
        self.optimize_with_options(pipeline, target, PassOptions::default())
    }

    /// Run a pipeline with explicit tuning on a snapshot and verify the result.
    pub fn optimize_with_options(
        &self,
        pipeline: &str,
        target: Option<&TargetMachine>,
        options: PassOptions<'_>,
    ) -> Result<Self> {
        self.optimize_snapshot(pipeline, target, options, None)
    }

    fn optimize_snapshot(
        &self,
        pipeline: &str,
        target: Option<&TargetMachine>,
        options: PassOptions<'_>,
        function: Option<&Function>,
    ) -> Result<Self> {
        let pipeline = CString::new(pipeline)
            .map_err(|_| Error::new("LLVM pass pipeline contains an embedded NUL"))?;
        let copy = self.snapshot()?;
        let inferred;
        let target = match target {
            None if copy.with_llvm(|module| module.get_data_layout().as_str().is_empty()) => {
                inferred = TargetMachine::for_module(&copy, OptimizationLevel::Default)?;
                Some(&inferred)
            }
            target => target,
        };
        let machine = if let Some(target) = target {
            target.configure_module(&copy)?;
            target.machine.as_mut_ptr()
        } else {
            ptr::null_mut()
        };
        options.with_llvm(|options| {
            copy.with_llvm(|module| {
                let passes = pipeline.as_ptr();
                let options = options.as_mut_ptr();
                // SAFETY: the copy, pipeline, optional machine and options live
                // for these calls. Each pass manager accepts a null machine.
                unsafe {
                    match function {
                        Some(function) => {
                            let function = snapshot_function(module, function)?;
                            if LLVMIsDeclaration(function) != 0 {
                                return Err(Error::new(
                                    "cannot optimize an LLVM function declaration",
                                ));
                            }
                            llvm_result(LLVMRunPassesOnFunction(function, passes, machine, options))
                        }
                        None => llvm_result(LLVMRunPasses(
                            module.as_mut_ptr(),
                            passes,
                            machine,
                            options,
                        )),
                    }
                }
            })
        })?;
        copy.verify()?;
        Ok(copy)
    }
}

impl Function {
    /// Run a function pipeline on this definition in a fresh snapshot of its
    /// owning module. Returns the verified module. Original handles stay live.
    pub fn optimize(
        &self,
        pipeline: &str,
        target: Option<&TargetMachine>,
        options: PassOptions<'_>,
    ) -> Result<Module> {
        self.module()
            .optimize_snapshot(pipeline, target, options, Some(self))
    }
}

fn snapshot_function(module: &LlvmModule<'_>, original: &Function) -> Result<LLVMValueRef> {
    let original = original.llvm();
    let mut len = 0;
    // SAFETY: the original function owns the name bytes through this lookup.
    let name = unsafe { LLVMGetValueName2(original.as_value_ref(), &mut len) };
    let raw = if len != 0 {
        // SAFETY: the copied module and the original name bytes are both live.
        unsafe { LLVMGetNamedFunctionWithLength(module.as_mut_ptr(), name, len) }
    } else {
        // LLVM bitcode retains the relative order of unnamed functions. Named
        // intrinsics can be added or removed when the bitcode is read.
        let unnamed = |function: &FunctionValue<'_>| {
            let mut len = 0;
            // SAFETY: these function values are alive in their owning modules.
            unsafe { LLVMGetValueName2(function.as_value_ref(), &mut len) };
            len == 0
        };
        let index = std::iter::successors(original.get_previous_function(), |previous| {
            previous.get_previous_function()
        })
        .filter(unnamed)
        .count();
        module
            .get_functions()
            .filter(unnamed)
            .nth(index)
            .map_or(ptr::null_mut(), |function| function.as_value_ref())
    };
    if raw.is_null() {
        return Err(Error::new(
            "LLVM function is missing from the optimization snapshot",
        ));
    }
    Ok(raw)
}

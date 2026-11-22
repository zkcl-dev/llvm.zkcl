use std::ffi::{CStr, CString};
use std::ptr;

use inkwell::AddressSpace;
use inkwell::comdat::Comdat as LlvmComdat;
pub use inkwell::comdat::ComdatSelectionKind;
use inkwell::llvm_sys::LLVMTypeKind::LLVMArrayTypeKind;
use inkwell::llvm_sys::comdat::LLVMSetComdat;
use inkwell::llvm_sys::core::{
    LLVMGetInitializer, LLVMGetPointerAddressSpace, LLVMGetSection, LLVMGetTypeKind,
    LLVMGlobalGetValueType, LLVMIsNull, LLVMSetInitializer, LLVMSetSection, LLVMTypeOf,
};
use inkwell::llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use inkwell::module::Linkage as LlvmLinkage;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum};
pub use inkwell::values::UnnamedAddress;
use inkwell::values::{AnyValue, AsValueRef, BasicValueEnum, GlobalValue};
pub use inkwell::{DLLStorageClass, GlobalVisibility, ThreadLocalMode};

use super::{Function, Module, Type, Value, set_value_name, supported_value_type, value_name};
use crate::{Error, Result, c_text};

// LLVM 21's unsigned C API truncates this valid power-of-two alignment to zero.
pub(super) const MAX_ALIGNMENT: u64 = 1 << 32;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Linkage {
    #[default]
    External,
    Internal,
    Private,
    AvailableExternally,
    LinkOnceAny,
    LinkOnceODR,
    WeakAny,
    WeakODR,
    Appending,
    ExternalWeak,
    Common,
}

impl From<Linkage> for LlvmLinkage {
    fn from(linkage: Linkage) -> Self {
        match linkage {
            Linkage::External => Self::External,
            Linkage::Internal => Self::Internal,
            Linkage::Private => Self::Private,
            Linkage::AvailableExternally => Self::AvailableExternally,
            Linkage::LinkOnceAny => Self::LinkOnceAny,
            Linkage::LinkOnceODR => Self::LinkOnceODR,
            Linkage::WeakAny => Self::WeakAny,
            Linkage::WeakODR => Self::WeakODR,
            Linkage::Appending => Self::Appending,
            Linkage::ExternalWeak => Self::ExternalWeak,
            Linkage::Common => Self::Common,
        }
    }
}

impl Linkage {
    pub(super) fn is_local(self) -> bool {
        matches!(self, Self::Internal | Self::Private)
    }

    pub(super) fn from_llvm(linkage: LlvmLinkage) -> Self {
        match linkage {
            LlvmLinkage::External => Self::External,
            LlvmLinkage::Internal => Self::Internal,
            LlvmLinkage::Private => Self::Private,
            LlvmLinkage::AvailableExternally => Self::AvailableExternally,
            LlvmLinkage::LinkOnceAny => Self::LinkOnceAny,
            LlvmLinkage::LinkOnceODR => Self::LinkOnceODR,
            LlvmLinkage::WeakAny => Self::WeakAny,
            LlvmLinkage::WeakODR => Self::WeakODR,
            LlvmLinkage::Appending => Self::Appending,
            LlvmLinkage::ExternalWeak => Self::ExternalWeak,
            LlvmLinkage::Common => Self::Common,
            _ => unreachable!("LLVM 21 does not return obsolete linkage kinds"),
        }
    }

    fn check_declaration(self, declaration: bool) -> Result<()> {
        if declaration && !matches!(self, Self::External | Self::ExternalWeak) {
            return Err(Error::new("LLVM linkage requires a definition"));
        }
        if !declaration && self == Self::ExternalWeak {
            return Err(Error::new(
                "external weak LLVM linkage requires a declaration",
            ));
        }
        Ok(())
    }

    pub(crate) fn for_function(self) -> Result<LlvmLinkage> {
        if matches!(self, Self::Appending | Self::Common) {
            return Err(Error::new("this LLVM linkage is unavailable for functions"));
        }
        Ok(self.into())
    }
}

impl Function {
    pub fn is_declaration(&self) -> bool {
        self.llvm().as_global_value().is_declaration()
    }

    pub fn linkage(&self) -> Linkage {
        Linkage::from_llvm(self.llvm().get_linkage())
    }

    pub fn set_linkage(&self, linkage: Linkage) -> Result<()> {
        let function = self.llvm();
        let llvm_linkage = linkage.for_function()?;
        linkage.check_declaration(self.is_declaration())?;
        check_comdat_linkage(function.as_global_value(), linkage)?;
        function.set_linkage(llvm_linkage);
        Ok(())
    }

    pub fn visibility(&self) -> GlobalVisibility {
        self.llvm().as_global_value().get_visibility()
    }

    pub fn set_visibility(&self, visibility: GlobalVisibility) -> Result<()> {
        set_visibility(self.llvm().as_global_value(), visibility)
    }

    pub fn dll_storage_class(&self) -> DLLStorageClass {
        self.llvm().as_global_value().get_dll_storage_class()
    }

    pub fn set_dll_storage_class(&self, class: DLLStorageClass) -> Result<()> {
        set_dll_storage_class(self.llvm().as_global_value(), class)
    }

    pub fn unnamed_address(&self) -> UnnamedAddress {
        self.llvm().as_global_value().get_unnamed_address()
    }

    pub fn set_unnamed_address(&self, address: UnnamedAddress) {
        self.llvm().as_global_value().set_unnamed_address(address);
    }

    pub fn section(&self) -> Result<Option<String>> {
        section(self.llvm().as_global_value())
    }

    pub fn set_section(&self, name: Option<&str>) -> Result<()> {
        set_section(self.llvm().as_global_value(), name)
    }

    pub fn comdat(&self) -> Option<Comdat> {
        comdat(self.llvm().as_global_value(), &self.module)
    }

    pub fn set_comdat(&self, group: Option<&Comdat>) -> Result<()> {
        set_comdat(self.llvm().as_global_value(), &self.module, group)
    }
}

/// A declaration has no initializer. A definition has an initializer.
#[derive(Default)]
pub struct GlobalOptions<'a> {
    pub initializer: Option<&'a Value>,
    pub constant: bool,
    pub linkage: Linkage,
}

/// A module-owned global variable, which may have read-only or writable storage.
#[derive(Clone)]
pub struct Global {
    raw: LLVMValueRef,
    module: Module,
}

/// A linker deduplication group owned by an LLVM module.
#[derive(Clone)]
pub struct Comdat {
    llvm: LlvmComdat,
    module: Module,
}

impl Comdat {
    pub fn selection_kind(&self) -> ComdatSelectionKind {
        self.llvm.get_selection_kind()
    }

    pub fn set_selection_kind(&self, kind: ComdatSelectionKind) {
        self.llvm.set_selection_kind(kind);
    }
}

impl Module {
    /// Add a global using LLVM's symbol naming rules. A conflicting name is uniqued.
    pub fn add_global(
        &self,
        name: &str,
        storage_type: &Type,
        options: GlobalOptions<'_>,
    ) -> Result<Global> {
        self.add_global_with_address_space(name, storage_type, None, options)
    }

    /// Add a global with an explicit pointer address space.
    pub fn add_global_in(
        &self,
        name: &str,
        storage_type: &Type,
        address_space: u32,
        options: GlobalOptions<'_>,
    ) -> Result<Global> {
        let address_space = AddressSpace::try_from(address_space)
            .map_err(|_| Error::new("LLVM address space must fit in 24 bits"))?;
        self.add_global_with_address_space(name, storage_type, Some(address_space), options)
    }

    fn add_global_with_address_space(
        &self,
        name: &str,
        storage_type: &Type,
        address_space: Option<AddressSpace>,
        options: GlobalOptions<'_>,
    ) -> Result<Global> {
        let name = c_text(name)?;
        let ty = storage_type.basic_in(self.context())?;
        if !ty.is_sized() {
            return Err(Error::new("global storage requires a sized LLVM type"));
        }
        let ty = ty.as_type_ref();
        let initializer = options
            .initializer
            .map(|value| initializer_in(self, ty, value).map(|value| value.as_value_ref()))
            .transpose()?;
        check_global_linkage(options.linkage, ty, initializer, options.constant)?;
        Ok(self.with_llvm(|module| {
            // SAFETY: the storage type was checked in the module's context.
            let ty = unsafe { BasicTypeEnum::new(ty) };
            let global = module.add_global(ty, address_space, name);
            global.set_linkage(options.linkage.into());
            global.set_constant(options.constant);
            if let Some(initializer) = initializer {
                // SAFETY: the initializer is live and matches the type and module.
                global.set_initializer(&unsafe { BasicValueEnum::new(initializer) });
            }
            self.global(global)
        }))
    }

    /// Get or create a named COMDAT. Existing groups retain their selection kind.
    pub fn get_or_insert_comdat(&self, name: &str) -> Result<Comdat> {
        let name = c_text(name)?;
        if name.is_empty() {
            return Err(Error::new("an LLVM COMDAT needs a name"));
        }
        let llvm = self.with_llvm(|module| module.get_or_insert_comdat(name));
        Ok(Comdat {
            llvm,
            module: self.clone(),
        })
    }

    pub fn find_global(&self, name: &str) -> Result<Option<Global>> {
        let name = c_text(name)?;
        Ok(self.with_llvm(|module| module.get_global(name).map(|value| self.global(value))))
    }

    pub fn globals(&self) -> impl Iterator<Item = Global> {
        std::iter::successors(self.first_global(), Global::next_global)
    }

    pub(crate) fn first_global(&self) -> Option<Global> {
        self.with_llvm(|module| module.get_first_global().map(|value| self.global(value)))
    }

    pub(super) fn global(&self, value: GlobalValue<'_>) -> Global {
        Global {
            raw: value.as_value_ref(),
            module: self.clone(),
        }
    }
}

impl Global {
    fn llvm(&self) -> GlobalValue<'_> {
        // SAFETY: this global cannot be deleted through the live module's API.
        unsafe { GlobalValue::new(self.raw) }
    }

    fn storage_type_ref(&self) -> LLVMTypeRef {
        // SAFETY: the retained module keeps the global and its storage type live.
        unsafe { LLVMGlobalGetValueType(self.raw) }
    }

    /// Test whether two handles refer to the same global variable.
    pub fn same_as(&self, other: &Self) -> bool {
        self.raw == other.raw
    }

    /// Return the actual LLVM name, including any uniquing suffix.
    pub fn name(&self) -> Result<String> {
        value_name(self.raw)
    }

    /// Rename this global using LLVM's symbol uniquing rules.
    pub fn set_name(&self, name: &str) -> Result<()> {
        // SAFETY: the retained module owns this global.
        unsafe { set_value_name(self.raw, name) }
    }

    /// Return the element type stored at the global's address.
    pub fn storage_type(&self) -> Result<Type> {
        let raw = self.storage_type_ref();
        supported_value_type(raw)?;
        Ok(self.module.context().type_ref(raw))
    }

    pub(crate) fn next_global(&self) -> Option<Self> {
        self.llvm()
            .get_next_global()
            .map(|value| self.module.global(value))
    }

    /// The opaque pointer to this global can be used in its module's IR.
    pub fn address(&self) -> Value {
        Value::constant(self.raw, self.module.context(), Some(&self.module))
    }

    pub fn address_space(&self) -> u32 {
        // SAFETY: the live global has an LLVM pointer type.
        unsafe { LLVMGetPointerAddressSpace(LLVMTypeOf(self.raw)) }
    }

    pub fn is_declaration(&self) -> bool {
        self.llvm().is_declaration()
    }

    pub fn is_constant(&self) -> bool {
        self.llvm().is_constant()
    }

    pub fn set_constant(&self, constant: bool) -> Result<()> {
        if constant && self.linkage() == Linkage::Common {
            return Err(Error::new("common LLVM globals must be writable"));
        }
        self.llvm().set_constant(constant);
        Ok(())
    }

    pub fn is_externally_initialized(&self) -> bool {
        self.llvm().is_externally_initialized()
    }

    pub fn set_externally_initialized(&self, externally_initialized: bool) {
        self.llvm()
            .set_externally_initialized(externally_initialized);
    }

    pub fn thread_local_mode(&self) -> Option<ThreadLocalMode> {
        self.llvm().get_thread_local_mode()
    }

    pub fn set_thread_local_mode(&self, mode: Option<ThreadLocalMode>) {
        self.llvm().set_thread_local_mode(mode);
    }

    pub fn linkage(&self) -> Linkage {
        Linkage::from_llvm(self.llvm().get_linkage())
    }

    pub fn set_linkage(&self, linkage: Linkage) -> Result<()> {
        let initializer = self.initializer_ref();
        check_global_linkage(
            linkage,
            self.storage_type_ref(),
            initializer,
            self.is_constant(),
        )?;
        let global = self.llvm();
        check_comdat_linkage(global, linkage)?;
        global.set_linkage(linkage.into());
        Ok(())
    }

    pub fn visibility(&self) -> GlobalVisibility {
        self.llvm().get_visibility()
    }

    pub fn set_visibility(&self, visibility: GlobalVisibility) -> Result<()> {
        set_visibility(self.llvm(), visibility)
    }

    pub fn dll_storage_class(&self) -> DLLStorageClass {
        self.llvm().get_dll_storage_class()
    }

    pub fn set_dll_storage_class(&self, class: DLLStorageClass) -> Result<()> {
        set_dll_storage_class(self.llvm(), class)
    }

    pub fn section(&self) -> Result<Option<String>> {
        section(self.llvm())
    }

    pub fn set_section(&self, name: Option<&str>) -> Result<()> {
        set_section(self.llvm(), name)
    }

    pub fn comdat(&self) -> Option<Comdat> {
        comdat(self.llvm(), &self.module)
    }

    pub fn set_comdat(&self, group: Option<&Comdat>) -> Result<()> {
        set_comdat(self.llvm(), &self.module, group)
    }

    pub fn unnamed_address(&self) -> UnnamedAddress {
        self.llvm().get_unnamed_address()
    }

    pub fn set_unnamed_address(&self, address: UnnamedAddress) {
        self.llvm().set_unnamed_address(address);
    }

    /// Explicit storage alignment in bytes, or zero when unspecified.
    pub fn alignment(&self) -> u64 {
        let global = self.llvm();
        let alignment = global.get_alignment();
        if alignment == 0 && prints_max_alignment(global.print_to_string().to_bytes()) {
            MAX_ALIGNMENT
        } else {
            alignment.into()
        }
    }

    /// Set a power-of-two storage alignment up to 2^31 bytes. Zero clears it.
    pub fn set_alignment(&self, alignment: u64) -> Result<()> {
        let bytes = alignment_bytes(alignment)?;
        if bytes != 0 && !bytes.is_power_of_two() {
            return Err(Error::new("LLVM alignment must be a power of two"));
        }
        self.llvm().set_alignment(bytes);
        Ok(())
    }

    pub fn initializer(&self) -> Result<Option<Value>> {
        let Some(raw) = self.initializer_ref() else {
            return Ok(None);
        };
        supported_value_type(unsafe { LLVMTypeOf(raw) })?;
        Ok(Some(Value::constant(
            raw,
            self.module.context(),
            Some(&self.module),
        )))
    }

    /// Set an initializer with the declared storage type and valid module ownership.
    pub fn set_initializer(&self, value: &Value) -> Result<()> {
        if self.dll_storage_class() == DLLStorageClass::Import {
            return Err(Error::new("DLL import requires an LLVM declaration"));
        }
        let ty = self.storage_type_ref();
        let value = initializer_in(&self.module, ty, value)?;
        check_global_linkage(
            self.linkage(),
            ty,
            Some(value.as_value_ref()),
            self.is_constant(),
        )?;
        self.llvm().set_initializer(&value);
        Ok(())
    }

    /// Turn an externally linked definition back into a declaration.
    pub fn clear_initializer(&self) -> Result<()> {
        self.linkage().check_declaration(true)?;
        if self.llvm().get_comdat().is_some() {
            return Err(Error::new("LLVM declarations cannot be in a COMDAT"));
        }
        // SAFETY: passing null removes the initializer from this live global.
        unsafe { LLVMSetInitializer(self.raw, ptr::null_mut()) };
        Ok(())
    }

    fn initializer_ref(&self) -> Option<LLVMValueRef> {
        // SAFETY: a non-null initializer is a constant referenced by this live global.
        let raw = unsafe { LLVMGetInitializer(self.raw) };
        (!raw.is_null()).then_some(raw)
    }
}

fn set_visibility(global: GlobalValue<'_>, visibility: GlobalVisibility) -> Result<()> {
    if visibility != GlobalVisibility::Default {
        if Linkage::from_llvm(global.get_linkage()).is_local() {
            return Err(Error::new("LLVM local linkage requires default visibility"));
        }
        if global.get_dll_storage_class() == DLLStorageClass::Import {
            return Err(Error::new("DLL import requires default LLVM visibility"));
        }
    }
    global.set_visibility(visibility);
    Ok(())
}

fn set_dll_storage_class(global: GlobalValue<'_>, class: DLLStorageClass) -> Result<()> {
    if class != DLLStorageClass::Default && Linkage::from_llvm(global.get_linkage()).is_local() {
        return Err(Error::new(
            "LLVM local linkage requires default DLL storage",
        ));
    }
    if class == DLLStorageClass::Import {
        if !global.is_declaration() {
            return Err(Error::new("DLL import requires an LLVM declaration"));
        }
        if global.get_visibility() != GlobalVisibility::Default {
            return Err(Error::new("DLL import requires default LLVM visibility"));
        }
        if prints_dso_local(global) {
            return Err(Error::new(
                "DLL import cannot be set on a dso_local LLVM symbol",
            ));
        }
    }
    global.set_dll_storage_class(class);
    Ok(())
}

pub(super) fn prints_dso_local(global: GlobalValue<'_>) -> bool {
    // The C API has no dso_local getter. When the symbol's linkage and
    // visibility do not imply locality, the printer spells it explicitly.
    let printed = global.print_to_string();
    let mut words = printed
        .to_bytes()
        .split(|byte| *byte == b'"')
        .step_by(2)
        .flat_map(|unquoted| unquoted.split(|byte| byte.is_ascii_whitespace()))
        .filter(|word| !word.is_empty());
    words
        .find(|word| matches!(*word, b"declare" | b"="))
        .is_some()
        && words.take(2).any(|word| word == b"dso_local")
}

fn section(global: GlobalValue<'_>) -> Result<Option<String>> {
    // The section spelling describes the module's target, which can differ
    // from the host. Read the LLVM string without host-specific rewriting.
    // SAFETY: the live global owns a NUL-terminated section string, if any.
    let name = unsafe { LLVMGetSection(global.as_value_ref()) };
    if name.is_null() {
        return Ok(None);
    }
    let name = unsafe { CStr::from_ptr(name) }
        .to_str()
        .map_err(|_| Error::new("LLVM section name is not UTF-8"))?;
    Ok(Some(name.to_owned()))
}

fn set_section(global: GlobalValue<'_>, name: Option<&str>) -> Result<()> {
    let name = CString::new(name.unwrap_or_default())
        .map_err(|_| Error::new("LLVM section name contains an embedded NUL"))?;
    // SAFETY: LLVM copies this section name onto a live function or variable.
    unsafe { LLVMSetSection(global.as_value_ref(), name.as_ptr()) };
    Ok(())
}

fn comdat(global: GlobalValue<'_>, module: &Module) -> Option<Comdat> {
    global.get_comdat().map(|llvm| Comdat {
        llvm,
        module: module.clone(),
    })
}

fn comdat_linkage(linkage: Linkage) -> Result<()> {
    if matches!(linkage, Linkage::Common | Linkage::AvailableExternally) {
        return Err(Error::new("this LLVM linkage cannot be used with a COMDAT"));
    }
    Ok(())
}

fn check_comdat_linkage(global: GlobalValue<'_>, linkage: Linkage) -> Result<()> {
    if global.get_comdat().is_some() {
        comdat_linkage(linkage)?;
    }
    Ok(())
}

fn set_comdat(global: GlobalValue<'_>, module: &Module, group: Option<&Comdat>) -> Result<()> {
    let group = if let Some(group) = group {
        group.module.check(module)?;
        if global.is_declaration() {
            return Err(Error::new("LLVM declarations cannot be in a COMDAT"));
        }
        comdat_linkage(Linkage::from_llvm(global.get_linkage()))?;
        group.llvm.as_mut_ptr()
    } else {
        ptr::null_mut()
    };
    // SAFETY: the group and global are live and belong to the same module.
    unsafe { LLVMSetComdat(global.as_value_ref(), group) };
    Ok(())
}

pub(super) fn alignment_bytes(alignment: u64) -> Result<u32> {
    u32::try_from(alignment)
        .map_err(|_| Error::new("LLVM alignment writes support at most 2^31 bytes"))
}

fn prints_max_alignment(ir: &[u8]) -> bool {
    // The printer escapes embedded quotes as \22. Only unquoted text can
    // contain the global's comma-delimited alignment clause.
    ir.split(|byte| *byte == b'"')
        .step_by(2)
        .flat_map(|unquoted| unquoted.split(|byte| *byte == b',').skip(1))
        .any(|clause| {
            clause
                .trim_ascii_start()
                .strip_prefix(b"align 4294967296")
                .is_some_and(|suffix| suffix.is_empty() || suffix[0].is_ascii_whitespace())
        })
}

fn check_global_linkage(
    linkage: Linkage,
    ty: LLVMTypeRef,
    initializer: Option<LLVMValueRef>,
    constant: bool,
) -> Result<()> {
    linkage.check_declaration(initializer.is_none())?;
    // SAFETY: the caller supplies a live storage type and any initializer is a live constant.
    if linkage == Linkage::Appending && unsafe { LLVMGetTypeKind(ty) } != LLVMArrayTypeKind {
        return Err(Error::new("appending LLVM globals require an array type"));
    }
    if linkage == Linkage::Common {
        let initializer = initializer.expect("common linkage requires a definition");
        if constant || unsafe { LLVMIsNull(initializer) } == 0 {
            return Err(Error::new(
                "common LLVM globals must be writable and zero-initialized",
            ));
        }
    }
    Ok(())
}

fn initializer_in<'a>(
    module: &Module,
    ty: LLVMTypeRef,
    value: &'a Value,
) -> Result<BasicValueEnum<'a>> {
    value.check_constant_publication()?;
    let (value, owner) = value.constant_in(module.context())?;
    if let Some(owner) = owner {
        owner.check(module)?;
    }
    if value.get_type().as_type_ref() != ty {
        return Err(Error::new("global initializer must match the storage type"));
    }
    Ok(value)
}

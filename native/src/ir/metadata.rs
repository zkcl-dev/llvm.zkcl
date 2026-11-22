use std::ffi::{CStr, CString, c_char};
use std::{ptr, slice, str};

use inkwell::context::AsContextRef;
use llvm_sys::core::{
    LLVMAddNamedMetadataOperand, LLVMAppendModuleInlineAsm, LLVMDisposeMessage,
    LLVMGetIntTypeWidth, LLVMGetMDNodeNumOperands, LLVMGetMDNodeOperands, LLVMGetMDString,
    LLVMGetModuleFlag, LLVMGetModuleIdentifier, LLVMGetModuleInlineAsm,
    LLVMGetNamedMetadataNumOperands, LLVMGetNamedMetadataOperands, LLVMGetSourceFileName,
    LLVMIsAConstantInt, LLVMMDNodeInContext2, LLVMMDStringInContext2, LLVMMetadataAsValue,
    LLVMPrintValueToString, LLVMSetModuleIdentifier, LLVMSetModuleInlineAsm2,
    LLVMSetSourceFileName, LLVMTypeOf, LLVMValueAsMetadata,
};
use llvm_sys::debuginfo::{
    LLVMGetMetadataKind, LLVMMetadataKind, LLVMMetadataReplaceAllUsesWith,
    LLVMStripModuleDebugInfo, LLVMTemporaryMDNode,
};
use llvm_sys::prelude::{LLVMMetadataRef, LLVMModuleRef, LLVMValueRef};
use num_bigint::{BigInt, Sign};

use super::{Context, Module, Value, ValueOwner};
use crate::{Error, Result, c_text};

const FLAGS: &CStr = c"llvm.module.flags";

/// LLVM IR module flag merge behavior, as recorded in `llvm.module.flags`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum ModuleFlagBehavior {
    Error = 1,
    Warning = 2,
    Require = 3,
    Override = 4,
    Append = 5,
    AppendUnique = 6,
    Max = 7,
    Min = 8,
}

impl TryFrom<u32> for ModuleFlagBehavior {
    type Error = Error;

    fn try_from(code: u32) -> Result<Self> {
        match code {
            1 => Ok(Self::Error),
            2 => Ok(Self::Warning),
            3 => Ok(Self::Require),
            4 => Ok(Self::Override),
            5 => Ok(Self::Append),
            6 => Ok(Self::AppendUnique),
            7 => Ok(Self::Max),
            8 => Ok(Self::Min),
            _ => Err(Error::new(format!(
                "unknown LLVM module flag behavior {code}"
            ))),
        }
    }
}

#[derive(Clone)]
enum MetadataOwner {
    Context(Context),
    Module(Module),
}

/// LLVM metadata retaining the context and any module it may reference.
#[derive(Clone)]
pub struct Metadata {
    raw: LLVMMetadataRef,
    owner: MetadataOwner,
}

impl Metadata {
    fn context(&self) -> &Context {
        match &self.owner {
            MetadataOwner::Context(context) => context,
            MetadataOwner::Module(module) => module.context(),
        }
    }

    fn check_module(&self, module: &Module) -> Result<()> {
        match &self.owner {
            MetadataOwner::Context(context) => context.check(module.context()),
            MetadataOwner::Module(owner) => owner.check(module),
        }
    }

    fn kind(&self) -> LLVMMetadataKind {
        // SAFETY: every handle retains the owner of its metadata.
        unsafe { LLVMGetMetadataKind(self.raw) }
    }

    fn is_tuple(&self) -> bool {
        matches!(self.kind(), LLVMMetadataKind::LLVMMDTupleMetadataKind)
    }

    fn as_value(&self) -> LLVMValueRef {
        // SAFETY: the metadata and context are live and belong together.
        unsafe { LLVMMetadataAsValue(self.context().llvm().as_ctx_ref(), self.raw) }
    }

    pub fn ir(&self) -> String {
        let value = self.as_value();
        let text = value_ir(value);
        // LLVM canonicalizes wrapped singleton tuples to the constant or, if
        // their operand is null, to the empty tuple.
        let canonical = unsafe { LLVMValueAsMetadata(value) };
        if self.is_tuple() && canonical != self.raw {
            let operand = match unsafe { LLVMGetMetadataKind(canonical) } {
                LLVMMetadataKind::LLVMMDTupleMetadataKind => "null",
                _ => &text,
            };
            return format!("!{{{operand}}}");
        }
        text
    }

    pub fn string(&self) -> Result<Option<String>> {
        self.string_bytes().map(text_from_bytes).transpose()
    }

    fn string_bytes(&self) -> Option<&[u8]> {
        if !matches!(self.kind(), LLVMMetadataKind::LLVMMDStringMetadataKind) {
            return None;
        }
        let mut len = 0;
        // SAFETY: this is an MDString and its owner retains the returned bytes.
        let data = unsafe { LLVMGetMDString(self.as_value(), &mut len) };
        Some(if len == 0 {
            &[]
        } else {
            // SAFETY: the owner retains this MDString for the lifetime of the borrow.
            unsafe { slice::from_raw_parts(data.cast(), len as usize) }
        })
    }

    /// Return the integer in LLVM's signed IR representation. `i1 true` is 1.
    pub fn integer(&self) -> Option<BigInt> {
        let text = value_ir(self.integer_ref()?);
        let (_, literal) = text.split_once(' ')?;
        match literal {
            "true" => Some(1.into()),
            "false" => Some(0.into()),
            _ => literal.parse().ok(),
        }
    }

    fn integer_ref(&self) -> Option<LLVMValueRef> {
        if !matches!(
            self.kind(),
            LLVMMetadataKind::LLVMConstantAsMetadataMetadataKind
        ) {
            return None;
        }
        let mut value = ptr::null_mut();
        // SAFETY: for ConstantAsMetadata the C API writes exactly one value.
        unsafe { LLVMGetMDNodeOperands(self.as_value(), &mut value) };
        // SAFETY: the value is live and retained by the same owner as the metadata.
        let integer = unsafe { LLVMIsAConstantInt(value) };
        (!integer.is_null()).then_some(integer)
    }

    /// Read a tuple's operands through LLVM's metadata-as-value interface.
    /// This interface canonicalizes a referenced singleton constant tuple to
    /// its constant and a referenced singleton null tuple to an empty tuple.
    pub fn operands(&self) -> Result<Vec<Option<Metadata>>> {
        if !self.is_tuple() {
            return Err(Error::new("expected an LLVM metadata tuple"));
        }
        let value = self.as_value();
        // SAFETY: this wraps a tuple or its singleton-constant representation.
        let len = unsafe { LLVMGetMDNodeNumOperands(value) } as usize;
        if len == 0 {
            // The raw handle distinguishes !{null} from !{} when available.
            return Ok(if unsafe { LLVMValueAsMetadata(value) } == self.raw {
                Vec::new()
            } else {
                vec![None]
            });
        }
        let mut values = vec![ptr::null_mut(); len];
        // SAFETY: the buffer has exactly the length reported by LLVM.
        unsafe { LLVMGetMDNodeOperands(value, values.as_mut_ptr()) };
        Ok(values
            .into_iter()
            .map(|value| {
                (!value.is_null()).then(|| Metadata {
                    // SAFETY: the operand is a wrapped metadata value or a constant.
                    raw: unsafe { LLVMValueAsMetadata(value) },
                    owner: self.owner.clone(),
                })
            })
            .collect())
    }
}

impl Context {
    pub fn metadata_string(&self, string: &str) -> Metadata {
        // SAFETY: LLVM copies the supplied bytes using their explicit length.
        let raw = unsafe {
            LLVMMDStringInContext2(
                self.llvm().as_ctx_ref(),
                string.as_ptr().cast(),
                string.len(),
            )
        };
        Metadata {
            raw,
            owner: MetadataOwner::Context(self.clone()),
        }
    }

    pub fn metadata_node(&self, operands: &[Option<Metadata>]) -> Result<Metadata> {
        let mut module = None::<&Module>;
        for operand in operands.iter().flatten() {
            self.check(operand.context())?;
            if let MetadataOwner::Module(owner) = &operand.owner {
                if let Some(previous) = module {
                    previous.check(owner)?;
                }
                module = Some(owner);
            }
        }
        if u32::try_from(operands.len()).is_err() {
            return Err(Error::new("too many LLVM metadata operands"));
        }
        let mut operands: Vec<_> = operands
            .iter()
            .map(|operand| operand.as_ref().map_or(ptr::null_mut(), |md| md.raw))
            .collect();
        // SAFETY: all operands are null or live metadata from the same context.
        let raw = unsafe {
            LLVMMDNodeInContext2(
                self.llvm().as_ctx_ref(),
                operands.as_mut_ptr(),
                operands.len(),
            )
        };
        let owner = match module {
            Some(module) => MetadataOwner::Module(module.clone()),
            None => MetadataOwner::Context(self.clone()),
        };
        Ok(Metadata { raw, owner })
    }
}

impl Value {
    /// Wrap a constant, including a module-owned global address, as metadata.
    pub fn as_metadata(&self) -> Result<Metadata> {
        self.check_constant_publication()?;
        if !self.is_constant() {
            return Err(Error::new("LLVM module metadata requires a constant value"));
        }
        let owner = match &self.owner {
            ValueOwner::Context(context) => MetadataOwner::Context(context.clone()),
            ValueOwner::Module(module) => MetadataOwner::Module(module.clone()),
            ValueOwner::Local(_) => {
                return Err(Error::new("LLVM module metadata requires a constant value"));
            }
        };
        // SAFETY: this is a live LLVM constant, kept live by `owner`.
        let raw = unsafe { LLVMValueAsMetadata(self.raw) };
        Ok(Metadata { raw, owner })
    }
}

impl Module {
    /// Append a tuple to a named metadata list in this module.
    pub fn add_named_metadata(&self, name: &str, node: &Metadata) -> Result<()> {
        let name = metadata_name(name)?;
        if name.as_c_str() == FLAGS {
            return Err(Error::new("use add_module_flag for llvm.module.flags"));
        }
        self.append_named_node(&name, node)
    }

    fn append_named_node(&self, name: &CStr, node: &Metadata) -> Result<()> {
        node.check_module(self)?;
        if !node.is_tuple() {
            return Err(Error::new("LLVM named metadata requires a tuple"));
        }
        self.with_llvm(|module| {
            // SAFETY: the tuple and module share an owner or the same context.
            unsafe {
                let value = node.as_value();
                if LLVMValueAsMetadata(value) == node.raw {
                    LLVMAddNamedMetadataOperand(module.as_mut_ptr(), name.as_ptr(), value);
                } else {
                    // Preserve the raw singleton tuple across the value-based API.
                    // LLVMMetadataReplaceAllUsesWith consumes the temporary node.
                    let context = self.context().llvm().as_ctx_ref();
                    let temporary = LLVMTemporaryMDNode(context, ptr::null_mut(), 0);
                    let value = LLVMMetadataAsValue(context, temporary);
                    LLVMAddNamedMetadataOperand(module.as_mut_ptr(), name.as_ptr(), value);
                    LLVMMetadataReplaceAllUsesWith(temporary, node.raw);
                }
            }
        });
        Ok(())
    }

    /// Return a named metadata list in module order.
    /// The LLVM C API canonicalizes singleton constant tuples on this path, so
    /// they are exposed as uniqued tuples. A singleton null tuple reads as empty.
    pub fn named_metadata(&self, name: &str) -> Result<Vec<Metadata>> {
        self.named_nodes(&metadata_name(name)?)
    }

    pub fn named_metadata_count(&self, name: &str) -> Result<u32> {
        let name = metadata_name(name)?;
        Ok(self.with_llvm(|module| {
            // SAFETY: the module and the NUL-terminated name are live.
            unsafe { LLVMGetNamedMetadataNumOperands(module.as_mut_ptr(), name.as_ptr()) }
        }))
    }

    fn named_nodes(&self, name: &CStr) -> Result<Vec<Metadata>> {
        let values = self.with_llvm(|module| {
            // SAFETY: the result buffer has the size reported by LLVM for this name.
            unsafe {
                let len = LLVMGetNamedMetadataNumOperands(module.as_mut_ptr(), name.as_ptr());
                let mut values = vec![ptr::null_mut(); len as usize];
                if len != 0 {
                    LLVMGetNamedMetadataOperands(
                        module.as_mut_ptr(),
                        name.as_ptr(),
                        values.as_mut_ptr(),
                    );
                }
                values
            }
        });
        values
            .into_iter()
            .map(|value| {
                // SAFETY: NamedMDNode stores non-null nodes and LLVM wraps each operand.
                let node = self.metadata(unsafe { LLVMValueAsMetadata(value) });
                match node.kind() {
                    LLVMMetadataKind::LLVMConstantAsMetadataMetadataKind => {
                        self.context().metadata_node(&[Some(node)])
                    }
                    _ => Ok(node),
                }
            })
            .collect()
    }

    fn metadata(&self, raw: LLVMMetadataRef) -> Metadata {
        Metadata {
            raw,
            owner: MetadataOwner::Module(self.clone()),
        }
    }

    /// Return an LLVM flag and its merge behavior. LLVM treats a null flag value
    /// as absent through this query.
    pub fn module_flag(&self, key: &str) -> Result<Option<(ModuleFlagBehavior, Metadata)>> {
        let Some(behavior) = self.module_flag_behavior(key)? else {
            return Ok(None);
        };
        let raw = self.with_llvm(|module| {
            // SAFETY: the borrowed key explicitly supplies its byte length.
            unsafe { LLVMGetModuleFlag(module.as_mut_ptr(), key.as_ptr().cast(), key.len()) }
        });
        Ok((!raw.is_null()).then(|| (behavior, self.metadata(raw))))
    }

    fn module_flag_behavior(&self, key: &str) -> Result<Option<ModuleFlagBehavior>> {
        // The LLVM 21 C enum omits Max and Min. Inspect the behavior integer in
        // the flag tuple. The raw getter is used separately for the value itself.
        for entry in self.named_nodes(FLAGS)? {
            let fields = entry.operands()?;
            let [Some(behavior), Some(name), _] = fields.as_slice() else {
                return Err(Error::new("invalid LLVM module flag entry"));
            };
            if name.string_bytes() != Some(key.as_bytes()) {
                continue;
            }
            let code = behavior
                .integer()
                .and_then(|code| u32::try_from(code).ok())
                .ok_or_else(|| Error::new("invalid LLVM module flag behavior"))?;
            return Ok(Some(ModuleFlagBehavior::try_from(code)?));
        }
        Ok(None)
    }

    /// Record a module flag. Each flag key may be added once through this API.
    pub fn add_module_flag(
        &self,
        key: &str,
        behavior: ModuleFlagBehavior,
        value: &Metadata,
    ) -> Result<()> {
        value.check_module(self)?;
        if self.module_flag_behavior(key)?.is_some() {
            return Err(Error::new(format!(
                "LLVM module flag '{key}' already exists"
            )));
        }
        match behavior {
            ModuleFlagBehavior::Append | ModuleFlagBehavior::AppendUnique if !value.is_tuple() => {
                return Err(Error::new(
                    "LLVM Append and AppendUnique flags require a metadata tuple",
                ));
            }
            ModuleFlagBehavior::Max if value.integer_ref().is_none() => {
                return Err(Error::new("LLVM Max flags require integer metadata"));
            }
            ModuleFlagBehavior::Min => {
                let integer = value.integer_ref().ok_or_else(|| {
                    Error::new("LLVM Min flags require non-negative integer metadata")
                })?;
                // SAFETY: the value is a live LLVM ConstantInt.
                let width = unsafe { LLVMGetIntTypeWidth(LLVMTypeOf(integer)) };
                let valid = value.integer().is_some_and(|integer| {
                    integer.sign() != Sign::Minus && (width != 1 || integer.sign() == Sign::NoSign)
                });
                if !valid {
                    return Err(Error::new(
                        "LLVM Min flags require non-negative integer metadata",
                    ));
                }
            }
            ModuleFlagBehavior::Require => {
                let operands = value.operands()?;
                let [Some(name), Some(_)] = operands.as_slice() else {
                    return Err(Error::new(
                        "LLVM Require flags need a pair of a flag name and value",
                    ));
                };
                if !matches!(name.kind(), LLVMMetadataKind::LLVMMDStringMetadataKind) {
                    return Err(Error::new(
                        "LLVM Require flags need a metadata string flag name",
                    ));
                }
            }
            _ => {}
        }
        let context = self.context();
        let code = context
            .int_type(32)?
            .const_int(&(behavior as u32).into())?
            .as_metadata()?;
        let entry = context.metadata_node(&[
            Some(code),
            Some(context.metadata_string(key)),
            Some(value.clone()),
        ])?;
        self.append_named_node(FLAGS, &entry)
    }

    /// Strip debug information from a verified, independent copy of the module.
    pub fn strip_debug_info(&self) -> Result<Module> {
        let module = self.snapshot()?;
        module.with_llvm(|llvm| {
            // SAFETY: LLVM may delete debug intrinsics only from this private copy.
            unsafe { LLVMStripModuleDebugInfo(llvm.as_mut_ptr()) };
        });
        module.verify()?;
        Ok(module)
    }

    pub fn identifier(&self) -> Result<String> {
        self.module_text(LLVMGetModuleIdentifier)
    }

    pub fn set_identifier(&self, identifier: &str) -> Result<()> {
        // LLVM prints the identifier verbatim into a NUL-terminated IR message.
        self.set_module_text(c_text(identifier)?, LLVMSetModuleIdentifier);
        Ok(())
    }

    pub fn source_filename(&self) -> Result<String> {
        self.module_text(LLVMGetSourceFileName)
    }

    pub fn set_source_filename(&self, filename: &str) {
        self.set_module_text(filename, LLVMSetSourceFileName);
    }

    pub fn inline_asm(&self) -> Result<String> {
        self.module_text(LLVMGetModuleInlineAsm)
    }

    pub fn set_inline_asm(&self, asm: &str) {
        self.set_module_text(asm, LLVMSetModuleInlineAsm2);
    }

    pub fn append_inline_asm(&self, asm: &str) {
        self.set_module_text(asm, LLVMAppendModuleInlineAsm);
    }

    fn module_text(
        &self,
        get: unsafe extern "C" fn(LLVMModuleRef, *mut usize) -> *const c_char,
    ) -> Result<String> {
        self.with_llvm(|module| {
            let mut len = 0;
            // SAFETY: the module stays live and keeps the bytes valid while copied.
            unsafe { copy_text(get(module.as_mut_ptr(), &mut len), len) }
        })
    }

    fn set_module_text(
        &self,
        text: &str,
        set: unsafe extern "C" fn(LLVMModuleRef, *const c_char, usize),
    ) {
        self.with_llvm(|module| {
            // SAFETY: LLVM copies the bytes of the borrowed string in this call.
            unsafe { set(module.as_mut_ptr(), text.as_ptr().cast(), text.len()) }
        });
    }
}

fn metadata_name(name: &str) -> Result<CString> {
    if name.is_empty() {
        return Err(Error::new("LLVM named metadata needs a name"));
    }
    CString::new(name).map_err(|_| Error::new("LLVM named metadata name contains an embedded NUL"))
}

fn value_ir(value: LLVMValueRef) -> String {
    // SAFETY: each caller provides a live LLVM value. The LLVM string is owned.
    unsafe {
        let message = LLVMPrintValueToString(value);
        let text = CStr::from_ptr(message).to_string_lossy().into_owned();
        LLVMDisposeMessage(message);
        text
    }
}

unsafe fn copy_text(data: *const c_char, len: usize) -> Result<String> {
    if len == 0 {
        return Ok(String::new());
    }
    // SAFETY: the caller guarantees the bytes remain readable through this call.
    let bytes = unsafe { slice::from_raw_parts(data.cast(), len) };
    text_from_bytes(bytes)
}

fn text_from_bytes(bytes: &[u8]) -> Result<String> {
    str::from_utf8(bytes)
        .map(str::to_owned)
        .map_err(|_| Error::new("LLVM text is not valid UTF-8"))
}

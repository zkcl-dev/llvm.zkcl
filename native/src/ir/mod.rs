mod abi;
mod aliases;
mod asm;
mod cfg;
mod constants;
mod editing;
mod function_body;
mod global;
mod global_string;
mod instructions;
mod intrinsics;
mod linker;
mod memory;
mod metadata;
mod types;
mod uses;

use std::{cell::Cell, ffi::CStr, path::Path, ptr, rc::Rc, slice, str};

use inkwell::basic_block::BasicBlock;
use inkwell::context::Context as LlvmContext;
use inkwell::llvm_sys::LLVMTypeKind;
use inkwell::llvm_sys::core::{
    LLVMBasicBlockAsValue, LLVMDisposeMessage, LLVMGetIntrinsicID, LLVMGetNamedFunctionWithLength,
    LLVMGetNamedGlobalAlias, LLVMGetNamedGlobalIFunc, LLVMGetNamedGlobalWithLength, LLVMGetParam,
    LLVMGetReturnType, LLVMGetTypeKind, LLVMGetValueKind, LLVMGetValueName2, LLVMIsAConstant,
    LLVMIsAFunction, LLVMIsAInlineAsm, LLVMPrintValueToString, LLVMSetValueName2, LLVMTypeOf,
};
use inkwell::llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module as LlvmModule;
use inkwell::types::{AsTypeRef, BasicTypeEnum, FunctionType};
use inkwell::values::{AsValueRef, BasicValue, BasicValueEnum, FunctionValue, PhiValue};
use self_cell::self_cell;

use crate::{Error, Result, c_text};

pub use abi::{AttributeData, AttributeInfo, AttributeLocation, AttributeValue, TailCallKind};
pub use aliases::GlobalAlias;
pub use asm::{InlineAsmDialect, InlineAsmInfo, InlineAsmOptions};
pub use editing::Edit;
pub(crate) use function_body::{BodyGeneration, FunctionBody};
pub use global::{
    Comdat, ComdatSelectionKind, DLLStorageClass, Global, GlobalOptions, GlobalVisibility, Linkage,
    ThreadLocalMode, UnnamedAddress,
};
pub use instructions::Instruction;
pub use metadata::{Metadata, ModuleFlagBehavior};
pub(crate) use types::supported_value_type;
pub use types::{Type, TypeKind};
pub use uses::{ValueKind, ValueUse};

fn named_symbol(module: &LlvmModule<'_>, name: &str) -> LLVMValueRef {
    let name_ptr = name.as_ptr().cast();
    for lookup in [
        LLVMGetNamedFunctionWithLength,
        LLVMGetNamedGlobalWithLength,
        LLVMGetNamedGlobalAlias,
        LLVMGetNamedGlobalIFunc,
    ] {
        // SAFETY: the module is live and the name bytes are valid for the
        // explicit length of the lookup.
        let symbol = unsafe { lookup(module.as_mut_ptr(), name_ptr, name.len()) };
        if !symbol.is_null() {
            return symbol;
        }
    }
    ptr::null_mut()
}

fn ordinary_symbol_name(name: &str) -> Result<&str> {
    let name = c_text(name)?;
    if name.starts_with("llvm.") {
        return Err(Error::new("LLVM reserves names starting with llvm."));
    }
    Ok(name)
}

/// An LLVM context and the types and constants allocated in it.
#[derive(Clone)]
pub struct Context(Rc<LlvmContext>);

impl Default for Context {
    fn default() -> Self {
        Self(Rc::new(LlvmContext::create()))
    }
}

impl Context {
    pub(crate) fn llvm(&self) -> &LlvmContext {
        &self.0
    }

    pub(crate) fn check(&self, other: &Self) -> Result<()> {
        if Rc::ptr_eq(&self.0, &other.0) {
            Ok(())
        } else {
            Err(Error::new("LLVM objects belong to different contexts"))
        }
    }

    pub fn module(&self, name: &str) -> Result<Module> {
        let name = c_text(name)?;
        Ok(Module::from_storage(ModuleStorage::new(
            self.clone(),
            |ctx| ctx.llvm().create_module(name),
        )))
    }

    pub fn parse_ir(&self, text: &str) -> Result<Module> {
        let buffer = MemoryBuffer::create_from_memory_range_copy(c_text(text)?.as_bytes(), "IR");
        self.parse_module_buffer(buffer)
    }

    /// Read and verify textual LLVM IR or bitcode. Diagnostics use the input path.
    pub fn read_module(&self, path: &Path) -> Result<Module> {
        let name = path
            .to_str()
            .ok_or_else(|| Error::new("LLVM input path must be UTF-8"))?;
        c_text(name)?;
        let buffer = MemoryBuffer::create_from_file(path)
            .map_err(|error| Error::new(format!("{}: {error}", path.display())))?;
        self.parse_module_buffer(buffer)
    }

    fn parse_module_buffer(&self, buffer: MemoryBuffer<'_>) -> Result<Module> {
        let module =
            ModuleStorage::try_new(self.clone(), |ctx| ctx.llvm().create_module_from_ir(buffer))?;
        Module::verified(module)
    }

    pub fn parse_bitcode(&self, bytes: &[u8]) -> Result<Module> {
        let buffer = MemoryBuffer::create_from_memory_range_copy(bytes, "bitcode");
        Module::verified(self.read_bitcode(&buffer)?)
    }

    fn read_bitcode(&self, buffer: &MemoryBuffer<'_>) -> Result<ModuleStorage> {
        Ok(ModuleStorage::try_new(self.clone(), |ctx| {
            LlvmModule::parse_bitcode_from_buffer(buffer, ctx.llvm())
        })?)
    }
}

self_cell! {
    struct ModuleStorage {
        owner: Context,
        #[not_covariant]
        dependent: LlvmModule,
    }
}

/// Mutable IR. Operations that may remove IR work on a separate module.
#[derive(Clone)]
pub struct Module(Rc<ModuleState>);

struct ModuleState {
    storage: ModuleStorage,
    debug_info_pending: Cell<bool>,
    bodies: function_body::FunctionBodies,
}

impl Module {
    fn from_storage(storage: ModuleStorage) -> Self {
        Self(Rc::new(ModuleState {
            storage,
            debug_info_pending: Cell::new(false),
            bodies: function_body::FunctionBodies::default(),
        }))
    }

    pub(crate) fn with_llvm<T>(&self, f: impl for<'ctx> FnOnce(&LlvmModule<'ctx>) -> T) -> T {
        self.0.storage.with_dependent(|_, module| f(module))
    }

    pub fn context(&self) -> &Context {
        self.0.storage.borrow_owner()
    }

    pub(crate) fn check(&self, other: &Self) -> Result<()> {
        if Rc::ptr_eq(&self.0, &other.0) {
            Ok(())
        } else {
            Err(Error::new("LLVM objects belong to different modules"))
        }
    }

    fn verified(module: ModuleStorage) -> Result<Self> {
        module.with_dependent(|_, module| module.verify())?;
        Ok(Self::from_storage(module))
    }

    pub(crate) fn snapshot(&self) -> Result<Self> {
        // Passes like strip mutate named types, which are owned by the context.
        let storage = Context::default().read_bitcode(&self.bitcode_buffer()?)?;
        let module = Self::from_storage(storage);
        module.set_identifier(&self.identifier()?)?;
        Ok(module)
    }

    pub fn ir(&self) -> String {
        self.with_llvm(|module| module.print_to_string().to_string())
    }

    pub fn verify(&self) -> Result<()> {
        if self.has_pending_function_body() {
            return Err(Error::new(
                "finish all LLVM function bodies before verification",
            ));
        }
        if self.0.debug_info_pending.get() {
            return Err(Error::new(
                "finish the module's LLVM debug info before verification",
            ));
        }
        Ok(self.with_llvm(|module| module.verify())?)
    }

    pub(crate) fn has_pending_function_body(&self) -> bool {
        self.0.bodies.is_pending()
    }

    pub(crate) fn begin_debug_info(&self) -> Result<()> {
        if self.0.debug_info_pending.replace(true) {
            return Err(Error::new(
                "the LLVM module already has an open debug info builder",
            ));
        }
        Ok(())
    }

    pub(crate) fn finish_debug_info(&self) {
        self.0.debug_info_pending.set(false);
    }

    pub(crate) fn bitcode_buffer(&self) -> Result<MemoryBuffer<'static>> {
        self.verify()?;
        Ok(self.with_llvm(|module| module.write_bitcode_to_memory()))
    }

    pub fn bitcode(&self) -> Result<Vec<u8>> {
        Ok(self.bitcode_buffer()?.as_slice().to_vec())
    }

    pub fn add_function(&self, name: &str, ty: &Type) -> Result<Function> {
        self.add_function_with_linkage(name, ty, Linkage::External)
    }

    pub fn add_function_with_linkage(
        &self,
        name: &str,
        ty: &Type,
        linkage: Linkage,
    ) -> Result<Function> {
        let name = ordinary_symbol_name(name)?;
        let signature = ty.function_in(self.context())?.as_type_ref();
        let linkage = linkage.for_function()?;
        Ok(self.with_llvm(|module| {
            // SAFETY: this is a live function type from the module's own context.
            let ty = unsafe { FunctionType::new(signature) };
            self.function(module.add_function(name, ty, Some(linkage)))
        }))
    }

    /// Declare an exact symbol, or reuse a function with the same signature.
    /// An unspecified linkage preserves an existing function's linkage and
    /// uses external linkage for new declarations.
    pub fn declare_function(
        &self,
        name: &str,
        ty: &Type,
        linkage: Option<Linkage>,
    ) -> Result<Function> {
        let name = ordinary_symbol_name(name)?;
        if name.is_empty() {
            return Err(Error::new("an LLVM declaration needs a name"));
        }
        let signature = ty.function_in(self.context())?.as_type_ref();
        let linkage = linkage.map(Linkage::for_function).transpose()?;
        self.with_llvm(|module| {
            let raw = named_symbol(module, name);
            let function = if raw.is_null() {
                // SAFETY: this is a live function type from the module's own context.
                let ty = unsafe { FunctionType::new(signature) };
                module.add_function(name, ty, linkage)
            } else {
                // SAFETY: `raw` is a live symbol owned by the module.
                if unsafe { LLVMIsAFunction(raw) }.is_null() {
                    return Err(Error::new(format!(
                        "LLVM symbol '{name}' is already in use"
                    )));
                }
                // SAFETY: this symbol is a function as established above.
                let existing = unsafe { FunctionValue::new(raw) }.expect("LLVM function symbol");
                if existing.get_type().as_type_ref() != signature {
                    return Err(Error::new(format!(
                        "LLVM function '{name}' has a different signature"
                    )));
                }
                if linkage.is_some_and(|linkage| existing.get_linkage() != linkage) {
                    return Err(Error::new(format!(
                        "LLVM function '{name}' has different linkage"
                    )));
                }
                existing
            };
            Ok(self.function(function))
        })
    }

    pub fn find_function(&self, name: &str) -> Result<Option<Function>> {
        let name = c_text(name)?;
        Ok(self.with_llvm(|module| module.get_function(name).map(|f| self.function(f))))
    }

    pub fn functions(&self) -> impl Iterator<Item = Function> {
        std::iter::successors(self.first_function(), Function::next_function)
    }

    pub(crate) fn first_function(&self) -> Option<Function> {
        self.with_llvm(|module| module.get_first_function().map(|f| self.function(f)))
    }

    fn function(&self, function: FunctionValue<'_>) -> Function {
        Function {
            raw: function.as_value_ref(),
            module: self.clone(),
        }
    }
}

pub(crate) fn function_result(signature: FunctionType<'_>) -> Result<Option<BasicTypeEnum<'_>>> {
    // SAFETY: the live signature owns the returned type reference.
    let raw = unsafe { LLVMGetReturnType(signature.as_type_ref()) };
    if unsafe { LLVMGetTypeKind(raw) } == LLVMTypeKind::LLVMVoidTypeKind {
        return Ok(None);
    }
    supported_value_type(raw)?;
    Ok(Some(unsafe { BasicTypeEnum::new(raw) }))
}

#[derive(Clone)]
pub struct Function {
    raw: LLVMValueRef,
    pub(crate) module: Module,
}

impl Function {
    pub(crate) fn llvm(&self) -> FunctionValue<'_> {
        // SAFETY: the retained module owns this function and never deletes it.
        unsafe { FunctionValue::new(self.raw).expect("live function") }
    }

    pub(crate) fn check(&self, other: &Self) -> Result<()> {
        if self.same_as(other) {
            Ok(())
        } else {
            Err(Error::new("LLVM objects belong to different functions"))
        }
    }

    pub(crate) fn body_generation(&self) -> Option<BodyGeneration> {
        self.module.0.bodies.active(self.raw)
    }

    /// Test whether two handles refer to the same LLVM function.
    pub fn same_as(&self, other: &Self) -> bool {
        self.raw == other.raw
    }

    /// Return the name assigned by LLVM, including any uniquing suffix.
    pub fn name(&self) -> Result<String> {
        value_name(self.raw)
    }

    /// Rename a non-intrinsic function using LLVM's symbol uniquing rules.
    pub fn set_name(&self, name: &str) -> Result<()> {
        if self.body_generation().is_some() {
            return Err(Error::new(
                "cannot rename an LLVM function while its body is being emitted",
            ));
        }
        // SAFETY: the retained module owns this function.
        unsafe { set_value_name(self.raw, name) }
    }

    pub fn module(&self) -> &Module {
        &self.module
    }

    pub(crate) fn next_function(&self) -> Option<Self> {
        self.llvm()
            .get_next_function()
            .map(|f| self.module.function(f))
    }

    pub fn signature(&self) -> Type {
        self.module.context().ty(self.llvm().get_type())
    }

    /// Return a module-owned function pointer for use in IR.
    pub fn address(&self) -> Result<Value> {
        if self.llvm().get_intrinsic_id() != 0 {
            return Err(Error::new("cannot take the address of an LLVM intrinsic"));
        }
        Ok(Value::constant(
            self.raw,
            self.module.context(),
            Some(&self.module),
        ))
    }

    pub(crate) fn return_type(&self) -> Result<Option<BasicTypeEnum<'_>>> {
        function_result(self.llvm().get_type())
    }

    pub fn parameter(&self, index: u32) -> Result<Option<Value>> {
        Ok(self.parameter_raw(index)?.map(|value| self.local(value)))
    }

    pub(crate) fn named_parameter(&self, index: u32, name: &str) -> Result<Value> {
        let value = self
            .parameter_raw(index)?
            .ok_or_else(|| Error::new("LLVM parameter index is out of bounds"))?;
        value.set_name(c_text(name)?);
        Ok(self.local(value))
    }

    fn parameter_raw(&self, index: u32) -> Result<Option<BasicValueEnum<'_>>> {
        if index >= self.llvm().count_params() {
            return Ok(None);
        }
        // SAFETY: the function is live and index is within its parameter array.
        let raw = unsafe { LLVMGetParam(self.raw, index) };
        supported_value_type(unsafe { LLVMTypeOf(raw) })?;
        Ok(Some(unsafe { BasicValueEnum::new(raw) }))
    }

    pub fn append_block(&self, name: &str) -> Result<Block> {
        if self.linkage() == Linkage::ExternalWeak {
            return Err(Error::new(
                "external weak LLVM linkage requires a declaration",
            ));
        }
        if self.dll_storage_class() == DLLStorageClass::Import {
            return Err(Error::new("DLL import requires an LLVM declaration"));
        }
        let block = self
            .module
            .context()
            .llvm()
            .append_basic_block(self.llvm(), c_text(name)?);
        Ok(self.block(block))
    }

    /// Insert an empty block before an existing block of this function.
    /// Inserting before the first block makes the new block the entry.
    /// The caller must build a valid function body before verification.
    pub fn insert_block_before(&self, anchor: &Block, name: &str) -> Result<Block> {
        self.check(&anchor.function)?;
        let block = self
            .module
            .context()
            .llvm()
            .prepend_basic_block(anchor.llvm(), c_text(name)?);
        Ok(self.block(block))
    }

    pub fn blocks(&self) -> impl Iterator<Item = Block> {
        std::iter::successors(self.first_block(), Block::next_block)
    }

    pub(crate) fn first_block(&self) -> Option<Block> {
        self.llvm()
            .get_first_basic_block()
            .map(|block| self.block(block))
    }

    fn block(&self, block: BasicBlock<'_>) -> Block {
        self.block_ref(block.as_mut_ptr())
    }

    fn block_ref(&self, raw: LLVMBasicBlockRef) -> Block {
        Block {
            raw,
            function: self.clone(),
            body: self.body_generation(),
        }
    }

    pub(crate) fn local(&self, value: BasicValueEnum<'_>) -> Value {
        Value::in_function(value.as_value_ref(), self)
    }
}

#[derive(Clone)]
pub struct Block {
    raw: LLVMBasicBlockRef,
    pub(crate) function: Function,
    body: Option<function_body::BodyGeneration>,
}

impl Block {
    /// The function that contains this basic block.
    pub fn function(&self) -> &Function {
        &self.function
    }

    /// Test whether two handles refer to the same basic block.
    pub fn same_as(&self, other: &Self) -> bool {
        self.raw == other.raw
    }

    pub fn name(&self) -> Result<String> {
        // SAFETY: the retained function owns this basic block.
        value_name(unsafe { LLVMBasicBlockAsValue(self.raw) })
    }

    /// Rename this block, or clear its name with an empty string.
    pub fn set_name(&self, name: &str) -> Result<()> {
        // SAFETY: the retained function owns this basic block.
        unsafe { set_value_name(LLVMBasicBlockAsValue(self.raw), name) }
    }

    /// Whether this block ends in a terminator instruction.
    pub fn is_terminated(&self) -> bool {
        self.llvm().get_terminator().is_some()
    }

    pub fn next_block(&self) -> Option<Self> {
        self.llvm()
            .get_next_basic_block()
            .map(|block| self.function.block(block))
    }

    pub fn previous_block(&self) -> Option<Self> {
        self.llvm()
            .get_previous_basic_block()
            .map(|block| self.function.block(block))
    }

    pub(crate) fn llvm(&self) -> BasicBlock<'_> {
        // SAFETY: the retained function and its module keep the block live.
        unsafe { BasicBlock::new(self.raw).expect("live block") }
    }

    pub(crate) fn check_live(&self) -> Result<()> {
        match &self.body {
            Some(body) => body.check_live(),
            None => Ok(()),
        }
    }
}

#[derive(Clone)]
enum ValueOwner {
    Context(Context),
    Module(Module),
    Local(Function),
}

/// A value retaining the context, module, or function that owns its IR.
#[derive(Clone)]
pub struct Value {
    raw: LLVMValueRef,
    owner: ValueOwner,
    bodies: Vec<function_body::BodyGeneration>,
}

impl Value {
    /// Test whether two handles refer to the same LLVM value.
    pub fn same_as(&self, other: &Self) -> bool {
        self.raw == other.raw
    }

    pub fn is_constant(&self) -> bool {
        // SAFETY: the owner keeps the value live for the LLVM kind query.
        !unsafe { LLVMIsAConstant(self.raw) }.is_null()
    }

    /// Return the assigned LLVM name, or an empty string for an unnamed value.
    pub fn name(&self) -> Result<String> {
        value_name(self.raw)
    }

    /// Set the name of a global symbol, block, argument, or instruction result.
    pub fn set_name(&self, name: &str) -> Result<()> {
        self.check_live()?;
        if let Some(function) = self.as_function() {
            return function.set_name(name);
        }
        // SAFETY: the owner keeps this value live.
        unsafe { set_value_name(self.raw, name) }
    }

    fn in_function(raw: LLVMValueRef, function: &Function) -> Self {
        use inkwell::llvm_sys::LLVMValueKind::*;

        // SAFETY: the value comes from this live function's IR or builder.
        let owner = match unsafe { LLVMGetValueKind(raw) } {
            LLVMConstantIntValueKind
            | LLVMConstantFPValueKind
            | LLVMConstantPointerNullValueKind
            | LLVMConstantDataArrayValueKind
            | LLVMConstantDataVectorValueKind
            | LLVMConstantAggregateZeroValueKind
            | LLVMConstantTokenNoneValueKind
            | LLVMConstantTargetNoneValueKind
            | LLVMUndefValueValueKind
            | LLVMPoisonValueKind => ValueOwner::Context(function.module.context().clone()),
            // Compound constants may refer to a symbol or block in the module.
            _ if !unsafe { LLVMIsAConstant(raw) }.is_null() => {
                ValueOwner::Module(function.module.clone())
            }
            _ => ValueOwner::Local(function.clone()),
        };
        Self::with_owner(raw, owner)
    }

    fn constant(raw: LLVMValueRef, context: &Context, module: Option<&Module>) -> Self {
        let owner = match module {
            Some(module) => ValueOwner::Module(module.clone()),
            None => ValueOwner::Context(context.clone()),
        };
        Self::with_owner(raw, owner)
    }

    fn with_owner(raw: LLVMValueRef, owner: ValueOwner) -> Self {
        let bodies = match &owner {
            ValueOwner::Context(_) => Vec::new(),
            ValueOwner::Module(module) => module.0.bodies.constant_dependencies(raw),
            ValueOwner::Local(function) => function.module.0.bodies.local_dependencies(raw),
        };
        Self { raw, owner, bodies }
    }

    pub(crate) fn check_live(&self) -> Result<()> {
        self.bodies.iter().try_for_each(|body| body.check_live())
    }

    pub(crate) fn check_constant_publication(&self) -> Result<()> {
        self.check_live()?;
        if self.bodies.iter().any(|body| body.is_pending()) {
            return Err(Error::new(
                "cannot publish an address of a block in an unfinished LLVM function body",
            ));
        }
        Ok(())
    }

    fn basic(&self) -> Result<BasicValueEnum<'_>> {
        // SAFETY: the owner keeps this value and its context live.
        unsafe {
            supported_value_type(LLVMTypeOf(self.raw))?;
            if !LLVMIsAInlineAsm(self.raw).is_null() {
                return Err(Error::new("LLVM inline asm can only be used as a callee"));
            }
            if !LLVMIsAFunction(self.raw).is_null() && LLVMGetIntrinsicID(self.raw) != 0 {
                return Err(Error::new("cannot take the address of an LLVM intrinsic"));
            }
            Ok(BasicValueEnum::new(self.raw))
        }
    }

    /// Check that the reference is valid in this function, including for values
    /// without an Inkwell basic value representation.
    pub(crate) fn scoped_ref(&self, function: &Function) -> Result<LLVMValueRef> {
        self.check_live()?;
        match &self.owner {
            ValueOwner::Context(context) => context.check(function.module.context())?,
            ValueOwner::Module(module) => {
                module.check(&function.module)?;
                if self.bodies.iter().any(|body| !body.permits(function)) {
                    return Err(Error::new(
                        "an unfinished LLVM block address can only be used in its own function",
                    ));
                }
            }
            ValueOwner::Local(owner) => owner.check(function)?,
        }
        Ok(self.raw)
    }

    pub(crate) fn operand(&self, function: &Function) -> Result<BasicValueEnum<'_>> {
        self.scoped_ref(function)?;
        self.basic()
    }

    fn context(&self) -> &Context {
        match &self.owner {
            ValueOwner::Context(context) => context,
            ValueOwner::Module(module) => module.context(),
            ValueOwner::Local(owner) => owner.module.context(),
        }
    }

    fn constant_in(&self, context: &Context) -> Result<(BasicValueEnum<'_>, Option<&Module>)> {
        self.check_live()?;
        self.context().check(context)?;
        let module = match &self.owner {
            ValueOwner::Context(_) => None,
            ValueOwner::Module(module) => Some(module),
            ValueOwner::Local(_) => return Err(Error::new("expected an LLVM constant operand")),
        };
        Ok((self.basic()?, module))
    }

    pub fn ty(&self) -> Type {
        // SAFETY: the owner retains this value and the context retains its type.
        self.context().type_ref(unsafe { LLVMTypeOf(self.raw) })
    }

    pub fn ir(&self) -> String {
        // SAFETY: LLVM prints every value kind and returns an owned message.
        unsafe {
            let message = LLVMPrintValueToString(self.raw);
            let text = CStr::from_ptr(message).to_string_lossy().into_owned();
            LLVMDisposeMessage(message);
            text
        }
    }

    pub fn add_incoming(&self, value: &Self, predecessor: &Block) -> Result<()> {
        let function = &predecessor.function;
        let target = self.operand(function)?;
        let phi = target
            .as_instruction_value()
            .and_then(|instruction| PhiValue::try_from(instruction).ok())
            .ok_or_else(|| Error::new("expected an LLVM phi node"))?;
        let incoming = value.operand(function)?;
        if target.get_type() != incoming.get_type() {
            return Err(Error::new(
                "incoming value type must match the phi node type",
            ));
        }
        phi.add_incoming(&[(&incoming, predecessor.llvm())]);
        Ok(())
    }
}

fn value_name(value: LLVMValueRef) -> Result<String> {
    let mut len = 0;
    // SAFETY: LLVM returns the name and byte length for the live value.
    let name = unsafe { LLVMGetValueName2(value, &mut len) };
    if len == 0 {
        return Ok(String::new());
    }
    // SAFETY: LLVM keeps these `len` bytes readable while the value stays live.
    let bytes = unsafe { slice::from_raw_parts(name.cast(), len) };
    str::from_utf8(bytes)
        .map(str::to_owned)
        .map_err(|_| Error::new("LLVM value name is not valid UTF-8"))
}

/// The caller must keep `value` live through the call.
unsafe fn set_value_name(value: LLVMValueRef, name: &str) -> Result<()> {
    use inkwell::llvm_sys::LLVMValueKind::*;

    // SAFETY: the caller retains the value. LLVM only permits names on these
    // symbol and local value kinds, and instructions must return a value.
    let name = unsafe {
        match LLVMGetValueKind(value) {
            LLVMFunctionValueKind => {
                if LLVMGetIntrinsicID(value) != 0 {
                    return Err(Error::new("cannot rename an LLVM intrinsic"));
                }
                ordinary_symbol_name(name)?
            }
            LLVMArgumentValueKind
            | LLVMBasicBlockValueKind
            | LLVMGlobalVariableValueKind
            | LLVMGlobalAliasValueKind
            | LLVMGlobalIFuncValueKind => c_text(name)?,
            LLVMInstructionValueKind
                if LLVMGetTypeKind(LLVMTypeOf(value)) != LLVMTypeKind::LLVMVoidTypeKind =>
            {
                c_text(name)?
            }
            _ => return Err(Error::new("this LLVM value cannot be named")),
        }
    };
    // SAFETY: this is a renameable value and the name bytes are live.
    unsafe { LLVMSetValueName2(value, name.as_ptr().cast(), name.len()) };
    Ok(())
}

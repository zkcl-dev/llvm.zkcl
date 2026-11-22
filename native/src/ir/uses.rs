use std::collections::HashSet;

use inkwell::llvm_sys::core::{
    LLVMGetBasicBlockParent, LLVMGetFirstGlobal, LLVMGetFirstGlobalAlias, LLVMGetFirstGlobalIFunc,
    LLVMGetFirstUse, LLVMGetGlobalParent, LLVMGetInstructionParent, LLVMGetNextGlobal,
    LLVMGetNextGlobalAlias, LLVMGetNextGlobalIFunc, LLVMGetNextUse, LLVMGetNumOperands,
    LLVMGetOperand, LLVMGetParamParent, LLVMGetPreviousParam, LLVMGetUser, LLVMGetValueKind,
    LLVMHasPersonalityFn, LLVMHasPrefixData, LLVMHasPrologueData, LLVMIsAConstant, LLVMIsAFunction,
    LLVMIsAGlobalValue, LLVMIsAInstruction, LLVMIsAUser, LLVMValueAsBasicBlock,
};
use inkwell::llvm_sys::prelude::{LLVMModuleRef, LLVMValueRef};
use inkwell::values::{GlobalValue, InstructionValue};
pub use llvm_sys::LLVMValueKind as ValueKind;
use llvm_sys::LLVMValueKind::*;

use super::{Block, Function, Global, GlobalAlias, Instruction, Module, Value, ValueOwner};
use crate::{Error, Result};

/// A snapshot of a direct operand occurrence in a user's operand list.
#[derive(Clone)]
pub struct ValueUse {
    pub user: Value,
    pub operand_index: u32,
}

impl Value {
    /// The concrete LLVM value kind when it has a name in the C API.
    pub fn kind(&self) -> Option<ValueKind> {
        // SAFETY: the retained owner keeps this value live. LLVM's C API maps
        // C++ kinds it does not expose to the generic instruction tag.
        unsafe {
            let kind = LLVMGetValueKind(self.raw);
            if kind == LLVMInstructionValueKind && LLVMIsAInstruction(self.raw).is_null() {
                return None;
            }
            Some(kind)
        }
    }

    /// The defining instruction for an instruction result.
    pub fn as_instruction(&self) -> Option<Instruction> {
        if self.kind() != Some(LLVMInstructionValueKind) {
            return None;
        }
        let block = self.local_parent_block()?;
        // SAFETY: this is a live instruction contained by the retained block.
        Some(block.instruction(unsafe { InstructionValue::new(self.raw) }))
    }

    /// The block represented by a basic block operand.
    pub fn as_block(&self) -> Option<Block> {
        if self.kind() != Some(LLVMBasicBlockValueKind) {
            return None;
        }
        self.local_parent_block()
    }

    /// The function represented by a function value, including an intrinsic.
    pub fn as_function(&self) -> Option<Function> {
        if self.kind() != Some(LLVMFunctionValueKind) {
            return None;
        }
        Some(Function {
            raw: self.raw,
            module: self.owning_module()?.clone(),
        })
    }

    /// The enclosing function when this value is an argument.
    pub fn argument_function(&self) -> Option<Function> {
        if self.kind() != Some(LLVMArgumentValueKind) {
            return None;
        }
        Some(Function {
            // SAFETY: the retained owner keeps the argument and its parent live.
            raw: unsafe { LLVMGetParamParent(self.raw) },
            module: self.owning_module()?.clone(),
        })
    }

    /// The zero-based position when this value is a formal argument.
    pub fn argument_index(&self) -> Option<u32> {
        if self.kind() != Some(LLVMArgumentValueKind) {
            return None;
        }
        let mut argument = self.raw;
        let mut index = 0;
        loop {
            // SAFETY: the starting argument and each predecessor are live.
            argument = unsafe { LLVMGetPreviousParam(argument) };
            if argument.is_null() {
                return Some(index);
            }
            index += 1;
        }
    }

    /// The global variable represented by a global variable operand.
    pub fn as_global(&self) -> Option<Global> {
        if self.kind() != Some(LLVMGlobalVariableValueKind) {
            return None;
        }
        // SAFETY: the kind and owner establish this is a live global variable.
        Some(
            self.owning_module()?
                .global(unsafe { GlobalValue::new(self.raw) }),
        )
    }

    /// The alias represented by a global alias operand.
    pub fn as_global_alias(&self) -> Option<GlobalAlias> {
        if self.kind() != Some(LLVMGlobalAliasValueKind) {
            return None;
        }
        self.owning_module()?.global_alias(self.raw)
    }

    /// The number of LLVM User operand slots, or zero for a value with no slots.
    /// Metadata references are tracked separately by LLVM.
    pub fn operand_count(&self) -> u32 {
        // SAFETY: only a live User reaches LLVMGetNumOperands.
        unsafe {
            if LLVMIsAUser(self.raw).is_null() {
                return 0;
            }
            LLVMGetNumOperands(self.raw) as u32
        }
    }

    /// The operand at a User operand slot. Some LLVM slots can be empty.
    pub fn operand_at(&self, index: u32) -> Option<Value> {
        if index >= self.operand_count() {
            return None;
        }
        // SAFETY: self is a live User and the index is a valid slot.
        let raw = unsafe { self.operand_ref(index) }?;
        Some(match &self.owner {
            ValueOwner::Local(function) => Self::in_function(raw, function),
            owner => Self::with_owner(raw, owner.clone()),
        })
    }

    fn operand_refs(&self) -> impl Iterator<Item = (u32, LLVMValueRef)> {
        (0..self.operand_count()).filter_map(move |index| {
            // SAFETY: this range contains only valid User operand slots.
            unsafe { self.operand_ref(index) }.map(|operand| (index, operand))
        })
    }

    /// The caller passes a valid User operand index.
    unsafe fn operand_ref(&self, index: u32) -> Option<LLVMValueRef> {
        // SAFETY: the function test guards the property checks, and the caller
        // established a valid operand index for this User.
        unsafe {
            if !LLVMIsAFunction(self.raw).is_null() {
                // LLVM reserves these slots once any of the three properties
                // is present. Unused slots contain internal null pointer constants.
                let present = match index {
                    0 => LLVMHasPersonalityFn(self.raw),
                    1 => LLVMHasPrefixData(self.raw),
                    2 => LLVMHasPrologueData(self.raw),
                    _ => 1,
                };
                if present == 0 {
                    return None;
                }
            }
            let raw = LLVMGetOperand(self.raw, index);
            (!raw.is_null()).then_some(raw)
        }
    }

    /// Return the direct operand uses within a module's IR. Compound constant
    /// users belong to the result when an attached module object uses them,
    /// directly or through another constant. LLVM metadata references and
    /// PHI predecessor bookkeeping are separate. Data arrays and vectors
    /// store their element bits directly, without value operand slots.
    ///
    /// The module defaults to the owner of a module or function value. A
    /// context-only value needs an explicit module in the same context.
    /// Each result retains the user and records its operand index at query time.
    pub fn uses(&self, module: Option<&Module>) -> Result<Vec<ValueUse>> {
        let owner = self.owning_module();
        let module = module.or(owner).ok_or_else(|| {
            Error::new("querying uses of a context-owned value requires a module")
        })?;
        if let Some(owner) = owner {
            owner.check(module)?;
        } else {
            self.context().check(module.context())?;
        }

        // LLVM 21 omits use lists for ConstantData. Search the selected
        // module's operands instead of calling LLVMGetFirstUse on such a value.
        if matches!(
            self.kind(),
            Some(
                LLVMUndefValueValueKind
                    | LLVMPoisonValueKind
                    | LLVMConstantAggregateZeroValueKind
                    | LLVMConstantDataArrayValueKind
                    | LLVMConstantDataVectorValueKind
                    | LLVMConstantIntValueKind
                    | LLVMConstantFPValueKind
                    | LLVMConstantTargetNoneValueKind
                    | LLVMConstantPointerNullValueKind
                    | LLVMConstantTokenNoneValueKind
            )
        ) {
            return Ok(scan_constant_uses(self.raw, module));
        }

        module.with_llvm(|llvm| {
            let module_ref = llvm.as_mut_ptr();
            let mut uses = Vec::new();
            let mut seen = HashSet::new();
            // SAFETY: self is live and the use list stays unchanged during collection.
            for raw in unsafe { users(self.raw) }.filter(|raw| seen.insert(*raw)) {
                let owner = match user_scope(raw, module_ref)? {
                    UserScope::Instruction(function) => ValueOwner::Local(Function {
                        raw: function,
                        module: module.clone(),
                    }),
                    UserScope::Global => ValueOwner::Module(module.clone()),
                    UserScope::Constant if constant_is_used_in(raw, module_ref)? => {
                        ValueOwner::Module(module.clone())
                    }
                    UserScope::Constant | UserScope::Outside => continue,
                };
                let user = Value::with_owner(raw, owner);
                for (operand_index, operand) in user.operand_refs() {
                    if operand == self.raw {
                        uses.push(ValueUse {
                            user: user.clone(),
                            operand_index,
                        });
                    }
                }
            }
            Ok(uses)
        })
    }

    fn owning_module(&self) -> Option<&Module> {
        match &self.owner {
            ValueOwner::Context(_) => None,
            ValueOwner::Module(module) => Some(module),
            ValueOwner::Local(function) => Some(function.module()),
        }
    }

    fn local_parent_block(&self) -> Option<Block> {
        let module = self.owning_module()?;
        // SAFETY: each parent accessor is guarded by its value kind. The owning
        // module retains the value, its parent function, and any parent block.
        unsafe {
            let block = match self.kind() {
                Some(LLVMInstructionValueKind) => LLVMGetInstructionParent(self.raw),
                Some(LLVMBasicBlockValueKind) => LLVMValueAsBasicBlock(self.raw),
                _ => return None,
            };
            if block.is_null() {
                return None;
            }
            let raw = LLVMGetBasicBlockParent(block);
            if raw.is_null() {
                return None;
            }
            let function = Function {
                raw,
                module: module.clone(),
            };
            Some(function.block_ref(block))
        }
    }
}

enum UserScope {
    Instruction(LLVMValueRef),
    Global,
    Constant,
    Outside,
}

fn scan_constant_uses(source: LLVMValueRef, module: &Module) -> Vec<ValueUse> {
    let mut scan = ConstantUseScan {
        source,
        uses: Vec::new(),
        seen_constants: HashSet::new(),
        pending_constants: Vec::new(),
    };
    let owner = ValueOwner::Module(module.clone());
    module.with_llvm(|llvm| {
        type First = unsafe extern "C" fn(LLVMModuleRef) -> LLVMValueRef;
        type Next = unsafe extern "C" fn(LLVMValueRef) -> LLVMValueRef;
        let lists: [(First, Next); 3] = [
            (LLVMGetFirstGlobal, LLVMGetNextGlobal),
            (LLVMGetFirstGlobalAlias, LLVMGetNextGlobalAlias),
            (LLVMGetFirstGlobalIFunc, LLVMGetNextGlobalIFunc),
        ];
        // SAFETY: each list and its elements belong to the retained module.
        unsafe {
            for (first, next) in lists {
                let mut raw = first(llvm.as_mut_ptr());
                while !raw.is_null() {
                    scan.visit(Value::with_owner(raw, owner.clone()));
                    raw = next(raw);
                }
            }
        }
    });
    for function in module.functions() {
        scan.visit(Value::with_owner(function.raw, owner.clone()));
        let local = ValueOwner::Local(function.clone());
        for block in function.blocks() {
            for instruction in block.instructions() {
                let (_, raw) = instruction.edit_source();
                scan.visit(Value::with_owner(raw, local.clone()));
            }
        }
    }
    while let Some(raw) = scan.pending_constants.pop() {
        scan.visit(Value::with_owner(raw, owner.clone()));
    }
    scan.uses
}

struct ConstantUseScan {
    source: LLVMValueRef,
    uses: Vec<ValueUse>,
    seen_constants: HashSet<LLVMValueRef>,
    pending_constants: Vec<LLVMValueRef>,
}

impl ConstantUseScan {
    fn visit(&mut self, user: Value) {
        for (operand_index, operand) in user.operand_refs() {
            if operand == self.source {
                self.uses.push(ValueUse {
                    user: user.clone(),
                    operand_index,
                });
                continue;
            }
            // SAFETY: operand_refs returns live operands. LLVM constants are
            // Users, and global values are already visited as module roots.
            let compound = unsafe {
                !LLVMIsAConstant(operand).is_null()
                    && LLVMIsAGlobalValue(operand).is_null()
                    && LLVMGetNumOperands(operand) != 0
            };
            if compound && self.seen_constants.insert(operand) {
                self.pending_constants.push(operand);
            }
        }
    }
}

fn user_scope(user: LLVMValueRef, module: LLVMModuleRef) -> Result<UserScope> {
    // SAFETY: callers pass a live LLVM User. The parent accessors are guarded
    // by the dynamic kind and by non-null parents for detached instructions.
    unsafe {
        if !LLVMIsAInstruction(user).is_null() {
            let block = LLVMGetInstructionParent(user);
            if !block.is_null() {
                let function = LLVMGetBasicBlockParent(block);
                if !function.is_null() && LLVMGetGlobalParent(function) == module {
                    return Ok(UserScope::Instruction(function));
                }
            }
            return Ok(UserScope::Outside);
        }
        if !LLVMIsAGlobalValue(user).is_null() {
            return Ok(if LLVMGetGlobalParent(user) == module {
                UserScope::Global
            } else {
                UserScope::Outside
            });
        }
        if !LLVMIsAConstant(user).is_null() {
            return Ok(UserScope::Constant);
        }
        Err(Error::new("unsupported LLVM User kind in the use list"))
    }
}

fn constant_is_used_in(constant: LLVMValueRef, module: LLVMModuleRef) -> Result<bool> {
    let mut pending = vec![constant];
    let mut seen = HashSet::new();
    while let Some(value) = pending.pop() {
        if !seen.insert(value) {
            continue;
        }
        // SAFETY: the initial value is live and each later constant comes from
        // a live use list. The module and context stay unchanged in this walk.
        for user in unsafe { users(value) } {
            match user_scope(user, module)? {
                UserScope::Instruction(_) | UserScope::Global => return Ok(true),
                UserScope::Constant => pending.push(user),
                UserScope::Outside => (),
            }
        }
    }
    Ok(false)
}

/// The caller must keep the value and its use list live and unchanged while iterating.
unsafe fn users(value: LLVMValueRef) -> impl Iterator<Item = LLVMValueRef> {
    let mut usage = unsafe { LLVMGetFirstUse(value) };
    std::iter::from_fn(move || {
        if usage.is_null() {
            return None;
        }
        // SAFETY: each cursor points into the live use list.
        unsafe {
            let user = LLVMGetUser(usage);
            usage = LLVMGetNextUse(usage);
            Some(user)
        }
    })
}

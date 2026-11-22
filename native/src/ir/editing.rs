mod fast_math;
mod splice;
mod switch;

use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, HashSet},
    ptr::NonNull,
    rc::Rc,
    slice,
};

use inkwell::basic_block::BasicBlock;
use inkwell::module::Module as LlvmModule;
use inkwell::values::{AsValueRef, InstructionValue};
use llvm_sys::core::*;
use llvm_sys::debuginfo::{LLVMInstructionGetDebugLoc, LLVMInstructionSetDebugLoc};
use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMBuilderRef, LLVMModuleRef, LLVMValueRef};
use llvm_sys::{LLVMOpcode::*, LLVMTypeKind::*, LLVMValueKind::*};

use super::{Block, Function, Instruction, Module, ModuleStorage, Value, ValueOwner};
use crate::{Error, Result};

/// A draft edited using handles from its source module. Finishing publishes a
/// separately owned module after verification. Source handles remain live.
pub struct Edit {
    draft: RefCell<Option<Draft>>,
    source: Module,
}

struct Draft {
    detached: HashSet<LLVMValueRef>,
    identities: HashMap<LLVMValueRef, LLVMValueRef>,
    module: Module,
}

impl Module {
    /// Copy a verified module into a fresh context for independent construction.
    /// Look up existing IR on the copy and use its context to construct new IR.
    pub fn copy(&self) -> Result<Self> {
        let copy = self.snapshot()?;
        // The bitcode reader can upgrade a layout based on the target triple.
        // This copy uses the exact layout that the live module was built with.
        self.with_llvm(|source| {
            copy.with_llvm(|module| module.set_data_layout(&source.get_data_layout()));
        });
        copy.verify()?;
        Ok(copy)
    }

    /// Begin editing a verified module. The draft shares the source context
    /// and includes the module's IR at the time of this call.
    pub fn edit(&self) -> Result<Edit> {
        self.verify()?;
        let (storage, identities) = self.with_llvm(|module| {
            let source = module.as_mut_ptr();
            // SAFETY: the source is verified, and the clone uses its LLVM context.
            let clone = unsafe { LLVMCloneModule(source) };
            let storage = ModuleStorage::new(self.context().clone(), |_| unsafe {
                LlvmModule::new(clone)
            });
            (storage, snapshot_values(source, clone))
        });
        Ok(Edit {
            draft: RefCell::new(Some(Draft {
                detached: HashSet::new(),
                identities,
                module: Module::from_storage(storage),
            })),
            source: self.clone(),
        })
    }
}

impl Draft {
    // Draft handles retain the module and can derive other handles from it.
    // Operations that delete nodes or change an instruction's parent need
    // exclusive ownership, just as they did before drafts exposed handles.
    fn require_unborrowed_ir(&self) -> Result<()> {
        if Rc::strong_count(&self.module.0) == 1 {
            Ok(())
        } else {
            Err(Error::new(
                "release the LLVM draft handles before replacing, moving, or erasing IR",
            ))
        }
    }

    fn function(&self, source: &Function) -> Result<Function> {
        Ok(Function {
            raw: self.value(source.raw)?,
            module: self.module.clone(),
        })
    }

    fn operand(&self, function: &Function, operand: &Value) -> Result<LLVMValueRef> {
        let owner = match &operand.owner {
            ValueOwner::Local(function) => Some(function.module()),
            ValueOwner::Module(module) => Some(module),
            ValueOwner::Context(_) => None,
        };
        if owner.is_some_and(|owner| Rc::ptr_eq(&owner.0, &self.module.0)) {
            let value = operand.operand(&self.function(function)?)?.as_value_ref();
            self.require_attached(value)?;
            Ok(value)
        } else {
            self.value(operand.operand(function)?.as_value_ref())
        }
    }

    // The caller has already checked the source module or context of the value.
    fn value(&self, source: LLVMValueRef) -> Result<LLVMValueRef> {
        let value = self.mapped_value(source)?;
        self.require_attached(value)?;
        Ok(value)
    }

    fn mapped_value(&self, source: LLVMValueRef) -> Result<LLVMValueRef> {
        if let Some(&value) = self.identities.get(&source) {
            return Ok(value);
        }
        // Independent constants are uniqued by the context shared by the copy.
        if is_context_constant(source) {
            return Ok(source);
        }
        Err(Error::new("LLVM object is absent from this edit draft"))
    }

    fn block(&self, source: &Block) -> Result<LLVMBasicBlockRef> {
        // SAFETY: the source block is live and a successful mapping is a live draft block.
        unsafe {
            Ok(LLVMValueAsBasicBlock(
                self.value(LLVMBasicBlockAsValue(source.raw))?,
            ))
        }
    }

    fn remap(&mut self, replacements: &HashMap<LLVMValueRef, LLVMValueRef>) {
        if !replacements.is_empty() {
            for value in self.identities.values_mut() {
                if let Some(&replacement) = replacements.get(value) {
                    *value = replacement;
                }
            }
        }
    }

    fn phi_additions(
        &self,
        function: &Function,
        predecessor: LLVMBasicBlockRef,
        destination: LLVMBasicBlockRef,
        incoming: &[Value],
    ) -> Result<Vec<(LLVMValueRef, LLVMValueRef)>> {
        let destination_phis: Vec<_> = phis(destination).collect();
        if destination_phis.len() != incoming.len() {
            return Err(Error::new(
                "expected one incoming value for each destination PHI",
            ));
        }
        let mut additions = Vec::with_capacity(incoming.len());
        for (phi, value) in destination_phis.into_iter().zip(incoming) {
            let value = self.operand(function, value)?;
            // SAFETY: both values are live, and an incoming position is checked.
            unsafe {
                if LLVMTypeOf(phi) != LLVMTypeOf(value) {
                    return Err(Error::new("LLVM incoming value has the wrong PHI type"));
                }
                if let Some(existing) = incoming_position(phi, predecessor)
                    && LLVMGetIncomingValue(phi, existing) != value
                {
                    return Err(Error::new(
                        "parallel LLVM edges require the same incoming PHI value",
                    ));
                }
            }
            additions.push((phi, value));
        }
        Ok(additions)
    }

    fn replace_symbol_uses(
        &mut self,
        source: LLVMValueRef,
        replacement: LLVMValueRef,
    ) -> Result<u64> {
        let (source, replacement) = checked_symbols(self, source, replacement)?;
        if source == replacement {
            return Ok(0);
        }
        self.require_unborrowed_ir()?;
        // SAFETY: these are compatible symbols from the private draft.
        unsafe {
            // LLVM 21 stores the block as BlockAddress's only operand.
            if !LLVMIsAFunction(source).is_null() {
                for block in llvm_list(LLVMGetFirstBasicBlock(source), LLVMGetNextBasicBlock) {
                    let value = LLVMBasicBlockAsValue(block);
                    if llvm_list(LLVMGetFirstUse(value), LLVMGetNextUse)
                        .any(|usage| !LLVMIsABlockAddress(LLVMGetUser(usage)).is_null())
                    {
                        return Err(Error::new(
                            "replacing an LLVM block address requires a target block",
                        ));
                    }
                }
            }
            let mut count = 0;
            let mut constants = HashSet::new();
            let mut pending = vec![source];
            while let Some(value) = pending.pop() {
                for usage in llvm_list(LLVMGetFirstUse(value), LLVMGetNextUse) {
                    if value == source {
                        count += 1;
                    }
                    let user = LLVMGetUser(usage);
                    self.require_attached(user)?;
                    if LLVMIsAGlobalValue(user).is_null()
                        && !LLVMIsAConstant(user).is_null()
                        && constants.insert(user)
                    {
                        pending.push(user);
                    }
                }
            }

            // RAUW can destroy or fold uniqued constants. Track each mapped
            // constant through an initializer so source handles keep following
            // their draft counterparts even if the constant's users disappear.
            let tracked: HashSet<_> = self
                .identities
                .values()
                .copied()
                .filter(|value| constants.contains(value))
                .collect();
            let module = LLVMGetGlobalParent(source);
            let mut anchors = HashMap::with_capacity(tracked.len());
            for value in tracked {
                let anchor = LLVMAddGlobal(module, LLVMTypeOf(value), c"".as_ptr());
                LLVMSetInitializer(anchor, value);
                anchors.insert(value, anchor);
            }

            LLVMReplaceAllUsesWith(source, replacement);
            for anchor in anchors.values_mut() {
                let initializer = LLVMGetInitializer(*anchor);
                LLVMDeleteGlobal(*anchor);
                *anchor = initializer;
            }
            self.remap(&anchors);
            Ok(count)
        }
    }
}

fn llvm_list<T>(
    first: *mut T,
    next: unsafe extern "C" fn(*mut T) -> *mut T,
) -> impl Iterator<Item = *mut T> {
    std::iter::successors(NonNull::new(first), move |value| {
        // SAFETY: traversal begins at a live element of the corresponding LLVM list.
        NonNull::new(unsafe { next(value.as_ptr()) })
    })
    .map(NonNull::as_ptr)
}

fn snapshot_values(
    source: LLVMModuleRef,
    clone: LLVMModuleRef,
) -> HashMap<LLVMValueRef, LLVMValueRef> {
    type FirstValue = unsafe extern "C" fn(LLVMModuleRef) -> LLVMValueRef;
    type NextValue = unsafe extern "C" fn(LLVMValueRef) -> LLVMValueRef;

    // SAFETY: the two live modules are paired immediately after LLVMCloneModule,
    // which copies definitions and operands in order within their lists.
    unsafe {
        let symbol_lists: [(FirstValue, NextValue); 3] = [
            (LLVMGetFirstGlobal, LLVMGetNextGlobal),
            (LLVMGetFirstGlobalAlias, LLVMGetNextGlobalAlias),
            (LLVMGetFirstGlobalIFunc, LLVMGetNextGlobalIFunc),
        ];
        let mut definitions = Vec::new();
        for (first, next) in symbol_lists {
            definitions.extend(llvm_list(first(source), next).zip(llvm_list(first(clone), next)));
        }
        for (from, to) in llvm_list(LLVMGetFirstFunction(source), LLVMGetNextFunction)
            .zip(llvm_list(LLVMGetFirstFunction(clone), LLVMGetNextFunction))
        {
            definitions.push((from, to));
            definitions.extend(
                llvm_list(LLVMGetFirstParam(from), LLVMGetNextParam)
                    .zip(llvm_list(LLVMGetFirstParam(to), LLVMGetNextParam)),
            );
            for (from, to) in llvm_list(LLVMGetFirstBasicBlock(from), LLVMGetNextBasicBlock)
                .zip(llvm_list(LLVMGetFirstBasicBlock(to), LLVMGetNextBasicBlock))
            {
                definitions.push((LLVMBasicBlockAsValue(from), LLVMBasicBlockAsValue(to)));
                definitions.extend(
                    llvm_list(LLVMGetFirstInstruction(from), LLVMGetNextInstruction).zip(
                        llvm_list(LLVMGetFirstInstruction(to), LLVMGetNextInstruction),
                    ),
                );
            }
        }

        let mut identities: HashMap<_, _> = definitions.iter().copied().collect();
        while let Some((from, to)) = definitions.pop() {
            if LLVMIsAUser(from).is_null() || LLVMIsAUser(to).is_null() {
                continue;
            }
            let count = LLVMGetNumOperands(from);
            if count != LLVMGetNumOperands(to) {
                continue;
            }
            for index in 0..count as u32 {
                let from = LLVMGetOperand(from, index);
                let to = LLVMGetOperand(to, index);
                if from.is_null() || to.is_null() || LLVMIsAConstant(from).is_null() {
                    continue;
                }
                if let std::collections::hash_map::Entry::Vacant(entry) = identities.entry(from) {
                    entry.insert(to);
                    definitions.push((from, to));
                }
            }
        }
        identities
    }
}

fn is_context_constant(value: LLVMValueRef) -> bool {
    let mut pending = vec![value];
    let mut visited = HashSet::new();
    // SAFETY: the caller retains the source value and the owners of its operands.
    unsafe {
        while let Some(value) = pending.pop() {
            if !visited.insert(value) {
                continue;
            }
            if LLVMIsAConstant(value).is_null()
                || !LLVMIsAGlobalValue(value).is_null()
                || !LLVMIsABlockAddress(value).is_null()
            {
                return false;
            }
            pending.extend(
                (0..LLVMGetNumOperands(value) as u32)
                    .map(|index| LLVMGetOperand(value, index))
                    .filter(|operand| !operand.is_null()),
            );
        }
    }
    true
}

fn finished() -> Error {
    Error::new("LLVM edit has already finished")
}

impl Edit {
    fn open(&self) -> Result<RefMut<'_, Draft>> {
        RefMut::filter_map(self.draft.borrow_mut(), Option::as_mut).map_err(|_| finished())
    }

    fn instruction<'a>(
        &self,
        instruction: &'a Instruction,
    ) -> Result<(&'a Function, LLVMValueRef)> {
        instruction.block().check_live()?;
        let (function, source) = instruction.edit_source();
        self.source.check(function.module())?;
        Ok((function, source))
    }

    /// Resolve a source instruction to its current draft counterpart.
    /// Drop draft handles before operations that erase or reconstruct IR.
    pub fn draft_instruction(&self, instruction: &Instruction) -> Result<Instruction> {
        let draft = self.open()?;
        let (function, source) = self.instruction(instruction)?;
        let instruction = draft.value(source)?;
        let function = draft.function(function)?;
        // SAFETY: the mapped instruction is attached to the mapped function.
        unsafe {
            let parent = LLVMGetInstructionParent(instruction);
            let block = function.block(BasicBlock::new(parent).expect("attached draft block"));
            Ok(block.instruction(InstructionValue::new(instruction)))
        }
    }

    /// Resolve a source value to the draft. Context-owned constants are shared.
    pub fn draft_value(&self, value: &Value) -> Result<Value> {
        let draft = self.open()?;
        value.check_live()?;
        match &value.owner {
            ValueOwner::Context(context) => {
                self.source.context().check(context)?;
                Ok(value.clone())
            }
            ValueOwner::Module(module) => {
                self.source.check(module)?;
                let raw = draft.value(value.raw)?;
                Ok(Value::constant(
                    raw,
                    draft.module.context(),
                    Some(&draft.module),
                ))
            }
            ValueOwner::Local(function) => {
                self.source.check(function.module())?;
                let raw = draft.value(value.raw)?;
                Ok(Value::in_function(raw, &draft.function(function)?))
            }
        }
    }

    /// Replace a data operand. LLVM checks dominance and opcode-specific
    /// constraints when the draft is finished.
    pub fn replace_operand(
        &self,
        instruction: &Instruction,
        index: u32,
        replacement: &Value,
    ) -> Result<()> {
        let draft = self.open()?;
        let (function, instruction) = self.instruction(instruction)?;
        let instruction = draft.value(instruction)?;
        let replacement = draft.operand(function, replacement)?;
        // SAFETY: the mapped instruction and replacement are live in the draft.
        unsafe {
            if index >= LLVMGetNumOperands(instruction) as u32 {
                return Err(Error::new(
                    "LLVM instruction operand index is out of bounds",
                ));
            }
            check_operand(instruction, index, replacement)?;
            LLVMSetOperand(instruction, index, replacement);
        }
        Ok(())
    }

    /// Replace ordinary data uses of a function-local SSA value, or references
    /// to a global variable or non-intrinsic function with a compatible symbol
    /// from the same source module. Symbol replacement includes constant users
    /// and LLVM metadata. A function with block-address references requires an
    /// explicit choice of target blocks and cannot be replaced by this method.
    ///
    /// Returns the number of direct LLVM operand uses before replacement.
    /// Metadata references do not contribute to this count.
    pub fn replace_all_uses(&self, source: &Value, replacement: &Value) -> Result<u64> {
        let mut draft = self.open()?;
        if let ValueOwner::Module(module) = &source.owner {
            self.source.check(module)?;
            let ValueOwner::Module(replacement_module) = &replacement.owner else {
                return Err(Error::new(
                    "expected a module-owned LLVM symbol replacement",
                ));
            };
            self.source.check(replacement_module)?;
            return draft.replace_symbol_uses(source.raw, replacement.raw);
        }
        let ValueOwner::Local(function) = &source.owner else {
            return Err(Error::new(
                "expected an LLVM SSA value or module-owned symbol",
            ));
        };
        self.source.check(function.module())?;
        let source = draft.value(source.operand(function)?.as_value_ref())?;
        let replacement = draft.operand(function, replacement)?;
        if source == replacement {
            return Ok(0);
        }

        let mut changes = Vec::new();
        let mut visited = HashSet::new();
        // SAFETY: the mapped values are live. The use list remains stable
        // while collecting and validating all replacement sites.
        unsafe {
            if LLVMTypeOf(source) != LLVMTypeOf(replacement) {
                return Err(Error::new("LLVM replacement value has the wrong type"));
            }
            let mut usage = LLVMGetFirstUse(source);
            while !usage.is_null() {
                let user = LLVMGetUser(usage);
                if visited.insert(user) {
                    if LLVMIsAInstruction(user).is_null() {
                        return Err(Error::new(
                            "replacement requires an LLVM instruction operand",
                        ));
                    }
                    draft.require_attached(user)?;
                    for index in 0..LLVMGetNumOperands(user) as u32 {
                        if LLVMGetOperand(user, index) == source {
                            check_operand(user, index, replacement)?;
                            changes.push((user, index));
                        }
                    }
                }
                usage = LLVMGetNextUse(usage);
            }
            for &(user, index) in &changes {
                LLVMSetOperand(user, index, replacement);
            }
        }
        Ok(changes.len() as u64)
    }

    /// Erase a non-terminator instruction whose result has no instruction users.
    /// LLVM invalidates references to it in debug information. The caller
    /// determines whether its effects can be discarded.
    pub fn erase_instruction(&self, instruction: &Instruction) -> Result<()> {
        let mut draft = self.open()?;
        let (_, source) = self.instruction(instruction)?;
        let instruction = draft.value(source)?;
        // SAFETY: this is a live draft instruction. Remove its mapping before deletion.
        unsafe {
            if !LLVMIsATerminatorInst(instruction).is_null() || is_eh_pad(instruction) {
                return Err(Error::new("cannot erase an LLVM terminator or EH pad"));
            }
            if !LLVMGetFirstUse(instruction).is_null() {
                return Err(Error::new("cannot erase an LLVM instruction that has uses"));
            }
            draft.require_unborrowed_ir()?;
            draft.identities.remove(&source);
            LLVMInstructionEraseFromParent(instruction);
        }
        Ok(())
    }

    /// Erase a non-entry block that has no incoming edges from other blocks,
    /// address references, or SSA uses outside the block's own outgoing PHI edges.
    pub fn erase_block(&self, block: &Block) -> Result<()> {
        let mut draft = self.open()?;
        self.source.check(block.function.module())?;
        let block = draft.block(block)?;
        draft.require_unborrowed_ir()?;
        // SAFETY: the block is live in the private draft. Check every external
        // use before changing any PHIs or deleting the block's instructions.
        unsafe {
            if block == LLVMGetEntryBasicBlock(LLVMGetBasicBlockParent(block)) {
                return Err(Error::new("cannot erase the LLVM entry block"));
            }
            let block_value = LLVMBasicBlockAsValue(block);
            if llvm_list(LLVMGetFirstUse(block_value), LLVMGetNextUse).any(|usage| {
                let user = LLVMGetUser(usage);
                LLVMIsAInstruction(user).is_null() || LLVMGetInstructionParent(user) != block
            }) {
                return Err(Error::new(
                    "cannot erase an LLVM block with external references",
                ));
            }

            // LLVM stores PHI predecessor blocks separately from ordinary uses.
            let terminator = LLVMGetBasicBlockTerminator(block);
            if terminator.is_null() {
                return Err(Error::new("cannot erase an unterminated LLVM draft block"));
            }
            let successors: HashSet<_> = (0..LLVMGetNumSuccessors(terminator))
                .map(|index| LLVMGetSuccessor(terminator, index))
                .filter(|&successor| successor != block)
                .collect();
            let mut outgoing_phis = Vec::new();
            let mut outgoing_uses = HashSet::new();
            for successor in successors {
                draft.require_attached(LLVMBasicBlockAsValue(successor))?;
                for phi in phis(successor) {
                    for index in 0..LLVMCountIncoming(phi) {
                        if LLVMGetIncomingBlock(phi, index) == block {
                            outgoing_uses.insert(LLVMGetOperandUse(phi, index));
                        }
                    }
                    outgoing_phis.push(phi);
                }
            }
            let mut removed: HashSet<_> =
                llvm_list(LLVMGetFirstInstruction(block), LLVMGetNextInstruction).collect();
            for &instruction in &removed {
                draft.require_attached(instruction)?;
                if llvm_list(LLVMGetFirstUse(instruction), LLVMGetNextUse).any(|usage| {
                    !removed.contains(&LLVMGetUser(usage)) && !outgoing_uses.contains(&usage)
                }) {
                    return Err(Error::new(
                        "cannot erase an LLVM block with external SSA uses",
                    ));
                }
            }

            let mut replacements = HashMap::new();
            if !outgoing_phis.is_empty() {
                let builder = draft.module.context().llvm().create_builder();
                for phi in outgoing_phis {
                    let replacement = without_incoming(builder.as_mut_ptr(), phi, |index| {
                        LLVMGetIncomingBlock(phi, index) == block
                    });
                    replacements.insert(phi, replacement);
                }
            }
            draft.remap(&replacements);
            removed.insert(block_value);
            draft.identities.retain(|_, value| !removed.contains(value));
            LLVMDeleteBasicBlock(block);
        }
        Ok(())
    }

    /// Redirect a branch or switch successor. Pass one value for each PHI of
    /// the new destination, in block order. One entry is removed from each
    /// PHI of the former destination.
    pub fn redirect_successor(
        &self,
        instruction: &Instruction,
        index: u32,
        destination: &Block,
        incoming: &[Value],
    ) -> Result<()> {
        let mut draft = self.open()?;
        let (function, instruction) = self.instruction(instruction)?;
        function.check(&destination.function)?;
        let instruction = draft.value(instruction)?;
        let destination = draft.block(destination)?;
        // SAFETY: the draft instruction and blocks are live and owned by the
        // same function. Successor and PHI indices are checked before use.
        unsafe {
            if !matches!(LLVMGetInstructionOpcode(instruction), LLVMBr | LLVMSwitch) {
                return Err(Error::new(
                    "can redirect successors of LLVM branches and switches only",
                ));
            }
            if index >= LLVMGetNumSuccessors(instruction) {
                return Err(Error::new("LLVM successor index is out of bounds"));
            }
            let old = LLVMGetSuccessor(instruction, index);
            if old == destination {
                return if incoming.is_empty() {
                    Ok(())
                } else {
                    Err(Error::new(
                        "the unchanged successor takes no incoming PHI values",
                    ))
                };
            }
            draft.require_attached(LLVMBasicBlockAsValue(old))?;

            let mut predecessor = LLVMGetInstructionParent(instruction);
            let additions = draft.phi_additions(function, predecessor, destination, incoming)?;
            let removals = phis(old)
                .map(|phi| {
                    incoming_position(phi, predecessor)
                        .map(|position| (phi, position))
                        .ok_or_else(|| Error::new("LLVM successor edge has no incoming PHI entry"))
                })
                .collect::<Result<Vec<_>>>()?;

            let mut replacements = HashMap::new();
            if !removals.is_empty() {
                draft.require_unborrowed_ir()?;
                let builder = draft.module.context().llvm().create_builder();
                for (phi, position) in removals {
                    let replacement =
                        without_incoming(builder.as_mut_ptr(), phi, |index| index == position);
                    replacements.insert(phi, replacement);
                }
            }
            draft.remap(&replacements);
            for (phi, mut value) in additions {
                if let Some(&replacement) = replacements.get(&value) {
                    value = replacement;
                }
                LLVMAddIncoming(phi, &mut value, &mut predecessor, 1);
            }
            LLVMSetSuccessor(instruction, index, destination);
        }
        Ok(())
    }

    /// Verify and return the edited module. A verification failure leaves the
    /// draft open for additional edits.
    pub fn finish(&self) -> Result<Module> {
        let mut state = self.draft.borrow_mut();
        let draft = state.as_ref().ok_or_else(finished)?;
        if !draft.detached.is_empty() {
            return Err(Error::new(
                "reinsert all detached LLVM objects before finishing the edit",
            ));
        }
        draft.module.verify()?;
        Ok(state.take().expect("verified draft").module)
    }
}

fn checked_symbols(
    draft: &Draft,
    source: LLVMValueRef,
    replacement: LLVMValueRef,
) -> Result<(LLVMValueRef, LLVMValueRef)> {
    // SAFETY: the input values are live symbols or constants of the source
    // module. Their mapped symbol definitions belong to the private draft.
    unsafe {
        let kind = LLVMGetValueKind(source);
        if !matches!(kind, LLVMFunctionValueKind | LLVMGlobalVariableValueKind)
            || kind != LLVMGetValueKind(replacement)
        {
            return Err(Error::new(
                "expected two LLVM global variables or two LLVM functions",
            ));
        }
        let source = draft.value(source)?;
        let replacement = draft.value(replacement)?;
        if LLVMTypeOf(source) != LLVMTypeOf(replacement) {
            return Err(Error::new(
                "LLVM replacement symbol has the wrong address space",
            ));
        }
        if kind == LLVMGlobalVariableValueKind {
            if LLVMGlobalGetValueType(source) != LLVMGlobalGetValueType(replacement) {
                return Err(Error::new(
                    "LLVM replacement global has the wrong storage type",
                ));
            }
            if LLVMIsThreadLocal(source) != LLVMIsThreadLocal(replacement) {
                return Err(Error::new(
                    "LLVM replacement global has different thread-local storage",
                ));
            }
            return Ok((source, replacement));
        }
        if LLVMGetIntrinsicID(source) != 0 || LLVMGetIntrinsicID(replacement) != 0 {
            return Err(Error::new(
                "cannot replace references to or with an LLVM intrinsic",
            ));
        }
        if LLVMGlobalGetValueType(source) != LLVMGlobalGetValueType(replacement) {
            return Err(Error::new(
                "LLVM replacement function has the wrong signature",
            ));
        }
        if LLVMGetFunctionCallConv(source) != LLVMGetFunctionCallConv(replacement)
            || !matching_abi_attributes(source, replacement)
        {
            return Err(Error::new(
                "LLVM replacement function has a different calling convention or ABI",
            ));
        }
        Ok((source, replacement))
    }
}

fn matching_abi_attributes(source: LLVMValueRef, replacement: LLVMValueRef) -> bool {
    // Return/parameter attributes that participate in LLVM's calling ABI.
    // LLVM uniques the attribute kind and its complete payload in the context.
    const ABI_ATTRIBUTES: &[&str] = &[
        "align",
        "alignstack",
        "byref",
        "byval",
        "inalloca",
        "inreg",
        "nest",
        "noext",
        "noundef",
        "preallocated",
        "returned",
        "signext",
        "sret",
        "swiftasync",
        "swifterror",
        "swiftself",
        "zeroext",
    ];
    // SAFETY: these functions have the same signature and context. Index zero
    // is the return value and parameter attribute indices start at one.
    unsafe {
        ABI_ATTRIBUTES.iter().all(|name| {
            let kind = LLVMGetEnumAttributeKindForName(name.as_ptr().cast(), name.len());
            (0..=LLVMCountParams(source)).all(|index| {
                LLVMGetEnumAttributeAtIndex(source, index, kind)
                    == LLVMGetEnumAttributeAtIndex(replacement, index, kind)
            })
        })
    }
}

fn phis(block: LLVMBasicBlockRef) -> impl Iterator<Item = LLVMValueRef> {
    // SAFETY: the caller keeps the block live. The next node is obtained before
    // yielding the current one, so the caller can replace the current PHI.
    llvm_list(
        unsafe { LLVMGetFirstInstruction(block) },
        LLVMGetNextInstruction,
    )
    .take_while(|&instruction| unsafe { LLVMGetInstructionOpcode(instruction) == LLVMPHI })
}

fn instruction_name(instruction: LLVMValueRef) -> Vec<u8> {
    let mut len = 0;
    // SAFETY: the live instruction retains its name bytes until changed.
    let name = unsafe { LLVMGetValueName2(instruction, &mut len) };
    if len == 0 {
        return Vec::new();
    }
    unsafe { slice::from_raw_parts(name.cast(), len) }.to_vec()
}

fn without_incoming(
    builder: LLVMBuilderRef,
    phi: LLVMValueRef,
    remove: impl Fn(u32) -> bool,
) -> LLVMValueRef {
    // SAFETY: `phi` is a live PHI in the same context as the builder.
    unsafe {
        LLVMPositionBuilderBefore(builder, phi);
        let replacement = LLVMBuildPhi(builder, LLVMTypeOf(phi), c"".as_ptr());
        for index in (0..LLVMCountIncoming(phi)).filter(|&index| !remove(index)) {
            let mut value = LLVMGetIncomingValue(phi, index);
            let mut block = LLVMGetIncomingBlock(phi, index);
            LLVMAddIncoming(replacement, &mut value, &mut block, 1);
        }

        copy_instruction_metadata(phi, replacement);
        if LLVMCanValueUseFastMathFlags(phi) != 0 {
            LLVMSetFastMathFlags(replacement, LLVMGetFastMathFlags(phi));
        }

        let name = instruction_name(phi);
        LLVMSetValueName2(phi, c"".as_ptr(), 0);
        LLVMSetValueName2(replacement, name.as_ptr().cast(), name.len());
        LLVMReplaceAllUsesWith(phi, replacement);
        LLVMInstructionEraseFromParent(phi);
        replacement
    }
}

fn copy_instruction_metadata(source: LLVMValueRef, replacement: LLVMValueRef) {
    // SAFETY: the caller supplies live instructions in the same context.
    unsafe {
        let context = LLVMGetValueContext(source);
        let mut count = 0;
        let entries = LLVMInstructionGetAllMetadataOtherThanDebugLoc(source, &mut count);
        for index in 0..count as u32 {
            let kind = LLVMValueMetadataEntriesGetKind(entries, index);
            let metadata = LLVMValueMetadataEntriesGetMetadata(entries, index);
            LLVMSetMetadata(replacement, kind, LLVMMetadataAsValue(context, metadata));
        }
        LLVMDisposeValueMetadataEntries(entries);
        LLVMInstructionSetDebugLoc(replacement, LLVMInstructionGetDebugLoc(source));
    }
}

fn incoming_position(phi: LLVMValueRef, predecessor: LLVMBasicBlockRef) -> Option<u32> {
    incoming_positions(phi, predecessor).next()
}

fn incoming_positions(
    phi: LLVMValueRef,
    predecessor: LLVMBasicBlockRef,
) -> impl Iterator<Item = u32> {
    // SAFETY: the caller passes a live PHI node. Indices are bounded by its incoming count.
    (0..unsafe { LLVMCountIncoming(phi) })
        .filter(move |&index| unsafe { LLVMGetIncomingBlock(phi, index) } == predecessor)
}

fn is_eh_pad(instruction: LLVMValueRef) -> bool {
    // SAFETY: the caller passes a live instruction.
    matches!(
        unsafe { LLVMGetInstructionOpcode(instruction) },
        LLVMLandingPad | LLVMCatchPad | LLVMCleanupPad | LLVMCatchSwitch
    )
}

fn check_operand(instruction: LLVMValueRef, index: u32, replacement: LLVMValueRef) -> Result<()> {
    // SAFETY: the caller passes a live instruction with an in-range index and
    // a live replacement from the same draft.
    unsafe {
        let operand_type = LLVMTypeOf(LLVMGetOperand(instruction, index));
        let opcode = LLVMGetInstructionOpcode(instruction);
        let data_type = !matches!(
            LLVMGetTypeKind(operand_type),
            LLVMVoidTypeKind
                | LLVMFunctionTypeKind
                | LLVMLabelTypeKind
                | LLVMMetadataTypeKind
                | LLVMTokenTypeKind
        );
        if !data_type || !allows_data_operand(instruction, index) {
            return Err(Error::new(format!(
                "replacement is unsupported for this LLVM {opcode:?} operand"
            )));
        }
        if operand_type != LLVMTypeOf(replacement) {
            return Err(Error::new("LLVM replacement operand has the wrong type"));
        }
    }
    Ok(())
}

fn allows_data_operand(instruction: LLVMValueRef, index: u32) -> bool {
    // SAFETY: the caller passes a live instruction with an in-range index.
    unsafe {
        match LLVMGetInstructionOpcode(instruction) {
            // LLVM stores arguments first and the callee last. Other slots
            // can carry operand bundles or successor labels.
            LLVMCall | LLVMInvoke | LLVMCallBr => {
                index < LLVMGetNumArgOperands(instruction)
                    || index + 1 == LLVMGetNumOperands(instruction) as u32
            }
            LLVMGetElementPtr => index <= 1 || gep_has_sequential_index(instruction, index),
            LLVMSwitch => index == 0,
            LLVMFNeg | LLVMSelect | LLVMFreeze | LLVMPHI | LLVMRet | LLVMBr | LLVMIndirectBr
            | LLVMResume | LLVMAlloca | LLVMVAArg | LLVMLoad | LLVMStore | LLVMAtomicRMW
            | LLVMAtomicCmpXchg | LLVMExtractElement | LLVMInsertElement | LLVMShuffleVector
            | LLVMExtractValue | LLVMInsertValue => true,
            _ => {
                !LLVMIsABinaryOperator(instruction).is_null()
                    || !LLVMIsACastInst(instruction).is_null()
                    || !LLVMIsACmpInst(instruction).is_null()
            }
        }
    }
}

fn gep_has_sequential_index(instruction: LLVMValueRef, index: u32) -> bool {
    // SAFETY: the caller passes a live GEP. Structure indices and the source
    // element type come from the verified module and cannot be edited here.
    unsafe {
        let mut container = LLVMGetGEPSourceElementType(instruction);
        for previous in 2..index {
            container = match LLVMGetTypeKind(container) {
                LLVMStructTypeKind => {
                    let mut field = LLVMGetOperand(instruction, previous);
                    if LLVMIsAConstantInt(field).is_null() {
                        field = LLVMGetAggregateElement(field, 0);
                        if field.is_null() || LLVMIsAConstantInt(field).is_null() {
                            return false;
                        }
                    }
                    let field = LLVMConstIntGetZExtValue(field) as u32;
                    LLVMStructGetTypeAtIndex(container, field)
                }
                LLVMArrayTypeKind | LLVMVectorTypeKind | LLVMScalableVectorTypeKind => {
                    LLVMGetElementType(container)
                }
                _ => return false,
            };
        }
        matches!(
            LLVMGetTypeKind(container),
            LLVMArrayTypeKind | LLVMVectorTypeKind | LLVMScalableVectorTypeKind
        )
    }
}

use std::slice;

use inkwell::basic_block::BasicBlock;
use inkwell::llvm_sys::LLVMOpcode;
use inkwell::llvm_sys::LLVMOpcode::*;
use inkwell::llvm_sys::LLVMTypeKind::LLVMVoidTypeKind;
use inkwell::llvm_sys::core::{
    LLVMCountIncoming, LLVMGEPGetNoWrapFlags, LLVMGEPSetNoWrapFlags, LLVMGetAllocatedType,
    LLVMGetCalledFunctionType, LLVMGetCalledValue, LLVMGetExact, LLVMGetGEPSourceElementType,
    LLVMGetIncomingBlock, LLVMGetInstructionOpcode, LLVMGetNSW, LLVMGetNUW, LLVMGetNumArgOperands,
    LLVMGetNumSuccessors, LLVMGetOperand, LLVMGetSuccessor, LLVMGetTypeKind, LLVMIsABinaryOperator,
    LLVMIsACastInst, LLVMIsACmpInst, LLVMIsAFreezeInst, LLVMIsAPHINode, LLVMIsASelectInst,
    LLVMIsAUnaryOperator, LLVMSetExact, LLVMSetNSW, LLVMSetNUW, LLVMSetOperand, LLVMTypeOf,
};
use inkwell::llvm_sys::core::{
    LLVMGetAlignment, LLVMGetNumOperandBundleArgs, LLVMGetNumOperandBundles,
    LLVMGetOperandBundleArgAtIndex, LLVMGetOperandBundleAtIndex, LLVMGetOperandBundleTag,
    LLVMSetAlignment,
};
use inkwell::llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use inkwell::values::{AnyValue, AsValueRef, InstructionValue, OperandBundle as LlvmOperandBundle};

use super::global::{MAX_ALIGNMENT, alignment_bytes};
use super::{Block, Context, Type, Value, ValueOwner, set_value_name, value_name};
use crate::{
    Error, FastMathFlags, FloatPredicate, GepFlags, IntFlags, IntPredicate, OperandBundle, Result,
};

impl Block {
    pub fn instructions(&self) -> impl Iterator<Item = Instruction> {
        std::iter::successors(self.first_instruction(), Instruction::next_instruction)
    }

    /// Return the first instruction in this block.
    pub fn first_instruction(&self) -> Option<Instruction> {
        self.llvm()
            .get_first_instruction()
            .map(|inst| self.instruction(inst))
    }

    /// Return the last instruction, including a terminator if present.
    pub fn last_instruction(&self) -> Option<Instruction> {
        self.llvm()
            .get_last_instruction()
            .map(|inst| self.instruction(inst))
    }

    pub fn terminator(&self) -> Option<Instruction> {
        self.llvm()
            .get_terminator()
            .map(|inst| self.instruction(inst))
    }

    pub(crate) fn instruction(&self, inst: InstructionValue<'_>) -> Instruction {
        Instruction {
            raw: inst.as_value_ref(),
            block: self.clone(),
        }
    }
}

/// An LLVM instruction whose handle retains its block, function, and module.
#[derive(Clone)]
pub struct Instruction {
    raw: LLVMValueRef,
    block: Block,
}

impl Instruction {
    pub(super) fn edit_source(&self) -> (&super::Function, LLVMValueRef) {
        (&self.block.function, self.raw)
    }

    pub(crate) fn llvm(&self) -> InstructionValue<'_> {
        // SAFETY: the retained block owns this instruction and never deletes it.
        unsafe { InstructionValue::new(self.raw) }
    }

    fn raw_opcode(&self) -> LLVMOpcode {
        // SAFETY: the retained block owns this live instruction.
        unsafe { LLVMGetInstructionOpcode(self.raw) }
    }

    fn context(&self) -> &Context {
        self.block.function.module.context()
    }

    /// The block that contains this instruction.
    pub fn block(&self) -> &Block {
        &self.block
    }

    /// Test whether two handles refer to the same LLVM instruction.
    pub fn same_as(&self, other: &Self) -> bool {
        self.raw == other.raw
    }

    /// Return the next instruction in this block.
    pub fn next_instruction(&self) -> Option<Self> {
        self.llvm()
            .get_next_instruction()
            .map(|inst| self.block.instruction(inst))
    }

    /// Return the previous instruction in this block.
    pub fn previous_instruction(&self) -> Option<Self> {
        self.llvm()
            .get_previous_instruction()
            .map(|inst| self.block.instruction(inst))
    }

    /// Return the assigned result name, or an empty string when unnamed.
    pub fn name(&self) -> Result<String> {
        value_name(self.raw)
    }

    /// Rename a non-void instruction result, or clear its name with an empty string.
    pub fn set_name(&self, name: &str) -> Result<()> {
        // SAFETY: the retained block owns this instruction.
        unsafe { set_value_name(self.raw, name) }
    }

    /// The LLVM mnemonic, such as `add`, `call`, or `ret`.
    pub fn opcode(&self) -> &'static str {
        match self.raw_opcode() {
            LLVMRet => "ret",
            LLVMBr => "br",
            LLVMSwitch => "switch",
            LLVMIndirectBr => "indirectbr",
            LLVMInvoke => "invoke",
            LLVMUnreachable => "unreachable",
            LLVMCallBr => "callbr",
            LLVMFNeg => "fneg",
            LLVMAdd => "add",
            LLVMFAdd => "fadd",
            LLVMSub => "sub",
            LLVMFSub => "fsub",
            LLVMMul => "mul",
            LLVMFMul => "fmul",
            LLVMUDiv => "udiv",
            LLVMSDiv => "sdiv",
            LLVMFDiv => "fdiv",
            LLVMURem => "urem",
            LLVMSRem => "srem",
            LLVMFRem => "frem",
            LLVMShl => "shl",
            LLVMLShr => "lshr",
            LLVMAShr => "ashr",
            LLVMAnd => "and",
            LLVMOr => "or",
            LLVMXor => "xor",
            LLVMAlloca => "alloca",
            LLVMLoad => "load",
            LLVMStore => "store",
            LLVMGetElementPtr => "getelementptr",
            LLVMTrunc => "trunc",
            LLVMZExt => "zext",
            LLVMSExt => "sext",
            LLVMFPToUI => "fptoui",
            LLVMFPToSI => "fptosi",
            LLVMUIToFP => "uitofp",
            LLVMSIToFP => "sitofp",
            LLVMFPTrunc => "fptrunc",
            LLVMFPExt => "fpext",
            LLVMPtrToInt => "ptrtoint",
            LLVMIntToPtr => "inttoptr",
            LLVMBitCast => "bitcast",
            LLVMAddrSpaceCast => "addrspacecast",
            LLVMICmp => "icmp",
            LLVMFCmp => "fcmp",
            LLVMPHI => "phi",
            LLVMCall => "call",
            LLVMSelect => "select",
            LLVMVAArg => "va_arg",
            LLVMExtractElement => "extractelement",
            LLVMInsertElement => "insertelement",
            LLVMShuffleVector => "shufflevector",
            LLVMExtractValue => "extractvalue",
            LLVMInsertValue => "insertvalue",
            LLVMFreeze => "freeze",
            LLVMFence => "fence",
            LLVMAtomicCmpXchg => "cmpxchg",
            LLVMAtomicRMW => "atomicrmw",
            LLVMResume => "resume",
            LLVMLandingPad => "landingpad",
            LLVMCleanupRet => "cleanupret",
            LLVMCatchRet => "catchret",
            LLVMCatchPad => "catchpad",
            LLVMCleanupPad => "cleanuppad",
            LLVMCatchSwitch => "catchswitch",
            LLVMUserOp1 | LLVMUserOp2 => "<Invalid operator> ",
        }
    }

    pub fn ir(&self) -> String {
        self.llvm().print_to_string().to_string()
    }

    /// Return the integer comparison predicate of an `icmp` instruction.
    pub fn icmp_predicate(&self) -> Option<IntPredicate> {
        self.llvm().get_icmp_predicate()
    }

    /// Return the floating-point comparison predicate of an `fcmp` instruction.
    pub fn fcmp_predicate(&self) -> Option<FloatPredicate> {
        self.llvm().get_fcmp_predicate()
    }

    /// Return the integer comparison's `samesign` flag, if applicable.
    pub fn icmp_same_sign(&self) -> Option<bool> {
        self.llvm().get_same_sign_flag().ok()
    }

    /// Set or clear the `samesign` flag on an `icmp` instruction.
    pub fn set_icmp_same_sign(&self, same_sign: bool) -> Result<()> {
        self.llvm()
            .set_same_sign_flag(same_sign)
            .map_err(|error| Error::new(error.to_string()))
    }

    /// Return the volatile flag for a load, store, atomicrmw, or cmpxchg.
    pub fn is_volatile(&self) -> Option<bool> {
        self.llvm().get_volatile().ok()
    }

    /// Alignment in bytes of an alloca, load, store, atomicrmw, or cmpxchg.
    pub fn alignment(&self) -> Result<u64> {
        self.check_supports_alignment()?;
        // SAFETY: the opcode check guards LLVM's alignment accessor.
        let alignment = unsafe { LLVMGetAlignment(self.raw) };
        // LLVM instructions always have nonzero alignment. Zero from the C API
        // is the truncated representation of 2^32.
        Ok(if alignment == 0 {
            MAX_ALIGNMENT
        } else {
            alignment.into()
        })
    }

    /// Set a nonzero power-of-two alignment up to 2^31 bytes.
    pub fn set_alignment(&self, alignment: u64) -> Result<()> {
        self.check_supports_alignment()?;
        if !alignment.is_power_of_two() {
            return Err(Error::new(
                "LLVM instruction alignment must be a nonzero power of two",
            ));
        }
        let alignment = alignment_bytes(alignment)?;
        // SAFETY: the instruction supports alignment and the value is valid.
        unsafe { LLVMSetAlignment(self.raw, alignment) };
        Ok(())
    }

    fn check_supports_alignment(&self) -> Result<()> {
        if matches!(
            self.raw_opcode(),
            LLVMAlloca | LLVMLoad | LLVMStore | LLVMAtomicRMW | LLVMAtomicCmpXchg
        ) {
            return Ok(());
        }
        Err(Error::new(format!(
            "LLVM {} instruction does not have alignment",
            self.opcode()
        )))
    }

    /// Read LLVM's fast-math flags, if this instruction supports them.
    pub fn fast_math_flags(&self) -> Option<FastMathFlags> {
        self.llvm().get_fast_math_flags().ok()
    }

    /// Add flags to a supported instruction. LLVM's C API unions the supplied
    /// flags with the instruction's existing flags.
    pub fn add_fast_math_flags(&self, flags: FastMathFlags) -> Result<()> {
        self.llvm()
            .set_fast_math_flags(flags)
            .map_err(|error| Error::new(error.to_string()))
    }

    /// Return integer no-wrap or exact flags on instructions that support them.
    pub fn int_flags(&self) -> Option<IntFlags> {
        match self.raw_opcode() {
            LLVMAdd | LLVMSub | LLVMMul | LLVMShl | LLVMTrunc => {
                // SAFETY: these opcodes support both signed and unsigned no-wrap flags.
                let (nsw, nuw) = unsafe { (LLVMGetNSW(self.raw) != 0, LLVMGetNUW(self.raw) != 0) };
                Some(IntFlags {
                    nsw,
                    nuw,
                    exact: false,
                })
            }
            LLVMUDiv | LLVMSDiv | LLVMLShr | LLVMAShr => {
                // SAFETY: these divide and right-shift opcodes support exact.
                let exact = unsafe { LLVMGetExact(self.raw) != 0 };
                Some(IntFlags {
                    exact,
                    ..IntFlags::default()
                })
            }
            _ => None,
        }
    }

    /// Replace the complete set of integer no-wrap or exact flags in place.
    pub fn set_int_flags(&self, flags: IntFlags) -> Result<()> {
        match self.raw_opcode() {
            LLVMAdd | LLVMSub | LLVMMul | LLVMShl | LLVMTrunc => {
                if flags.exact {
                    return Err(Error::new("exact requires udiv, sdiv, lshr, or ashr"));
                }
                // SAFETY: these opcodes support both no-wrap flags.
                unsafe {
                    LLVMSetNSW(self.raw, flags.nsw.into());
                    LLVMSetNUW(self.raw, flags.nuw.into());
                }
            }
            LLVMUDiv | LLVMSDiv | LLVMLShr | LLVMAShr => {
                if flags.nsw || flags.nuw {
                    return Err(Error::new(
                        "nsw and nuw require add, sub, mul, shl, or trunc",
                    ));
                }
                // SAFETY: these divide and right-shift opcodes support exact.
                unsafe { LLVMSetExact(self.raw, flags.exact.into()) };
            }
            _ => {
                return Err(Error::new(
                    "LLVM instruction does not support integer flags",
                ));
            }
        }
        Ok(())
    }

    /// The instruction's SSA value, if it has a non-void result.
    pub fn result(&self) -> Option<Value> {
        // SAFETY: this instruction and its type are live.
        (unsafe { LLVMGetTypeKind(LLVMTypeOf(self.raw)) } != LLVMVoidTypeKind)
            .then(|| Value::with_owner(self.raw, ValueOwner::Local(self.block.function.clone())))
    }

    pub fn operand_count(&self) -> u32 {
        self.llvm().get_num_operands()
    }

    /// Return this call-like instruction's callee, including indirect callees.
    pub fn call_target(&self) -> Option<Value> {
        self.is_call_site().then(|| {
            // SAFETY: this instruction is a CallBase and has a callee.
            let raw = unsafe { LLVMGetCalledValue(self.raw) };
            Value::in_function(raw, &self.block.function)
        })
    }

    /// Return the complete function type used at a call site.
    pub fn call_signature(&self) -> Option<Type> {
        self.is_call_site().then(|| {
            // SAFETY: this instruction is a CallBase with a live function type.
            self.context()
                .type_ref(unsafe { LLVMGetCalledFunctionType(self.raw) })
        })
    }

    /// The number of actual arguments on a call, invoke, or callbr.
    pub fn call_argument_count(&self) -> Option<u32> {
        // SAFETY: only CallBase instructions reach the argument-count accessor.
        self.is_call_site()
            .then(|| unsafe { LLVMGetNumArgOperands(self.raw) })
    }

    /// An actual call-site argument, excluding the callee and operand bundles.
    pub fn call_argument(&self, index: u32) -> Option<Value> {
        if index >= self.call_argument_count()? {
            return None;
        }
        // In LLVM's CallBase layout the arguments are the first operands.
        self.operand(index)
    }

    /// The number of operand bundles on a call, invoke, or callbr.
    pub fn call_operand_bundle_count(&self) -> Option<u32> {
        // SAFETY: only CallBase instructions reach the bundle-count accessor.
        self.is_call_site()
            .then(|| unsafe { LLVMGetNumOperandBundles(self.raw) })
    }

    /// Read an operand bundle in IR order, retaining the owners of its operands.
    pub fn call_operand_bundle(&self, index: u32) -> Option<OperandBundle> {
        if index >= self.call_operand_bundle_count()? {
            return None;
        }
        // SAFETY: the index addresses a live bundle. The Inkwell wrapper owns
        // the copy returned by LLVM and disposes it after both fields are copied.
        let bundle =
            unsafe { LlvmOperandBundle::new(LLVMGetOperandBundleAtIndex(self.raw, index)) };
        let raw = bundle.as_mut_ptr();
        let function = &self.block.function;
        let mut len = 0;
        // SAFETY: the tag and operand refs are live while their LLVM owners are retained.
        unsafe {
            let tag = LLVMGetOperandBundleTag(raw, &mut len);
            let tag = slice::from_raw_parts(tag.cast::<u8>(), len);
            let operands = (0..LLVMGetNumOperandBundleArgs(raw))
                .map(|index| {
                    Value::in_function(LLVMGetOperandBundleArgAtIndex(raw, index), function)
                })
                .collect();
            Some(OperandBundle::new(tag, operands))
        }
    }

    fn is_call_site(&self) -> bool {
        matches!(self.raw_opcode(), LLVMCall | LLVMInvoke | LLVMCallBr)
    }

    /// The source element type encoded by a GEP instruction.
    pub fn gep_source_type(&self) -> Option<Type> {
        (self.raw_opcode() == LLVMGetElementPtr).then(|| {
            // SAFETY: the opcode predicate guards the GEP-only accessor.
            self.context()
                .type_ref(unsafe { LLVMGetGEPSourceElementType(self.raw) })
        })
    }

    /// The base pointer of a GEP instruction.
    pub fn gep_pointer(&self) -> Option<Value> {
        self.operand_for(LLVMGetElementPtr, 0)
    }

    /// The number of index operands of a GEP, excluding the base pointer.
    pub fn gep_index_count(&self) -> Option<u32> {
        (self.raw_opcode() == LLVMGetElementPtr).then(|| self.operand_count() - 1)
    }

    /// A GEP index in source order, excluding the base pointer.
    pub fn gep_index(&self, index: u32) -> Option<Value> {
        self.operand_for(LLVMGetElementPtr, index.checked_add(1)?)
    }

    /// The `inbounds` flag of a GEP instruction.
    pub fn gep_in_bounds(&self) -> Option<bool> {
        self.gep_flags().map(|flags| flags.in_bounds)
    }

    /// The no-wrap flags of a GEP instruction, including `nusw` implied by `inbounds`.
    pub fn gep_flags(&self) -> Option<GepFlags> {
        (self.raw_opcode() == LLVMGetElementPtr).then(|| {
            // SAFETY: only GEP instructions reach the GEP flag accessor.
            GepFlags::from_bits(unsafe { LLVMGEPGetNoWrapFlags(self.raw) })
        })
    }

    /// Replace the complete set of GEP no-wrap flags. `inbounds` implies `nusw`.
    pub fn set_gep_flags(&self, flags: GepFlags) -> Result<()> {
        if self.raw_opcode() != LLVMGetElementPtr {
            return Err(Error::new("LLVM instruction is not getelementptr"));
        }
        // SAFETY: this is a GEP instruction and `bits` uses supported flags.
        unsafe { LLVMGEPSetNoWrapFlags(self.raw, flags.bits()) };
        Ok(())
    }

    /// The pointer from which a load reads.
    pub fn load_pointer(&self) -> Option<Value> {
        self.operand_for(LLVMLoad, 0)
    }

    /// The pointer to which a store writes.
    pub fn store_pointer(&self) -> Option<Value> {
        self.operand_for(LLVMStore, 1)
    }

    /// The stored value operand of a store instruction.
    pub fn stored_value(&self) -> Option<Value> {
        self.operand_for(LLVMStore, 0)
    }

    /// The allocated type encoded by an alloca instruction.
    pub fn allocated_type(&self) -> Option<Type> {
        (self.raw_opcode() == LLVMAlloca).then(|| {
            // SAFETY: the opcode predicate guards the alloca-only accessor.
            self.context()
                .type_ref(unsafe { LLVMGetAllocatedType(self.raw) })
        })
    }

    /// The element count on an alloca, including the implicit count of one.
    pub fn alloca_count(&self) -> Option<Value> {
        self.operand_for(LLVMAlloca, 0)
    }

    fn operand_for(&self, opcode: LLVMOpcode, index: u32) -> Option<Value> {
        (self.raw_opcode() == opcode)
            .then(|| self.operand(index))
            .flatten()
    }

    /// Inspect an operand in LLVM's internal order. This includes values such
    /// as labels and metadata. Use `successor` for control-flow order.
    /// Constants that can refer to symbols or blocks retain the originating module.
    pub fn operand(&self, index: u32) -> Option<Value> {
        let raw = self.operand_ref(index)?;
        Some(Value::in_function(raw, &self.block.function))
    }

    fn operand_ref(&self, index: u32) -> Option<LLVMValueRef> {
        if index >= self.operand_count() {
            return None;
        }
        // SAFETY: the live instruction owns this operand index.
        let raw = unsafe { LLVMGetOperand(self.raw, index) };
        (!raw.is_null()).then_some(raw)
    }

    pub fn successor_count(&self) -> u32 {
        if self.llvm().is_terminator() {
            // SAFETY: this is a live terminator instruction.
            unsafe { LLVMGetNumSuccessors(self.raw) }
        } else {
            0
        }
    }

    pub fn successor(&self, index: u32) -> Option<Block> {
        if index >= self.successor_count() {
            return None;
        }
        // SAFETY: the checked index names a block in this live terminator.
        Some(unsafe { self.successor_unchecked(index) })
    }

    /// Successor blocks in LLVM order, with one result per control-flow edge.
    pub fn successors(&self) -> impl ExactSizeIterator<Item = Block> + use<> {
        let instruction = self.clone();
        (0..self.successor_count()).map(move |index| {
            // SAFETY: the iterator retains the terminator and each index is in bounds.
            unsafe { instruction.successor_unchecked(index) }
        })
    }

    /// The instruction must be a terminator and the index must be in bounds.
    unsafe fn successor_unchecked(&self, index: u32) -> Block {
        // SAFETY: an in-bounds successor belongs to the terminator's function.
        self.block
            .function
            .block_ref(unsafe { LLVMGetSuccessor(self.raw, index) })
    }

    /// The number of explicit switch cases, excluding the default successor.
    pub fn switch_case_count(&self) -> u32 {
        // SAFETY: the instruction is live, and only a switch is queried for cases.
        unsafe {
            if LLVMGetInstructionOpcode(self.raw) == LLVMSwitch {
                LLVMGetNumSuccessors(self.raw) - 1
            } else {
                0
            }
        }
    }

    /// An explicit switch case in IR order. Successor zero is the default block.
    pub fn switch_case(&self, index: u32) -> Option<(Value, Block)> {
        if index >= self.switch_case_count() {
            return None;
        }
        // SAFETY: the instruction is a live switch, and this case exists.
        let (key, destination) = unsafe { switch_case_refs(self.raw, index) };
        let function = &self.block.function;
        Some((
            Value::in_function(key, function),
            function.block(unsafe { BasicBlock::new(destination) }?),
        ))
    }

    /// The predecessor for a PHI operand, in the same order as `operand`.
    pub fn phi_incoming_block(&self, index: u32) -> Option<Block> {
        // SAFETY: LLVM's PHI accessors are called only on a live PHI and with
        // an in-bounds index.
        unsafe {
            if LLVMIsAPHINode(self.raw).is_null() || index >= LLVMCountIncoming(self.raw) {
                return None;
            }
            let block = BasicBlock::new(LLVMGetIncomingBlock(self.raw, index))?;
            Some(self.block.function.block(block))
        }
    }

    /// Replace an operand of a unary, binary, comparison, select, freeze, or
    /// cast instruction. The replacement must have the original type and
    /// belong to the instruction's scope. If verification fails, restore the
    /// original operand. The instruction and its SSA result keep their identity.
    pub fn replace_operand(&self, index: u32, value: &Value) -> Result<()> {
        // SAFETY: these LLVM predicates only inspect this live instruction.
        let allows_replacement = unsafe {
            !LLVMIsAUnaryOperator(self.raw).is_null()
                || !LLVMIsABinaryOperator(self.raw).is_null()
                || !LLVMIsACmpInst(self.raw).is_null()
                || !LLVMIsASelectInst(self.raw).is_null()
                || !LLVMIsAFreezeInst(self.raw).is_null()
                || !LLVMIsACastInst(self.raw).is_null()
        };
        if !allows_replacement {
            return Err(Error::new(format!(
                "operand replacement is unsupported for LLVM {} instructions",
                self.opcode()
            )));
        }
        let old = self
            .operand_ref(index)
            .ok_or_else(|| Error::new("LLVM instruction operand index is out of bounds"))?;
        let function = &self.block.function;
        let new = value.operand(function)?.as_value_ref();
        // SAFETY: both operand values and their types are live.
        if unsafe { LLVMTypeOf(old) != LLVMTypeOf(new) } {
            return Err(Error::new("LLVM replacement operand has the wrong type"));
        }

        function.module.verify()?;
        // SAFETY: these instruction kinds accept same-type data operands. The
        // verifier below checks constraints such as dominance and self-use.
        unsafe { LLVMSetOperand(self.raw, index, new) };
        if let Err(error) = function.module.verify() {
            // SAFETY: the original operand is still live and valid here.
            unsafe { LLVMSetOperand(self.raw, index, old) };
            return Err(error);
        }
        Ok(())
    }
}

/// The caller passes a live switch and an in-bounds index of an explicit case.
pub(super) unsafe fn switch_case_refs(
    instruction: LLVMValueRef,
    index: u32,
) -> (LLVMValueRef, LLVMBasicBlockRef) {
    let successor = index + 1;
    // SAFETY: a switch stores its condition and default label first, then
    // alternating case keys and destinations.
    unsafe {
        (
            LLVMGetOperand(instruction, 2 * successor),
            LLVMGetSuccessor(instruction, successor),
        )
    }
}

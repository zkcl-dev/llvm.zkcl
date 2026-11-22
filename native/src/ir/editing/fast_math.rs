use super::*;
use crate::FastMathFlags;

impl Edit {
    /// Replace all fast-math promises on an instruction in the private draft.
    /// Source handles continue to identify the replacement across later edits.
    pub fn set_fast_math_flags(
        &self,
        instruction: &Instruction,
        flags: FastMathFlags,
    ) -> Result<()> {
        let mut draft = self.open()?;
        let (_, source) = self.instruction(instruction)?;
        let instruction = draft.value(source)?;
        // SAFETY: reconstruction uses the live draft instruction's types and
        // operands. The source module and its handles are never modified.
        unsafe {
            if LLVMCanValueUseFastMathFlags(instruction) == 0 {
                let opcode = LLVMGetInstructionOpcode(instruction);
                return Err(Error::new(format!(
                    "LLVM 21 does not support fast-math flags on {opcode:?}"
                )));
            }
            if LLVMGetFastMathFlags(instruction) == flags.bits() {
                return Ok(());
            }
            draft.require_unborrowed_ir()?;
            let builder = draft.module.context().llvm().create_builder();
            LLVMPositionBuilderBefore(builder.as_mut_ptr(), instruction);
            let replacement = rebuild(builder.as_mut_ptr(), instruction)?;
            drop(builder);
            LLVMSetFastMathFlags(replacement, flags.bits());
            copy_instruction_metadata(instruction, replacement);
            let name = instruction_name(instruction);
            LLVMSetValueName2(instruction, c"".as_ptr(), 0);
            LLVMSetValueName2(replacement, name.as_ptr().cast(), name.len());
            LLVMReplaceAllUsesWith(instruction, replacement);
            draft.remap(&HashMap::from([(instruction, replacement)]));
            LLVMInstructionEraseFromParent(instruction);
        }
        Ok(())
    }
}

unsafe fn rebuild(builder: LLVMBuilderRef, instruction: LLVMValueRef) -> Result<LLVMValueRef> {
    // SAFETY: the caller supplies an attached, verified instruction and a
    // same-context builder positioned immediately before it.
    unsafe {
        let opcode = LLVMGetInstructionOpcode(instruction);
        match opcode {
            LLVMCall => Ok(rebuild_call(builder, instruction)),
            LLVMPHI => {
                let replacement = LLVMBuildPhi(builder, LLVMTypeOf(instruction), c"".as_ptr());
                for index in 0..LLVMCountIncoming(instruction) {
                    let mut value = LLVMGetIncomingValue(instruction, index);
                    let mut block = LLVMGetIncomingBlock(instruction, index);
                    LLVMAddIncoming(replacement, &mut value, &mut block, 1);
                }
                // The later RAUW also repairs self-referential incoming values.
                Ok(replacement)
            }
            LLVMFAdd | LLVMFSub | LLVMFMul | LLVMFDiv | LLVMFRem | LLVMFNeg | LLVMFCmp
            | LLVMSelect | LLVMFPTrunc | LLVMFPExt => {
                let count = LLVMGetNumOperands(instruction) as u32;
                let operands: Vec<_> = (0..count).map(|i| LLVMGetOperand(instruction, i)).collect();
                // Detached freezes make every operand nonconstant, including
                // the select condition and arms, so IRBuilder cannot fold it.
                let detached = LLVMCreateBuilderInContext(LLVMGetValueContext(instruction));
                let placeholders: Vec<_> = operands
                    .iter()
                    .map(|&value| LLVMBuildFreeze(detached, value, c"".as_ptr()))
                    .collect();
                let replacement = match opcode {
                    LLVMFPTrunc | LLVMFPExt => LLVMBuildCast(
                        builder,
                        opcode,
                        placeholders[0],
                        LLVMTypeOf(instruction),
                        c"".as_ptr(),
                    ),
                    LLVMFNeg => LLVMBuildFNeg(builder, placeholders[0], c"".as_ptr()),
                    LLVMFCmp => LLVMBuildFCmp(
                        builder,
                        LLVMGetFCmpPredicate(instruction),
                        placeholders[0],
                        placeholders[1],
                        c"".as_ptr(),
                    ),
                    LLVMSelect => LLVMBuildSelect(
                        builder,
                        placeholders[0],
                        placeholders[1],
                        placeholders[2],
                        c"".as_ptr(),
                    ),
                    _ => LLVMBuildBinOp(
                        builder,
                        opcode,
                        placeholders[0],
                        placeholders[1],
                        c"".as_ptr(),
                    ),
                };
                for (index, &operand) in operands.iter().enumerate() {
                    LLVMSetOperand(replacement, index as u32, operand);
                }
                for placeholder in placeholders {
                    LLVMDeleteInstruction(placeholder);
                }
                LLVMDisposeBuilder(detached);
                Ok(replacement)
            }
            _ => Err(Error::new(format!(
                "LLVM C API cannot reconstruct fast-math opcode {opcode:?}"
            ))),
        }
    }
}

unsafe fn rebuild_call(builder: LLVMBuilderRef, instruction: LLVMValueRef) -> LLVMValueRef {
    // SAFETY: all call operands, destinations, bundles and attributes are
    // copied from the live same-context CallBase before it is replaced.
    unsafe {
        let mut args: Vec<_> = (0..LLVMGetNumArgOperands(instruction))
            .map(|index| LLVMGetOperand(instruction, index))
            .collect();
        let mut bundles: Vec<_> = (0..LLVMGetNumOperandBundles(instruction))
            .map(|index| LLVMGetOperandBundleAtIndex(instruction, index))
            .collect();
        let ty = LLVMGetCalledFunctionType(instruction);
        let callee = LLVMGetCalledValue(instruction);
        let replacement = LLVMBuildCallWithOperandBundles(
            builder,
            ty,
            callee,
            args.as_mut_ptr(),
            args.len() as u32,
            bundles.as_mut_ptr(),
            bundles.len() as u32,
            c"".as_ptr(),
        );
        for bundle in bundles {
            LLVMDisposeOperandBundle(bundle);
        }
        LLVMSetInstructionCallConv(replacement, LLVMGetInstructionCallConv(instruction));
        LLVMSetTailCallKind(replacement, LLVMGetTailCallKind(instruction));
        for index in
            (0..=args.len() as u32).chain(std::iter::once(llvm_sys::LLVMAttributeFunctionIndex))
        {
            let count = LLVMGetCallSiteAttributeCount(instruction, index) as usize;
            let mut attributes = vec![std::ptr::null_mut(); count];
            LLVMGetCallSiteAttributes(instruction, index, attributes.as_mut_ptr());
            for attribute in attributes {
                LLVMAddCallSiteAttribute(replacement, index, attribute);
            }
        }
        replacement
    }
}

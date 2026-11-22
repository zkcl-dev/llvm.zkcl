use std::{collections::HashMap, ptr, slice};

use inkwell::values::AsValueRef;
use llvm_sys::core::*;
use llvm_sys::prelude::LLVMValueRef;

use super::super::instructions::switch_case_refs;
use super::{
    Block, Edit, Error, Function, Instruction, Result, Value, copy_instruction_metadata,
    incoming_positions, phis, without_incoming,
};

impl Edit {
    /// Append a distinct constant integer case to a switch. Supply one value
    /// per destination PHI in block order, including for a parallel edge.
    /// A new case starts at zero weight when the switch carries branch weights.
    pub fn add_switch_case(
        &self,
        instruction: &Instruction,
        key: &Value,
        destination: &Block,
        incoming: &[Value],
    ) -> Result<()> {
        let draft = self.open()?;
        let (function, instruction) = self.instruction(instruction)?;
        function.check(&destination.function)?;
        let instruction = draft.value(instruction)?;
        let destination = draft.block(destination)?;
        let key = switch_key(instruction, function, key)?;

        // SAFETY: the switch, destination, and new key belong to this draft or
        // its context. All PHI values and the case's uniqueness are checked
        // before changing the draft.
        unsafe {
            let successors = LLVMGetNumSuccessors(instruction);
            if (0..successors - 1).any(|index| switch_case_refs(instruction, index).0 == key) {
                return Err(Error::new("duplicate LLVM switch key"));
            }
            let mut predecessor = LLVMGetInstructionParent(instruction);
            let additions = draft.phi_additions(function, predecessor, destination, incoming)?;
            update_branch_weights(instruction, successors, None);
            LLVMAddCase(instruction, key, destination);
            for (phi, mut value) in additions {
                LLVMAddIncoming(phi, &mut value, &mut predecessor, 1);
            }
        }
        Ok(())
    }

    /// Remove the case with this constant integer key and its PHI entries.
    /// Remaining cases, PHI entries and branch weights retain their order.
    pub fn remove_switch_case(&self, instruction: &Instruction, key: &Value) -> Result<()> {
        let mut draft = self.open()?;
        let (function, instruction) = self.instruction(instruction)?;
        let instruction = draft.value(instruction)?;
        let key = switch_key(instruction, function, key)?;
        draft.require_unborrowed_ir()?;

        // SAFETY: all LLVM references are live in the draft or its context.
        // The case, its destination, and the PHI positions are found before
        // inserting or removing any instruction.
        unsafe {
            let successors = LLVMGetNumSuccessors(instruction);
            let cases: Vec<_> = (0..successors - 1)
                .map(|index| switch_case_refs(instruction, index))
                .collect();
            let index = cases
                .iter()
                .position(|&(case_key, _)| case_key == key)
                .ok_or_else(|| Error::new("LLVM switch case key was not found"))?;
            let destination = cases[index].1;
            draft.require_attached(LLVMBasicBlockAsValue(destination))?;
            let predecessor = LLVMGetInstructionParent(instruction);
            let successor_index = index as u32 + 1;
            let parallel_edge = (0..successor_index)
                .filter(|&earlier| LLVMGetSuccessor(instruction, earlier) == destination)
                .count();
            let incoming = phis(destination)
                .map(|phi| {
                    incoming_positions(phi, predecessor)
                        .nth(parallel_edge)
                        .map(|position| (phi, position))
                        .ok_or_else(|| Error::new("LLVM switch case has no incoming PHI entry"))
                })
                .collect::<Result<Vec<_>>>()?;

            let builder = draft.module.context().llvm().create_builder();
            let raw_builder = builder.as_mut_ptr();
            // Insert after the old terminator so that erasing it passes its
            // debug records on to the new terminator at the same source position.
            LLVMPositionBuilderAtEnd(raw_builder, predecessor);
            let replacement = LLVMBuildSwitch(
                raw_builder,
                LLVMGetOperand(instruction, 0),
                LLVMGetSwitchDefaultDest(instruction),
                successors - 2,
            );
            for (position, (case_key, block)) in cases.into_iter().enumerate() {
                if position != index {
                    LLVMAddCase(replacement, case_key, block);
                }
            }
            copy_instruction_metadata(instruction, replacement);
            update_branch_weights(replacement, successors, Some(successor_index));
            LLVMInstructionEraseFromParent(instruction);

            let mut replacements = HashMap::from([(instruction, replacement)]);
            for (phi, remove) in incoming {
                let replacement = without_incoming(raw_builder, phi, |index| index == remove);
                replacements.insert(phi, replacement);
            }
            drop(builder);
            draft.remap(&replacements);
        }
        Ok(())
    }
}

fn switch_key(instruction: LLVMValueRef, function: &Function, key: &Value) -> Result<LLVMValueRef> {
    let key = key.operand(function)?.as_value_ref();
    // SAFETY: the instruction and operand are live. A switch has a condition
    // at operand zero and its ConstantInt keys are uniqued by the context.
    unsafe {
        if LLVMIsASwitchInst(instruction).is_null() {
            return Err(Error::new("expected an LLVM switch instruction"));
        }
        if LLVMIsAConstantInt(key).is_null()
            || LLVMTypeOf(key) != LLVMTypeOf(LLVMGetOperand(instruction, 0))
        {
            return Err(Error::new(
                "switch keys must be integer constants of the condition's type",
            ));
        }
    }
    Ok(key)
}

fn update_branch_weights(instruction: LLVMValueRef, old_successors: u32, removed: Option<u32>) {
    // SAFETY: the caller retains the switch and its metadata. Reading the
    // metadata node uses its reported length. Weight positions are checked
    // by the node length before reading or removing an operand.
    unsafe {
        let context = LLVMGetValueContext(instruction);
        let kind = LLVMGetMDKindIDInContext(context, c"prof".as_ptr(), 4);
        let profile = LLVMGetMetadata(instruction, kind);
        if profile.is_null() {
            return;
        }
        let count = LLVMGetMDNodeNumOperands(profile);
        let Some(prefix @ (1 | 2)) = count.checked_sub(old_successors) else {
            return;
        };
        let mut operands = vec![ptr::null_mut(); count as usize];
        LLVMGetMDNodeOperands(profile, operands.as_mut_ptr());
        if operands[0].is_null() || LLVMIsAMDString(operands[0]).is_null() {
            return;
        }
        let mut tag_len = 0;
        let tag = LLVMGetMDString(operands[0], &mut tag_len);
        if tag.is_null()
            || slice::from_raw_parts(tag.cast::<u8>(), tag_len as usize) != b"branch_weights"
        {
            return;
        }

        match removed {
            Some(index) => {
                operands.remove((prefix + index) as usize);
            }
            None => operands.push(LLVMConstInt(LLVMInt32TypeInContext(context), 0, 0)),
        }
        let mut metadata: Vec<_> = operands
            .into_iter()
            .map(|value| LLVMValueAsMetadata(value))
            .collect();
        let node = LLVMMDNodeInContext2(context, metadata.as_mut_ptr(), metadata.len());
        LLVMSetMetadata(instruction, kind, LLVMMetadataAsValue(context, node));
    }
}

use llvm_sys::core::*;
use llvm_sys::debuginfo::{LLVMInstructionGetDebugLoc, LLVMInstructionSetDebugLoc};
use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use llvm_sys::{LLVMOpcode::*, LLVMTailCallKind::LLVMTailCallKindMustTail};

use super::{Block, Draft, Edit, Instruction, instruction_name, is_eh_pad};
use crate::{Error, Result};

#[derive(Clone, Copy)]
enum Position {
    Before,
    After,
}

#[derive(Clone, Copy)]
enum Placement {
    Move,
    Reinsert,
}

impl Draft {
    pub(super) fn require_attached(&self, value: LLVMValueRef) -> Result<()> {
        // Keep draft nodes parented between detachment and reinsertion so LLVM
        // retains their ownership, PHI predecessors, and block addresses.
        if self.detached.is_empty() {
            return Ok(());
        }
        // SAFETY: the caller passes a live draft value. Draft instructions
        // retain their parents while any detached marks are pending.
        let detached = self.detached.contains(&value)
            || unsafe {
                !LLVMIsAInstruction(value).is_null()
                    && self
                        .detached
                        .contains(&LLVMBasicBlockAsValue(LLVMGetInstructionParent(value)))
            };
        if detached {
            return Err(Error::new("LLVM object is detached in this edit"));
        }
        Ok(())
    }

    fn place_value(&self, source: LLVMValueRef, placement: Placement) -> Result<LLVMValueRef> {
        let value = self.mapped_value(source)?;
        match placement {
            Placement::Move => self.require_attached(value)?,
            Placement::Reinsert if !self.detached.contains(&value) => {
                return Err(Error::new("LLVM object is not detached in this edit"));
            }
            Placement::Reinsert => {}
        }
        Ok(value)
    }
}

impl Edit {
    /// Move an ordinary instruction before another instruction in its function.
    /// Debug records stay at their original position. The caller determines
    /// whether the move preserves effects.
    pub fn move_before(&self, instruction: &Instruction, anchor: &Instruction) -> Result<()> {
        self.place_instruction(instruction, anchor, Position::Before, Placement::Move)
    }

    /// Move an ordinary instruction after another instruction in its function.
    /// Debug records stay at their original position. The caller determines
    /// whether the move preserves effects.
    pub fn move_after(&self, instruction: &Instruction, anchor: &Instruction) -> Result<()> {
        self.place_instruction(instruction, anchor, Position::After, Placement::Move)
    }

    /// Mark an ordinary instruction for reinsertion in this edit. Its original
    /// handle identifies it, and it must be reinserted before finishing.
    pub fn detach_instruction(&self, instruction: &Instruction) -> Result<()> {
        let mut draft = self.open()?;
        let (_, source) = self.instruction(instruction)?;
        let instruction = draft.value(source)?;
        check_movable_instruction(instruction)?;
        draft.detached.insert(instruction);
        Ok(())
    }

    /// Reinsert a detached instruction before an attached instruction in the
    /// same function.
    pub fn insert_instruction_before(
        &self,
        instruction: &Instruction,
        anchor: &Instruction,
    ) -> Result<()> {
        self.place_instruction(instruction, anchor, Position::Before, Placement::Reinsert)
    }

    /// Reinsert a detached instruction after an attached instruction in the
    /// same function.
    pub fn insert_instruction_after(
        &self,
        instruction: &Instruction,
        anchor: &Instruction,
    ) -> Result<()> {
        self.place_instruction(instruction, anchor, Position::After, Placement::Reinsert)
    }

    fn place_instruction(
        &self,
        instruction: &Instruction,
        anchor: &Instruction,
        position: Position,
        placement: Placement,
    ) -> Result<()> {
        let mut draft = self.open()?;
        let (function, source) = self.instruction(instruction)?;
        let (anchor_function, anchor) = anchor.edit_source();
        function.check(anchor_function)?;
        let instruction = draft.place_value(source, placement)?;
        let anchor = draft.value(anchor)?;
        if instruction == anchor {
            return Ok(());
        }
        check_movable_instruction(instruction)?;

        // SAFETY: these are draft instructions in the same function. All
        // checks precede the LLVM removal and insertion. Removing the source
        // leaves its debug records behind, and its own debug location follows it.
        unsafe {
            let before = match position {
                Position::Before => anchor,
                Position::After => LLVMGetNextInstruction(anchor),
            };
            if before.is_null() {
                return Err(Error::new(
                    "cannot insert an instruction after an LLVM terminator",
                ));
            }
            if is_phi_or_eh_pad(before) {
                return Err(Error::new(
                    "cannot insert an instruction before an LLVM PHI or EH pad",
                ));
            }
            let previous = LLVMGetPreviousInstruction(before);
            if follows_musttail(previous) {
                return Err(Error::new(
                    "cannot insert an instruction between an LLVM musttail call and its return",
                ));
            }
            if before != instruction && previous != instruction {
                draft.require_unborrowed_ir()?;
                let name = instruction_name(instruction);
                let debug_location = LLVMInstructionGetDebugLoc(instruction);
                let builder = draft.module.context().llvm().create_builder();
                LLVMPositionBuilderBeforeInstrAndDbgRecords(builder.as_mut_ptr(), before);
                LLVMInstructionRemoveFromParent(instruction);
                LLVMInsertIntoBuilder(builder.as_mut_ptr(), instruction);
                if !name.is_empty() {
                    LLVMSetValueName2(instruction, name.as_ptr().cast(), name.len());
                }
                LLVMInstructionSetDebugLoc(instruction, debug_location);
            }
        }
        if let Placement::Reinsert = placement {
            draft.detached.remove(&instruction);
        }
        Ok(())
    }

    /// Move a block before another block in its function. The entry block
    /// remains first and the CFG retains the same edges.
    pub fn move_block_before(&self, block: &Block, anchor: &Block) -> Result<()> {
        self.place_block(block, anchor, Position::Before, Placement::Move)
    }

    /// Move a block after another block in its function. The entry block
    /// remains first and the CFG retains the same edges.
    pub fn move_block_after(&self, block: &Block, anchor: &Block) -> Result<()> {
        self.place_block(block, anchor, Position::After, Placement::Move)
    }

    /// Mark a non-entry block for reinsertion in this edit. Its original
    /// handle identifies it, and it must be reinserted before finishing.
    pub fn detach_block(&self, block: &Block) -> Result<()> {
        let mut draft = self.open()?;
        self.source.check(block.function.module())?;
        let block = draft.block(block)?;
        // SAFETY: this draft block retains a live parent function.
        unsafe {
            if block_entry(block) == block {
                return Err(Error::new("cannot detach the LLVM entry block"));
            }
            draft.detached.insert(LLVMBasicBlockAsValue(block));
        }
        Ok(())
    }

    /// Reinsert a detached block before an attached non-entry block in the
    /// same function. Its CFG edges and instruction identities are preserved.
    pub fn insert_block_before(&self, block: &Block, anchor: &Block) -> Result<()> {
        self.place_block(block, anchor, Position::Before, Placement::Reinsert)
    }

    /// Reinsert a detached block after an attached block in the same function.
    /// Its CFG edges and instruction identities are preserved.
    pub fn insert_block_after(&self, block: &Block, anchor: &Block) -> Result<()> {
        self.place_block(block, anchor, Position::After, Placement::Reinsert)
    }

    fn place_block(
        &self,
        block: &Block,
        anchor: &Block,
        position: Position,
        placement: Placement,
    ) -> Result<()> {
        let mut draft = self.open()?;
        self.source.check(block.function.module())?;
        block.function.check(&anchor.function)?;
        let anchor = draft.block(anchor)?;
        // SAFETY: source and mapped blocks belong to the checked source and
        // draft functions, respectively, and all retain their LLVM parents.
        unsafe {
            let value = draft.place_value(LLVMBasicBlockAsValue(block.raw), placement)?;
            let block = LLVMValueAsBasicBlock(value);
            let before = matches!(position, Position::Before);
            let adjacent = if before {
                LLVMGetNextBasicBlock(block)
            } else {
                LLVMGetPreviousBasicBlock(block)
            };
            if block != anchor && adjacent != anchor {
                let entry = block_entry(block);
                if block == entry || (before && anchor == entry) {
                    return Err(Error::new("the LLVM entry block must remain first"));
                }
                match position {
                    Position::Before => LLVMMoveBasicBlockBefore(block, anchor),
                    Position::After => LLVMMoveBasicBlockAfter(block, anchor),
                }
            }
            if let Placement::Reinsert = placement {
                draft.detached.remove(&value);
            }
        }
        Ok(())
    }
}

fn block_entry(block: LLVMBasicBlockRef) -> LLVMBasicBlockRef {
    // SAFETY: the caller passes a block attached to a live function.
    unsafe { LLVMGetEntryBasicBlock(LLVMGetBasicBlockParent(block)) }
}

fn is_phi_or_eh_pad(instruction: LLVMValueRef) -> bool {
    // SAFETY: the caller passes a live instruction.
    (unsafe { LLVMGetInstructionOpcode(instruction) } == LLVMPHI) || is_eh_pad(instruction)
}

fn check_movable_instruction(instruction: LLVMValueRef) -> Result<()> {
    // SAFETY: the caller passes a live draft instruction attached to a block.
    unsafe {
        if !LLVMIsATerminatorInst(instruction).is_null() || is_phi_or_eh_pad(instruction) {
            return Err(Error::new(
                "cannot move LLVM terminators, EH pads, or PHI nodes",
            ));
        }
        if is_musttail_call(instruction)
            || (LLVMGetInstructionOpcode(instruction) == LLVMBitCast
                && is_musttail_call(LLVMGetPreviousInstruction(instruction)))
        {
            return Err(Error::new(
                "cannot move part of an LLVM musttail return sequence",
            ));
        }
    }
    Ok(())
}

fn is_musttail_call(instruction: LLVMValueRef) -> bool {
    // SAFETY: the pointer, when non-null, refers to a live instruction.
    unsafe {
        !instruction.is_null()
            && LLVMGetInstructionOpcode(instruction) == LLVMCall
            && LLVMGetTailCallKind(instruction) == LLVMTailCallKindMustTail
    }
}

fn follows_musttail(previous: LLVMValueRef) -> bool {
    if previous.is_null() {
        return false;
    }
    // SAFETY: the pointer refers to a live instruction in a draft block.
    unsafe {
        let call = if LLVMGetInstructionOpcode(previous) == LLVMBitCast {
            LLVMGetPreviousInstruction(previous)
        } else {
            previous
        };
        is_musttail_call(call)
    }
}

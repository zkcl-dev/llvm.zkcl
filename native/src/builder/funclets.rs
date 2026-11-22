use std::ptr;

use inkwell::llvm_sys::core::{
    LLVMAddHandler, LLVMBuildCatchPad, LLVMBuildCatchRet, LLVMBuildCatchSwitch,
    LLVMBuildCleanupPad, LLVMBuildCleanupRet, LLVMGetNumHandlers,
};
use inkwell::llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use inkwell::values::InstructionOpcode::{
    CatchPad, CatchSwitch, CleanupPad, CleanupRet, Invoke, LandingPad, Phi,
};
use inkwell::values::{AsValueRef, InstructionOpcode, InstructionValue};

use super::exception::check_personality;
use super::{Builder, InsertionPoint, MAX_USER_OPERANDS, c_name};
use crate::{Block, Error, Function, Instruction, Result, Value};

impl Builder {
    /// End a dispatch block with an initially empty catchswitch.
    /// Append handlers in match order with [`Instruction::add_catch_switch_handler`] before verification.
    /// A missing parent means `within none`. A missing unwind block means `unwind to caller`.
    pub fn catch_switch(
        &self,
        parent: Option<&Instruction>,
        unwind: Option<&Block>,
        name: &str,
    ) -> Result<Instruction> {
        let point = self.eh_pad_insertion("catchswitch")?;
        let InsertionPoint::End(block) = &*point else {
            return Err(Error::new("LLVM catchswitch must terminate its block"));
        };
        let parent = enclosing_pad(&block.function, parent)?;
        let unwind = unwind_block(block, unwind)?;
        let name = c_name(name)?;
        // SAFETY: the parent and unwind destination belong to this function. LLVM
        // substitutes the token `none` for a null parent and accepts a null unwind.
        let instruction = unsafe {
            InstructionValue::new(LLVMBuildCatchSwitch(
                self.llvm().as_mut_ptr(),
                parent,
                unwind,
                0,
                name.as_ptr(),
            ))
        };
        Ok(block.instruction(instruction))
    }

    /// Build a catchpad under its catchswitch. Add this block as a handler of that switch.
    pub fn catch_pad(
        &self,
        switch: &Instruction,
        args: &[Value],
        name: &str,
    ) -> Result<Instruction> {
        let point = self.eh_pad_insertion("catchpad")?;
        let block = point.block();
        block.function.check(switch.block().function())?;
        if switch.llvm().get_opcode() != CatchSwitch {
            return Err(Error::new("LLVM catchpad requires a catchswitch parent"));
        }
        if switch
            .exception_unwind_destination()
            .is_some_and(|unwind| unwind.same_as(block))
        {
            return Err(Error::new("LLVM catchswitch cannot unwind to its handler"));
        }
        let mut args = pad_args(&block.function, args)?;
        let name = c_name(name)?;
        // SAFETY: the live catchswitch and arguments are scoped to the insertion function.
        let instruction = unsafe {
            InstructionValue::new(LLVMBuildCatchPad(
                self.llvm().as_mut_ptr(),
                switch.llvm().as_value_ref(),
                args.as_mut_ptr(),
                args.len() as u32,
                name.as_ptr(),
            ))
        };
        Ok(block.instruction(instruction))
    }

    /// Build a cleanuppad, optionally nested within a catchpad or cleanuppad.
    pub fn cleanup_pad(
        &self,
        parent: Option<&Instruction>,
        args: &[Value],
        name: &str,
    ) -> Result<Instruction> {
        let point = self.eh_pad_insertion("cleanuppad")?;
        let block = point.block();
        let parent = enclosing_pad(&block.function, parent)?;
        let mut args = pad_args(&block.function, args)?;
        let name = c_name(name)?;
        // SAFETY: these arguments and the optional enclosing pad are live in this function.
        // LLVM substitutes the token `none` for a null parent.
        let instruction = unsafe {
            InstructionValue::new(LLVMBuildCleanupPad(
                self.llvm().as_mut_ptr(),
                parent,
                args.as_mut_ptr(),
                args.len() as u32,
                name.as_ptr(),
            ))
        };
        Ok(block.instruction(instruction))
    }

    /// End a catch funclet, transferring control to a normal block in this function.
    pub fn catch_ret(&self, pad: &Instruction, destination: &Block) -> Result<Instruction> {
        let block = self.terminator_insertion()?;
        check_exit_pad(&block, pad, CatchPad)?;
        check_destination(&block, destination)?;
        if block_eh_pad(destination).is_some() {
            return Err(Error::new("LLVM catchret requires a normal destination"));
        }
        // SAFETY: the pad is a live CatchPadInst and the destination is in this function.
        let instruction = unsafe {
            InstructionValue::new(LLVMBuildCatchRet(
                self.llvm().as_mut_ptr(),
                pad.llvm().as_value_ref(),
                destination.llvm().as_mut_ptr(),
            ))
        };
        Ok(block.instruction(instruction))
    }

    /// End a cleanup funclet, continuing unwinding to a block or to the caller.
    pub fn cleanup_ret(&self, pad: &Instruction, unwind: Option<&Block>) -> Result<Instruction> {
        let block = self.terminator_insertion()?;
        check_exit_pad(&block, pad, CleanupPad)?;
        if unwind.is_some_and(|destination| destination.same_as(pad.block())) {
            return Err(Error::new("LLVM cleanupret must exit its cleanuppad"));
        }
        let unwind = unwind_block(&block, unwind)?;
        // SAFETY: only a live CleanupPadInst reaches the C cast, with a checked unwind block.
        let instruction = unsafe {
            InstructionValue::new(LLVMBuildCleanupRet(
                self.llvm().as_mut_ptr(),
                pad.llvm().as_value_ref(),
                unwind,
            ))
        };
        Ok(block.instruction(instruction))
    }
}

impl Instruction {
    /// Append a handler to a catchswitch. Its block may be filled in later.
    pub fn add_catch_switch_handler(&self, handler: &Block) -> Result<()> {
        let switch = self.llvm();
        if switch.get_opcode() != CatchSwitch {
            return Err(Error::new("LLVM catch handler requires a catchswitch"));
        }
        check_destination(self.block(), handler)?;
        if self
            .exception_unwind_destination()
            .is_some_and(|unwind| unwind.same_as(handler))
        {
            return Err(Error::new("LLVM catchswitch cannot unwind to its handler"));
        }
        if let Some(pad) = block_eh_pad(handler)
            && (pad.llvm().get_opcode() != CatchPad
                || !pad
                    .exception_parent_pad()
                    .is_some_and(|parent| parent.same_as(self)))
        {
            return Err(Error::new(
                "LLVM catchswitch handler must begin with a catchpad of that switch",
            ));
        }
        if self.operand_count() as usize >= MAX_USER_OPERANDS {
            return Err(Error::new("too many LLVM catchswitch handlers"));
        }
        // SAFETY: the opcode, operand count, and destination scope were checked.
        unsafe { LLVMAddHandler(switch.as_value_ref(), handler.llvm().as_mut_ptr()) };
        Ok(())
    }

    /// Return the number of catchswitch handlers, excluding the unwind destination.
    pub fn catch_switch_handler_count(&self) -> u32 {
        let instruction = self.llvm();
        if instruction.get_opcode() == CatchSwitch {
            // SAFETY: only a live catchswitch reaches the catchswitch-specific query.
            unsafe { LLVMGetNumHandlers(instruction.as_value_ref()) }
        } else {
            0
        }
    }

    /// Return a handler in match order, starting at index zero.
    pub fn catch_switch_handler(&self, index: u32) -> Option<Block> {
        let handlers = self.catch_switch_handler_count();
        if index >= handlers {
            return None;
        }
        // LLVM numbers the optional unwind successor before the handlers.
        self.successor(self.successor_count() - handlers + index)
    }

    /// Return the enclosing instruction of a nested EH pad.
    pub fn exception_parent_pad(&self) -> Option<Instruction> {
        let index = match self.llvm().get_opcode() {
            CatchSwitch => 0,
            CatchPad | CleanupPad => self.operand_count() - 1,
            _ => return None,
        };
        self.operand(index)?.as_instruction()
    }

    /// Return the unwind successor of an invoke, catchswitch, or cleanupret when present.
    pub fn exception_unwind_destination(&self) -> Option<Block> {
        match self.llvm().get_opcode() {
            Invoke => self.successor(1),
            CleanupRet => self.successor(0),
            CatchSwitch if self.successor_count() > self.catch_switch_handler_count() => {
                self.successor(0)
            }
            _ => None,
        }
    }
}

fn enclosing_pad(function: &Function, parent: Option<&Instruction>) -> Result<LLVMValueRef> {
    let Some(parent) = parent else {
        return Ok(ptr::null_mut());
    };
    function.check(parent.block().function())?;
    let parent = parent.llvm();
    if !matches!(parent.get_opcode(), CatchPad | CleanupPad) {
        return Err(Error::new(
            "LLVM enclosing pad must be a catchpad or cleanuppad",
        ));
    }
    Ok(parent.as_value_ref())
}

fn pad_args(function: &Function, args: &[Value]) -> Result<Vec<LLVMValueRef>> {
    // Both pad instructions store the parent as one additional operand.
    if args.len() >= MAX_USER_OPERANDS {
        return Err(Error::new("too many LLVM funclet pad arguments"));
    }
    args.iter()
        .map(|arg| Ok(arg.operand(function)?.as_value_ref()))
        .collect()
}

fn block_eh_pad(block: &Block) -> Option<Instruction> {
    block
        .instructions()
        .find(|inst| inst.llvm().get_opcode() != Phi)
        .filter(|inst| {
            matches!(
                inst.llvm().get_opcode(),
                CatchPad | CleanupPad | CatchSwitch | LandingPad
            )
        })
}

fn check_exit_pad(block: &Block, pad: &Instruction, expected: InstructionOpcode) -> Result<()> {
    check_personality(&block.function)?;
    block.function.check(pad.block().function())?;
    if pad.llvm().get_opcode() != expected {
        return Err(Error::new(match expected {
            CatchPad => "LLVM catchret requires a catchpad",
            _ => "LLVM cleanupret requires a cleanuppad",
        }));
    }
    if block_eh_pad(block).is_some_and(|current| !current.same_as(pad)) {
        return Err(Error::new("LLVM funclet return must exit the current pad"));
    }
    Ok(())
}

fn check_destination(block: &Block, destination: &Block) -> Result<()> {
    block.function.check(&destination.function)?;
    if block.same_as(destination)
        || block
            .function
            .first_block()
            .is_some_and(|entry| entry.same_as(destination))
    {
        return Err(Error::new(
            "LLVM funclet destination cannot be the current or entry block",
        ));
    }
    Ok(())
}

fn unwind_block(block: &Block, unwind: Option<&Block>) -> Result<LLVMBasicBlockRef> {
    let Some(unwind) = unwind else {
        return Ok(ptr::null_mut());
    };
    check_destination(block, unwind)?;
    if block_eh_pad(unwind)
        .is_some_and(|pad| !matches!(pad.llvm().get_opcode(), CleanupPad | CatchSwitch))
    {
        return Err(Error::new(
            "LLVM funclet must unwind to a catchswitch or cleanuppad",
        ));
    }
    Ok(unwind.llvm().as_mut_ptr())
}

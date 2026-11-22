use std::collections::HashSet;

use inkwell::llvm_sys::core::{
    LLVMAddDestination, LLVMBuildCallBr, LLVMBuildIndirectBr, LLVMBuildSelect, LLVMIsAConstantInt,
};
use inkwell::types::{AsTypeRef, BasicTypeEnum};
use inkwell::values::{
    AsValueRef, BasicValue, BasicValueEnum, InstructionOpcode, InstructionValue, IntValue,
};

use super::call::{call_arguments, call_name, prepare_operand_bundles};
use super::vector::vector_shape;
use super::{
    Builder, MAX_USER_OPERANDS, arithmetic_element_type, c_name, int_value, pointer_value,
    same_type_operands,
};
use crate::{Block, Error, Function, Instruction, OperandBundle, Result, Type, Value, c_text};

impl Builder {
    pub fn phi(&self, ty: &Type, name: &str) -> Result<Value> {
        let point = self.insertion_point()?;
        if point
            .previous_instruction()
            .is_some_and(|previous| previous.get_opcode() != InstructionOpcode::Phi)
        {
            return Err(Error::new(
                "phi nodes must precede other instructions in a block",
            ));
        }
        let ty = ty.basic_in(self.context())?;
        let value = self.llvm().build_phi(ty, c_text(name)?)?;
        Ok(point.block().function.local(value.as_basic_value()))
    }

    pub fn ret(&self, value: Option<&Value>) -> Result<()> {
        let block = self.terminator_insertion()?;
        let function = &block.function;
        let value = value.map(|value| value.operand(function)).transpose()?;
        if value.as_ref().map(BasicValueEnum::get_type) != function.return_type()? {
            return Err(Error::new(
                "return value must match the function return type",
            ));
        }
        self.llvm()
            .build_return(value.as_ref().map(|value| value as &dyn BasicValue))?;
        Ok(())
    }

    pub fn br(&self, target: &Block) -> Result<()> {
        let block = self.terminator_insertion()?;
        block.function.check(&target.function)?;
        self.llvm().build_unconditional_branch(target.llvm())?;
        Ok(())
    }

    pub fn cond_br(&self, condition: &Value, yes: &Block, no: &Block) -> Result<()> {
        let block = self.terminator_insertion()?;
        let function = &block.function;
        function.check(&yes.function)?;
        function.check(&no.function)?;
        let condition = condition_value(condition, function)?;
        self.llvm()
            .build_conditional_branch(condition, yes.llvm(), no.llvm())?;
        Ok(())
    }

    /// End the block with an indirect branch. List every possible target of
    /// `address`, which must evaluate to a blockaddress in this function.
    pub fn indirect_br(&self, address: &Value, destinations: &[Block]) -> Result<Instruction> {
        let block = self.terminator_insertion()?;
        let function = block.function();
        if destinations.len() >= MAX_USER_OPERANDS {
            return Err(Error::new("too many LLVM indirectbr destinations"));
        }
        for destination in destinations {
            function.check(destination.function())?;
        }
        if let Some(target) = address.block_address_target() {
            function.check(target.function())?;
            if !destinations.iter().any(|block| block.same_as(&target)) {
                return Err(Error::new(
                    "LLVM indirectbr is missing its known destination",
                ));
            }
        }
        let address = pointer_value(address, function)?;
        // SAFETY: the insertion point and all labels belong to this function.
        // LLVM receives a pointer operand and the capacity for these labels.
        let instruction = unsafe {
            let raw = LLVMBuildIndirectBr(
                self.llvm().as_mut_ptr(),
                address.as_value_ref(),
                destinations.len() as u32,
            );
            for destination in destinations {
                LLVMAddDestination(raw, destination.llvm().as_mut_ptr());
            }
            InstructionValue::new(raw)
        };
        Ok(block.instruction(instruction))
    }

    /// Emit `callbr` for asm goto. The call must not unwind. Indirect labels
    /// are listed in the order of the assembly's label constraints.
    pub fn call_br(
        &self,
        asm: &Value,
        args: &[Value],
        normal: &Block,
        indirect: &[Block],
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        let block = self.terminator_insertion()?;
        let function = block.function();
        let info = asm
            .inline_asm_info()
            .ok_or_else(|| Error::new("LLVM callbr requires an inline asm callee"))?;
        if info.options.unwind {
            return Err(Error::new("LLVM callbr cannot unwind"));
        }
        if indirect.len() > MAX_USER_OPERANDS - 2 {
            return Err(Error::new("too many LLVM callbr destinations"));
        }
        let signature = info.signature.function_in(self.context())?;
        let callee = asm.scoped_ref(function)?;
        function.check(normal.function())?;
        let mut labels = Vec::with_capacity(indirect.len());
        for destination in indirect {
            function.check(destination.function())?;
            labels.push(destination.llvm().as_mut_ptr());
        }
        let other_operands = 2 + labels.len();
        let mut arguments = call_arguments(signature, function, args, other_operands)?;
        let mut bundles = prepare_operand_bundles(args.len(), other_operands, bundles, function)?;
        let name = call_name(signature, name)?;
        // SAFETY: the callee and arguments match the signature, and every
        // successor and bundle operand is live and scoped to this call.
        let instruction = unsafe {
            InstructionValue::new(LLVMBuildCallBr(
                self.llvm().as_mut_ptr(),
                signature.as_type_ref(),
                callee,
                normal.llvm().as_mut_ptr(),
                labels.as_mut_ptr(),
                labels.len() as u32,
                arguments.as_mut_ptr(),
                arguments.len() as u32,
                bundles.as_mut_ptr(),
                bundles.len(),
                name.as_ptr(),
            ))
        };
        Ok(block.instruction(instruction))
    }

    /// Select two same-type SSA values using an i1 condition or a matching vector of i1 lanes.
    pub fn select(&self, condition: &Value, yes: &Value, no: &Value, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        let function = &block.function;
        let condition = condition.operand(function)?;
        let (yes, no) = same_type_operands(yes, no, function)?;
        let condition_type = condition.get_type();
        if !matches!(arithmetic_element_type(condition_type), BasicTypeEnum::IntType(ty) if ty.get_bit_width() == 1)
        {
            return Err(Error::new(
                "LLVM select condition must be i1 or a vector of i1",
            ));
        }
        if let Some(shape) = vector_shape(condition_type)
            && vector_shape(yes.get_type()) != Some(shape)
        {
            return Err(Error::new(
                "LLVM select condition and value vectors must have the same shape",
            ));
        }
        let name = c_name(name)?;
        // SAFETY: the live value operands have the same type and the condition is i1 or a matching vector.
        let result = unsafe {
            BasicValueEnum::new(LLVMBuildSelect(
                self.llvm().as_mut_ptr(),
                condition.as_value_ref(),
                yes.as_value_ref(),
                no.as_value_ref(),
                name.as_ptr(),
            ))
        };
        Ok(function.local(result))
    }

    /// End the block with a switch on distinct constant integers of the condition's type.
    pub fn switch(
        &self,
        condition: &Value,
        default: &Block,
        cases: &[(&Value, &Block)],
    ) -> Result<()> {
        let block = self.terminator_insertion()?;
        let function = &block.function;
        let condition = int_value(condition, function)?;
        function.check(&default.function)?;
        if u32::try_from(cases.len()).is_err() {
            return Err(Error::new("too many LLVM switch cases"));
        }
        let mut keys = HashSet::with_capacity(cases.len());
        let mut destinations = Vec::with_capacity(cases.len());
        for (key, destination) in cases {
            function.check(&destination.function)?;
            let key = int_value(key, function)?;
            // SAFETY: the operand is live. LLVMAddCase requires a ConstantInt.
            let constant = unsafe { LLVMIsAConstantInt(key.as_value_ref()) };
            if constant.is_null() || key.get_type() != condition.get_type() {
                return Err(Error::new(
                    "switch keys must be integer constants of the condition's type",
                ));
            }
            // LLVM uniques constant integers of a given type within a context.
            if !keys.insert(constant) {
                return Err(Error::new("duplicate LLVM switch key"));
            }
            destinations.push((key, destination.llvm()));
        }
        self.llvm()
            .build_switch(condition, default.llvm(), &destinations)?;
        Ok(())
    }

    pub fn unreachable(&self) -> Result<()> {
        let _block = self.terminator_insertion()?;
        self.llvm().build_unreachable()?;
        Ok(())
    }
}

fn condition_value<'a>(condition: &'a Value, function: &Function) -> Result<IntValue<'a>> {
    let condition = int_value(condition, function)?;
    if condition.get_type().get_bit_width() != 1 {
        return Err(Error::new("LLVM condition must have type i1"));
    }
    Ok(condition)
}

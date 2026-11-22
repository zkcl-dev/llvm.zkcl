use std::{cell::Ref, ptr};

use inkwell::llvm_sys::core::{
    LLVMAddClause, LLVMBuildInvokeWithOperandBundles, LLVMBuildLandingPad, LLVMGetPersonalityFn,
    LLVMIsAConstantAggregateZero, LLVMIsAConstantArray, LLVMSetCleanup, LLVMSetPersonalityFn,
    LLVMTypeOf,
};
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::types::{AsTypeRef, BasicTypeEnum, FunctionType};
use inkwell::values::{
    AsValueRef, BasicValueEnum, CallSiteValue, FunctionValue, InstructionOpcode,
};

use super::call::{
    call_arguments, call_name, finish_call, indirect_callee, prepare_operand_bundles,
};
use super::{Builder, InsertionPoint, MAX_USER_OPERANDS, c_name, pointer_value};
use crate::ir::supported_value_type;
use crate::{Block, Error, Function, Instruction, OperandBundle, Result, Type, Value};

// An invoke stores its callee and both successors in addition to the arguments and bundles.
const INVOKE_NON_ARGUMENT_OPERANDS: usize = 3;

impl Function {
    /// Return the function's personality pointer, which may refer to an LLVM alias.
    pub fn personality(&self) -> Result<Option<Value>> {
        let function = self.llvm();
        if !function.has_personality_function() {
            return Ok(None);
        }
        // SAFETY: this live function has a personality constant owned by its module or context.
        let value = unsafe {
            let raw = LLVMGetPersonalityFn(function.as_value_ref());
            supported_value_type(LLVMTypeOf(raw))?;
            BasicValueEnum::new(raw)
        };
        Ok(Some(self.local(value)))
    }

    /// Attach a constant personality pointer to a function body, or clear it.
    /// The caller chooses the routine and ABI required by the target and language runtime.
    pub fn set_personality(&self, personality: Option<&Value>) -> Result<()> {
        let raw = if let Some(personality) = personality {
            personality.check_constant_publication()?;
            if !personality.is_constant() {
                return Err(Error::new("LLVM personality must be a constant pointer"));
            }
            pointer_value(personality, self)?.as_value_ref()
        } else {
            ptr::null_mut()
        };
        // SAFETY: the function is live and the optional constant pointer belongs to its scope.
        unsafe { LLVMSetPersonalityFn(self.llvm().as_value_ref(), raw) };
        Ok(())
    }
}

impl Builder {
    /// End a block with a direct invoke. Its SSA result is available on the normal edge.
    pub fn invoke(
        &self,
        callee: &Function,
        args: &[Value],
        normal: &Block,
        unwind: &Block,
        name: &str,
    ) -> Result<Instruction> {
        self.invoke_with_bundles(callee, args, normal, unwind, &[], name)
    }

    /// End a block with a direct invoke whose call site has operand bundles.
    pub fn invoke_with_bundles(
        &self,
        callee: &Function,
        args: &[Value],
        normal: &Block,
        unwind: &Block,
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        let block = self.invoke_insertion(normal, unwind)?;
        block.function.module.check(&callee.module)?;
        let function = callee.llvm();
        self.build_invoke_instruction(
            (&block, normal, unwind),
            function.get_type(),
            (function.as_value_ref(), Some(function)),
            args,
            bundles,
            name,
        )
    }

    /// Invoke an opaque function pointer with an explicit signature.
    pub fn invoke_indirect(
        &self,
        signature: &Type,
        callee: &Value,
        args: &[Value],
        normal: &Block,
        unwind: &Block,
        name: &str,
    ) -> Result<Instruction> {
        self.invoke_indirect_with_bundles(signature, callee, args, (normal, unwind), &[], name)
    }

    /// Invoke an opaque pointer with operand bundles and `(normal, unwind)` destinations.
    pub fn invoke_indirect_with_bundles(
        &self,
        signature: &Type,
        callee: &Value,
        args: &[Value],
        destinations: (&Block, &Block),
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        self.indirect_invoke((signature, None), callee, args, destinations, bundles, name)
    }

    /// Invoke an opaque pointer with a prototype's signature and call-site ABI.
    pub fn invoke_indirect_with_prototype(
        &self,
        prototype: &Function,
        callee: &Value,
        args: &[Value],
        normal: &Block,
        unwind: &Block,
        name: &str,
    ) -> Result<Instruction> {
        self.invoke_indirect_with_prototype_and_bundles(
            prototype,
            callee,
            args,
            (normal, unwind),
            &[],
            name,
        )
    }

    /// Invoke an opaque pointer with a prototype's ABI and operand bundles.
    pub fn invoke_indirect_with_prototype_and_bundles(
        &self,
        prototype: &Function,
        callee: &Value,
        args: &[Value],
        destinations: (&Block, &Block),
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        self.indirect_invoke(
            (&prototype.signature(), Some(prototype.llvm())),
            callee,
            args,
            destinations,
            bundles,
            name,
        )
    }

    fn indirect_invoke(
        &self,
        (signature, prototype): (&Type, Option<FunctionValue<'_>>),
        callee: &Value,
        args: &[Value],
        (normal, unwind): (&Block, &Block),
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        let block = self.invoke_insertion(normal, unwind)?;
        let signature = signature.function_in(self.context())?;
        let target = indirect_callee(signature, prototype, callee, &block.function)?;
        self.build_invoke_instruction(
            (&block, normal, unwind),
            signature,
            target,
            args,
            bundles,
            name,
        )
    }

    fn build_invoke_instruction(
        &self,
        (block, normal, unwind): (&Block, &Block, &Block),
        signature: FunctionType<'_>,
        (callee, abi): (LLVMValueRef, Option<FunctionValue<'_>>),
        args: &[Value],
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        let caller = &block.function;
        let mut args = call_arguments(signature, caller, args, INVOKE_NON_ARGUMENT_OPERANDS)?;
        let name = call_name(signature, name)?;
        let mut bundles =
            prepare_operand_bundles(args.len(), INVOKE_NON_ARGUMENT_OPERANDS, bundles, caller)?;
        // SAFETY: this is an unterminated block, both successors are in the same function,
        // and the callee, argument types, and bundles have been checked.
        let invoke = unsafe {
            CallSiteValue::new(LLVMBuildInvokeWithOperandBundles(
                self.llvm().as_mut_ptr(),
                signature.as_type_ref(),
                callee,
                args.as_mut_ptr(),
                args.len() as u32,
                normal.llvm().as_mut_ptr(),
                unwind.llvm().as_mut_ptr(),
                bundles.as_mut_ptr(),
                bundles.len(),
                name.as_ptr(),
            ))
        };
        Ok(finish_call(invoke, abi, block))
    }

    fn invoke_insertion(&self, normal: &Block, unwind: &Block) -> Result<Ref<'_, Block>> {
        let block = self.terminator_insertion()?;
        block.function.check(&normal.function)?;
        block.function.check(&unwind.function)?;
        if normal.same_as(unwind) {
            return Err(Error::new("LLVM invoke requires distinct destinations"));
        }
        if block
            .function
            .first_block()
            .is_some_and(|entry| entry.same_as(normal) || entry.same_as(unwind))
        {
            return Err(Error::new("LLVM invoke cannot target the entry block"));
        }
        Ok(block)
    }

    /// Build an Itanium-style landing pad as the first non-PHI instruction.
    /// Pointer constants are catch clauses and constant pointer arrays are filter clauses.
    pub fn landing_pad(
        &self,
        ty: &Type,
        cleanup: bool,
        clauses: &[Value],
        name: &str,
    ) -> Result<Value> {
        let point = self.eh_pad_insertion("landingpad")?;
        let function = &point.block().function;
        if !cleanup && clauses.is_empty() {
            return Err(Error::new("LLVM landingpad requires cleanup or a clause"));
        }
        let ty = ty.basic_in(self.context())?;
        check_exception_type(ty)?;
        if clauses.len() > MAX_USER_OPERANDS {
            return Err(Error::new("too many LLVM landingpad clauses"));
        }
        let count = clauses.len() as u32;
        let clauses = clauses
            .iter()
            .map(|clause| landing_pad_clause(clause, function))
            .collect::<Result<Vec<_>>>()?;
        let name = c_name(name)?;
        // SAFETY: the insertion point, type and clauses were validated. Null preserves the
        // personality already attached to the function.
        let value = unsafe {
            let raw = LLVMBuildLandingPad(
                self.llvm().as_mut_ptr(),
                ty.as_type_ref(),
                ptr::null_mut(),
                count,
                name.as_ptr(),
            );
            for clause in clauses {
                LLVMAddClause(raw, clause);
            }
            LLVMSetCleanup(raw, cleanup.into());
            BasicValueEnum::new(raw)
        };
        Ok(function.local(value))
    }

    pub(super) fn eh_pad_insertion(&self, opcode: &str) -> Result<Ref<'_, InsertionPoint>> {
        let point = self.insertion_point()?;
        let block = point.block();
        let function = &block.function;
        check_personality(function)?;
        if function
            .first_block()
            .is_some_and(|entry| entry.same_as(block))
        {
            return Err(Error::new(format!(
                "LLVM {opcode} cannot be in the entry block"
            )));
        }
        if point
            .previous_instruction()
            .is_some_and(|previous| previous.get_opcode() != InstructionOpcode::Phi)
            || matches!(&*point, InsertionPoint::Before(anchor) if matches!(
                anchor.llvm().get_opcode(),
                InstructionOpcode::Phi | InstructionOpcode::LandingPad | InstructionOpcode::CatchPad
                    | InstructionOpcode::CleanupPad | InstructionOpcode::CatchSwitch
            ))
        {
            return Err(Error::new(format!(
                "LLVM {opcode} must be the first and only EH pad after any phi nodes"
            )));
        }
        Ok(point)
    }

    /// Resume unwinding with an exception aggregate of the function's landing pad type.
    pub fn resume(&self, exception: &Value) -> Result<()> {
        let block = self.terminator_insertion()?;
        check_personality(&block.function)?;
        let exception = exception.operand(&block.function)?;
        check_exception_type(exception.get_type())?;
        self.llvm().build_resume(exception)?;
        Ok(())
    }
}

pub(super) fn check_personality(function: &Function) -> Result<()> {
    if !function.llvm().has_personality_function() {
        return Err(Error::new("LLVM exception handling requires a personality"));
    }
    Ok(())
}

fn check_exception_type(ty: BasicTypeEnum<'_>) -> Result<()> {
    if let BasicTypeEnum::StructType(ty) = ty
        && ty.count_fields() == 2
        && matches!(
            ty.get_field_type_at_index(0),
            Some(BasicTypeEnum::PointerType(_))
        )
        && matches!(
            ty.get_field_type_at_index(1),
            Some(BasicTypeEnum::IntType(_))
        )
    {
        return Ok(());
    }
    Err(Error::new(
        "Itanium-style LLVM exception type must be a struct with a pointer and integer selector",
    ))
}

fn landing_pad_clause(clause: &Value, function: &Function) -> Result<LLVMValueRef> {
    let value = clause.operand(function)?;
    let raw = value.as_value_ref();
    if clause.is_constant() {
        match value.get_type() {
            BasicTypeEnum::PointerType(_) => return Ok(raw),
            BasicTypeEnum::ArrayType(ty) if ty.get_element_type().is_pointer_type() => {
                // SAFETY: both predicates inspect a live constant. LLVM's verifier accepts these
                // two representations for filter clauses, including an empty zero array.
                if unsafe {
                    !LLVMIsAConstantArray(raw).is_null()
                        || !LLVMIsAConstantAggregateZero(raw).is_null()
                } {
                    return Ok(raw);
                }
            }
            _ => {}
        }
    }
    Err(Error::new(
        "LLVM landingpad clauses require a constant pointer or constant pointer array",
    ))
}

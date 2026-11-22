use std::ptr;

use inkwell::values::{AsValueRef, BasicValue, InstructionOpcode};
use llvm_sys::debuginfo::{
    LLVMDIBuilderCreateAutoVariable, LLVMDIBuilderCreateExpression,
    LLVMDIBuilderCreateParameterVariable, LLVMDIBuilderInsertDbgValueRecordBefore,
    LLVMDIBuilderInsertDeclareRecordBefore, LLVMDIFlagZero,
};
use llvm_sys::prelude::LLVMMetadataRef;

use super::{DebugInfo, DebugLocation, DebugScope, DebugType};
use crate::ir::BodyGeneration;
use crate::{Block, Error, Function, Result, Value, c_text};

pub struct DebugVariable {
    raw: LLVMMetadataRef,
    function: Function,
    subprogram: LLVMMetadataRef,
    body: Option<BodyGeneration>,
}

impl DebugVariable {
    pub(crate) fn check_live(&self) -> Result<()> {
        if let Some(body) = &self.body {
            body.check_live()?;
        }
        Ok(())
    }
}

enum VariableRecord<'a> {
    Value(&'a Value),
    Address(&'a Value),
}

impl DebugInfo {
    /// Describe a local variable or a zero-based parameter in the function's source scope.
    pub fn variable(
        &self,
        scope: &DebugScope,
        name: &str,
        ty: &DebugType,
        line: u32,
        parameter: Option<u16>,
    ) -> Result<DebugVariable> {
        scope.check_live()?;
        let builder = self.builder()?;
        self.require_full()?;
        if self.subprogram_finished(scope.subprogram)? {
            return Err(Error::new("LLVM function debug info is already finished"));
        }
        let name = c_text(name)?;
        let ty = ty.in_module(&self.module)?;

        // SAFETY: the live scope and type belong to the module. Parameter indices
        // are checked against the function and fit LLVM's one-based 16-bit field.
        let raw = unsafe {
            match parameter {
                Some(index) => {
                    let function = scope.function.llvm();
                    if scope.raw != scope.subprogram {
                        return Err(Error::new(
                            "describe debug parameters in the function's scope",
                        ));
                    }
                    if u32::from(index) >= function.count_params() || index == u16::MAX {
                        return Err(Error::new("LLVM debug parameter index is out of range"));
                    }
                    LLVMDIBuilderCreateParameterVariable(
                        builder,
                        scope.raw,
                        name.as_ptr().cast(),
                        name.len(),
                        u32::from(index) + 1,
                        scope.file,
                        line,
                        ty,
                        1,
                        LLVMDIFlagZero,
                    )
                }
                None => LLVMDIBuilderCreateAutoVariable(
                    builder,
                    scope.raw,
                    name.as_ptr().cast(),
                    name.len(),
                    scope.file,
                    line,
                    ty,
                    1,
                    LLVMDIFlagZero,
                    0,
                ),
            }
        };
        Ok(DebugVariable {
            raw,
            function: scope.function.clone(),
            subprogram: scope.subprogram,
            body: scope.body.clone(),
        })
    }

    /// Describe a value before an instruction or, by default, before the block terminator.
    pub fn record_value(
        &self,
        block: &Block,
        variable: &DebugVariable,
        value: &Value,
        location: &DebugLocation,
        before: Option<&Value>,
    ) -> Result<()> {
        self.record_variable(
            block,
            variable,
            VariableRecord::Value(value),
            location,
            before,
        )
    }

    /// Declare a variable's storage address before an instruction or the block terminator.
    pub fn record_address(
        &self,
        block: &Block,
        variable: &DebugVariable,
        address: &Value,
        location: &DebugLocation,
        before: Option<&Value>,
    ) -> Result<()> {
        self.record_variable(
            block,
            variable,
            VariableRecord::Address(address),
            location,
            before,
        )
    }

    fn record_variable(
        &self,
        block: &Block,
        variable: &DebugVariable,
        record: VariableRecord<'_>,
        location: &DebugLocation,
        before: Option<&Value>,
    ) -> Result<()> {
        let builder = self.builder()?;
        self.subprogram_finished(variable.subprogram)?;
        variable.check_live()?;
        block.check_live()?;
        variable.function.check(&block.function)?;
        location.check_for_function(&block.function)?;
        if location.inlined_at().is_some() {
            return Err(Error::new(
                "LLVM variable records require a non-inlined debug location",
            ));
        }
        let is_address = matches!(record, VariableRecord::Address(_));
        let (VariableRecord::Value(value) | VariableRecord::Address(value)) = record;
        let value = value.operand(&block.function)?;
        if is_address && !value.is_pointer_value() {
            return Err(Error::new(
                "an LLVM debug storage address must be a pointer",
            ));
        }
        let insert = if is_address {
            LLVMDIBuilderInsertDeclareRecordBefore
        } else {
            LLVMDIBuilderInsertDbgValueRecordBefore
        };
        let llvm_block = block.llvm();
        let instruction = if let Some(before) = before {
            let instruction = before.operand(&block.function)?.as_instruction_value();
            match instruction {
                Some(instruction)
                    if instruction.get_parent() == Some(llvm_block)
                        && instruction.get_opcode() != InstructionOpcode::Phi =>
                {
                    instruction
                }
                _ => {
                    return Err(Error::new(
                        "expected a non-phi instruction in the LLVM debug record's block",
                    ));
                }
            }
        } else {
            llvm_block.get_terminator().ok_or_else(|| {
                Error::new(
                    "supply an instruction or finish the block before inserting a debug record",
                )
            })?
        };
        // SAFETY: full compile units use the new debug format. All operands
        // belong to the retained function and the anchor is an inserted instruction.
        unsafe {
            let expression = LLVMDIBuilderCreateExpression(builder, ptr::null_mut(), 0);
            insert(
                builder,
                value.as_value_ref(),
                variable.raw,
                expression,
                location.raw,
                instruction.as_value_ref(),
            );
        }
        Ok(())
    }
}

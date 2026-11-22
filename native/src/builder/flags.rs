use inkwell::llvm_sys::LLVMOpcode::*;
use inkwell::llvm_sys::core::{
    LLVMBuildBinOp, LLVMBuildFreeze, LLVMDeleteInstruction, LLVMIsAConstant, LLVMSetExact,
    LLVMSetNSW, LLVMSetNUW, LLVMSetOperand,
};
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::values::{AsValueRef, BasicValueEnum};

use super::{Builder, arithmetic_element_type, c_name, same_type_operands};
use crate::{Error, Result, Value};

#[derive(Clone, Copy)]
pub enum IntOp {
    Add,
    Sub,
    Mul,
    UDiv,
    SDiv,
    URem,
    SRem,
    And,
    Or,
    Xor,
    Shl,
    LShr,
    AShr,
}

/// LLVM arithmetic promises applied to an integer scalar or vector operation.
/// Violating a promise makes the corresponding result or vector lane poison.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct IntFlags {
    /// Signed overflow is excluded for add, subtract, multiply, shift left, or truncation.
    pub nsw: bool,
    /// Unsigned overflow is excluded for add, subtract, multiply, shift left, or truncation.
    pub nuw: bool,
    /// Division has no remainder, or a right shift discards only zero bits.
    pub exact: bool,
}

impl Builder {
    pub fn int_binop(&self, op: IntOp, lhs: &Value, rhs: &Value, name: &str) -> Result<Value> {
        self.int_binop_with_flags(op, lhs, rhs, IntFlags::default(), name)
    }

    /// Build an integer operation with LLVM's no-wrap or exact flags.
    /// A flagged operation is emitted even when both operands are constants.
    pub fn int_binop_with_flags(
        &self,
        op: IntOp,
        lhs: &Value,
        rhs: &Value,
        flags: IntFlags,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let (lhs, rhs) = same_type_operands(lhs, rhs, &block.function)?;
        if !arithmetic_element_type(lhs.get_type()).is_int_type() {
            return Err(Error::new("expected integer operands"));
        }
        if (flags.nsw || flags.nuw)
            && !matches!(op, IntOp::Add | IntOp::Sub | IntOp::Mul | IntOp::Shl)
        {
            return Err(Error::new("nsw and nuw require add, sub, mul, or shl"));
        }
        if flags.exact && !matches!(op, IntOp::UDiv | IntOp::SDiv | IntOp::LShr | IntOp::AShr) {
            return Err(Error::new("exact requires udiv, sdiv, lshr, or ashr"));
        }
        let opcode = match op {
            IntOp::Add => LLVMAdd,
            IntOp::Sub => LLVMSub,
            IntOp::Mul => LLVMMul,
            IntOp::UDiv => LLVMUDiv,
            IntOp::SDiv => LLVMSDiv,
            IntOp::URem => LLVMURem,
            IntOp::SRem => LLVMSRem,
            IntOp::And => LLVMAnd,
            IntOp::Or => LLVMOr,
            IntOp::Xor => LLVMXor,
            IntOp::Shl => LLVMShl,
            IntOp::LShr => LLVMLShr,
            IntOp::AShr => LLVMAShr,
        };
        let name = c_name(name)?;
        let lhs = lhs.as_value_ref();
        let rhs = rhs.as_value_ref();
        let builder = self.llvm().as_mut_ptr();
        // SAFETY: both operands are live integers of the same scalar or vector
        // type. The opcode has been checked to support the requested flags.
        let value = unsafe {
            let build = |lhs| LLVMBuildBinOp(builder, opcode, lhs, rhs, name.as_ptr());
            let raw = if flags == IntFlags::default() {
                build(lhs)
            } else {
                self.build_without_constant_folding(lhs, Some(rhs), build)
            };
            if flags.nsw {
                LLVMSetNSW(raw, 1);
            }
            if flags.nuw {
                LLVMSetNUW(raw, 1);
            }
            if flags.exact {
                LLVMSetExact(raw, 1);
            }
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(value))
    }

    /// `build` must emit a unary or binary instruction with `lhs` as operand
    /// zero and `rhs`, when present, as operand one. It may fold all-constant
    /// operands, and must emit an instruction when any operand is nonconstant.
    pub(super) unsafe fn build_without_constant_folding(
        &self,
        lhs: LLVMValueRef,
        rhs: Option<LLVMValueRef>,
        build: impl FnOnce(LLVMValueRef) -> LLVMValueRef,
    ) -> LLVMValueRef {
        // SAFETY: the caller provides live operands and the build operation
        // described above. A detached, same-type freeze prevents LLVM's default
        // folder from returning a constant before the caller can set the flags.
        unsafe {
            if LLVMIsAConstant(lhs).is_null()
                || rhs.is_some_and(|rhs| LLVMIsAConstant(rhs).is_null())
            {
                return build(lhs);
            }
            let detached = self.context().llvm().create_builder();
            let placeholder = LLVMBuildFreeze(detached.as_mut_ptr(), lhs, c"".as_ptr());
            let raw = build(placeholder);
            LLVMSetOperand(raw, 0, lhs);
            LLVMDeleteInstruction(placeholder);
            raw
        }
    }
}

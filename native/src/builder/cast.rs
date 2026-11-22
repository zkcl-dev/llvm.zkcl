use inkwell::types::{AsTypeRef, BasicTypeEnum};
use inkwell::values::{AsValueRef, BasicValueEnum};
use llvm_sys::LLVMOpcode;
use llvm_sys::LLVMOpcode::*;
use llvm_sys::core::{LLVMBuildCast, LLVMBuildIntCast2};

use super::vector::vector_shape;
use super::{Builder, arithmetic_element_type, c_name};
use crate::{Error, Result, Type, Value};

/// An explicit LLVM cast instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum CastOp {
    Trunc = LLVMTrunc as u32,
    ZExt = LLVMZExt as u32,
    SExt = LLVMSExt as u32,
    FpToUi = LLVMFPToUI as u32,
    FpToSi = LLVMFPToSI as u32,
    UiToFp = LLVMUIToFP as u32,
    SiToFp = LLVMSIToFP as u32,
    FpTrunc = LLVMFPTrunc as u32,
    FpExt = LLVMFPExt as u32,
    PtrToInt = LLVMPtrToInt as u32,
    IntToPtr = LLVMIntToPtr as u32,
    BitCast = LLVMBitCast as u32,
    AddrSpaceCast = LLVMAddrSpaceCast as u32,
}

impl CastOp {
    fn opcode(self) -> LLVMOpcode {
        match self {
            Self::Trunc => LLVMTrunc,
            Self::ZExt => LLVMZExt,
            Self::SExt => LLVMSExt,
            Self::FpToUi => LLVMFPToUI,
            Self::FpToSi => LLVMFPToSI,
            Self::UiToFp => LLVMUIToFP,
            Self::SiToFp => LLVMSIToFP,
            Self::FpTrunc => LLVMFPTrunc,
            Self::FpExt => LLVMFPExt,
            Self::PtrToInt => LLVMPtrToInt,
            Self::IntToPtr => LLVMIntToPtr,
            Self::BitCast => LLVMBitCast,
            Self::AddrSpaceCast => LLVMAddrSpaceCast,
        }
    }
}

impl Builder {
    /// Emit the selected cast if LLVM permits it for this operand and result type.
    pub fn cast(&self, op: CastOp, value: &Value, ty: &Type, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        let value = value.operand(&block.function)?;
        let target = ty.basic_in(self.context())?;
        let name = c_name(name)?;
        if !valid_cast(op, value.get_type(), target) {
            return Err(Error::new(format!(
                "invalid LLVM {op:?} cast from {} to {}",
                value.get_type(),
                ty.ir(),
            )));
        }
        // SAFETY: the operand and target are live in the builder's context,
        // and the cast has been checked against LLVM's type rules.
        let result = unsafe {
            BasicValueEnum::new(LLVMBuildCast(
                self.llvm().as_mut_ptr(),
                op.opcode(),
                value.as_value_ref(),
                target.as_type_ref(),
                name.as_ptr(),
            ))
        };
        Ok(block.function.local(result))
    }

    /// Change the integer width of a scalar or vector. `signed` selects sign extension.
    pub fn int_cast(&self, value: &Value, ty: &Type, signed: bool, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        let value = value.operand(&block.function)?;
        let source = value.get_type();
        let target = ty.basic_in(self.context())?;
        if !arithmetic_element_type(source).is_int_type()
            || !arithmetic_element_type(target).is_int_type()
            || vector_shape(source) != vector_shape(target)
        {
            return Err(Error::new(
                "integer cast requires integer types with the same scalar or vector shape",
            ));
        }
        let name = c_name(name)?;
        // SAFETY: the operands are integer scalars or equally shaped integer vectors in this context.
        let result = unsafe {
            BasicValueEnum::new(LLVMBuildIntCast2(
                self.llvm().as_mut_ptr(),
                value.as_value_ref(),
                target.as_type_ref(),
                signed.into(),
                name.as_ptr(),
            ))
        };
        Ok(block.function.local(result))
    }
}

fn valid_cast(op: CastOp, source: BasicTypeEnum<'_>, target: BasicTypeEnum<'_>) -> bool {
    use BasicTypeEnum::*;
    use CastOp::*;

    let source_shape = vector_shape(source);
    let target_shape = vector_shape(target);
    let source = arithmetic_element_type(source);
    let target = arithmetic_element_type(target);

    // LLVM bitcasts compare total bit widths. Pointer bitcasts instead compare
    // address spaces and element counts, treating a scalar as one fixed lane.
    if op == BitCast {
        let source_shape = source_shape.unwrap_or((1, false));
        let target_shape = target_shape.unwrap_or((1, false));
        return match (source, target) {
            (PointerType(src), PointerType(dst)) => {
                src.get_address_space() == dst.get_address_space() && source_shape == target_shape
            }
            _ => match (numeric_width(source), numeric_width(target)) {
                (Some(src), Some(dst)) => {
                    source_shape.1 == target_shape.1
                        && u64::from(src) * u64::from(source_shape.0)
                            == u64::from(dst) * u64::from(target_shape.0)
                }
                _ => false,
            },
        };
    }

    if source_shape != target_shape {
        return false;
    }
    match (op, source, target) {
        (Trunc, IntType(src), IntType(dst)) => src.get_bit_width() > dst.get_bit_width(),
        (ZExt | SExt, IntType(src), IntType(dst)) => src.get_bit_width() < dst.get_bit_width(),
        (FpTrunc, FloatType(src), FloatType(dst)) => src.get_bit_width() > dst.get_bit_width(),
        (FpExt, FloatType(src), FloatType(dst)) => src.get_bit_width() < dst.get_bit_width(),
        (FpToUi | FpToSi, FloatType(_), IntType(_))
        | (UiToFp | SiToFp, IntType(_), FloatType(_))
        | (PtrToInt, PointerType(_), IntType(_))
        | (IntToPtr, IntType(_), PointerType(_)) => true,
        (AddrSpaceCast, PointerType(src), PointerType(dst)) => {
            src.get_address_space() != dst.get_address_space()
        }
        _ => false,
    }
}

fn numeric_width(ty: BasicTypeEnum<'_>) -> Option<u32> {
    match ty {
        BasicTypeEnum::IntType(ty) => Some(ty.get_bit_width()),
        BasicTypeEnum::FloatType(ty) => Some(ty.get_bit_width()),
        _ => None,
    }
}

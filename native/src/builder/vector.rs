use inkwell::llvm_sys::core::{
    LLVMBuildExtractElement, LLVMBuildInsertElement, LLVMBuildShuffleVector,
    LLVMConstIntGetZExtValue, LLVMGetAggregateElement, LLVMIsAConstantAggregateZero,
    LLVMIsAConstantDataVector, LLVMIsAConstantInt, LLVMIsAConstantVector, LLVMIsPoison,
    LLVMIsUndef,
};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{AsValueRef, BasicValueEnum};

use super::{Builder, c_name, int_value};
use crate::{Error, Result, Value};

impl Builder {
    /// Read one lane of a fixed or scalable vector using a scalar integer index.
    pub fn extract_element(&self, vector: &Value, index: &Value, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        let vector = vector.operand(&block.function)?;
        vector_element_type(vector.get_type())?;
        let index = int_value(index, &block.function)?;
        let name = c_name(name)?;
        // SAFETY: both operands are live in this function and have vector and scalar integer types.
        let result = unsafe {
            BasicValueEnum::new(LLVMBuildExtractElement(
                self.llvm().as_mut_ptr(),
                vector.as_value_ref(),
                index.as_value_ref(),
                name.as_ptr(),
            ))
        };
        Ok(block.function.local(result))
    }

    /// Replace one lane of a fixed or scalable vector using a scalar integer index.
    pub fn insert_element(
        &self,
        vector: &Value,
        value: &Value,
        index: &Value,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let vector = vector.operand(&block.function)?;
        let value = value.operand(&block.function)?;
        if vector_element_type(vector.get_type())? != value.get_type() {
            return Err(Error::new(
                "inserted value must match the LLVM vector element type",
            ));
        }
        let index = int_value(index, &block.function)?;
        let name = c_name(name)?;
        // SAFETY: operands are live in this function. The scalar element matches the vector type.
        let result = unsafe {
            BasicValueEnum::new(LLVMBuildInsertElement(
                self.llvm().as_mut_ptr(),
                vector.as_value_ref(),
                value.as_value_ref(),
                index.as_value_ref(),
                name.as_ptr(),
            ))
        };
        Ok(block.function.local(result))
    }

    /// Shuffle two vectors using an LLVM constant i32 vector mask.
    pub fn shuffle_vector(
        &self,
        left: &Value,
        right: &Value,
        mask: &Value,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let left = left.operand(&block.function)?;
        let right = right.operand(&block.function)?;
        let mask = mask.operand(&block.function)?;
        if left.get_type() != right.get_type() || !valid_shuffle_mask(left.get_type(), mask) {
            return Err(Error::new(
                "invalid LLVM shufflevector operands or constant mask",
            ));
        }
        let name = c_name(name)?;
        // SAFETY: the live vectors have the same type and the mask is a legal constant i32 vector.
        let result = unsafe {
            BasicValueEnum::new(LLVMBuildShuffleVector(
                self.llvm().as_mut_ptr(),
                left.as_value_ref(),
                right.as_value_ref(),
                mask.as_value_ref(),
                name.as_ptr(),
            ))
        };
        Ok(block.function.local(result))
    }
}

fn valid_shuffle_mask(input: BasicTypeEnum<'_>, mask: BasicValueEnum<'_>) -> bool {
    let Some((input_lanes, scalable)) = vector_shape(input) else {
        return false;
    };
    let mask_type = mask.get_type();
    let Some((mask_lanes, mask_scalable)) = vector_shape(mask_type) else {
        return false;
    };
    if scalable != mask_scalable
        || !matches!(vector_element_type(mask_type), Ok(BasicTypeEnum::IntType(ty)) if ty.get_bit_width() == 32)
    {
        return false;
    }
    let mask = mask.as_value_ref();
    // SAFETY: these kind checks accept any live LLVM value.
    unsafe {
        if !LLVMIsAConstantAggregateZero(mask).is_null()
            || LLVMIsUndef(mask) != 0
            || LLVMIsPoison(mask) != 0
        {
            return true;
        }
        // LLVM 21 accepts only zero, undef, and poison masks for scalable vectors.
        if scalable {
            return false;
        }
        // LLVM compares zero-extended indices with twice the source lane count, computed as unsigned.
        let upper_bound = u64::from(input_lanes.wrapping_mul(2));
        // LLVM can store a fixed vector splat as a ConstantInt with a vector type.
        if !LLVMIsAConstantInt(mask).is_null() {
            return LLVMConstIntGetZExtValue(mask) < upper_bound;
        }
        if LLVMIsAConstantVector(mask).is_null() && LLVMIsAConstantDataVector(mask).is_null() {
            return false;
        }
        (0..mask_lanes).all(|index| {
            let lane = LLVMGetAggregateElement(mask, index);
            !lane.is_null()
                && (LLVMIsUndef(lane) != 0
                    || LLVMIsPoison(lane) != 0
                    || (!LLVMIsAConstantInt(lane).is_null()
                        && LLVMConstIntGetZExtValue(lane) < upper_bound))
        })
    }
}

pub(super) fn vector_element_type(ty: BasicTypeEnum<'_>) -> Result<BasicTypeEnum<'_>> {
    match ty {
        BasicTypeEnum::VectorType(ty) => Ok(ty.get_element_type()),
        BasicTypeEnum::ScalableVectorType(ty) => Ok(ty.get_element_type()),
        _ => Err(Error::new("expected an LLVM vector operand")),
    }
}

/// Minimum lane count and whether it scales at runtime.
pub(super) fn vector_shape(ty: BasicTypeEnum<'_>) -> Option<(u32, bool)> {
    match ty {
        BasicTypeEnum::VectorType(ty) => Some((ty.get_size(), false)),
        BasicTypeEnum::ScalableVectorType(ty) => Some((ty.get_size(), true)),
        _ => None,
    }
}

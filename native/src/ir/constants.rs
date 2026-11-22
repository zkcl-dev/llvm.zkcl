use inkwell::llvm_sys::core::{
    LLVMConstArray2, LLVMConstGEP2, LLVMConstInBoundsGEP2, LLVMConstInsertElement,
    LLVMConstIntGetSExtValue, LLVMConstIntGetZExtValue, LLVMConstNamedStruct,
    LLVMConstShuffleVector, LLVMConstVector, LLVMGetPoison, LLVMGetUndef, LLVMIsAConstantInt,
    LLVMStructGetTypeAtIndex,
};
use inkwell::llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum};
use inkwell::values::{AsValueRef, BasicValueEnum};
use num_bigint::{BigInt, Sign};

use super::{Context, Module, Type, Value};
use crate::builder::validate_gep_indices;
use crate::{Error, Result};

impl Context {
    /// Create a constant `[N x i8]` containing these bytes and an optional trailing NUL.
    pub fn const_bytes(&self, bytes: &[u8], nul_terminated: bool) -> Value {
        let array = self.llvm().const_string(bytes, nul_terminated);
        Value::constant(array.as_value_ref(), self, None)
    }
}

impl Type {
    pub fn const_int(&self, value: &BigInt) -> Result<Value> {
        let BasicTypeEnum::IntType(ty) = self.basic()? else {
            return Err(Error::new("expected an LLVM integer type"));
        };
        let width = ty.get_bit_width() as usize;
        let (_, words) = if value.sign() == Sign::Minus || value.bits() > width as u64 {
            let mask = (BigInt::from(1) << width) - BigInt::from(1);
            (value & mask).to_u64_digits()
        } else {
            value.to_u64_digits()
        };
        let constant = if words.is_empty() {
            ty.const_zero()
        } else {
            ty.const_int_arbitrary_precision(&words)
        };
        Ok(self.constant(constant))
    }

    pub fn const_float(&self, value: f64) -> Result<Value> {
        Ok(self.constant(self.llvm_float()?.const_float(value)))
    }

    /// Parse an ASCII decimal literal directly in the destination floating-point format.
    /// An optional sign, decimal point and `e` or `E` exponent are supported.
    pub fn const_float_from_string(&self, literal: &str) -> Result<Value> {
        let ty = self.llvm_float()?;
        if u32::try_from(literal.len()).is_err() || !is_decimal_float(literal) {
            return Err(Error::new("invalid LLVM decimal floating-point literal"));
        }
        // SAFETY: the literal has valid decimal floating-point syntax and LLVM's
        // length argument can represent its length.
        Ok(self.constant(unsafe { ty.const_float_from_string(literal) }))
    }

    pub fn const_zero(&self) -> Result<Value> {
        Ok(self.constant(self.basic()?.const_zero()))
    }

    pub fn const_poison(&self) -> Result<Value> {
        let ty = self.basic()?.as_type_ref();
        // SAFETY: this is a live LLVM value type.
        let poison = unsafe { LLVMGetPoison(ty) };
        Ok(Value::constant(poison, &self.context, None))
    }

    pub fn const_undef(&self) -> Result<Value> {
        let ty = self.basic()?.as_type_ref();
        // SAFETY: this is a live LLVM value type.
        let undef = unsafe { LLVMGetUndef(ty) };
        Ok(Value::constant(undef, &self.context, None))
    }

    /// Build an array constant whose element type is `self`.
    pub fn const_array(&self, elements: &[Value]) -> Result<Value> {
        let element_type = self.basic()?.as_type_ref();
        let (mut values, module) = self.constant_operands(elements, |_| element_type)?;
        // SAFETY: all values are constants with the specified element type in this context.
        let array =
            unsafe { LLVMConstArray2(element_type, values.as_mut_ptr(), values.len() as u64) };
        Ok(Value::constant(array, &self.context, module))
    }

    /// Build a constant with exactly the fields and layout of this struct type.
    pub fn const_struct(&self, fields: &[Value]) -> Result<Value> {
        let BasicTypeEnum::StructType(ty) = self.basic()? else {
            return Err(Error::new("expected an LLVM struct type"));
        };
        if ty.is_opaque() || fields.len() != ty.count_fields() as usize {
            return Err(Error::new(
                "constant fields must match a defined LLVM struct type",
            ));
        }
        let ty = ty.as_type_ref();
        let (mut values, module) = self.constant_operands(fields, |index| {
            // SAFETY: the field count above establishes the bounds for each index.
            unsafe { LLVMStructGetTypeAtIndex(ty, index as u32) }
        })?;
        // SAFETY: the complete list of constant operands matches this struct's field types.
        let structure =
            unsafe { LLVMConstNamedStruct(ty, values.as_mut_ptr(), values.len() as u32) };
        Ok(Value::constant(structure, &self.context, module))
    }

    /// Build a fixed-length vector constant of this type.
    pub fn const_vector(&self, elements: &[Value]) -> Result<Value> {
        let BasicTypeEnum::VectorType(ty) = self.basic()? else {
            return Err(Error::new("expected a fixed-length LLVM vector type"));
        };
        let count = ty.get_size();
        if elements.len() != count as usize {
            return Err(Error::new(
                "constant element count must match the LLVM vector type",
            ));
        }
        let element_type = ty.get_element_type().as_type_ref();
        let (mut values, module) = self.constant_operands(elements, |_| element_type)?;
        // SAFETY: the vector type establishes a nonzero count and a valid element type.
        let vector = unsafe { LLVMConstVector(values.as_mut_ptr(), count) };
        Ok(Value::constant(vector, &self.context, module))
    }

    /// Repeat a constant in every lane of this fixed or scalable vector type.
    pub fn const_splat(&self, value: &Value) -> Result<Value> {
        let ty = self.basic()?;
        let i32 = self.context.llvm().i32_type();
        let (element, mask) = match ty {
            BasicTypeEnum::VectorType(vector) => (
                vector.get_element_type(),
                i32.vec_type(vector.get_size()).const_zero().as_value_ref(),
            ),
            BasicTypeEnum::ScalableVectorType(vector) => (
                vector.get_element_type(),
                i32.scalable_vec_type(vector.get_size())
                    .const_zero()
                    .as_value_ref(),
            ),
            _ => return Err(Error::new("expected an LLVM vector type")),
        };
        let (value, module) = value.constant_in(&self.context)?;
        if value.get_type() != element {
            return Err(Error::new(
                "splat operand must match the vector element type",
            ));
        }
        // SAFETY: the constant matches the vector's element type, lane zero exists,
        // and the zero shuffle mask matches the fixed or scalable vector shape.
        let vector = unsafe {
            let poison = LLVMGetPoison(ty.as_type_ref());
            let first = LLVMConstInsertElement(
                poison,
                value.as_value_ref(),
                i32.const_zero().as_value_ref(),
            );
            LLVMConstShuffleVector(first, poison, mask)
        };
        Ok(Value::constant(vector, &self.context, module))
    }

    /// Compute a constant pointer using this source element type and scalar indices.
    pub fn const_gep(&self, address: &Value, indices: &[Value], in_bounds: bool) -> Result<Value> {
        let ty = self.basic()?;
        if !ty.is_sized() {
            return Err(Error::new("getelementptr requires a sized LLVM type"));
        }
        let (address, mut owner) = address.constant_in(&self.context)?;
        let BasicValueEnum::PointerValue(address) = address else {
            return Err(Error::new("getelementptr requires a scalar pointer"));
        };
        let count = u32::try_from(indices.len())
            .map_err(|_| Error::new("too many LLVM getelementptr indices"))?;
        let indices = indices
            .iter()
            .map(|index| {
                let (index, module) = index.constant_in(&self.context)?;
                let BasicValueEnum::IntValue(index) = index else {
                    return Err(Error::new("getelementptr requires scalar integer indices"));
                };
                merge_constant_owner(&mut owner, module)?;
                Ok(index)
            })
            .collect::<Result<Vec<_>>>()?;
        validate_gep_indices(ty, &indices)?;
        let mut indices: Vec<_> = indices.iter().map(AsValueRef::as_value_ref).collect();
        let build = if in_bounds {
            LLVMConstInBoundsGEP2
        } else {
            LLVMConstGEP2
        };
        // SAFETY: the pointer and all indices are live constants in compatible modules.
        // The source type is sized and its indexed type path was checked above.
        let pointer = unsafe {
            build(
                ty.as_type_ref(),
                address.as_value_ref(),
                indices.as_mut_ptr(),
                count,
            )
        };
        Ok(Value::constant(pointer, &self.context, owner))
    }

    fn constant_operands<'a>(
        &self,
        values: &'a [Value],
        expected_type: impl Fn(usize) -> LLVMTypeRef,
    ) -> Result<(Vec<LLVMValueRef>, Option<&'a Module>)> {
        let mut operands = Vec::with_capacity(values.len());
        let mut owner: Option<&Module> = None;
        for (index, value) in values.iter().enumerate() {
            let (value, module) = value.constant_in(&self.context)?;
            if value.get_type().as_type_ref() != expected_type(index) {
                return Err(Error::new(format!(
                    "constant operand {index} has the wrong LLVM type"
                )));
            }
            merge_constant_owner(&mut owner, module)?;
            operands.push(value.as_value_ref());
        }
        Ok((operands, owner))
    }

    fn constant(&self, value: impl AsValueRef) -> Value {
        Value::constant(value.as_value_ref(), &self.context, None)
    }
}

impl Value {
    /// Read a scalar integer constant's bits as a signed or unsigned integer.
    pub fn constant_integer(&self, signed: bool) -> Result<BigInt> {
        // SAFETY: the value's owner retains it for the LLVM kind query.
        if unsafe { LLVMIsAConstantInt(self.raw) }.is_null() {
            return Err(Error::new("expected an LLVM integer constant"));
        }
        let width = self.ty().integer_width()?;
        if width <= 64 {
            // SAFETY: the constant and width checks make both conversions valid.
            return Ok(unsafe {
                if signed {
                    LLVMConstIntGetSExtValue(self.raw).into()
                } else {
                    LLVMConstIntGetZExtValue(self.raw).into()
                }
            });
        }

        // LLVM prints a wide ConstantInt as its type followed by a signed decimal.
        let ir = self.ir();
        let mut value = ir
            .split_once(' ')
            .and_then(|(_, literal)| literal.parse::<BigInt>().ok())
            .ok_or_else(|| Error::new("LLVM printed an invalid integer constant"))?;
        if !signed && value.sign() == Sign::Minus {
            value += BigInt::from(1) << width;
        }
        Ok(value)
    }
}

fn is_decimal_float(literal: &str) -> bool {
    fn unsigned_part(part: &str) -> &str {
        part.strip_prefix(['+', '-']).unwrap_or(part)
    }

    let literal = unsigned_part(literal);
    let significand = if let Some((significand, exponent)) = literal.split_once(['e', 'E']) {
        let exponent = unsigned_part(exponent);
        if exponent.is_empty() || !exponent.bytes().all(|byte| byte.is_ascii_digit()) {
            return false;
        }
        significand
    } else {
        literal
    };
    let (integer, fraction) = significand.split_once('.').unwrap_or((significand, ""));
    (!integer.is_empty() || !fraction.is_empty())
        && integer
            .bytes()
            .chain(fraction.bytes())
            .all(|byte| byte.is_ascii_digit())
}

fn merge_constant_owner<'a>(
    owner: &mut Option<&'a Module>,
    module: Option<&'a Module>,
) -> Result<()> {
    if let Some(module) = module {
        if let Some(owner) = *owner {
            owner.check(module)?;
        }
        *owner = Some(module);
    }
    Ok(())
}

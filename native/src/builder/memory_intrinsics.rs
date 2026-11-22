use inkwell::attributes::Attribute;
use inkwell::llvm_sys::core::{LLVMGetPointerAddressSpace, LLVMSetAlignment};
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::types::{AsTypeRef, IntType};
use inkwell::values::{AsValueRef, BasicMetadataValueEnum, InstructionValue, IntValue};

use super::memory::check_alignment;
use super::{Builder, int_value, pointer_value, sized_type};
use crate::{
    AttributeLocation, Block, DataLayout, Error, Function, Instruction, Module, Result, Type,
    TypeKind, Value, c_text,
};

impl Builder {
    /// Allocate `count` elements of `ty` in stack storage.
    pub fn array_alloca(&self, ty: &Type, count: &Value, name: &str) -> Result<Value> {
        self.array_alloca_with_alignment(ty, count, None, name)
    }

    /// Allocate a typed stack array with an optional explicit byte alignment.
    pub fn array_alloca_with_alignment(
        &self,
        ty: &Type,
        count: &Value,
        alignment: Option<u32>,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let ty = sized_type(ty.basic_in(self.context())?)?;
        let count = int_value(count, &block.function)?;
        check_alignment(alignment.map(u64::from))?;
        let value = self.llvm().build_array_alloca(ty, count, c_text(name)?)?;
        if let Some(alignment) = alignment {
            // SAFETY: the builder inserted an alloca and the alignment is representable.
            unsafe { LLVMSetAlignment(value.as_value_ref(), alignment) };
        }
        Ok(block.function.local(value.into()))
    }

    /// Call an explicit allocator with `count * abi-size(ty)` bytes.
    /// The allocator takes one integer byte count and returns a pointer. The
    /// unsigned product must fit the byte-count type, as specified by `mul nuw`.
    pub fn malloc(
        &self,
        allocator: &Function,
        ty: &Type,
        count: &Value,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let module = &block.function.module;
        module.check(&allocator.module)?;
        self.context().check(ty.context())?;
        let signature = allocator.signature();
        let parameter = single_parameter(&signature).filter(|parameter| {
            parameter.kind() == TypeKind::LLVMIntegerTypeKind
                && signature
                    .subtype(0)
                    .is_some_and(|result| result.kind() == TypeKind::LLVMPointerTypeKind)
        });
        let parameter = parameter
            .ok_or_else(|| Error::new("LLVM allocator must have signature ptr (integer)"))?;
        let count = int_value(count, &block.function)?;
        let size_type = count.get_type();
        let width = parameter.integer_width()?;
        if size_type.get_bit_width() != width {
            return Err(Error::new(
                "LLVM allocation count must match the allocator's byte-count type",
            ));
        }
        let stride = known_layout(module)?.alloc_size(ty)?;
        if width < u64::BITS && stride >= 1u64 << width {
            return Err(Error::new(
                "LLVM allocation stride exceeds the allocator's byte-count type",
            ));
        }
        let name = c_text(name)?;
        let bytes = self
            .llvm()
            .build_int_nuw_mul(count, size_type.const_int(stride, false), "")?;
        let allocator = allocator.llvm();
        let call = self
            .llvm()
            .build_direct_call(allocator, &[bytes.into()], name)?;
        Ok(super::call::finish_call(call, Some(allocator), &block)
            .result()
            .expect("allocator has a pointer result"))
    }

    /// Call an explicit deallocator taking one opaque pointer and returning void.
    pub fn free(&self, deallocator: &Function, pointer: &Value) -> Result<Instruction> {
        let signature = deallocator.signature();
        let valid = single_parameter(&signature).is_some_and(|parameter| {
            parameter.kind() == TypeKind::LLVMPointerTypeKind
                && signature
                    .subtype(0)
                    .is_some_and(|result| result.kind() == TypeKind::LLVMVoidTypeKind)
        });
        if !valid {
            return Err(Error::new(
                "LLVM deallocator must have signature void (ptr)",
            ));
        }
        self.call_instruction(deallocator, std::slice::from_ref(pointer), "")
    }

    /// Return the signed element distance between two pointers into one object.
    /// The result uses the module's pointer index width for their address space.
    pub fn ptr_diff(&self, ty: &Type, lhs: &Value, rhs: &Value, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        self.context().check(ty.context())?;
        let lhs = pointer_value(lhs, &block.function)?;
        let rhs = pointer_value(rhs, &block.function)?;
        let pointer_type = lhs.get_type();
        if pointer_type != rhs.get_type() {
            return Err(Error::new(
                "LLVM pointer difference requires matching pointer address spaces",
            ));
        }
        // SAFETY: the type is a live scalar pointer type.
        let space = unsafe { LLVMGetPointerAddressSpace(pointer_type.as_type_ref()) };
        let layout = known_layout(&block.function.module)?;
        if layout.is_non_integral_pointer(space) {
            return Err(Error::new(
                "LLVM pointer difference requires an integral pointer address space",
            ));
        }
        let width = layout.pointer_index_size_bits(space);
        let index_type = self.context().int_type(width)?;
        let stride = layout.alloc_size(ty)?;
        if stride == 0 || (width <= u64::BITS && stride >= 1u64 << (width - 1)) {
            return Err(Error::new(
                "LLVM pointer difference requires a positive stride that fits the signed index type",
            ));
        }
        let index_type = IntType::try_from(index_type.basic()?).expect("integer index type");
        let name = c_text(name)?;
        let builder = self.llvm();
        let lhs = builder.build_ptr_to_int(lhs, index_type, "")?;
        let rhs = builder.build_ptr_to_int(rhs, index_type, "")?;
        let bytes = builder.build_int_sub(lhs, rhs, "")?;
        let distance =
            builder.build_int_exact_signed_div(bytes, index_type.const_int(stride, false), name)?;
        Ok(block.function.local(distance.into()))
    }

    /// Copy a byte range. Source and destination may be equal or disjoint.
    pub fn memcpy(
        &self,
        dest: &Value,
        source: &Value,
        len: &Value,
        dest_alignment: Option<u64>,
        source_alignment: Option<u64>,
        volatile: bool,
    ) -> Result<Instruction> {
        self.memory_transfer(
            "llvm.memcpy",
            dest,
            source,
            len,
            [dest_alignment, source_alignment],
            volatile,
        )
    }

    /// Move a byte range, allowing the source and destination to overlap.
    pub fn memmove(
        &self,
        dest: &Value,
        source: &Value,
        len: &Value,
        dest_alignment: Option<u64>,
        source_alignment: Option<u64>,
        volatile: bool,
    ) -> Result<Instruction> {
        self.memory_transfer(
            "llvm.memmove",
            dest,
            source,
            len,
            [dest_alignment, source_alignment],
            volatile,
        )
    }

    /// Fill `len` bytes of a memory region with the supplied `i8` value.
    pub fn memset(
        &self,
        dest: &Value,
        byte: &Value,
        len: &Value,
        dest_alignment: Option<u64>,
        volatile: bool,
    ) -> Result<Instruction> {
        let block = self.insertion()?;
        let dest = pointer_value(dest, &block.function)?;
        let byte = int_value(byte, &block.function)?;
        if byte.get_type().get_bit_width() != 8 {
            return Err(Error::new("LLVM memset fill value must have type i8"));
        }
        let len = int_value(len, &block.function)?;
        let mut overloads = [dest.get_type().as_type_ref(), len.get_type().as_type_ref()];
        let arguments = [
            dest.into(),
            byte.into(),
            len.into(),
            self.volatile_flag(volatile).into(),
        ];
        // SAFETY: llvm.memset overloads on its destination pointer and integer length types.
        unsafe {
            self.emit_memory_intrinsic(
                &block,
                "llvm.memset",
                &mut overloads,
                &arguments,
                &[dest_alignment],
            )
        }
    }

    fn memory_transfer(
        &self,
        name: &str,
        dest: &Value,
        source: &Value,
        len: &Value,
        alignments: [Option<u64>; 2],
        volatile: bool,
    ) -> Result<Instruction> {
        let block = self.insertion()?;
        let dest = pointer_value(dest, &block.function)?;
        let source = pointer_value(source, &block.function)?;
        let len = int_value(len, &block.function)?;
        let mut overloads = [
            dest.get_type().as_type_ref(),
            source.get_type().as_type_ref(),
            len.get_type().as_type_ref(),
        ];
        let arguments = [
            dest.into(),
            source.into(),
            len.into(),
            self.volatile_flag(volatile).into(),
        ];
        // SAFETY: llvm.memcpy and llvm.memmove overload on both pointer types and the length type.
        unsafe { self.emit_memory_intrinsic(&block, name, &mut overloads, &arguments, &alignments) }
    }

    fn volatile_flag(&self, volatile: bool) -> IntValue<'_> {
        self.context()
            .llvm()
            .bool_type()
            .const_int(u64::from(volatile), false)
    }

    /// The caller must validate the arguments and overload types for the intrinsic.
    unsafe fn emit_memory_intrinsic(
        &self,
        block: &Block,
        name: &str,
        overloads: &mut [LLVMTypeRef],
        arguments: &[BasicMetadataValueEnum<'_>; 4],
        alignments: &[Option<u64>],
    ) -> Result<Instruction> {
        for &alignment in alignments {
            check_alignment(alignment)?;
        }
        // SAFETY: both the intrinsic and its overload types are known to the caller.
        let intrinsic = unsafe {
            block
                .function
                .module
                .declare_intrinsic_with_overloads(name, overloads)?
        };
        let call = self
            .llvm()
            .build_direct_call(intrinsic.llvm(), arguments, "")?;
        for (index, alignment) in alignments.iter().enumerate() {
            if let Some(alignment) = alignment {
                let kind = Attribute::get_named_enum_kind_id("align");
                let attribute = self
                    .context()
                    .llvm()
                    .create_enum_attribute(kind, *alignment);
                call.add_attribute(AttributeLocation::Param(index as u32), attribute);
            }
        }
        // SAFETY: this call is a live instruction inserted into this block.
        Ok(block.instruction(unsafe { InstructionValue::new(call.as_value_ref()) }))
    }
}

fn known_layout(module: &Module) -> Result<DataLayout> {
    let layout = module.target_data();
    if layout.text().is_empty() {
        return Err(Error::new(
            "LLVM instruction requires an explicit module data layout",
        ));
    }
    Ok(layout)
}

fn single_parameter(signature: &Type) -> Option<Type> {
    if signature.is_variadic_function().ok()? || signature.subtype_count() != 2 {
        return None;
    }
    signature.subtype(1)
}

use std::{collections::HashSet, ffi::CStr, num::NonZeroU32, ptr};

use super::Context;
use crate::{Error, Result, c_text};
use inkwell::AddressSpace;
use inkwell::context::AsContextRef;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum, FloatType, FunctionType, StructType};
pub use llvm_sys::LLVMTypeKind as TypeKind;
use llvm_sys::core::{
    LLVMArrayType2, LLVMDisposeMessage, LLVMGetArrayLength2, LLVMGetElementType,
    LLVMGetIntTypeWidth, LLVMGetNumContainedTypes, LLVMGetPointerAddressSpace, LLVMGetReturnType,
    LLVMGetSubtypes, LLVMGetTargetExtTypeTypeParam, LLVMGetTypeKind, LLVMGetVectorSize,
    LLVMLabelTypeInContext, LLVMMetadataTypeInContext, LLVMPrintTypeToString,
    LLVMScalableVectorType, LLVMStructGetTypeAtIndex, LLVMStructSetBody, LLVMTokenTypeInContext,
    LLVMTypeIsSized, LLVMVectorType,
};
use llvm_sys::prelude::LLVMTypeRef;

impl Context {
    pub fn int_type(&self, bits: u32) -> Result<Type> {
        let bits =
            NonZeroU32::new(bits).ok_or_else(|| Error::new("integer width must be positive"))?;
        let ty = self
            .llvm()
            .custom_width_int_type(bits)
            .map_err(Error::new)?;
        Ok(self.ty(ty))
    }

    pub fn float_type(&self) -> Type {
        self.ty(self.llvm().f32_type())
    }

    pub fn double_type(&self) -> Type {
        self.ty(self.llvm().f64_type())
    }

    pub fn floating_type(&self, kind: TypeKind) -> Result<Type> {
        let context = self.llvm();
        let ty = match kind {
            TypeKind::LLVMHalfTypeKind => context.f16_type(),
            TypeKind::LLVMBFloatTypeKind => context.bf16_type(),
            TypeKind::LLVMFloatTypeKind => context.f32_type(),
            TypeKind::LLVMDoubleTypeKind => context.f64_type(),
            TypeKind::LLVMX86_FP80TypeKind => context.x86_f80_type(),
            TypeKind::LLVMFP128TypeKind => context.f128_type(),
            TypeKind::LLVMPPC_FP128TypeKind => context.ppc_f128_type(),
            _ => return Err(Error::new("expected an LLVM floating-point type kind")),
        };
        Ok(self.ty(ty))
    }

    pub fn void_type(&self) -> Type {
        self.ty(self.llvm().void_type())
    }

    pub fn label_type(&self) -> Type {
        // SAFETY: the context is live and retained by the returned type.
        self.type_ref(unsafe { LLVMLabelTypeInContext(self.llvm().as_ctx_ref()) })
    }

    pub fn metadata_type(&self) -> Type {
        // SAFETY: the context is live and retained by the returned type.
        self.type_ref(unsafe { LLVMMetadataTypeInContext(self.llvm().as_ctx_ref()) })
    }

    pub fn token_type(&self) -> Type {
        // SAFETY: the context is live and retained by the returned type.
        self.type_ref(unsafe { LLVMTokenTypeInContext(self.llvm().as_ctx_ref()) })
    }

    pub fn pointer_type(&self) -> Type {
        self.ty(self.llvm().ptr_type(AddressSpace::default()))
    }

    pub fn pointer_type_in(&self, address_space: u32) -> Result<Type> {
        let address_space = AddressSpace::try_from(address_space)
            .map_err(|_| Error::new("LLVM address space must fit in 24 bits"))?;
        Ok(self.ty(self.llvm().ptr_type(address_space)))
    }

    pub fn struct_type(&self, fields: &[Type], packed: bool) -> Result<Type> {
        let fields = fields
            .iter()
            .map(|ty| ty.basic_in(self))
            .collect::<Result<Vec<_>>>()?;
        Ok(self.ty(self.llvm().struct_type(&fields, packed)))
    }

    /// Create an identified struct. LLVM may unique the requested name.
    pub fn opaque_struct_type(&self, name: &str) -> Result<Type> {
        Ok(self.ty(self.llvm().opaque_struct_type(c_text(name)?)))
    }

    /// Look up an identified struct by its actual LLVM name in this context.
    pub fn find_struct_type(&self, name: &str) -> Result<Option<Type>> {
        Ok(self
            .llvm()
            .get_struct_type(c_text(name)?)
            .map(|ty| self.ty(ty)))
    }

    pub(super) fn ty(&self, ty: impl AsTypeRef) -> Type {
        self.type_ref(ty.as_type_ref())
    }

    pub(super) fn type_ref(&self, raw: LLVMTypeRef) -> Type {
        Type {
            raw,
            context: self.clone(),
        }
    }
}

/// An LLVM type retained by its context.
#[derive(Clone)]
pub struct Type {
    raw: LLVMTypeRef,
    pub(super) context: Context,
}

impl Type {
    pub fn context(&self) -> &Context {
        &self.context
    }

    pub(crate) fn same_as(&self, other: &Self) -> bool {
        self.raw == other.raw
    }

    pub fn kind(&self) -> TypeKind {
        // SAFETY: the retained context owns this type.
        unsafe { LLVMGetTypeKind(self.raw) }
    }

    /// Whether LLVM can calculate this type's size for a target data layout.
    pub fn is_sized(&self) -> bool {
        // SAFETY: the retained context owns this type.
        unsafe { LLVMTypeIsSized(self.raw) != 0 }
    }

    pub fn is_floating_point(&self) -> bool {
        is_floating_point_kind(self.kind())
    }

    /// The bit width of a scalar floating-point type, including exponent and sign.
    pub fn floating_point_width(&self) -> Result<u32> {
        Ok(self.llvm_float()?.get_bit_width())
    }

    /// A vector's element type, or this type for any other kind.
    pub fn scalar_type(&self) -> Type {
        if matches!(
            self.kind(),
            TypeKind::LLVMVectorTypeKind | TypeKind::LLVMScalableVectorTypeKind
        ) {
            // SAFETY: this is a live fixed or scalable vector type.
            self.context
                .type_ref(unsafe { LLVMGetElementType(self.raw) })
        } else {
            self.clone()
        }
    }

    pub fn ir(&self) -> String {
        // SAFETY: LLVM allocates a string for every live type, including types
        // that Inkwell cannot wrap. Dispose it after copying.
        unsafe {
            let message = LLVMPrintTypeToString(self.raw);
            let text = CStr::from_ptr(message).to_string_lossy().into_owned();
            LLVMDisposeMessage(message);
            text
        }
    }

    /// The number of contained types, as indexed by [`Self::subtype`].
    pub fn subtype_count(&self) -> u32 {
        // SAFETY: the retained context owns this type.
        unsafe { LLVMGetNumContainedTypes(self.raw) }
    }

    /// Inspect a contained type in LLVM order. Function types start with the
    /// result type, structs list their fields, and arrays and vectors have one
    /// element type. Opaque pointers contain no types.
    pub fn subtype(&self, index: u32) -> Option<Type> {
        use TypeKind::*;

        // SAFETY: this context owns the type, and each accessor below only
        // receives its expected kind and an in-bounds index or output buffer.
        let raw = unsafe {
            let count = self.subtype_count();
            if index >= count {
                return None;
            }
            match self.kind() {
                LLVMStructTypeKind => LLVMStructGetTypeAtIndex(self.raw, index),
                LLVMArrayTypeKind | LLVMVectorTypeKind | LLVMScalableVectorTypeKind => {
                    LLVMGetElementType(self.raw)
                }
                LLVMFunctionTypeKind if index == 0 => LLVMGetReturnType(self.raw),
                LLVMTargetExtTypeKind => LLVMGetTargetExtTypeTypeParam(self.raw, index),
                _ => {
                    let mut subtypes = vec![ptr::null_mut(); count as usize];
                    LLVMGetSubtypes(self.raw, subtypes.as_mut_ptr());
                    subtypes[index as usize]
                }
            }
        };
        Some(self.context.type_ref(raw))
    }

    pub fn integer_width(&self) -> Result<u32> {
        if self.kind() != TypeKind::LLVMIntegerTypeKind {
            return Err(Error::new("expected an LLVM integer type"));
        }
        Ok(unsafe { LLVMGetIntTypeWidth(self.raw) })
    }

    pub fn pointer_address_space(&self) -> Result<u32> {
        if self.kind() != TypeKind::LLVMPointerTypeKind {
            return Err(Error::new("expected an LLVM pointer type"));
        }
        Ok(unsafe { LLVMGetPointerAddressSpace(self.raw) })
    }

    pub fn array_length(&self) -> Result<u64> {
        if self.kind() != TypeKind::LLVMArrayTypeKind {
            return Err(Error::new("expected an LLVM array type"));
        }
        Ok(unsafe { LLVMGetArrayLength2(self.raw) })
    }

    /// Element count, or the minimum element count for a scalable vector.
    pub fn vector_length(&self) -> Result<u32> {
        if !matches!(
            self.kind(),
            TypeKind::LLVMVectorTypeKind | TypeKind::LLVMScalableVectorTypeKind
        ) {
            return Err(Error::new("expected an LLVM vector type"));
        }
        // SAFETY: LLVM accepts a live fixed or scalable vector type.
        Ok(unsafe { LLVMGetVectorSize(self.raw) })
    }

    pub fn struct_name(&self) -> Result<Option<String>> {
        self.llvm_struct()?
            .get_name()
            .map(|name| {
                name.to_str()
                    .map(str::to_owned)
                    .map_err(|_| Error::new("LLVM struct name is not valid UTF-8"))
            })
            .transpose()
    }

    pub fn is_opaque_struct(&self) -> Result<bool> {
        Ok(self.llvm_struct()?.is_opaque())
    }

    pub fn is_packed_struct(&self) -> Result<bool> {
        Ok(self.llvm_struct()?.is_packed())
    }

    pub fn is_variadic_function(&self) -> Result<bool> {
        Ok(self.llvm_function()?.is_var_arg())
    }

    /// Define an opaque struct's body. A field may refer to another opaque
    /// struct. A by-value cycle is rejected before the body is installed.
    pub fn set_struct_body(&self, fields: &[Type], packed: bool) -> Result<()> {
        let ty = self.llvm_struct()?;
        if !ty.is_opaque() {
            return Err(Error::new("LLVM struct already has a body"));
        }
        let mut fields = fields
            .iter()
            .map(|ty| ty.basic_in(&self.context).map(|ty| ty.as_type_ref()))
            .collect::<Result<Vec<_>>>()?;
        let count = u32::try_from(fields.len())
            .map_err(|_| Error::new("too many fields for an LLVM struct"))?;
        let mut pending = fields.clone();
        let mut visited = HashSet::new();
        while let Some(field) = pending.pop() {
            if field == self.raw {
                let name = ty.get_name().map(CStr::to_string_lossy).unwrap_or_default();
                return Err(Error::new(format!(
                    "identified structure type '{name}' is recursive"
                )));
            }
            if !visited.insert(field) {
                continue;
            }
            // SAFETY: the type is live in this context and the remaining
            // capacity is initialized for exactly its number of subtypes.
            unsafe {
                let offset = pending.len();
                pending.resize(
                    offset + LLVMGetNumContainedTypes(field) as usize,
                    ptr::null_mut(),
                );
                LLVMGetSubtypes(field, pending[offset..].as_mut_ptr());
            }
        }
        // SAFETY: this is an opaque identified struct with valid field types
        // from its context and the by-value type graph is acyclic.
        unsafe { LLVMStructSetBody(self.raw, fields.as_mut_ptr(), count, packed.into()) };
        Ok(())
    }

    pub fn array(&self, count: u32) -> Result<Self> {
        self.array_with_length(count.into())
    }

    pub fn array_with_length(&self, count: u64) -> Result<Self> {
        let element = self.basic()?.as_type_ref();
        // SAFETY: the checked element is a live LLVM basic type.
        Ok(self
            .context
            .type_ref(unsafe { LLVMArrayType2(element, count) }))
    }

    pub fn vector(&self, count: u32, scalable: bool) -> Result<Self> {
        let element = match self.basic()? {
            BasicTypeEnum::IntType(element) => element.as_type_ref(),
            BasicTypeEnum::FloatType(element) => element.as_type_ref(),
            BasicTypeEnum::PointerType(element) => element.as_type_ref(),
            _ => return Err(Error::new("expected an LLVM scalar vector element type")),
        };
        if count == 0 {
            return Err(Error::new("LLVM vectors must have at least one element"));
        }
        // SAFETY: the scalar element type is live and LLVM requires a nonzero count.
        Ok(self.context.type_ref(unsafe {
            if scalable {
                LLVMScalableVectorType(element, count)
            } else {
                LLVMVectorType(element, count)
            }
        }))
    }

    pub fn function(&self, parameters: &[Self], variadic: bool) -> Result<Self> {
        let parameters = parameters
            .iter()
            .map(|ty| ty.basic_in(&self.context).map(Into::into))
            .collect::<Result<Vec<_>>>()?;
        let ty = if self.kind() == TypeKind::LLVMVoidTypeKind {
            self.context
                .llvm()
                .void_type()
                .fn_type(&parameters, variadic)
        } else {
            self.basic()?.fn_type(&parameters, variadic)
        };
        Ok(self.context.ty(ty))
    }

    pub(crate) fn basic_in(&self, context: &Context) -> Result<BasicTypeEnum<'_>> {
        self.context.check(context)?;
        self.basic()
    }

    pub(crate) fn function_in(&self, context: &Context) -> Result<FunctionType<'_>> {
        self.context.check(context)?;
        self.llvm_function()
    }

    pub(crate) fn basic(&self) -> Result<BasicTypeEnum<'_>> {
        supported_value_type(self.raw)?;
        // SAFETY: the live type has a representation in BasicTypeEnum.
        Ok(unsafe { BasicTypeEnum::new(self.raw) })
    }

    pub(super) fn llvm_float(&self) -> Result<FloatType<'_>> {
        if !self.is_floating_point() {
            return Err(Error::new("expected an LLVM floating-point type"));
        }
        // SAFETY: the retained context owns this scalar floating-point type.
        Ok(unsafe { FloatType::new(self.raw) })
    }

    fn llvm_function(&self) -> Result<FunctionType<'_>> {
        if self.kind() != TypeKind::LLVMFunctionTypeKind {
            return Err(Error::new("expected an LLVM function type"));
        }
        Ok(unsafe { FunctionType::new(self.raw) })
    }

    fn llvm_struct(&self) -> Result<StructType<'_>> {
        if self.kind() != TypeKind::LLVMStructTypeKind {
            return Err(Error::new("expected an LLVM struct type"));
        }
        Ok(unsafe { StructType::new(self.raw) })
    }
}

fn is_floating_point_kind(kind: TypeKind) -> bool {
    use TypeKind::*;

    matches!(
        kind,
        LLVMHalfTypeKind
            | LLVMBFloatTypeKind
            | LLVMFloatTypeKind
            | LLVMDoubleTypeKind
            | LLVMX86_FP80TypeKind
            | LLVMFP128TypeKind
            | LLVMPPC_FP128TypeKind
    )
}

pub(crate) fn supported_value_type(raw: LLVMTypeRef) -> Result<()> {
    use TypeKind::*;

    // SAFETY: callers obtain this type from a live LLVM type or value.
    match unsafe { LLVMGetTypeKind(raw) } {
        LLVMIntegerTypeKind
        | LLVMPointerTypeKind
        | LLVMArrayTypeKind
        | LLVMStructTypeKind
        | LLVMVectorTypeKind
        | LLVMScalableVectorTypeKind => Ok(()),
        kind if is_floating_point_kind(kind) => Ok(()),
        _ => Err(Error::new("unsupported LLVM value type")),
    }
}

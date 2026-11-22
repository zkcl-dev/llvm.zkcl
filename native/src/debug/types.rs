use std::ptr;

use llvm_sys::debuginfo::{
    LLVMDIBuilderCreateArrayType, LLVMDIBuilderCreateBasicType, LLVMDIBuilderCreateMemberType,
    LLVMDIBuilderCreatePointerType, LLVMDIBuilderCreateStructType, LLVMDIBuilderCreateUnionType,
    LLVMDIBuilderGetOrCreateSubrange, LLVMDIFlagZero,
};
use llvm_sys::prelude::LLVMMetadataRef;

use super::{DebugFile, DebugInfo};
use crate::{Error, Module, Result, c_text};

/// Scalar encodings from DWARF's DW_ATE enumeration.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum DebugEncoding {
    Address = 0x01,
    Boolean = 0x02,
    Float = 0x04,
    Signed = 0x05,
    Unsigned = 0x07,
}

/// A source-language type used to describe a signature or variable to the debugger.
#[derive(Clone)]
pub struct DebugType {
    raw: LLVMMetadataRef,
    module: Module,
}

impl DebugType {
    pub(super) fn in_module(&self, module: &Module) -> Result<LLVMMetadataRef> {
        module.check(&self.module)?;
        Ok(self.raw)
    }
}

/// A field's source type and layout within a structure or union.
#[derive(Clone)]
pub struct DebugMember {
    name: String,
    ty: DebugType,
    bits: u64,
    align_bits: u32,
    offset_bits: u64,
}

impl DebugMember {
    /// Supply size, alignment and offset in bits. Zero alignment leaves it unspecified.
    pub fn new(
        name: &str,
        ty: &DebugType,
        bits: u64,
        align_bits: u32,
        offset_bits: u64,
    ) -> Result<Self> {
        Ok(Self {
            name: c_text(name)?.to_owned(),
            ty: ty.clone(),
            bits,
            align_bits,
            offset_bits,
        })
    }
}

enum CompositeKind {
    Struct,
    Union,
}

impl DebugInfo {
    pub fn basic_type(&self, name: &str, bits: u64, encoding: DebugEncoding) -> Result<DebugType> {
        let builder = self.builder()?;
        self.require_full()?;
        let name = c_text(name)?;
        if bits == 0 {
            return Err(Error::new(
                "an LLVM debug basic type must have a positive bit size",
            ));
        }
        // SAFETY: LLVM borrows the live builder and copies the sized string.
        let raw = unsafe {
            LLVMDIBuilderCreateBasicType(
                builder,
                name.as_ptr().cast(),
                name.len(),
                bits,
                encoding as u32,
                LLVMDIFlagZero,
            )
        };
        Ok(self.debug_type(raw))
    }

    /// Describe a pointer using its target layout and DWARF address space.
    /// A missing pointee denotes a void pointer.
    pub fn pointer_type(
        &self,
        pointee: Option<&DebugType>,
        bits: u64,
        align_bits: u32,
        dwarf_address_space: u32,
        name: &str,
    ) -> Result<DebugType> {
        let builder = self.builder()?;
        self.require_full()?;
        let name = c_text(name)?;
        if bits == 0 {
            return Err(Error::new(
                "an LLVM debug pointer type must have a positive bit size",
            ));
        }
        let pointee = pointee
            .map(|ty| ty.in_module(&self.module))
            .transpose()?
            .unwrap_or(ptr::null_mut());
        // SAFETY: the pointee is either absent or a type in the live module.
        let raw = unsafe {
            LLVMDIBuilderCreatePointerType(
                builder,
                pointee,
                bits,
                align_bits,
                dwarf_address_space,
                name.as_ptr().cast(),
                name.len(),
            )
        };
        Ok(self.debug_type(raw))
    }

    /// Describe a zero-based fixed-length array. Nested arrays can describe further dimensions.
    pub fn array_type(
        &self,
        element: &DebugType,
        bits: u64,
        align_bits: u32,
        count: u64,
    ) -> Result<DebugType> {
        let builder = self.builder()?;
        self.require_full()?;
        let element = element.in_module(&self.module)?;
        let count = i64::try_from(count)
            .map_err(|_| Error::new("LLVM debug array length exceeds a signed 64-bit count"))?;
        // SAFETY: the element belongs to the live module and the subrange is local to the builder.
        let raw = unsafe {
            let mut subrange = LLVMDIBuilderGetOrCreateSubrange(builder, 0, count);
            LLVMDIBuilderCreateArrayType(builder, bits, align_bits, element, &mut subrange, 1)
        };
        Ok(self.debug_type(raw))
    }

    /// Describe a structure with explicit size, alignment, and field offsets in bits.
    pub fn struct_type(
        &self,
        name: &str,
        bits: u64,
        align_bits: u32,
        line: u32,
        file: Option<&DebugFile>,
        members: &[DebugMember],
    ) -> Result<DebugType> {
        self.composite_type(
            CompositeKind::Struct,
            name,
            (bits, align_bits),
            line,
            file,
            members,
        )
    }

    /// Describe a union with explicit size and alignment in bits. Members start at offset zero.
    pub fn union_type(
        &self,
        name: &str,
        bits: u64,
        align_bits: u32,
        line: u32,
        file: Option<&DebugFile>,
        members: &[DebugMember],
    ) -> Result<DebugType> {
        self.composite_type(
            CompositeKind::Union,
            name,
            (bits, align_bits),
            line,
            file,
            members,
        )
    }

    fn composite_type(
        &self,
        kind: CompositeKind,
        name: &str,
        (bits, align_bits): (u64, u32),
        line: u32,
        file: Option<&DebugFile>,
        members: &[DebugMember],
    ) -> Result<DebugType> {
        let builder = self.builder()?;
        self.require_full()?;
        let name = c_text(name)?;
        let file = self.file_ref(file, self.file)?;
        let count = u32::try_from(members.len())
            .map_err(|_| Error::new("too many LLVM debug composite members"))?;
        let member_types = members
            .iter()
            .map(|member| {
                if matches!(kind, CompositeKind::Union) && member.offset_bits != 0 {
                    return Err(Error::new(
                        "an LLVM debug union member must be at offset zero",
                    ));
                }
                if member.offset_bits > bits || member.bits > bits - member.offset_bits {
                    return Err(Error::new(
                        "LLVM debug member exceeds its composite type's size",
                    ));
                }
                member.ty.in_module(&self.module)
            })
            .collect::<Result<Vec<_>>>()?;
        // SAFETY: all types and the file belong to the live module. Members without a
        // separate scope are attached to their containing type through its elements array.
        let raw = unsafe {
            let mut elements = members
                .iter()
                .zip(member_types)
                .map(|(member, ty)| {
                    LLVMDIBuilderCreateMemberType(
                        builder,
                        ptr::null_mut(),
                        member.name.as_ptr().cast(),
                        member.name.len(),
                        file,
                        line,
                        member.bits,
                        member.align_bits,
                        member.offset_bits,
                        LLVMDIFlagZero,
                        ty,
                    )
                })
                .collect::<Vec<_>>();
            match kind {
                CompositeKind::Struct => LLVMDIBuilderCreateStructType(
                    builder,
                    file,
                    name.as_ptr().cast(),
                    name.len(),
                    file,
                    line,
                    bits,
                    align_bits,
                    LLVMDIFlagZero,
                    ptr::null_mut(),
                    elements.as_mut_ptr(),
                    count,
                    0,
                    ptr::null_mut(),
                    c"".as_ptr(),
                    0,
                ),
                CompositeKind::Union => LLVMDIBuilderCreateUnionType(
                    builder,
                    file,
                    name.as_ptr().cast(),
                    name.len(),
                    file,
                    line,
                    bits,
                    align_bits,
                    LLVMDIFlagZero,
                    elements.as_mut_ptr(),
                    count,
                    0,
                    c"".as_ptr(),
                    0,
                ),
            }
        };
        Ok(self.debug_type(raw))
    }

    fn debug_type(&self, raw: LLVMMetadataRef) -> DebugType {
        DebugType {
            raw,
            module: self.module.clone(),
        }
    }
}

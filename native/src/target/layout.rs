use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::rc::Rc;

use inkwell::targets::{ByteOrdering, TargetData};
use inkwell::types::{AsTypeRef, BasicTypeEnum};
use llvm_sys::LLVMTypeKind::*;
use llvm_sys::core::{
    LLVMCountStructElementTypes, LLVMGetElementType, LLVMGetTypeKind, LLVMStructGetTypeAtIndex,
    LLVMTypeIsSized,
};
use llvm_sys::prelude::LLVMTypeRef;
use llvm_sys::target::LLVMOffsetOfElement;

use crate::{Context, Error, Result, Type, c_text};

/// An independent snapshot of LLVM's data layout.
#[derive(Clone, Debug)]
pub struct DataLayout(Rc<LayoutData>);

#[derive(Debug)]
struct LayoutData {
    raw: TargetData,
    text: String,
    pointer_widths: BTreeMap<u32, (u32, u32)>,
    non_integral: BTreeSet<u32>,
}

impl DataLayout {
    /// Parse a layout description and return LLVM's diagnostic on invalid input.
    pub fn parse(text: &str) -> Result<Self> {
        // LLVMCreateTargetData treats malformed input as fatal. The IR parser
        // validates the same layout grammar and reports errors to its caller.
        let quoted = c_text(text)?.replace('\\', "\\5C").replace('"', "\\22");
        let module = Context::default().parse_ir(&format!("target datalayout = \"{quoted}\""))?;
        Ok(module.target_data())
    }

    pub(super) fn from_llvm(raw: TargetData) -> Self {
        let text = raw
            .get_data_layout()
            .as_str()
            .to_string_lossy()
            .into_owned();
        // LLVM-C rounds pointer sizes to whole bytes and has no query for GEP
        // index widths. Read the exact widths from the validated pointer specs.
        let pointer_widths = text
            .split('-')
            .filter_map(|spec| pointer_spec(spec.strip_prefix('p')?))
            .map(|(space, [size, _, _, index])| (space, (size, index)))
            .collect();
        let non_integral = non_integral_address_spaces(&text)
            .expect("LLVM validated the non-integral address space specifications");
        Self(Rc::new(LayoutData {
            raw,
            text,
            pointer_widths,
            non_integral,
        }))
    }

    pub(super) fn llvm(&self) -> &TargetData {
        &self.0.raw
    }

    pub fn text(&self) -> String {
        self.0.text.clone()
    }

    pub fn is_little_endian(&self) -> bool {
        self.0.raw.get_byte_ordering() == ByteOrdering::LittleEndian
    }

    pub fn pointer_size_bits(&self, address_space: u32) -> u32 {
        self.pointer_widths(address_space).0
    }

    /// The bit width used for GEP indices for the given pointer address space.
    pub fn pointer_index_size_bits(&self, address_space: u32) -> u32 {
        self.pointer_widths(address_space).1
    }

    pub fn is_non_integral_pointer(&self, address_space: u32) -> bool {
        self.0.non_integral.contains(&address_space)
    }

    fn pointer_widths(&self, address_space: u32) -> (u32, u32) {
        self.0
            .pointer_widths
            .get(&address_space)
            .or_else(|| self.0.pointer_widths.get(&0))
            .copied()
            .unwrap_or_else(|| {
                let bits = self.0.raw.get_pointer_byte_size(None) * 8;
                (bits, bits)
            })
    }

    /// LLVM's type size in bits, which can differ from its byte-rounded store size.
    pub fn size_in_bits(&self, ty: &Type) -> Result<u64> {
        self.query(ty, |data, ty| data.get_bit_size(&ty))
    }

    /// The number of bytes LLVM may overwrite when storing a value of this type.
    pub fn store_size(&self, ty: &Type) -> Result<u64> {
        self.query(ty, |data, ty| data.get_store_size(&ty))
    }

    /// The space occupied by one value in an array of this type, in bytes.
    pub fn alloc_size(&self, ty: &Type) -> Result<u64> {
        self.query(ty, |data, ty| data.get_abi_size(&ty))
    }

    pub fn abi_alignment(&self, ty: &Type) -> Result<u32> {
        self.query(ty, |data, ty| data.get_abi_alignment(&ty))
    }

    pub fn preferred_alignment(&self, ty: &Type) -> Result<u32> {
        self.query(ty, |data, ty| data.get_preferred_alignment(&ty))
    }

    /// The byte offset of a field in a fixed-size struct.
    pub fn field_offset(&self, ty: &Type, index: u32) -> Result<u64> {
        self.query(ty, |data, ty| {
            let BasicTypeEnum::StructType(ty) = ty else {
                return Err(Error::new("field offsets require an LLVM struct type"));
            };
            if index >= ty.count_fields() {
                return Err(Error::new("LLVM struct field index is out of range"));
            }
            // SAFETY: the struct has a fixed layout and the index is in bounds.
            Ok(unsafe { LLVMOffsetOfElement(data.as_mut_ptr(), ty.as_type_ref(), index) })
        })?
    }

    fn query<T>(
        &self,
        ty: &Type,
        query: impl FnOnce(&TargetData, BasicTypeEnum<'_>) -> T,
    ) -> Result<T> {
        let ty = ty.basic()?;
        // SAFETY: `ty` and its contained types live for the duration of the query.
        if !unsafe { has_fixed_layout(ty.as_type_ref()) } {
            return Err(Error::new("a fixed-size LLVM storage type is required"));
        }
        // A layout caches struct offsets by raw type address. A temporary copy
        // confines aggregate entries to the lifetime of the queried context.
        match ty {
            BasicTypeEnum::ArrayType(_) | BasicTypeEnum::StructType(_) => {
                Ok(query(&TargetData::create(&self.0.text), ty))
            }
            _ => Ok(query(&self.0.raw, ty)),
        }
    }
}

/// Compare two layouts already parsed and accepted by LLVM.
pub(crate) fn layouts_equal(left: &str, right: &str) -> bool {
    left == right || matches!((canonical(left), canonical(right)), (Some(a), Some(b)) if a == b)
}

#[derive(PartialEq, Eq)]
struct Layout {
    specs: BTreeMap<(u8, u32), Vec<u32>>,
    native_widths: Vec<u32>,
    non_integral: BTreeSet<u32>,
}

fn canonical(text: &str) -> Option<Layout> {
    // LLVM 21 DataLayout initializes these entries before applying the specs.
    let mut specs = BTreeMap::from([
        ((b'e', 0), vec![0]),
        ((b'P', 0), vec![0]),
        ((b'A', 0), vec![0]),
        ((b'G', 0), vec![0]),
        ((b'a', 0), vec![8, 64]),
        ((b'p', 0), vec![64, 64, 64, 64]),
        ((b'i', 1), vec![8, 8]),
        ((b'i', 8), vec![8, 8]),
        ((b'i', 16), vec![16, 16]),
        ((b'i', 32), vec![32, 32]),
        ((b'i', 64), vec![32, 64]),
        ((b'f', 16), vec![16, 16]),
        ((b'f', 32), vec![32, 32]),
        ((b'f', 64), vec![64, 64]),
        ((b'f', 128), vec![128, 128]),
        ((b'v', 64), vec![64, 64]),
        ((b'v', 128), vec![128, 128]),
    ]);
    let mut native_widths = Vec::new();
    let non_integral = non_integral_address_spaces(text)?;
    for spec in text.split('-').filter(|_| !text.is_empty()) {
        if spec.starts_with("ni:") {
            continue;
        }
        let kind = *spec.as_bytes().first()?;
        let rest = spec.get(1..)?;
        if kind == b'n' {
            for width in rest.split(':') {
                native_widths.push(width.parse().ok()?);
            }
            continue;
        }
        if kind == b's' {
            continue; // Deprecated stack-object alignment has no effect in LLVM 21.
        }
        let (key, value) = match kind {
            b'e' | b'E' => ((b'e', 0), vec![u32::from(kind == b'E')]),
            b'P' | b'A' | b'G' | b'S' => ((kind, 0), vec![rest.parse().ok()?]),
            b'm' => (
                (kind, 0),
                vec![u32::from(*rest.strip_prefix(':')?.as_bytes().first()?)],
            ),
            b'F' => (
                (kind, 0),
                vec![
                    u32::from(*rest.as_bytes().first()?),
                    rest.get(1..)?.parse().ok()?,
                ],
            ),
            b'p' => {
                let (space, fields) = pointer_spec(rest)?;
                ((kind, space), fields.to_vec())
            }
            b'i' | b'v' | b'f' | b'a' => {
                let (index, fields) = rest.split_once(':')?;
                let index = if index.is_empty() {
                    0
                } else {
                    index.parse().ok()?
                };
                let mut fields = fields.split(':').map(str::parse::<u32>);
                let abi = fields.next()?.ok()?;
                let pref = fields.next().transpose().ok()?.unwrap_or(abi);
                ((kind, index), vec![abi.max(8), pref.max(8)])
            }
            _ => return None,
        };
        specs.insert(key, value);
    }

    // LLVM materializes an address-space-zero pointer spec for each nonintegral
    // address space that has no explicit spec of its own.
    let default_pointer = specs[&(b'p', 0)].clone();
    for &space in &non_integral {
        specs
            .entry((b'p', space))
            .or_insert_with(|| default_pointer.clone());
    }
    Some(Layout {
        specs,
        native_widths,
        non_integral,
    })
}

fn non_integral_address_spaces(text: &str) -> Option<BTreeSet<u32>> {
    text.split('-')
        .filter_map(|spec| spec.strip_prefix("ni:"))
        .flat_map(|spaces| spaces.split(':'))
        .map(|space| space.parse().ok())
        .collect()
}

fn pointer_spec(spec: &str) -> Option<(u32, [u32; 4])> {
    let (space, fields) = spec.split_once(':')?;
    let space = if space.is_empty() {
        0
    } else {
        space.parse().ok()?
    };
    let mut fields = fields.split(':').map(str::parse::<u32>);
    let size = fields.next()?.ok()?;
    let abi = fields.next()?.ok()?;
    let pref = fields.next().transpose().ok()?.unwrap_or(abi);
    let index = fields.next().transpose().ok()?.unwrap_or(size);
    Some((space, [size, abi, pref, index]))
}

/// Safety: `ty` and its recursively referenced storage types are live.
unsafe fn has_fixed_layout(ty: LLVMTypeRef) -> bool {
    // SAFETY: all traversed elements are borrowed from the input type's context.
    unsafe {
        if LLVMTypeIsSized(ty) == 0 {
            return false;
        }
        let mut remaining = vec![ty];
        let mut visited = HashSet::new();
        while let Some(ty) = remaining.pop() {
            if !visited.insert(ty) {
                continue;
            }
            match LLVMGetTypeKind(ty) {
                // LLVM-C does not expose the storage type of a target extension.
                LLVMScalableVectorTypeKind | LLVMTargetExtTypeKind => return false,
                LLVMArrayTypeKind => remaining.push(LLVMGetElementType(ty)),
                LLVMStructTypeKind => remaining.extend(
                    (0..LLVMCountStructElementTypes(ty))
                        .map(|index| LLVMStructGetTypeAtIndex(ty, index)),
                ),
                _ => {}
            }
        }
        true
    }
}

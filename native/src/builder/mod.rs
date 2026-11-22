mod call;
mod cast;
mod control;
mod exception;
mod flags;
mod funclets;
pub(crate) mod memory;
mod memory_intrinsics;
mod vector;

use std::cell::{Ref, RefCell};
use std::{ffi::CString, ptr};

use inkwell::builder::Builder as LlvmBuilder;
use inkwell::llvm_sys::core::{
    LLVMBuildBinOp, LLVMBuildExtractValue, LLVMBuildFCmp, LLVMBuildFNeg, LLVMBuildFreeze,
    LLVMBuildGEPWithNoWrapFlags, LLVMBuildICmp, LLVMBuildInsertValue, LLVMBuildNot,
    LLVMCountStructElementTypes, LLVMGetArrayLength2, LLVMGetCurrentDebugLocation2,
    LLVMGetElementType, LLVMGetTypeKind, LLVMSetCurrentDebugLocation2, LLVMSetFastMathFlags,
    LLVMStructGetTypeAtIndex,
};
use inkwell::llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use inkwell::llvm_sys::{
    LLVMGEPFlagInBounds, LLVMGEPFlagNUSW, LLVMGEPFlagNUW, LLVMOpcode::*, LLVMTypeKind::*,
};
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum};
use inkwell::values::LLVMTailCallKind::LLVMTailCallKindMustTail;
use inkwell::values::{
    AsValueRef, BasicValueEnum, InstructionOpcode, InstructionValue, IntValue, PointerValue,
};
use self_cell::self_cell;

use crate::ir::{FunctionBody, supported_value_type};
use crate::{
    Block, Context, DebugLocation, Error, FastMathFlags, Function, Instruction, Linkage, Module,
    Result, Type, Value,
};

pub use call::OperandBundle;
pub use cast::CastOp;
pub use flags::{IntFlags, IntOp};
pub use inkwell::{FloatPredicate, IntPredicate};
pub use memory::{
    AtomicOptions, AtomicOrdering, AtomicRmwOp, AtomicRmwOptions, CmpXchgOptions, MemoryOptions,
    SyncScope,
};

// LLVM User stores its operand count in a 27-bit field.
const MAX_USER_OPERANDS: usize = (1 << 27) - 1;

/// Promises about the address and index arithmetic of a getelementptr.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct GepFlags {
    pub in_bounds: bool,
    pub no_unsigned_wrap: bool,
    pub no_unsigned_signed_wrap: bool,
}

impl GepFlags {
    pub(crate) const ALL_BITS: u32 = LLVMGEPFlagInBounds | LLVMGEPFlagNUW | LLVMGEPFlagNUSW;

    pub(crate) fn bits(self) -> u32 {
        (u32::from(self.in_bounds) * LLVMGEPFlagInBounds)
            | (u32::from(self.no_unsigned_wrap) * LLVMGEPFlagNUW)
            | (u32::from(self.no_unsigned_signed_wrap) * LLVMGEPFlagNUSW)
    }

    pub(crate) fn from_bits(bits: u32) -> Self {
        Self {
            in_bounds: bits & LLVMGEPFlagInBounds != 0,
            no_unsigned_wrap: bits & LLVMGEPFlagNUW != 0,
            no_unsigned_signed_wrap: bits & LLVMGEPFlagNUSW != 0,
        }
    }
}

#[derive(Clone, Copy)]
pub enum FloatOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

self_cell! {
    struct BuilderStorage {
        owner: Context,
        #[covariant]
        dependent: LlvmBuilder,
    }
}

enum InsertionPoint {
    End(Block),
    Before(Instruction),
}

impl InsertionPoint {
    fn block(&self) -> &Block {
        match self {
            Self::End(block) => block,
            Self::Before(instruction) => instruction.block(),
        }
    }

    fn previous_instruction(&self) -> Option<InstructionValue<'_>> {
        match self {
            Self::End(block) => block.llvm().get_last_instruction(),
            Self::Before(instruction) => instruction.llvm().get_previous_instruction(),
        }
    }
}

/// An LLVM builder with an optional insertion point in a basic block.
pub struct Builder {
    // Drop the LLVM builder before releasing its insertion point or undoing a body.
    storage: BuilderStorage,
    position: RefCell<Option<InsertionPoint>>,
    body: RefCell<Option<FunctionBody>>,
}

impl Builder {
    pub fn new(context: &Context) -> Self {
        Self {
            storage: BuilderStorage::new(context.clone(), |ctx| ctx.llvm().create_builder()),
            position: RefCell::default(),
            body: RefCell::default(),
        }
    }

    pub(crate) fn begin_function_body(function: &Function) -> Result<Self> {
        let (body, entry) = FunctionBody::begin(function)?;
        Self::with_body(body, &entry)
    }

    pub(crate) fn begin_named_function_body(
        module: &Module,
        name: &str,
        signature: &Type,
        linkage: Option<Linkage>,
    ) -> Result<Self> {
        let (body, entry) = FunctionBody::begin_named(module, name, signature, linkage)?;
        Self::with_body(body, &entry)
    }

    fn with_body(body: FunctionBody, entry: &Block) -> Result<Self> {
        let builder = Self::new(body.function().module.context());
        builder.position_at_end(entry)?;
        builder.body.replace(Some(body));
        Ok(builder)
    }

    pub(crate) fn body_function(&self) -> Result<Function> {
        self.body
            .borrow()
            .as_ref()
            .map(|body| body.function().clone())
            .ok_or_else(|| Error::new("LLVM builder has no open function body"))
    }

    pub(crate) fn finish_function_body(&self) -> Result<()> {
        let mut state = self.body.borrow_mut();
        let body = state
            .as_ref()
            .ok_or_else(|| Error::new("LLVM builder has no open function body"))?;
        body.verify()?;
        state.take().expect("verified body").commit();
        Ok(())
    }

    fn llvm(&self) -> &LlvmBuilder<'_> {
        self.storage.borrow_dependent()
    }

    pub(crate) fn context(&self) -> &Context {
        self.storage.borrow_owner()
    }

    pub(crate) fn block(&self) -> Result<Option<Block>> {
        self.position
            .borrow()
            .as_ref()
            .map(|point| {
                let block = point.block();
                block.check_live()?;
                Ok(block.clone())
            })
            .transpose()
    }

    pub fn position_at_end(&self, block: &Block) -> Result<()> {
        block.check_live()?;
        self.context().check(block.function.module.context())?;
        self.check_body_position(block)?;
        self.llvm().position_at_end(block.llvm());
        self.clear_debug_location();
        *self.position.borrow_mut() = Some(InsertionPoint::End(block.clone()));
        Ok(())
    }

    /// Emit subsequent instructions immediately before `instruction`, including
    /// when it is the block's terminator. PHIs must stay grouped at the start
    /// of their block, and new terminators require an unterminated block end.
    pub fn position_before(&self, instruction: &Instruction) -> Result<()> {
        let block = instruction.block();
        block.check_live()?;
        self.context().check(block.function.module.context())?;
        self.check_body_position(block)?;
        self.llvm().position_before(&instruction.llvm());
        self.clear_debug_location();
        *self.position.borrow_mut() = Some(InsertionPoint::Before(instruction.clone()));
        Ok(())
    }

    fn check_body_position(&self, block: &Block) -> Result<()> {
        if let Some(body) = self.body.borrow().as_ref() {
            body.function().check(block.function())?;
        }
        Ok(())
    }

    pub fn clear_position(&self) {
        self.llvm().clear_insertion_position();
        self.clear_debug_location();
        self.position.take();
    }

    /// Set the location of subsequent instructions in this block.
    /// Changing the insertion point clears the location.
    pub fn set_debug_location(&self, location: &DebugLocation) -> Result<()> {
        let position = self.position.borrow();
        let point = position
            .as_ref()
            .ok_or_else(|| Error::new("LLVM builder has no insertion block"))?;
        point.block().check_live()?;
        location.check_for_function(point.block().function())?;
        // SAFETY: location metadata belongs to the retained insertion function and context.
        unsafe { LLVMSetCurrentDebugLocation2(self.llvm().as_mut_ptr(), location.raw) };
        Ok(())
    }

    /// Return the location currently used for newly built instructions.
    pub fn debug_location(&self) -> Option<DebugLocation> {
        let position = self.position.borrow();
        let block = position.as_ref()?.block();
        block.check_live().ok()?;
        let function = &block.function;
        // SAFETY: the builder and its insertion function are both retained.
        let raw = unsafe { LLVMGetCurrentDebugLocation2(self.llvm().as_mut_ptr()) };
        DebugLocation::from_raw(raw, function)
    }

    pub fn clear_debug_location(&self) {
        // SAFETY: null clears the current location of the live LLVM builder.
        unsafe { LLVMSetCurrentDebugLocation2(self.llvm().as_mut_ptr(), ptr::null_mut()) };
    }

    fn insertion_point(&self) -> Result<Ref<'_, InsertionPoint>> {
        let point = Ref::filter_map(self.position.borrow(), Option::as_ref)
            .map_err(|_| Error::new("LLVM builder has no insertion block"))?;
        point.block().check_live()?;
        if let InsertionPoint::End(block) = &*point
            && block.is_terminated()
        {
            return Err(Error::new("LLVM insertion block already has a terminator"));
        }
        Ok(point)
    }

    fn insertion(&self) -> Result<Ref<'_, Block>> {
        let point = self.insertion_point()?;
        if let InsertionPoint::Before(anchor) = &*point {
            if matches!(
                anchor.llvm().get_opcode(),
                InstructionOpcode::Phi
                    | InstructionOpcode::LandingPad
                    | InstructionOpcode::CatchPad
                    | InstructionOpcode::CleanupPad
                    | InstructionOpcode::CatchSwitch
            ) {
                return Err(Error::new(
                    "cannot insert an ordinary instruction before an LLVM PHI or EH pad",
                ));
            }
            if point.previous_instruction().is_some_and(follows_musttail) {
                return Err(Error::new(
                    "cannot insert an instruction between an LLVM musttail call and its return",
                ));
            }
        }
        Ok(Ref::map(point, InsertionPoint::block))
    }

    fn terminator_insertion(&self) -> Result<Ref<'_, Block>> {
        let point = self.insertion_point()?;
        if matches!(&*point, InsertionPoint::Before(_)) {
            return Err(Error::new(
                "an LLVM terminator requires the end of an unterminated block",
            ));
        }
        Ok(Ref::map(point, InsertionPoint::block))
    }

    pub fn float_binop(&self, op: FloatOp, lhs: &Value, rhs: &Value, name: &str) -> Result<Value> {
        self.float_binop_with_flags(op, lhs, rhs, FastMathFlags::empty(), name)
    }

    /// Emit a floating-point operation with LLVM's fast-math flags.
    /// A flagged operation is emitted even when both operands are constants.
    pub fn float_binop_with_flags(
        &self,
        op: FloatOp,
        lhs: &Value,
        rhs: &Value,
        flags: FastMathFlags,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let (lhs, rhs) = same_type_operands(lhs, rhs, &block.function)?;
        check_float_operand(lhs.get_type())?;
        let opcode = match op {
            FloatOp::Add => LLVMFAdd,
            FloatOp::Sub => LLVMFSub,
            FloatOp::Mul => LLVMFMul,
            FloatOp::Div => LLVMFDiv,
            FloatOp::Rem => LLVMFRem,
        };
        let name = c_name(name)?;
        let rhs = rhs.as_value_ref();
        // SAFETY: operands have the same live floating-point scalar or vector type.
        let value = unsafe {
            self.build_with_fast_math(lhs.as_value_ref(), Some(rhs), flags, |lhs| {
                LLVMBuildBinOp(self.llvm().as_mut_ptr(), opcode, lhs, rhs, name.as_ptr())
            })
        };
        Ok(block.function.local(value))
    }

    pub fn fneg(&self, value: &Value, name: &str) -> Result<Value> {
        self.fneg_with_flags(value, FastMathFlags::empty(), name)
    }

    pub fn fneg_with_flags(
        &self,
        value: &Value,
        flags: FastMathFlags,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let value = value.operand(&block.function)?;
        check_float_operand(value.get_type())?;
        let name = c_name(name)?;
        // SAFETY: the operand is a live floating-point scalar or vector in this function.
        let negated = unsafe {
            self.build_with_fast_math(value.as_value_ref(), None, flags, |value| {
                LLVMBuildFNeg(self.llvm().as_mut_ptr(), value, name.as_ptr())
            })
        };
        Ok(block.function.local(negated))
    }

    /// Invert every bit of an integer scalar or vector.
    pub fn not(&self, value: &Value, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        let value = value.operand(&block.function)?;
        if !arithmetic_element_type(value.get_type()).is_int_type() {
            return Err(Error::new("expected an integer operand"));
        }
        let name = c_name(name)?;
        // SAFETY: the operand is a live integer scalar or vector in this function.
        let inverted = unsafe {
            BasicValueEnum::new(LLVMBuildNot(
                self.llvm().as_mut_ptr(),
                value.as_value_ref(),
                name.as_ptr(),
            ))
        };
        Ok(block.function.local(inverted))
    }

    pub fn icmp(
        &self,
        predicate: IntPredicate,
        lhs: &Value,
        rhs: &Value,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let (lhs, rhs) = same_type_operands(lhs, rhs, &block.function)?;
        if !matches!(
            arithmetic_element_type(lhs.get_type()),
            BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_)
        ) {
            return Err(Error::new(
                "expected integer or pointer comparison operands",
            ));
        }
        let name = c_name(name)?;
        // SAFETY: icmp accepts two live, same-type integer or pointer operands.
        let value = unsafe {
            let raw = LLVMBuildICmp(
                self.llvm().as_mut_ptr(),
                predicate.into(),
                lhs.as_value_ref(),
                rhs.as_value_ref(),
                name.as_ptr(),
            );
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(value))
    }

    pub fn fcmp(
        &self,
        predicate: FloatPredicate,
        lhs: &Value,
        rhs: &Value,
        name: &str,
    ) -> Result<Value> {
        self.fcmp_with_flags(predicate, lhs, rhs, FastMathFlags::empty(), name)
    }

    pub fn fcmp_with_flags(
        &self,
        predicate: FloatPredicate,
        lhs: &Value,
        rhs: &Value,
        flags: FastMathFlags,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let (lhs, rhs) = same_type_operands(lhs, rhs, &block.function)?;
        check_float_operand(lhs.get_type())?;
        let name = c_name(name)?;
        let rhs = rhs.as_value_ref();
        // SAFETY: fcmp accepts two live, same-type floating-point scalars or vectors.
        let value = unsafe {
            self.build_with_fast_math(lhs.as_value_ref(), Some(rhs), flags, |lhs| {
                LLVMBuildFCmp(
                    self.llvm().as_mut_ptr(),
                    predicate.into(),
                    lhs,
                    rhs,
                    name.as_ptr(),
                )
            })
        };
        Ok(block.function.local(value))
    }

    /// `build` must use `lhs` as operand zero and `rhs`, when supplied, as
    /// operand one of an LLVM floating-point arithmetic or comparison operation.
    unsafe fn build_with_fast_math(
        &self,
        lhs: LLVMValueRef,
        rhs: Option<LLVMValueRef>,
        flags: FastMathFlags,
        build: impl FnOnce(LLVMValueRef) -> LLVMValueRef,
    ) -> BasicValueEnum<'_> {
        // SAFETY: the caller checks operands and uses a floating-point opcode.
        // Suppressing the default fold preserves the flags, including poison
        // when a constant violates nnan or ninf.
        unsafe {
            let raw = if flags.is_empty() {
                build(lhs)
            } else {
                let raw = self.build_without_constant_folding(lhs, rhs, build);
                LLVMSetFastMathFlags(raw, flags.bits());
                raw
            };
            BasicValueEnum::new(raw)
        }
    }

    /// Emit LLVM's freeze operation, which removes poison and undef from an SSA value.
    pub fn freeze(&self, value: &Value, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        let value = value.operand(&block.function)?;
        let name = c_name(name)?;
        // SAFETY: freeze accepts the first-class type of this live operand.
        let frozen = unsafe {
            BasicValueEnum::new(LLVMBuildFreeze(
                self.llvm().as_mut_ptr(),
                value.as_value_ref(),
                name.as_ptr(),
            ))
        };
        Ok(block.function.local(frozen))
    }

    pub fn gep(&self, ty: &Type, ptr: &Value, indices: &[Value], name: &str) -> Result<Value> {
        self.gep_with_flags(ty, ptr, indices, GepFlags::default(), name)
    }

    /// Emit getelementptr with LLVM's inbounds guarantee.
    pub fn gep_in_bounds(
        &self,
        ty: &Type,
        ptr: &Value,
        indices: &[Value],
        name: &str,
    ) -> Result<Value> {
        self.gep_with_flags(
            ty,
            ptr,
            indices,
            GepFlags {
                in_bounds: true,
                ..GepFlags::default()
            },
            name,
        )
    }

    /// Build a GEP with explicit LLVM no-wrap guarantees. Violating a guarantee yields poison.
    pub fn gep_with_flags(
        &self,
        ty: &Type,
        ptr: &Value,
        indices: &[Value],
        flags: GepFlags,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let ty = ty.basic_in(self.context())?;
        let ptr = pointer_value(ptr, &block.function)?;
        let indices = indices
            .iter()
            .map(|index| int_value(index, &block.function))
            .collect::<Result<Vec<_>>>()?;
        validate_gep_indices(ty, &indices)?;
        let name = c_name(name)?;
        let mut indices = indices
            .iter()
            .map(AsValueRef::as_value_ref)
            .collect::<Vec<_>>();
        let count =
            u32::try_from(indices.len()).map_err(|_| Error::new("too many LLVM GEP indices"))?;
        // SAFETY: the source is sized, the pointer and scalar indices belong
        // to the insertion function, and the type path above validates each index.
        let value = unsafe {
            let raw = LLVMBuildGEPWithNoWrapFlags(
                self.llvm().as_mut_ptr(),
                ty.as_type_ref(),
                ptr.as_value_ref(),
                indices.as_mut_ptr(),
                count,
                name.as_ptr(),
                flags.bits(),
            );
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(value))
    }

    /// Read a field of a struct or element of an array SSA value.
    pub fn extract_value(&self, aggregate: &Value, index: u32, name: &str) -> Result<Value> {
        let block = self.insertion()?;
        let aggregate = aggregate.operand(&block.function)?;
        aggregate_element(aggregate.get_type(), index)?;
        let name = c_name(name)?;
        // SAFETY: the insertion block and aggregate are live, and the field exists
        // and has a supported value type.
        let value = unsafe {
            let raw = LLVMBuildExtractValue(
                self.llvm().as_mut_ptr(),
                aggregate.as_value_ref(),
                index,
                name.as_ptr(),
            );
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(value))
    }

    /// Return an aggregate SSA value with the specified field or element replaced.
    pub fn insert_value(
        &self,
        aggregate: &Value,
        value: &Value,
        index: u32,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let aggregate = aggregate.operand(&block.function)?;
        let value = value.operand(&block.function)?;
        if aggregate_element(aggregate.get_type(), index)? != value.get_type().as_type_ref() {
            return Err(Error::new(
                "inserted value must match the LLVM aggregate element type",
            ));
        }
        let name = c_name(name)?;
        // SAFETY: both operands are valid in this block and the replacement
        // matches the selected field's type.
        let updated = unsafe {
            let raw = LLVMBuildInsertValue(
                self.llvm().as_mut_ptr(),
                aggregate.as_value_ref(),
                value.as_value_ref(),
                index,
                name.as_ptr(),
            );
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(updated))
    }
}

fn follows_musttail(previous: InstructionValue<'_>) -> bool {
    let call = if previous.get_opcode() == InstructionOpcode::BitCast {
        previous.get_previous_instruction()
    } else {
        Some(previous)
    };
    call.is_some_and(|call| matches!(call.get_tail_call_kind(), Ok(LLVMTailCallKindMustTail)))
}

fn aggregate_element(ty: BasicTypeEnum<'_>, index: u32) -> Result<LLVMTypeRef> {
    let raw = match ty {
        BasicTypeEnum::ArrayType(ty) => {
            let ty = ty.as_type_ref();
            // SAFETY: `ty` is an array. Its element count can exceed u32.
            if u64::from(index) >= unsafe { LLVMGetArrayLength2(ty) } {
                return Err(Error::new("LLVM array index is out of range"));
            }
            unsafe { LLVMGetElementType(ty) }
        }
        BasicTypeEnum::StructType(ty) => {
            if index >= ty.count_fields() {
                return Err(Error::new("LLVM struct field index is out of range"));
            }
            // SAFETY: the field index is within this struct.
            unsafe { LLVMStructGetTypeAtIndex(ty.as_type_ref(), index) }
        }
        _ => return Err(Error::new("expected an LLVM array or struct value")),
    };
    supported_value_type(raw)?;
    Ok(raw)
}

fn sized_type(ty: BasicTypeEnum<'_>) -> Result<BasicTypeEnum<'_>> {
    if ty.is_sized() {
        Ok(ty)
    } else {
        Err(Error::new("instruction requires a sized LLVM type"))
    }
}

fn int_value<'a>(value: &'a Value, function: &Function) -> Result<IntValue<'a>> {
    IntValue::try_from(value.operand(function)?)
        .map_err(|_| Error::new("expected a scalar integer operand"))
}

fn pointer_value<'a>(value: &'a Value, function: &Function) -> Result<PointerValue<'a>> {
    PointerValue::try_from(value.operand(function)?)
        .map_err(|_| Error::new("expected a pointer operand"))
}

fn c_name(name: &str) -> Result<CString> {
    CString::new(name).map_err(|_| Error::new("LLVM text contains an embedded NUL"))
}

fn same_type_operands<'a>(
    lhs: &'a Value,
    rhs: &'a Value,
    function: &Function,
) -> Result<(BasicValueEnum<'a>, BasicValueEnum<'a>)> {
    let lhs = lhs.operand(function)?;
    let rhs = rhs.operand(function)?;
    if lhs.get_type() != rhs.get_type() {
        return Err(Error::new("operands must have the same LLVM type"));
    }
    Ok((lhs, rhs))
}

fn arithmetic_element_type(ty: BasicTypeEnum<'_>) -> BasicTypeEnum<'_> {
    match ty {
        BasicTypeEnum::VectorType(ty) => ty.get_element_type(),
        BasicTypeEnum::ScalableVectorType(ty) => ty.get_element_type(),
        ty => ty,
    }
}

fn check_float_operand(ty: BasicTypeEnum<'_>) -> Result<()> {
    if arithmetic_element_type(ty).is_float_type() {
        Ok(())
    } else {
        Err(Error::new("expected a floating-point operand"))
    }
}

pub(crate) fn validate_gep_indices(ty: BasicTypeEnum<'_>, indices: &[IntValue<'_>]) -> Result<()> {
    let [_, remaining @ ..] = indices else {
        return Err(Error::new("getelementptr requires at least one index"));
    };
    if u32::try_from(indices.len()).is_err() {
        return Err(Error::new("too many LLVM getelementptr indices"));
    }
    let mut element = sized_type(ty)?.as_type_ref();
    // A sized struct with scalable storage has homogeneous scalable vector fields.
    // LLVM permits allocation of such structs but disallows a GEP based on one.
    if let BasicTypeEnum::StructType(ty) = ty
        && ty.count_fields() != 0
        // SAFETY: the preceding guards establish a nonempty struct type.
        && unsafe { LLVMGetTypeKind(LLVMStructGetTypeAtIndex(element, 0)) }
            == LLVMScalableVectorTypeKind
    {
        return Err(Error::new(
            "getelementptr cannot target an LLVM struct with scalable vector fields",
        ));
    }
    for index in remaining {
        // SAFETY: `element` is a live type reached by the validated path so far.
        element = match unsafe { LLVMGetTypeKind(element) } {
            LLVMArrayTypeKind | LLVMVectorTypeKind | LLVMScalableVectorTypeKind => {
                // SAFETY: these types all have an element type.
                unsafe { LLVMGetElementType(element) }
            }
            LLVMStructTypeKind => {
                // SAFETY: `element` is a struct type.
                let count = unsafe { LLVMCountStructElementTypes(element) };
                let field = (index.get_type().get_bit_width() == 32)
                    .then(|| index.get_zero_extended_constant())
                    .flatten()
                    .filter(|&field| field < u64::from(count))
                    .ok_or_else(|| Error::new("struct index must be an in-range i32 constant"))?;
                // SAFETY: the index is within the struct's field range.
                unsafe { LLVMStructGetTypeAtIndex(element, field as u32) }
            }
            _ => {
                return Err(Error::new(
                    "getelementptr index does not select an aggregate element",
                ));
            }
        };
    }
    Ok(())
}

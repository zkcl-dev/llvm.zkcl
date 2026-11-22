use inkwell::llvm_sys::LLVMAtomicOrdering;
use inkwell::llvm_sys::core::{
    LLVMBuildAlloca, LLVMBuildAtomicCmpXchg, LLVMBuildAtomicRMW, LLVMBuildFence, LLVMBuildLoad2,
    LLVMBuildStore, LLVMSetAlignment, LLVMSetAtomicSingleThread, LLVMSetOrdering,
    LLVMSetValueName2, LLVMSetVolatile, LLVMSetWeak,
};
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::llvm_sys::target::{LLVMGetModuleDataLayout, LLVMSizeOfTypeInBits};
use inkwell::types::{AsTypeRef, BasicTypeEnum};
use inkwell::values::{AsValueRef, BasicValueEnum, InstructionOpcode};

use super::{Builder, c_name, pointer_value, same_type_operands, sized_type};
use crate::{Error, Module, Result, Type, Value, c_text};

pub use inkwell::AtomicRMWBinOp as AtomicRmwOp;

/// LLVM memory ordering for an atomic instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AtomicOrdering {
    Unordered,
    Monotonic,
    Acquire,
    Release,
    AcquireRelease,
    SequentiallyConsistent,
}

impl From<AtomicOrdering> for LLVMAtomicOrdering {
    fn from(ordering: AtomicOrdering) -> Self {
        match ordering {
            AtomicOrdering::Unordered => Self::LLVMAtomicOrderingUnordered,
            AtomicOrdering::Monotonic => Self::LLVMAtomicOrderingMonotonic,
            AtomicOrdering::Acquire => Self::LLVMAtomicOrderingAcquire,
            AtomicOrdering::Release => Self::LLVMAtomicOrderingRelease,
            AtomicOrdering::AcquireRelease => Self::LLVMAtomicOrderingAcquireRelease,
            AtomicOrdering::SequentiallyConsistent => {
                Self::LLVMAtomicOrderingSequentiallyConsistent
            }
        }
    }
}

impl AtomicOrdering {
    pub(crate) fn from_llvm(ordering: LLVMAtomicOrdering) -> Option<Self> {
        use LLVMAtomicOrdering::*;

        Some(match ordering {
            LLVMAtomicOrderingNotAtomic => return None,
            LLVMAtomicOrderingUnordered => Self::Unordered,
            LLVMAtomicOrderingMonotonic => Self::Monotonic,
            LLVMAtomicOrderingAcquire => Self::Acquire,
            LLVMAtomicOrderingRelease => Self::Release,
            LLVMAtomicOrderingAcquireRelease => Self::AcquireRelease,
            LLVMAtomicOrderingSequentiallyConsistent => Self::SequentiallyConsistent,
        })
    }
}

/// LLVM's built-in synchronization scopes.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum SyncScope {
    #[default]
    System,
    SingleThread,
}

impl SyncScope {
    pub(crate) fn single_thread(self) -> i32 {
        i32::from(self == Self::SingleThread)
    }
}

/// Ordering and scope of an atomic load, store or fence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AtomicOptions {
    pub ordering: AtomicOrdering,
    pub scope: SyncScope,
}

/// Options for a load or store. By default, the access is neither atomic nor volatile.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct MemoryOptions {
    /// Known byte alignment, as a nonzero power of two. The default uses LLVM's
    /// ABI alignment for ordinary accesses and the access size for atomic accesses.
    pub alignment: Option<u32>,
    pub volatile: bool,
    pub atomic: Option<AtomicOptions>,
}

/// Options for an atomic read-modify-write operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AtomicRmwOptions {
    pub ordering: AtomicOrdering,
    pub scope: SyncScope,
    /// Known byte alignment, as a nonzero power of two. Defaults to the access size.
    pub alignment: Option<u32>,
    pub volatile: bool,
}

/// Options for an atomic compare-and-exchange operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CmpXchgOptions {
    pub success: AtomicOrdering,
    pub failure: AtomicOrdering,
    pub scope: SyncScope,
    /// Known byte alignment, as a nonzero power of two. Defaults to the access size.
    pub alignment: Option<u32>,
    pub volatile: bool,
    /// Permit failure even when the expected value matches memory.
    pub weak: bool,
}

impl Builder {
    pub fn alloca(&self, ty: &Type, name: &str) -> Result<Value> {
        self.alloca_with_alignment(ty, None, name)
    }

    /// Allocate a value on the stack using the requested byte alignment.
    pub fn alloca_with_alignment(
        &self,
        ty: &Type,
        alignment: Option<u32>,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let ty = sized_type(ty.basic_in(self.context())?)?;
        let name = c_name(name)?;
        check_alignment(alignment.map(u64::from))?;
        // SAFETY: the type is sized and belongs to the insertion context. The
        // optional alignment is a nonzero power of two.
        let value = unsafe {
            let raw = LLVMBuildAlloca(self.llvm().as_mut_ptr(), ty.as_type_ref(), name.as_ptr());
            if let Some(alignment) = alignment {
                LLVMSetAlignment(raw, alignment);
            }
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(value))
    }

    pub fn load(&self, ty: &Type, ptr: &Value, name: &str) -> Result<Value> {
        self.load_with_options(ty, ptr, MemoryOptions::default(), name)
    }

    /// Load a sized value through an opaque pointer.
    pub fn load_with_options(
        &self,
        ty: &Type,
        ptr: &Value,
        options: MemoryOptions,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let ty = sized_type(ty.basic_in(self.context())?)?;
        let ptr = pointer_value(ptr, &block.function)?;
        let name = c_name(name)?;
        validate_memory_ordering(
            InstructionOpcode::Load,
            options.atomic.map(|atomic| atomic.ordering),
        )?;
        let options = options.for_access(ty, &block.function.module)?;
        // SAFETY: the sized type and pointer belong to the insertion context.
        // The optional alignment, operand type and atomic load ordering are validated.
        let value = unsafe {
            let raw = LLVMBuildLoad2(
                self.llvm().as_mut_ptr(),
                ty.as_type_ref(),
                ptr.as_value_ref(),
                name.as_ptr(),
            );
            options.apply(raw);
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(value))
    }

    pub fn store(&self, value: &Value, ptr: &Value) -> Result<()> {
        self.store_with_options(value, ptr, MemoryOptions::default())
    }

    /// Store a sized value through an opaque pointer.
    pub fn store_with_options(
        &self,
        value: &Value,
        ptr: &Value,
        options: MemoryOptions,
    ) -> Result<()> {
        let block = self.insertion()?;
        let value = value.operand(&block.function)?;
        let ty = sized_type(value.get_type())?;
        let ptr = pointer_value(ptr, &block.function)?;
        validate_memory_ordering(
            InstructionOpcode::Store,
            options.atomic.map(|atomic| atomic.ordering),
        )?;
        let options = options.for_access(ty, &block.function.module)?;
        // SAFETY: the operands are live and the access has a valid type,
        // alignment and atomic store ordering.
        unsafe {
            let raw = LLVMBuildStore(
                self.llvm().as_mut_ptr(),
                value.as_value_ref(),
                ptr.as_value_ref(),
            );
            options.apply(raw);
        }
        Ok(())
    }

    /// Emit a fence with acquire, release, acquire-release, or sequentially consistent ordering.
    pub fn fence(&self, options: AtomicOptions) -> Result<()> {
        let _block = self.insertion()?;
        validate_memory_ordering(InstructionOpcode::Fence, Some(options.ordering))?;
        // SAFETY: the insertion block is live and this ordering is valid for a fence.
        unsafe {
            LLVMBuildFence(
                self.llvm().as_mut_ptr(),
                options.ordering.into(),
                options.scope.single_thread(),
                c"".as_ptr(),
            );
        }
        Ok(())
    }

    /// Atomically update memory and return the previous value.
    pub fn atomic_rmw(
        &self,
        op: AtomicRmwOp,
        ptr: &Value,
        value: &Value,
        options: AtomicRmwOptions,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let ptr = pointer_value(ptr, &block.function)?;
        let value = value.operand(&block.function)?;
        let name = c_text(name)?;
        check_alignment(options.alignment.map(u64::from))?;
        validate_memory_ordering(InstructionOpcode::AtomicRMW, Some(options.ordering))?;
        let ty = value.get_type();
        validate_atomic_rmw_operation(op, ty)?;
        atomic_access_size(ty, &block.function.module)?;
        // SAFETY: operands are live and the operation, alignment, type and ordering are valid.
        let result = unsafe {
            let raw = LLVMBuildAtomicRMW(
                self.llvm().as_mut_ptr(),
                op.into(),
                ptr.as_value_ref(),
                value.as_value_ref(),
                options.ordering.into(),
                options.scope.single_thread(),
            );
            set_memory_flags(raw, options.alignment, options.volatile);
            LLVMSetValueName2(raw, name.as_ptr().cast(), name.len());
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(result))
    }

    /// Compare and conditionally replace a memory value. The result is an LLVM
    /// struct containing the prior value and an `i1` success flag.
    pub fn cmpxchg(
        &self,
        ptr: &Value,
        expected: &Value,
        replacement: &Value,
        options: CmpXchgOptions,
        name: &str,
    ) -> Result<Value> {
        let block = self.insertion()?;
        let ptr = pointer_value(ptr, &block.function)?;
        let (expected, replacement) = same_type_operands(expected, replacement, &block.function)?;
        let name = c_text(name)?;
        check_alignment(options.alignment.map(u64::from))?;
        validate_cmpxchg_orderings(options.success, options.failure)?;
        let ty = expected.get_type();
        if !ty.is_int_type() && !ty.is_pointer_type() {
            return Err(Error::new(
                "cmpxchg requires scalar integer or pointer values",
            ));
        }
        atomic_access_size(ty, &block.function.module)?;
        // SAFETY: operands are live, the expected and replacement types match,
        // and the alignment, operand type and both orderings are valid.
        let result = unsafe {
            let raw = LLVMBuildAtomicCmpXchg(
                self.llvm().as_mut_ptr(),
                ptr.as_value_ref(),
                expected.as_value_ref(),
                replacement.as_value_ref(),
                options.success.into(),
                options.failure.into(),
                options.scope.single_thread(),
            );
            set_memory_flags(raw, options.alignment, options.volatile);
            LLVMSetWeak(raw, i32::from(options.weak));
            LLVMSetValueName2(raw, name.as_ptr().cast(), name.len());
            BasicValueEnum::new(raw)
        };
        Ok(block.function.local(result))
    }
}

impl MemoryOptions {
    fn for_access(mut self, ty: BasicTypeEnum<'_>, module: &Module) -> Result<Self> {
        check_alignment(self.alignment.map(u64::from))?;
        if self.atomic.is_some() {
            let size = validate_atomic_load_store_type(ty, module)?;
            self.alignment.get_or_insert(size);
        }
        Ok(self)
    }

    /// # Safety
    /// `raw` must be a live load or store with these options validated for its type.
    unsafe fn apply(self, raw: LLVMValueRef) {
        // SAFETY: setting the ordering first makes the memory access atomic
        // before setting its synchronization scope.
        unsafe {
            set_memory_flags(raw, self.alignment, self.volatile);
            if let Some(atomic) = self.atomic {
                LLVMSetOrdering(raw, atomic.ordering.into());
                LLVMSetAtomicSingleThread(raw, atomic.scope.single_thread());
            }
        }
    }
}

/// # Safety
/// `raw` must be a live load, store, atomicrmw or cmpxchg and `alignment` must
/// be valid for LLVM's alignment setter.
unsafe fn set_memory_flags(raw: LLVMValueRef, alignment: Option<u32>, volatile: bool) {
    // SAFETY: these LLVM setters accept each of the specified instruction kinds.
    unsafe {
        if let Some(alignment) = alignment {
            LLVMSetAlignment(raw, alignment);
        }
        LLVMSetVolatile(raw, i32::from(volatile));
    }
}

pub(super) fn check_alignment(alignment: Option<u64>) -> Result<()> {
    if let Some(alignment) = alignment
        && (!alignment.is_power_of_two() || alignment > (1 << 32))
    {
        return Err(Error::new(
            "LLVM alignment must be a nonzero power of two up to 2^32 bytes",
        ));
    }
    Ok(())
}

fn scalar_atomic_type(ty: BasicTypeEnum<'_>) -> bool {
    ty.is_int_type() || ty.is_float_type() || ty.is_pointer_type()
}

pub(crate) fn validate_memory_ordering(
    opcode: InstructionOpcode,
    ordering: Option<AtomicOrdering>,
) -> Result<()> {
    use AtomicOrdering::*;
    use InstructionOpcode::{AtomicRMW, Fence, Load, Store};

    let message = match (opcode, ordering) {
        (Load, Some(Release | AcquireRelease)) => {
            "atomic load cannot have release or acquire-release ordering"
        }
        (Store, Some(Acquire | AcquireRelease)) => {
            "atomic store cannot have acquire or acquire-release ordering"
        }
        (Fence, None | Some(Unordered | Monotonic)) => {
            "fence requires acquire, release, acquire-release or sequentially consistent ordering"
        }
        (AtomicRMW, None | Some(Unordered)) => "atomicrmw requires monotonic ordering or stronger",
        (Load | Store | Fence | AtomicRMW, _) => return Ok(()),
        _ => "LLVM instruction does not have a single memory ordering",
    };
    Err(Error::new(message))
}

pub(crate) fn validate_cmpxchg_orderings(
    success: AtomicOrdering,
    failure: AtomicOrdering,
) -> Result<()> {
    use AtomicOrdering::*;

    if success == Unordered {
        return Err(Error::new(
            "cmpxchg success requires monotonic ordering or stronger",
        ));
    }
    if !matches!(failure, Monotonic | Acquire | SequentiallyConsistent) {
        return Err(Error::new(
            "cmpxchg failure requires monotonic, acquire or sequentially consistent ordering",
        ));
    }
    Ok(())
}

pub(crate) fn validate_atomic_load_store_type(
    ty: BasicTypeEnum<'_>,
    module: &Module,
) -> Result<u32> {
    if !scalar_atomic_type(ty) {
        return Err(Error::new(
            "atomic load or store requires a scalar integer, floating-point or pointer type",
        ));
    }
    atomic_access_size(ty, module)
}

pub(crate) fn validate_atomic_rmw_operation(op: AtomicRmwOp, ty: BasicTypeEnum<'_>) -> Result<()> {
    use AtomicRmwOp::*;

    let message = match op {
        Xchg if scalar_atomic_type(ty) => return Ok(()),
        Xchg => "atomic exchange requires an integer, floating-point or pointer operand",
        FAdd | FSub | FMax | FMin | FMaximum | FMinimum => {
            let fixed_float_vector = matches!(ty, BasicTypeEnum::VectorType(vector) if vector.get_element_type().is_float_type());
            if ty.is_float_type() || fixed_float_vector {
                return Ok(());
            }
            "floating-point atomicrmw requires a floating-point scalar or fixed vector"
        }
        _ if ty.is_int_type() => return Ok(()),
        _ => "integer atomicrmw requires a scalar integer operand",
    };
    Err(Error::new(message))
}

fn atomic_access_size(ty: BasicTypeEnum<'_>, module: &Module) -> Result<u32> {
    let bits = module.with_llvm(|module| {
        // SAFETY: the module keeps its layout live and the checked atomic
        // operand type has a fixed size in that layout.
        unsafe {
            LLVMSizeOfTypeInBits(
                LLVMGetModuleDataLayout(module.as_mut_ptr()),
                ty.as_type_ref(),
            )
        }
    });
    if bits < 8 || !bits.is_power_of_two() {
        return Err(Error::new(
            "atomic memory access requires a power-of-two bit width of at least eight",
        ));
    }
    u32::try_from(bits / 8)
        .map_err(|_| Error::new("atomic memory access size exceeds LLVM alignment range"))
}

use inkwell::llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic;
use inkwell::llvm_sys::core::{
    LLVMGetAtomicSyncScopeID, LLVMGetCmpXchgFailureOrdering, LLVMGetCmpXchgSuccessOrdering,
    LLVMGetOperand, LLVMGetWeak, LLVMIsAtomic, LLVMSetAtomicRMWBinOp, LLVMSetAtomicSingleThread,
    LLVMSetCmpXchgFailureOrdering, LLVMSetCmpXchgSuccessOrdering, LLVMSetOrdering, LLVMSetWeak,
    LLVMTypeOf,
};
use inkwell::values::AsValueRef;
use inkwell::values::InstructionOpcode::{AtomicCmpXchg, AtomicRMW, Load, Store};

use super::Instruction;
use crate::builder::memory::{
    validate_atomic_load_store_type, validate_atomic_rmw_operation, validate_cmpxchg_orderings,
    validate_memory_ordering,
};
use crate::{AtomicOrdering, AtomicRmwOp, Error, Result, SyncScope};

impl Instruction {
    /// Return the ordering of an atomic load, store, fence, or atomicrmw.
    /// A cmpxchg has two orderings, available through `cmpxchg_orderings`.
    pub fn memory_ordering(&self) -> Option<AtomicOrdering> {
        let ordering = self.llvm().get_atomic_ordering().ok()?;
        AtomicOrdering::from_llvm(ordering.into())
    }

    /// Set the ordering of a load, store, fence, or atomicrmw. Pass `None` to
    /// make a load or store non-atomic and clear its synchronization scope.
    /// This preserves the instruction's type, alignment, and volatile flag.
    pub fn set_memory_ordering(&self, ordering: Option<AtomicOrdering>) -> Result<()> {
        let instruction = self.llvm();
        let opcode = instruction.get_opcode();
        validate_memory_ordering(opcode, ordering)?;

        let raw = instruction.as_value_ref();
        if ordering.is_some() && matches!(opcode, Load | Store) {
            let module = self.block().function().module();
            // SAFETY: stores have the stored value as operand zero. A load's
            // result type is its memory access type.
            let ty = unsafe {
                LLVMTypeOf(if opcode == Store {
                    LLVMGetOperand(raw, 0)
                } else {
                    raw
                })
            };
            let ty = module.context().type_ref(ty);
            validate_atomic_load_store_type(ty.basic()?, module)?;
        }

        // SAFETY: the opcode accepts the validated ordering and only a load
        // or store may become non-atomic. Clear its scope while still atomic.
        unsafe {
            if ordering.is_none() {
                LLVMSetAtomicSingleThread(raw, 0);
            }
            LLVMSetOrdering(
                raw,
                ordering.map_or(LLVMAtomicOrderingNotAtomic, Into::into),
            );
        }
        Ok(())
    }

    /// Return LLVM's synchronization scope ID for an atomic instruction.
    /// IDs 0 and 1 represent single-thread and system. Other IDs are local
    /// to the instruction's context.
    pub fn atomic_sync_scope_id(&self) -> Option<u32> {
        let raw = self.llvm().as_value_ref();
        // SAFETY: only atomic instructions reach LLVM's scope accessor.
        unsafe { (LLVMIsAtomic(raw) != 0).then(|| LLVMGetAtomicSyncScopeID(raw)) }
    }

    /// Set an atomic instruction's synchronization scope to an LLVM built-in.
    pub fn set_atomic_sync_scope(&self, scope: SyncScope) -> Result<()> {
        let raw = self.llvm().as_value_ref();
        // SAFETY: the instruction handle keeps the LLVM instruction alive.
        unsafe {
            if LLVMIsAtomic(raw) == 0 {
                return Err(Error::new(
                    "LLVM synchronization scope requires an atomic instruction",
                ));
            }
            LLVMSetAtomicSingleThread(raw, scope.single_thread());
        }
        Ok(())
    }

    /// Return the success and failure orderings of a cmpxchg, respectively.
    pub fn cmpxchg_orderings(&self) -> Option<(AtomicOrdering, AtomicOrdering)> {
        let instruction = self.llvm();
        if instruction.get_opcode() != AtomicCmpXchg {
            return None;
        }
        let raw = instruction.as_value_ref();
        // SAFETY: the checked opcode permits both cmpxchg ordering accessors.
        unsafe {
            Some((
                AtomicOrdering::from_llvm(LLVMGetCmpXchgSuccessOrdering(raw))?,
                AtomicOrdering::from_llvm(LLVMGetCmpXchgFailureOrdering(raw))?,
            ))
        }
    }

    /// Update both orderings of a cmpxchg after validating both arguments.
    pub fn set_cmpxchg_orderings(
        &self,
        success: AtomicOrdering,
        failure: AtomicOrdering,
    ) -> Result<()> {
        let instruction = self.llvm();
        if instruction.get_opcode() != AtomicCmpXchg {
            return Err(Error::new(
                "LLVM cmpxchg orderings require a cmpxchg instruction",
            ));
        }
        validate_cmpxchg_orderings(success, failure)?;
        let raw = instruction.as_value_ref();
        // SAFETY: this is a cmpxchg and LLVM allows both selected orderings.
        unsafe {
            LLVMSetCmpXchgSuccessOrdering(raw, success.into());
            LLVMSetCmpXchgFailureOrdering(raw, failure.into());
        }
        Ok(())
    }

    /// Return whether a cmpxchg can fail spuriously.
    pub fn cmpxchg_is_weak(&self) -> Option<bool> {
        let instruction = self.llvm();
        (instruction.get_opcode() == AtomicCmpXchg).then(|| {
            // SAFETY: the opcode predicate guards LLVM's cmpxchg-only accessor.
            unsafe { LLVMGetWeak(instruction.as_value_ref()) != 0 }
        })
    }

    /// Set whether a cmpxchg can fail spuriously.
    pub fn set_cmpxchg_weak(&self, weak: bool) -> Result<()> {
        let instruction = self.llvm();
        if instruction.get_opcode() != AtomicCmpXchg {
            return Err(Error::new("LLVM weak flag requires a cmpxchg instruction"));
        }
        // SAFETY: the opcode check guards LLVM's cmpxchg-only setter.
        unsafe { LLVMSetWeak(instruction.as_value_ref(), i32::from(weak)) };
        Ok(())
    }

    /// Return the operation of an atomicrmw.
    pub fn atomic_rmw_operation(&self) -> Option<AtomicRmwOp> {
        self.llvm().get_atomic_rmw_bin_op()
    }

    /// Change an atomicrmw operation while retaining its existing operand type.
    pub fn set_atomic_rmw_operation(&self, operation: AtomicRmwOp) -> Result<()> {
        let instruction = self.llvm();
        if instruction.get_opcode() != AtomicRMW {
            return Err(Error::new(
                "LLVM atomicrmw operation requires an atomicrmw instruction",
            ));
        }
        let raw = instruction.as_value_ref();
        let context = self.block().function().module().context();
        // SAFETY: this live atomicrmw's result and value operand have the same type.
        let ty = context.type_ref(unsafe { LLVMTypeOf(raw) });
        validate_atomic_rmw_operation(operation, ty.basic()?)?;
        // SAFETY: LLVM permits the selected operation on this operand type.
        unsafe { LLVMSetAtomicRMWBinOp(raw, operation.into()) };
        Ok(())
    }

    /// Set the volatile flag of a load, store, atomicrmw, or cmpxchg.
    pub fn set_volatile(&self, volatile: bool) -> Result<()> {
        self.llvm()
            .set_volatile(volatile)
            .map_err(|error| Error::new(error.to_string()))
    }
}

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use inkwell::llvm_sys::orc2::{
    LLVMOrcJITDylibCreateResourceTracker, LLVMOrcReleaseResourceTracker, LLVMOrcResourceTrackerRef,
    LLVMOrcResourceTrackerRemove,
};
use inkwell::llvm_sys::prelude::{LLVMContextRef, LLVMMemoryBufferRef};
use object::{BinaryFormat, Object, ObjectSymbol};

use super::Dylib;
use super::ir::IrContext;
use crate::{Error, Result, llvm_result};

pub(super) struct Submission {
    pub(super) tracker: Option<LLVMOrcResourceTrackerRef>,
    pub(super) definitions: HashSet<Vec<u8>>,
    pub(super) imports: HashSet<Vec<u8>>,
    pub(super) materialization: Materialization,
}

pub(super) enum Materialization {
    // Only used as an identity in the object transform, never dereferenced.
    PendingObject(LLVMMemoryBufferRef),
    // Retain the context until code generation passes its object to the linker.
    PendingIr(IrContext),
    Started,
}

impl Submission {
    pub(super) fn ir(context: IrContext, tracker: Option<LLVMOrcResourceTrackerRef>) -> Self {
        Self {
            tracker,
            definitions: HashSet::new(),
            imports: HashSet::new(),
            materialization: Materialization::PendingIr(context),
        }
    }

    pub(super) fn read(bytes: &[u8], tracker: Option<LLVMOrcResourceTrackerRef>) -> Result<Self> {
        let object = object::File::parse(bytes).map_err(|error| {
            Error::new(format!(
                "LLVM JIT could not read the relocatable object: {error}"
            ))
        })?;
        let mut submission = Self {
            tracker,
            definitions: HashSet::new(),
            imports: HashSet::new(),
            materialization: Materialization::Started,
        };
        for symbol in object.symbols() {
            let name = symbol.name_bytes().map_err(|error| {
                Error::new(format!("LLVM JIT could not read an object symbol: {error}"))
            })?;
            if name.is_empty() {
                continue;
            }
            if tracker.is_some()
                && !symbol.is_undefined()
                && (symbol.is_global() || symbol.is_weak() || symbol.is_common())
            {
                submission.definitions.insert(name.to_vec());
            }
            // Weak and common symbols may bind to another object's definition.
            if symbol.is_undefined() || symbol.is_weak() || symbol.is_common() {
                submission.imports.insert(name.to_vec());
                if object.format() == BinaryFormat::Coff
                    && let Some(plain) = name.strip_prefix(b"__imp_")
                {
                    submission.imports.insert(plain.to_vec());
                }
            }
        }
        Ok(submission)
    }
}

#[derive(Default)]
pub(super) struct Tracking {
    pub(super) submissions: Vec<Submission>,
    groups: HashMap<LLVMOrcResourceTrackerRef, Rc<()>>,
}

impl Tracking {
    pub(super) fn pending_ir(&mut self, context: LLVMContextRef) -> Option<&mut Submission> {
        self.submissions.iter_mut().find(|unit| {
            matches!(&unit.materialization, Materialization::PendingIr(ir) if ir.llvm == context)
        })
    }

    pub(super) fn begin_object_link(&mut self, buffer: LLVMMemoryBufferRef) {
        // ORC can discard a weak unit before linking it. If LLVM reuses its
        // buffer address, the last submission with that address owns the new buffer.
        if let Some(unit) = self.submissions.iter_mut().rev().find(|unit| {
            matches!(unit.materialization, Materialization::PendingObject(raw) if raw == buffer)
        }) {
            unit.materialization = Materialization::Started;
        }
    }

    pub(super) fn leases(&self, name: &[u8]) -> Vec<Rc<()>> {
        self.submissions
            .iter()
            .filter(|submission| submission.definitions.contains(name))
            .filter_map(|submission| self.groups.get(&submission.tracker?))
            .cloned()
            .collect()
    }

    fn ensure_removable(
        &self,
        tracker: LLVMOrcResourceTrackerRef,
        releasing_leases: &[Rc<()>],
    ) -> Result<()> {
        let group = &self.groups[&tracker];
        let releasing = releasing_leases
            .iter()
            .filter(|lease| Rc::ptr_eq(group, lease))
            .count();
        if Rc::strong_count(group) > 1 + releasing {
            return Err(Error::new("ORC resource tracker has live JIT symbols"));
        }
        let definitions = self
            .submissions
            .iter()
            .filter(|submission| submission.tracker == Some(tracker))
            .flat_map(|submission| submission.definitions.iter());
        for name in definitions {
            if self.submissions.iter().any(|other| {
                other.tracker != Some(tracker)
                    && matches!(other.materialization, Materialization::Started)
                    && other.imports.contains(name)
            }) {
                return Err(Error::new(
                    "ORC resource tracker is referenced by submitted JIT code",
                ));
            }
        }
        Ok(())
    }
}

/// A group of ORC submissions in one JITDylib. Dropping the handle keeps its
/// code in the dylib. Call [`Self::remove`] to unload the group explicitly.
pub struct ResourceTracker {
    inner: RefCell<Option<LiveTracker>>,
}

struct LiveTracker {
    raw: LLVMOrcResourceTrackerRef,
    dylib: Rc<Dylib>,
}

impl ResourceTracker {
    pub(super) fn new(dylib: Rc<Dylib>) -> Self {
        // SAFETY: this retained dylib is live. We own one tracker reference.
        let raw = unsafe { LLVMOrcJITDylibCreateResourceTracker(dylib.raw) };
        dylib
            .session
            .tracking
            .borrow_mut()
            .groups
            .insert(raw, Rc::new(()));
        Self {
            inner: RefCell::new(Some(LiveTracker { raw, dylib })),
        }
    }

    pub(super) fn for_dylib(&self, dylib: &Rc<Dylib>) -> Result<LLVMOrcResourceTrackerRef> {
        let inner = self.inner.borrow();
        let live = inner
            .as_ref()
            .ok_or_else(|| Error::new("ORC resource tracker has been removed"))?;
        if !Rc::ptr_eq(dylib, &live.dylib) {
            return Err(Error::new(
                "ORC resource tracker belongs to a different JITDylib",
            ));
        }
        Ok(live.raw)
    }

    /// Unload this group so its names can be submitted again. A successful
    /// removal is terminal. Repeating it has no effect.
    ///
    /// Live lookup symbols, imports resolved by children, and materializing
    /// or linked objects that reference this group prevent removal.
    /// Pending submissions bind to available definitions when they materialize.
    pub fn remove(&self) -> Result<()> {
        self.remove_with_leases(&[])?
    }

    /// The outer error leaves the tracker live. Once ORC starts removal, the
    /// tracker is defunct even if a resource manager reports a cleanup error.
    pub(super) fn remove_with_leases(&self, releasing_leases: &[Rc<()>]) -> Result<Result<()>> {
        {
            let inner = self.inner.borrow();
            let Some(live) = inner.as_ref() else {
                return Ok(Ok(()));
            };
            let mut tracking = live.dylib.session.tracking.borrow_mut();
            tracking.ensure_removable(live.raw, releasing_leases)?;
            tracking.groups.remove(&live.raw);
            tracking
                .submissions
                .retain(|unit| unit.tracker != Some(live.raw));
        }
        let live = self.inner.take().expect("live resource tracker");
        // SAFETY: only the symbol being released may still own a lease, and no
        // dependent submission can reference this group.
        unsafe {
            let result = llvm_result(LLVMOrcResourceTrackerRemove(live.raw));
            LLVMOrcReleaseResourceTracker(live.raw);
            Ok(result)
        }
    }
}

impl Drop for ResourceTracker {
    fn drop(&mut self) {
        if let Some(live) = self.inner.get_mut().take() {
            {
                let mut tracking = live.dylib.session.tracking.borrow_mut();
                tracking.groups.remove(&live.raw);
                for submission in &mut tracking.submissions {
                    if submission.tracker == Some(live.raw) {
                        submission.tracker = None;
                        submission.definitions.clear();
                    }
                }
            }
            // SAFETY: release our one tracker reference while its dylib is
            // alive. ORC transfers remaining resources to the default group.
            unsafe { LLVMOrcReleaseResourceTracker(live.raw) };
        }
    }
}

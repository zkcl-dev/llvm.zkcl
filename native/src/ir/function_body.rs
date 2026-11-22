use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    ffi::{CStr, CString},
    ptr,
    rc::Rc,
};

use inkwell::llvm_sys::comdat::{LLVMGetComdat, LLVMSetComdat};
use inkwell::llvm_sys::core::*;
use inkwell::llvm_sys::debuginfo::{LLVMGetSubprogram, LLVMSetSubprogram};
use inkwell::llvm_sys::prelude::{LLVMAttributeRef, LLVMComdatRef, LLVMMetadataRef, LLVMValueRef};
use inkwell::llvm_sys::{LLVMDLLStorageClass, LLVMLinkage, LLVMUnnamedAddr, LLVMVisibility};
use inkwell::types::AsTypeRef;

use super::{Block, Function, Linkage, Module, Type};
use crate::{Error, Result};

#[derive(Default)]
pub(super) struct FunctionBodies {
    active: RefCell<HashMap<LLVMValueRef, BodyGeneration>>,
    // A new symbol survives failure as an external declaration. Its requested
    // definition linkage may still be applied when a definition is retried.
    retry_linkage: RefCell<HashMap<LLVMValueRef, Linkage>>,
}

impl FunctionBodies {
    pub(super) fn active(&self, function: LLVMValueRef) -> Option<BodyGeneration> {
        self.active.borrow().get(&function).cloned()
    }

    pub(super) fn is_pending(&self) -> bool {
        !self.active.borrow().is_empty()
    }

    fn complete(&self, function: LLVMValueRef) {
        self.active.borrow_mut().remove(&function);
        self.retry_linkage.borrow_mut().remove(&function);
    }

    pub(super) fn local_dependencies(&self, raw: LLVMValueRef) -> Vec<BodyGeneration> {
        if !self.is_pending() {
            return Vec::new();
        }
        // SAFETY: `raw` is live. Each parent accessor is guarded by its kind.
        let block = unsafe {
            if !LLVMIsAInstruction(raw).is_null() {
                LLVMGetInstructionParent(raw)
            } else if !LLVMIsABasicBlock(raw).is_null() {
                LLVMValueAsBasicBlock(raw)
            } else {
                return Vec::new();
            }
        };
        if block.is_null() {
            return Vec::new();
        }
        // SAFETY: a non-null block obtained above is live.
        self.active(unsafe { LLVMGetBasicBlockParent(block) })
            .into_iter()
            .collect()
    }

    pub(super) fn constant_dependencies(&self, raw: LLVMValueRef) -> Vec<BodyGeneration> {
        let active = self.active.borrow();
        if active.is_empty() {
            return Vec::new();
        }
        let mut matching_functions = HashSet::new();
        let mut seen = HashSet::new();
        let mut pending = vec![raw];
        // SAFETY: the caller retains the live constant. We only follow operands
        // of compound constants, stopping at global symbols.
        unsafe {
            while let Some(raw) = pending.pop() {
                if !seen.insert(raw) {
                    continue;
                }
                if !LLVMIsABlockAddress(raw).is_null() {
                    matching_functions.insert(LLVMGetBlockAddressFunction(raw));
                } else if LLVMIsAGlobalValue(raw).is_null() && !LLVMIsAConstant(raw).is_null() {
                    pending.extend((0..LLVMGetNumOperands(raw) as u32).filter_map(|index| {
                        let operand = LLVMGetOperand(raw, index);
                        (!operand.is_null()).then_some(operand)
                    }));
                }
            }
        }
        matching_functions
            .into_iter()
            .filter_map(|function| active.get(&function).cloned())
            .collect()
    }
}

#[derive(Clone)]
pub(crate) struct BodyGeneration(Rc<BodyStatus>);

struct BodyStatus {
    function: LLVMValueRef,
    phase: Cell<BodyPhase>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BodyPhase {
    Pending,
    Committed,
    Aborted,
}

impl BodyGeneration {
    pub(crate) fn check_live(&self) -> Result<()> {
        if self.0.phase.get() == BodyPhase::Aborted {
            return Err(Error::new(
                "LLVM object belongs to an abandoned function body",
            ));
        }
        Ok(())
    }

    pub(super) fn is_pending(&self) -> bool {
        self.0.phase.get() == BodyPhase::Pending
    }

    pub(crate) fn permits(&self, function: &Function) -> bool {
        !self.is_pending() || function.raw == self.0.function
    }
}

/// Owns a function construction attempt for the native Lisp entry points.
/// Dropping an unfinished attempt restores the declaration.
pub(crate) struct FunctionBody {
    function: Function,
    status: BodyGeneration,
    declaration: Declaration,
}

impl FunctionBody {
    pub(crate) fn begin(function: &Function) -> Result<(Self, Block)> {
        if function.first_block().is_some() {
            return Err(Error::new(format!(
                "LLVM function '{}' already has a body",
                function.name()?
            )));
        }
        if function.llvm().get_intrinsic_id() != 0 {
            return Err(Error::new("LLVM intrinsics cannot have a body"));
        }
        let status = BodyGeneration(Rc::new(BodyStatus {
            function: function.raw,
            phase: Cell::new(BodyPhase::Pending),
        }));
        let declaration = Declaration::capture(function.raw);
        function
            .module
            .0
            .bodies
            .active
            .borrow_mut()
            .insert(function.raw, status.clone());
        let body = Self {
            function: function.clone(),
            status,
            declaration,
        };
        let block = function.append_block("entry")?;
        Ok((body, block))
    }

    pub(crate) fn begin_named(
        module: &Module,
        name: &str,
        ty: &Type,
        linkage: Option<Linkage>,
    ) -> Result<(Self, Block)> {
        let requested = linkage.map(Linkage::for_function).transpose()?;
        if linkage == Some(Linkage::ExternalWeak) {
            return Err(Error::new(
                "external weak LLVM linkage requires a declaration",
            ));
        }
        // Use the exact-symbol declaration checks before doing any emission.
        let existing = module.find_function(name)?;
        let function = module.declare_function(name, ty, None)?;
        let retries = &module.0.bodies.retry_linkage;
        if existing.is_none() {
            if let Some(linkage) = linkage
                && linkage != Linkage::External
            {
                retries.borrow_mut().insert(function.raw, linkage);
            }
        } else if let Some(linkage) = linkage
            && linkage != function.linkage()
            && retries.borrow().get(&function.raw) != Some(&linkage)
        {
            return Err(Error::new(format!(
                "LLVM function '{name}' has different linkage"
            )));
        }
        let requested = requested.or_else(|| {
            retries
                .borrow()
                .get(&function.raw)
                .and_then(|linkage| linkage.for_function().ok())
        });
        let (body, block) = Self::begin(&function)?;
        if let Some(linkage) = requested {
            function.llvm().set_linkage(linkage);
        }
        Ok((body, block))
    }

    pub(crate) fn function(&self) -> &Function {
        &self.function
    }

    pub(crate) fn verify(&self) -> Result<()> {
        for block in self.function.blocks() {
            if !block.is_terminated() {
                return Err(Error::new(format!(
                    "LLVM block '{}' in function '{}' has no terminator",
                    block.name()?,
                    self.function.name()?,
                )));
            }
        }
        if !self.function.llvm().verify(false) {
            return Err(Error::new(format!(
                "LLVM function '{}' failed verification",
                self.function.name()?,
            )));
        }
        Ok(())
    }

    pub(crate) fn commit(self) {
        self.status.0.phase.set(BodyPhase::Committed);
        self.function.module.0.bodies.complete(self.function.raw);
    }
}

impl Drop for FunctionBody {
    fn drop(&mut self) {
        if !self.status.is_pending() {
            return;
        }
        self.status.0.phase.set(BodyPhase::Aborted);
        let function = &self.function;
        function
            .module
            .0
            .bodies
            .active
            .borrow_mut()
            .remove(&function.raw);
        // LLVM deletes a function body in two passes to handle references
        // between blocks. Let LLVM do that on a temporary function, leaving
        // the original function, parameters, and all uses of its symbol live.
        // SAFETY: every block is from this abandoned body. Pending block
        // addresses cannot have published users. Rust marks all body handles
        // stale before transferring or deleting any raw references.
        function.module.with_llvm(|module| unsafe {
            let mut block = LLVMGetFirstBasicBlock(function.raw);
            if block.is_null() {
                return;
            }
            let retired = LLVMAddFunction(
                module.as_mut_ptr(),
                c"".as_ptr(),
                function.llvm().get_type().as_type_ref(),
            );
            while !block.is_null() {
                let next = LLVMGetNextBasicBlock(block);
                LLVMRemoveBasicBlockFromParent(block);
                LLVMAppendExistingBasicBlock(retired, block);
                block = next;
            }
            LLVMDeleteFunction(retired);
        });
        self.declaration.restore(function.raw);
    }
}

struct Declaration {
    linkage: LLVMLinkage,
    visibility: LLVMVisibility,
    dll_storage: LLVMDLLStorageClass,
    unnamed_address: LLVMUnnamedAddr,
    section: CString,
    comdat: LLVMComdatRef,
    calling_convention: u32,
    parameter_names: Vec<Vec<u8>>,
    attributes: Vec<(u32, Vec<LLVMAttributeRef>)>,
    personality: LLVMValueRef,
    subprogram: LLVMMetadataRef,
}

impl Declaration {
    fn capture(function: LLVMValueRef) -> Self {
        // SAFETY: the function is live and each argument index is in range.
        // Attributes and subprograms are owned by the context.
        unsafe {
            let count = LLVMCountParams(function);
            let parameter_names = (0..count)
                .map(|index| {
                    let mut length = 0;
                    let raw = LLVMGetValueName2(LLVMGetParam(function, index), &mut length);
                    if length == 0 {
                        Vec::new()
                    } else {
                        std::slice::from_raw_parts(raw.cast(), length).to_vec()
                    }
                })
                .collect();
            let attributes = (0..=count)
                .chain(std::iter::once(u32::MAX))
                .filter_map(|index| {
                    let attributes = attributes_at(function, index);
                    (!attributes.is_empty()).then_some((index, attributes))
                })
                .collect();
            let section = LLVMGetSection(function);
            Self {
                linkage: LLVMGetLinkage(function),
                visibility: LLVMGetVisibility(function),
                dll_storage: LLVMGetDLLStorageClass(function),
                unnamed_address: LLVMGetUnnamedAddress(function),
                section: if section.is_null() {
                    c"".to_owned()
                } else {
                    CStr::from_ptr(section).to_owned()
                },
                comdat: LLVMGetComdat(function),
                calling_convention: LLVMGetFunctionCallConv(function),
                parameter_names,
                attributes,
                personality: if LLVMHasPersonalityFn(function) != 0 {
                    LLVMGetPersonalityFn(function)
                } else {
                    ptr::null_mut()
                },
                subprogram: LLVMGetSubprogram(function),
            }
        }
    }

    fn restore(&self, function: LLVMValueRef) {
        // SAFETY: this is the same live function and its unchanged parameter
        // list. Captured attributes and metadata retain their context owner.
        unsafe {
            LLVMSetLinkage(function, self.linkage);
            LLVMSetVisibility(function, self.visibility);
            LLVMSetDLLStorageClass(function, self.dll_storage);
            LLVMSetUnnamedAddress(function, self.unnamed_address);
            LLVMSetSection(function, self.section.as_ptr());
            LLVMSetComdat(function, self.comdat);
            LLVMSetFunctionCallConv(function, self.calling_convention);
            LLVMSetPersonalityFn(function, self.personality);
            LLVMSetSubprogram(function, self.subprogram);
            // Release all temporary names before restoring any original name.
            // This also handles parameter renames that swapped their names.
            for index in 0..self.parameter_names.len() {
                LLVMSetValueName2(LLVMGetParam(function, index as u32), c"".as_ptr(), 0);
            }
            for (index, name) in self.parameter_names.iter().enumerate() {
                if !name.is_empty() {
                    LLVMSetValueName2(
                        LLVMGetParam(function, index as u32),
                        name.as_ptr().cast(),
                        name.len(),
                    );
                }
            }
            let count = LLVMCountParams(function);
            for index in (0..=count).chain(std::iter::once(u32::MAX)) {
                for attribute in attributes_at(function, index) {
                    if LLVMIsStringAttribute(attribute) != 0 {
                        let mut len = 0;
                        let name = LLVMGetStringAttributeKind(attribute, &mut len);
                        LLVMRemoveStringAttributeAtIndex(function, index, name, len);
                    } else {
                        let kind = LLVMGetEnumAttributeKind(attribute);
                        LLVMRemoveEnumAttributeAtIndex(function, index, kind);
                    }
                }
            }
            for (index, originals) in &self.attributes {
                for &attribute in originals {
                    LLVMAddAttributeAtIndex(function, *index, attribute);
                }
            }
        }
    }
}

/// `function` is live and `index` is an LLVM attribute index for it.
unsafe fn attributes_at(function: LLVMValueRef, index: u32) -> Vec<LLVMAttributeRef> {
    // SAFETY: LLVM writes the count it reports to the allocated buffer.
    unsafe {
        let count = LLVMGetAttributeCountAtIndex(function, index);
        let mut values = vec![ptr::null_mut(); count as usize];
        if count != 0 {
            LLVMGetAttributesAtIndex(function, index, values.as_mut_ptr());
        }
        values
    }
}

#[cfg(test)]
mod tests {
    use crate::{Builder, Context, IntOp};

    #[test]
    fn failed_verification_restores_the_declaration() -> crate::Result<()> {
        let context = Context::default();
        let module = context.module("rollback")?;
        let i32 = context.int_type(32)?;
        let signature = i32.function(std::slice::from_ref(&i32), false)?;
        let function = module.declare_function("step", &signature, None)?;
        let parameter = function.parameter(0)?.expect("one parameter");
        parameter.set_name("original")?;
        let builder = Builder::begin_function_body(&function)?;
        let entry = function.first_block().expect("entry block");
        let target = function.append_block("target")?;
        let address = target.address()?;
        let aggregate = address.ty().const_array(std::slice::from_ref(&address))?;
        let plus = builder.int_binop(IntOp::Add, &parameter, &parameter, "double")?;
        let escaped_builder = Builder::new(&context);
        escaped_builder.position_before(&plus.as_instruction().expect("integer instruction"))?;
        builder.indirect_br(&address, &[target])?;
        assert!(builder.finish_function_body().is_err());
        assert!(module.bitcode().is_err());
        parameter.set_name("attempt")?;
        drop(builder);
        assert!(function.is_declaration());
        assert!(entry.check_live().is_err());
        assert!(plus.check_live().is_err());
        assert!(address.check_live().is_err());
        assert!(aggregate.check_live().is_err());
        assert!(escaped_builder.ret(Some(&parameter)).is_err());
        assert_eq!(parameter.name()?, "original");
        assert!(parameter.same_as(&function.parameter(0)?.expect("same parameter")));
        module.verify()?;

        let retry = Builder::begin_function_body(&function)?;
        retry.ret(Some(&parameter))?;
        retry.finish_function_body()?;
        assert!(plus.check_live().is_err());
        module.verify()
    }
}

use std::{collections::HashSet, ffi::CString};

use inkwell::llvm_sys::core::{
    LLVMAddAlias2, LLVMAliasGetAliasee, LLVMAliasSetAliasee, LLVMGetFirstGlobalAlias,
    LLVMGetNamedGlobalAlias, LLVMGetNextGlobalAlias, LLVMGetNumOperands, LLVMGetOperand,
    LLVMGetPointerAddressSpace, LLVMGlobalGetValueType, LLVMIsAConstant, LLVMIsAConstantExpr,
    LLVMIsAGlobalAlias, LLVMIsAGlobalValue, LLVMTypeOf,
};
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::types::AsTypeRef;
use inkwell::values::GlobalValue;

use super::global::prints_dso_local;
use super::{
    GlobalVisibility, Linkage, Module, Type, TypeKind, Value, named_symbol, set_value_name,
    value_name,
};
use crate::{Error, Result, c_text};

/// A global alias retaining the module that owns the symbol and its target.
#[derive(Clone)]
pub struct GlobalAlias {
    raw: LLVMValueRef,
    module: Module,
}

impl Module {
    /// Define an exact symbol whose address is given by a constant LLVM pointer.
    /// The declared type describes the alias, and its address space comes from the target.
    pub fn add_global_alias(
        &self,
        name: &str,
        value_type: &Type,
        target: &Value,
        linkage: Linkage,
    ) -> Result<GlobalAlias> {
        let name = alias_name(name)?;
        let ty = if value_type.kind() == TypeKind::LLVMFunctionTypeKind {
            value_type.function_in(self.context())?.as_type_ref()
        } else {
            value_type.basic_in(self.context())?.as_type_ref()
        };
        if matches!(
            linkage,
            Linkage::Appending | Linkage::ExternalWeak | Linkage::Common
        ) {
            return Err(Error::new(
                "this LLVM linkage is unavailable for global aliases",
            ));
        }
        let address_space = target.ty().pointer_address_space()?;
        check_target(self, target, linkage, None)?;
        let c_name = CString::new(name).expect("checked alias name");

        self.with_llvm(|module| {
            if !named_symbol(module, name).is_null() {
                return Err(symbol_in_use(name));
            }
            // SAFETY: the type and target are live in the module's context,
            // the target is a constant with this pointer address space, and
            // the alias uses an available name and valid linkage.
            let raw = unsafe {
                let raw = LLVMAddAlias2(
                    module.as_mut_ptr(),
                    ty,
                    address_space,
                    target.raw,
                    c_name.as_ptr(),
                );
                GlobalValue::new(raw).set_linkage(linkage.into());
                raw
            };
            Ok(GlobalAlias {
                raw,
                module: self.clone(),
            })
        })
    }

    pub fn find_global_alias(&self, name: &str) -> Result<Option<GlobalAlias>> {
        let name = c_text(name)?;
        // SAFETY: LLVM reads the name bytes for exactly the supplied length.
        Ok(self.with_llvm(|module| {
            self.global_alias(unsafe {
                LLVMGetNamedGlobalAlias(module.as_mut_ptr(), name.as_ptr().cast(), name.len())
            })
        }))
    }

    pub fn global_aliases(&self) -> impl Iterator<Item = GlobalAlias> {
        std::iter::successors(self.first_global_alias(), GlobalAlias::next_global_alias)
    }

    pub(crate) fn first_global_alias(&self) -> Option<GlobalAlias> {
        // SAFETY: the module remains live during iteration.
        self.with_llvm(|module| {
            self.global_alias(unsafe { LLVMGetFirstGlobalAlias(module.as_mut_ptr()) })
        })
    }

    pub(super) fn global_alias(&self, raw: LLVMValueRef) -> Option<GlobalAlias> {
        (!raw.is_null()).then(|| GlobalAlias {
            raw,
            module: self.clone(),
        })
    }
}

impl GlobalAlias {
    pub fn module(&self) -> &Module {
        &self.module
    }

    pub fn same_as(&self, other: &Self) -> bool {
        self.raw == other.raw
    }

    pub fn name(&self) -> Result<String> {
        value_name(self.raw)
    }

    /// Rename this alias to an exact symbol name.
    pub fn set_name(&self, name: &str) -> Result<()> {
        let name = alias_name(name)?;
        let existing = self.module.with_llvm(|module| named_symbol(module, name));
        if !existing.is_null() && existing != self.raw {
            return Err(symbol_in_use(name));
        }
        // SAFETY: the retained module owns the alias.
        unsafe { set_value_name(self.raw, name) }
    }

    pub fn linkage(&self) -> Linkage {
        // SAFETY: aliases are LLVM global values owned by the retained module.
        Linkage::from_llvm(unsafe { GlobalValue::new(self.raw) }.get_linkage())
    }

    /// Return the declared type of the aliased data or function.
    pub fn value_type(&self) -> Type {
        // SAFETY: the retained module owns the alias and its context owns its type.
        self.module
            .context()
            .type_ref(unsafe { LLVMGlobalGetValueType(self.raw) })
    }

    pub fn address_space(&self) -> u32 {
        // SAFETY: a live LLVM global alias always has a pointer type.
        unsafe { LLVMGetPointerAddressSpace(LLVMTypeOf(self.raw)) }
    }

    /// Return a module-owned pointer to the alias symbol for use in IR.
    pub fn address(&self) -> Value {
        Value::constant(self.raw, self.module.context(), Some(&self.module))
    }

    /// Return the immediate aliasee, preserving constant expressions and alias chains.
    pub fn target(&self) -> Value {
        // SAFETY: every alias has an aliasee and its raw pointer stays live
        // while the module remains live, including after retargeting.
        let raw = unsafe { LLVMAliasGetAliasee(self.raw) };
        Value::constant(raw, self.module.context(), Some(&self.module))
    }

    pub(crate) fn next_global_alias(&self) -> Option<Self> {
        // SAFETY: this is a live member of its module's alias list.
        self.module
            .global_alias(unsafe { LLVMGetNextGlobalAlias(self.raw) })
    }

    /// Retarget the existing symbol while preserving its pointer type and linkage.
    pub fn set_target(&self, target: &Value) -> Result<()> {
        check_target(&self.module, target, self.linkage(), Some(self.raw))?;
        // SAFETY: both values are live in the same context and the aliasee
        // is a constant with the alias's pointer type.
        unsafe { LLVMAliasSetAliasee(self.raw, target.raw) };
        Ok(())
    }
}

fn alias_name(name: &str) -> Result<&str> {
    let name = super::ordinary_symbol_name(name)?;
    if name.is_empty() {
        return Err(Error::new("an LLVM global alias needs a name"));
    }
    Ok(name)
}

fn symbol_in_use(name: &str) -> Error {
    Error::new(format!("LLVM symbol '{name}' is already in use"))
}

fn check_target(
    module: &Module,
    target: &Value,
    linkage: Linkage,
    replacing: Option<LLVMValueRef>,
) -> Result<()> {
    target.check_constant_publication()?;
    let (_, owner) = target.constant_in(module.context())?;
    if let Some(owner) = owner {
        owner.check(module)?;
    }

    // SAFETY: the initial target and every operand traversed are live. All
    // Constant instances are LLVM Users. Global definitions' initializers and
    // function bodies are outside the expression defining the aliasee.
    unsafe {
        if let Some(alias) = replacing
            && LLVMTypeOf(alias) != LLVMTypeOf(target.raw)
        {
            return Err(Error::new("LLVM alias and target pointer types must match"));
        }
        if LLVMIsAGlobalValue(target.raw).is_null() && LLVMIsAConstantExpr(target.raw).is_null() {
            return Err(Error::new(
                "an LLVM alias target must be a global value or constant expression",
            ));
        }
        let mut seen = HashSet::new();
        let mut pending = vec![target.raw];
        while let Some(value) = pending.pop() {
            if Some(value) == replacing {
                return Err(Error::new("LLVM aliases cannot form a cycle"));
            }
            if !seen.insert(value) {
                continue;
            }
            if LLVMIsAGlobalValue(value).is_null() {
                if linkage == Linkage::AvailableExternally {
                    return Err(Error::new(
                        "an available_externally LLVM alias needs an available_externally global target",
                    ));
                }
            } else {
                let symbol = GlobalValue::new(value);
                let symbol_linkage = Linkage::from_llvm(symbol.get_linkage());
                if linkage == Linkage::AvailableExternally {
                    if symbol_linkage != Linkage::AvailableExternally {
                        return Err(Error::new(
                            "an available_externally LLVM alias needs an available_externally global target",
                        ));
                    }
                } else if symbol.is_declaration() || symbol_linkage == Linkage::AvailableExternally
                {
                    return Err(Error::new(
                        "an LLVM alias target must point to a definition",
                    ));
                }
                if LLVMIsAGlobalAlias(value).is_null() {
                    continue;
                }
                if is_interposable_alias(module, symbol, symbol_linkage)? {
                    return Err(Error::new(
                        "an LLVM alias cannot point to an interposable alias",
                    ));
                }
            }
            pending.extend(
                (0..LLVMGetNumOperands(value) as u32)
                    .map(|index| LLVMGetOperand(value, index))
                    .filter(|operand| !LLVMIsAConstant(*operand).is_null()),
            );
        }
    }
    Ok(())
}

fn is_interposable_alias(
    module: &Module,
    alias: GlobalValue<'_>,
    linkage: Linkage,
) -> Result<bool> {
    if matches!(linkage, Linkage::WeakAny | Linkage::LinkOnceAny) {
        return Ok(true);
    }
    if linkage.is_local() || alias.get_visibility() != GlobalVisibility::Default {
        return Ok(false);
    }
    let semantic_interposition = module
        .module_flag("SemanticInterposition")?
        .and_then(|(_, value)| value.integer())
        .is_some_and(|value| value != 0.into());
    Ok(semantic_interposition && !prints_dso_local(alias))
}

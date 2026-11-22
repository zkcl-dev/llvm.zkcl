use std::{ffi::CString, ptr, slice, str};

use inkwell::context::AsContextRef;
use inkwell::module::Module as LlvmModule;
use inkwell::types::{AsTypeRef, FunctionType};
use inkwell::values::FunctionValue;
use llvm_sys::core::{
    LLVMAddFunction, LLVMDisposeMessage, LLVMGetIntrinsicID, LLVMIntrinsicCopyOverloadedName2,
    LLVMIntrinsicGetName, LLVMIntrinsicGetType, LLVMIntrinsicIsOverloaded, LLVMIsAFunction,
    LLVMLookupIntrinsicID,
};
use llvm_sys::prelude::LLVMTypeRef;

use super::{Context, Function, Module, Type, named_symbol};
use crate::{Error, Result, c_text};

impl Module {
    /// Declare an LLVM intrinsic by its base name and complete function type.
    /// LLVM determines the overloaded name and installs its attributes.
    pub fn declare_intrinsic(&self, name: &str, signature: &Type) -> Result<Function> {
        let name = c_text(name)?;
        let function_type = signature.function_in(self.context())?;
        let id = base_intrinsic_id(name)?;
        // SAFETY: the ID was returned by LLVM and verified as a base name.
        let symbol = if unsafe { LLVMIntrinsicIsOverloaded(id) } != 0 {
            resolve_overloaded_name(self.context(), name, id, signature, function_type)?
        } else {
            // SAFETY: non-overloaded intrinsics take no type parameters.
            let expected = unsafe {
                LLVMIntrinsicGetType(self.context().llvm().as_ctx_ref(), id, ptr::null_mut(), 0)
            };
            if expected != function_type.as_type_ref() {
                return Err(signature_mismatch(name));
            }
            name.to_owned()
        };
        self.with_llvm(|module| self.get_or_declare_intrinsic(module, &symbol, function_type))
    }

    /// Declare an intrinsic whose overload types are already known by the builder.
    ///
    /// # Safety
    /// `overloads` must contain the exact overload types expected by this
    /// intrinsic, in order, and all must belong to this module's context.
    pub(crate) unsafe fn declare_intrinsic_with_overloads(
        &self,
        name: &str,
        overloads: &mut [LLVMTypeRef],
    ) -> Result<Function> {
        let id = base_intrinsic_id(name)?;
        self.with_llvm(|module| {
            let mut length = 0;
            // SAFETY: the caller has validated the overload list and context.
            let symbol = unsafe {
                LLVMIntrinsicCopyOverloadedName2(
                    module.as_mut_ptr(),
                    id,
                    overloads.as_mut_ptr(),
                    overloads.len(),
                    &mut length,
                )
            };
            // SAFETY: LLVM allocated `length` readable bytes for this symbol name.
            let bytes = unsafe { slice::from_raw_parts(symbol.cast(), length) };
            let result = str::from_utf8(bytes)
                .map_err(|_| Error::new("LLVM intrinsic name is not UTF-8"))
                .and_then(|symbol| {
                    // SAFETY: the complete validated overload list determines the function type.
                    let function_type = unsafe {
                        FunctionType::new(LLVMIntrinsicGetType(
                            self.context().llvm().as_ctx_ref(),
                            id,
                            overloads.as_mut_ptr(),
                            overloads.len(),
                        ))
                    };
                    self.get_or_declare_intrinsic(module, symbol, function_type)
                });
            // SAFETY: ownership of this message was transferred by LLVM.
            unsafe { LLVMDisposeMessage(symbol) };
            result
        })
    }

    fn get_or_declare_intrinsic(
        &self,
        module: &LlvmModule<'_>,
        symbol: &str,
        function_type: FunctionType<'_>,
    ) -> Result<Function> {
        let raw = named_symbol(module, symbol);
        if raw.is_null() {
            let name = CString::new(symbol).expect("validated LLVM intrinsic name");
            // SAFETY: this exact symbol is free and the signature belongs to the module.
            let raw = unsafe {
                LLVMAddFunction(
                    module.as_mut_ptr(),
                    name.as_ptr(),
                    function_type.as_type_ref(),
                )
            };
            return Ok(Function {
                raw,
                module: self.clone(),
            });
        }
        // SAFETY: the module owns this named symbol. Only functions may
        // be wrapped, and existing intrinsic declarations are reusable.
        if !unsafe { LLVMIsAFunction(raw) }.is_null() {
            let function = unsafe { FunctionValue::new(raw) }.expect("LLVM function symbol");
            if function.count_basic_blocks() == 0 && function.get_type() == function_type {
                return Ok(self.function(function));
            }
        }
        Err(Error::new(format!(
            "LLVM intrinsic name '{symbol}' is already in use"
        )))
    }
}

fn base_intrinsic_id(name: &str) -> Result<u32> {
    // SAFETY: LLVM reads exactly the supplied bytes, and known IDs have a
    // static name. Compare the base spelling because lookup also accepts suffixes.
    unsafe {
        let id = LLVMLookupIntrinsicID(name.as_ptr().cast(), name.len());
        if id != 0 {
            let mut len = 0;
            let base = LLVMIntrinsicGetName(id, &mut len);
            if slice::from_raw_parts(base.cast::<u8>(), len) == name.as_bytes() {
                return Ok(id);
            }
        }
    }
    Err(Error::new(format!(
        "unknown LLVM intrinsic base name '{name}'"
    )))
}

fn signature_mismatch(name: &str) -> Error {
    Error::new(format!("signature does not match LLVM intrinsic '{name}'"))
}

fn resolve_overloaded_name(
    context: &Context,
    name: &str,
    id: u32,
    signature: &Type,
    function_type: FunctionType<'_>,
) -> Result<String> {
    // LLVM's bitcode reader remangles a valid overloaded declaration from its
    // full signature. Use a private one-declaration module to keep incomplete
    // or invalid declarations out of the caller's IR.
    let declaration = context.module("intrinsic signature")?;
    let bitcode = declaration.with_llvm(|module| {
        // SAFETY: `function_type` belongs to this temporary module's context.
        let ty = unsafe { FunctionType::new(function_type.as_type_ref()) };
        module.add_function(name, ty, None);
        module.write_bitcode_to_memory()
    });
    let decoded = Module::verified(Context::default().read_bitcode(&bitcode)?)?;
    let mut functions = decoded.functions();
    let function = functions.next().ok_or_else(|| signature_mismatch(name))?;
    let symbol = function.name()?;
    // SAFETY: the decoded module retains this function.
    let actual_id = unsafe { LLVMGetIntrinsicID(function.raw) };
    if functions.next().is_some()
        || actual_id != id
        || !symbol
            .strip_prefix(name)
            .is_some_and(|suffix| suffix.starts_with('.'))
        || function.signature().ir() != signature.ir()
    {
        return Err(signature_mismatch(name));
    }
    Ok(symbol)
}

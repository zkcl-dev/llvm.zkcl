use std::{collections::HashSet, ffi::CString, ptr};

use inkwell::llvm_sys::LLVMTypeKind::{
    LLVMFunctionTypeKind, LLVMLabelTypeKind, LLVMPointerTypeKind, LLVMTokenTypeKind,
    LLVMVoidTypeKind,
};
use inkwell::llvm_sys::core::{
    LLVMBuildCallWithOperandBundles, LLVMCreateOperandBundle, LLVMGetInlineAsmFunctionType,
    LLVMGetIntrinsicID, LLVMGetParamTypes, LLVMGetReturnType, LLVMGetTypeKind, LLVMIsACatchPadInst,
    LLVMIsACleanupPadInst, LLVMIsAFunction, LLVMIsAInlineAsm, LLVMTypeOf,
};
use inkwell::llvm_sys::prelude::{LLVMOperandBundleRef, LLVMValueRef};
use inkwell::types::{AsTypeRef, FunctionType};
use inkwell::values::{
    AsValueRef, CallSiteValue, FunctionValue, InstructionValue, OperandBundle as LlvmOperandBundle,
};

use super::{Builder, MAX_USER_OPERANDS, c_name};
use crate::{AttributeLocation, Block, Error, Function, Instruction, Result, Type, Value};

// A call stores the callee in addition to its arguments and bundle operands.
const CALL_NON_ARGUMENT_OPERANDS: usize = 1;

/// An ordered, tagged list of operands attached to an LLVM call site.
#[derive(Clone)]
pub struct OperandBundle {
    tag: Vec<u8>,
    operands: Vec<Value>,
}

impl OperandBundle {
    pub fn new(tag: impl AsRef<[u8]>, operands: Vec<Value>) -> Self {
        Self {
            tag: tag.as_ref().to_vec(),
            operands,
        }
    }

    /// The exact LLVM tag bytes, including embedded NULs if present.
    pub fn tag(&self) -> &[u8] {
        &self.tag
    }

    pub fn operands(&self) -> &[Value] {
        &self.operands
    }
}

impl Builder {
    pub fn call(&self, callee: &Function, args: &[Value], name: &str) -> Result<Option<Value>> {
        Ok(self.call_instruction(callee, args, name)?.result())
    }

    /// Emit a direct call and return its instruction, including for a void call.
    pub fn call_instruction(
        &self,
        callee: &Function,
        args: &[Value],
        name: &str,
    ) -> Result<Instruction> {
        self.call_instruction_with_bundles(callee, args, &[], name)
    }

    /// Emit a direct call with the supplied operand bundles in IR order.
    pub fn call_instruction_with_bundles(
        &self,
        callee: &Function,
        args: &[Value],
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        let block = self.insertion()?;
        block.function.module.check(&callee.module)?;
        let function = callee.llvm();
        self.build_call_instruction(
            &block,
            function.get_type(),
            (function.as_value_ref(), Some(function)),
            args,
            bundles,
            name,
        )
    }

    /// Call an opaque function pointer with an explicit LLVM signature.
    /// A dynamically resolved callee must match the signature and the C calling convention.
    pub fn call_indirect(
        &self,
        signature: &Type,
        callee: &Value,
        args: &[Value],
        name: &str,
    ) -> Result<Option<Value>> {
        Ok(self
            .call_indirect_instruction(signature, callee, args, name)?
            .result())
    }

    /// Emit a call through an opaque pointer and return its instruction.
    pub fn call_indirect_instruction(
        &self,
        signature: &Type,
        callee: &Value,
        args: &[Value],
        name: &str,
    ) -> Result<Instruction> {
        self.call_indirect_instruction_with_bundles(signature, callee, args, &[], name)
    }

    /// Emit an indirect call with an explicit function type and operand bundles.
    pub fn call_indirect_instruction_with_bundles(
        &self,
        signature: &Type,
        callee: &Value,
        args: &[Value],
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        self.indirect_call_instruction(signature, None, callee, args, bundles, name)
    }

    /// Call an opaque pointer with the signature, calling convention and
    /// parameter and return attributes of a prototype from this context.
    pub fn call_indirect_with_prototype(
        &self,
        prototype: &Function,
        callee: &Value,
        args: &[Value],
        name: &str,
    ) -> Result<Option<Value>> {
        Ok(self
            .call_indirect_instruction_with_prototype(prototype, callee, args, name)?
            .result())
    }

    /// Emit an indirect call with the prototype's ABI and return its instruction.
    pub fn call_indirect_instruction_with_prototype(
        &self,
        prototype: &Function,
        callee: &Value,
        args: &[Value],
        name: &str,
    ) -> Result<Instruction> {
        self.call_indirect_instruction_with_prototype_and_bundles(
            prototype,
            callee,
            args,
            &[],
            name,
        )
    }

    /// Emit an indirect call with a prototype's ABI and the supplied bundles.
    pub fn call_indirect_instruction_with_prototype_and_bundles(
        &self,
        prototype: &Function,
        callee: &Value,
        args: &[Value],
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        self.indirect_call_instruction(
            &prototype.signature(),
            Some(prototype.llvm()),
            callee,
            args,
            bundles,
            name,
        )
    }

    fn indirect_call_instruction(
        &self,
        signature: &Type,
        prototype: Option<FunctionValue<'_>>,
        callee: &Value,
        args: &[Value],
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        let block = self.insertion()?;
        let signature = signature.function_in(self.context())?;
        let target = indirect_callee(signature, prototype, callee, &block.function)?;
        self.build_call_instruction(&block, signature, target, args, bundles, name)
    }

    fn build_call_instruction(
        &self,
        block: &Block,
        signature: FunctionType<'_>,
        (callee, abi): (LLVMValueRef, Option<FunctionValue<'_>>),
        args: &[Value],
        bundles: &[OperandBundle],
        name: &str,
    ) -> Result<Instruction> {
        let caller = &block.function;
        let mut args = call_arguments(signature, caller, args, CALL_NON_ARGUMENT_OPERANDS)?;
        let name = call_name(signature, name)?;
        let mut bundles =
            prepare_operand_bundles(args.len(), CALL_NON_ARGUMENT_OPERANDS, bundles, caller)?;
        // SAFETY: the insertion point, function type, callee, arguments, and bundles are live
        // and validated. LLVM copies the operands and bundles into the instruction.
        let call = unsafe {
            CallSiteValue::new(LLVMBuildCallWithOperandBundles(
                self.llvm().as_mut_ptr(),
                signature.as_type_ref(),
                callee,
                args.as_mut_ptr(),
                args.len() as u32,
                bundles.as_mut_ptr(),
                bundles.len(),
                name.as_ptr(),
            ))
        };
        Ok(finish_call(call, abi, block))
    }
}

pub(super) fn indirect_callee<'a>(
    signature: FunctionType<'a>,
    prototype: Option<FunctionValue<'a>>,
    callee: &'a Value,
    caller: &'a Function,
) -> Result<(LLVMValueRef, Option<FunctionValue<'a>>)> {
    let raw = callee.scoped_ref(caller)?;
    // SAFETY: the Value retains this callee and its type.
    let known = unsafe {
        if LLVMGetTypeKind(LLVMTypeOf(raw)) != LLVMPointerTypeKind {
            return Err(Error::new("expected a pointer operand"));
        }
        if !LLVMIsAInlineAsm(raw).is_null()
            && LLVMGetInlineAsmFunctionType(raw) != signature.as_type_ref()
        {
            return Err(Error::new(
                "call signature does not match the LLVM inline asm",
            ));
        }
        FunctionValue::new(raw)
    };
    if known.is_some_and(|function| function.get_intrinsic_id() != 0) {
        return Err(Error::new("cannot take the address of an LLVM intrinsic"));
    }
    if known.is_some_and(|function| function.get_type() != signature) {
        return Err(Error::new(
            "call signature does not match the known LLVM function",
        ));
    }
    if let (Some(known), Some(prototype)) = (known, prototype)
        && !call_abis_match(known, prototype)
    {
        return Err(Error::new(
            "call prototype does not match the known LLVM function's ABI",
        ));
    }
    Ok((raw, known.or(prototype)))
}

pub(super) fn call_arguments(
    signature: FunctionType<'_>,
    caller: &Function,
    args: &[Value],
    non_argument_operands: usize,
) -> Result<Vec<LLVMValueRef>> {
    let fixed = signature.count_param_types() as usize;
    if args.len() < fixed || (args.len() != fixed && !signature.is_var_arg()) {
        return Err(Error::new(
            "call argument count does not match the function signature",
        ));
    }
    if non_argument_operands > MAX_USER_OPERANDS
        || args.len() > MAX_USER_OPERANDS - non_argument_operands
    {
        return Err(Error::new("too many LLVM call arguments"));
    }
    let mut parameters = vec![ptr::null_mut(); fixed];
    // SAFETY: the slice has room for the complete parameter type list.
    unsafe { LLVMGetParamTypes(signature.as_type_ref(), parameters.as_mut_ptr()) };
    args.iter()
        .enumerate()
        .map(|(index, arg)| {
            let value = call_operand(arg, caller, false)?;
            // SAFETY: the argument and parameter types are live.
            let ty = unsafe { LLVMTypeOf(value) };
            if unsafe { LLVMGetTypeKind(ty) } == LLVMLabelTypeKind
                || parameters.get(index).is_some_and(|&param| param != ty)
            {
                return Err(Error::new(format!(
                    "call argument {index} has the wrong LLVM type"
                )));
            }
            Ok(value)
        })
        .collect()
}

fn call_operand(arg: &Value, caller: &Function, attached_call: bool) -> Result<LLVMValueRef> {
    let raw = arg.scoped_ref(caller)?;
    // SAFETY: the operand and its type are live, and the function check guards the ID query.
    unsafe {
        if matches!(
            LLVMGetTypeKind(LLVMTypeOf(raw)),
            LLVMVoidTypeKind | LLVMFunctionTypeKind
        ) {
            return Err(Error::new("expected a first-class LLVM call operand"));
        }
        if !LLVMIsAInlineAsm(raw).is_null() {
            return Err(Error::new("LLVM inline asm can only be used as a callee"));
        }
        if !attached_call && !LLVMIsAFunction(raw).is_null() && LLVMGetIntrinsicID(raw) != 0 {
            return Err(Error::new("cannot take the address of an LLVM intrinsic"));
        }
    }
    Ok(raw)
}

pub(super) fn call_name(signature: FunctionType<'_>, name: &str) -> Result<CString> {
    // SAFETY: the live function type owns its return type.
    let is_void =
        unsafe { LLVMGetTypeKind(LLVMGetReturnType(signature.as_type_ref())) == LLVMVoidTypeKind };
    c_name(if is_void { "" } else { name })
}

pub(super) struct PreparedOperandBundles<'a> {
    raw: Vec<LLVMOperandBundleRef>,
    _owned: Vec<LlvmOperandBundle<'a>>,
}

impl PreparedOperandBundles<'_> {
    pub(super) fn as_mut_ptr(&mut self) -> *mut LLVMOperandBundleRef {
        self.raw.as_mut_ptr()
    }

    pub(super) fn len(&self) -> u32 {
        self.raw.len() as u32
    }
}

pub(super) fn prepare_operand_bundles<'a>(
    argument_count: usize,
    non_argument_operands: usize,
    bundles: &'a [OperandBundle],
    caller: &Function,
) -> Result<PreparedOperandBundles<'a>> {
    // Empty bundles also consume storage in LLVM's operand bundle descriptor.
    if bundles.len() > MAX_USER_OPERANDS {
        return Err(Error::new("too many LLVM operand bundles"));
    }
    let mut remaining = MAX_USER_OPERANDS
        .checked_sub(argument_count)
        .and_then(|count| count.checked_sub(non_argument_operands))
        .ok_or_else(|| Error::new("too many LLVM call operands"))?;
    let mut seen = HashSet::new();
    let mut owned = Vec::with_capacity(bundles.len());
    for bundle in bundles {
        remaining = remaining
            .checked_sub(bundle.operands().len())
            .ok_or_else(|| Error::new("too many LLVM operand bundle operands"))?;
        let tag = bundle.tag();
        if matches!(
            tag,
            b"deopt"
                | b"funclet"
                | b"gc-transition"
                | b"cfguardtarget"
                | b"preallocated"
                | b"gc-live"
                | b"ptrauth"
                | b"kcfi"
                | b"clang.arc.attachedcall"
                | b"convergencectrl"
        ) && !seen.insert(tag)
        {
            return Err(Error::new(format!(
                "duplicate LLVM operand bundle '{}'",
                String::from_utf8_lossy(tag)
            )));
        }
        let mut operands = bundle
            .operands()
            .iter()
            .map(|arg| call_operand(arg, caller, tag == b"clang.arc.attachedcall"))
            .collect::<Result<Vec<_>>>()?;
        // SAFETY: the values belong to the caller's function, module, or context.
        // LLVM copies the tag and operand references into this temporary bundle.
        let bundle = unsafe {
            if tag == b"funclet"
                && !matches!(operands.as_slice(), [value] if
                !LLVMIsACatchPadInst(*value).is_null() || !LLVMIsACleanupPadInst(*value).is_null())
            {
                return Err(Error::new(
                    "LLVM funclet bundle requires one catchpad or cleanuppad token",
                ));
            }
            if tag == b"convergencectrl"
                && !matches!(operands.as_slice(), [value] if
                LLVMGetTypeKind(LLVMTypeOf(*value)) == LLVMTokenTypeKind)
            {
                return Err(Error::new("LLVM convergencectrl bundle requires one token"));
            }
            LlvmOperandBundle::new(LLVMCreateOperandBundle(
                tag.as_ptr().cast(),
                tag.len(),
                operands.as_mut_ptr(),
                operands.len() as u32,
            ))
        };
        owned.push(bundle);
    }
    Ok(PreparedOperandBundles {
        raw: owned.iter().map(LlvmOperandBundle::as_mut_ptr).collect(),
        _owned: owned,
    })
}

fn call_attribute_locations(
    function: FunctionValue<'_>,
) -> impl Iterator<Item = AttributeLocation> {
    std::iter::once(AttributeLocation::Return)
        .chain((0..function.count_params()).map(AttributeLocation::Param))
}

fn call_abis_match(left: FunctionValue<'_>, right: FunctionValue<'_>) -> bool {
    left.get_call_conventions() == right.get_call_conventions()
        && call_attribute_locations(left).all(|location| {
            // LLVM interns attributes in the context and stores each set in
            // order. Compare handles to include arbitrary string bytes and
            // payloads beyond Inkwell's typed attribute accessors.
            left.attributes(location)
                .into_iter()
                .map(|attribute| attribute.as_mut_ptr())
                .eq(right
                    .attributes(location)
                    .into_iter()
                    .map(|attribute| attribute.as_mut_ptr()))
        })
}

pub(super) fn finish_call(
    call: CallSiteValue<'_>,
    callee: Option<FunctionValue<'_>>,
    block: &Block,
) -> Instruction {
    if let Some(callee) = callee {
        call.set_call_convention(callee.get_call_conventions());
        for location in call_attribute_locations(callee) {
            for attribute in callee.attributes(location) {
                call.add_attribute(location, attribute);
            }
        }
    }
    // SAFETY: the builder just inserted this call instruction in `block`.
    block.instruction(unsafe { InstructionValue::new(call.as_value_ref()) })
}

use std::{fmt::Write, slice};

use inkwell::basic_block::BasicBlock;
use inkwell::llvm_sys::LLVMInlineAsmDialect::{LLVMInlineAsmDialectATT, LLVMInlineAsmDialectIntel};
use inkwell::llvm_sys::core::{
    LLVMBlockAddress, LLVMGetBlockAddressBasicBlock, LLVMGetBlockAddressFunction, LLVMGetInlineAsm,
    LLVMGetInlineAsmAsmString, LLVMGetInlineAsmCanUnwind, LLVMGetInlineAsmConstraintString,
    LLVMGetInlineAsmDialect, LLVMGetInlineAsmFunctionType, LLVMGetInlineAsmHasSideEffects,
    LLVMGetInlineAsmNeedsAlignedStack, LLVMIsABlockAddress, LLVMIsAInlineAsm,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::types::{AsTypeRef, BasicTypeEnum, FunctionType};
use inkwell::values::FunctionValue;

use super::{Block, Context, Type, Value, ValueOwner, function_result};
use crate::{Error, Result};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum InlineAsmDialect {
    #[default]
    Att,
    Intel,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct InlineAsmOptions {
    pub dialect: InlineAsmDialect,
    pub side_effects: bool,
    pub align_stack: bool,
    pub unwind: bool,
}

/// Inline assembly bytes and call-site options recovered from a value.
pub struct InlineAsmInfo<'a> {
    pub signature: Type,
    pub assembly: &'a [u8],
    pub constraints: &'a [u8],
    pub options: InlineAsmOptions,
}

impl Type {
    /// Construct a context-owned callee with this function type. The template
    /// and constraints must be supported by the eventual code generation target.
    pub fn inline_asm(
        &self,
        assembly: &[u8],
        constraints: &[u8],
        options: InlineAsmOptions,
    ) -> Result<Value> {
        let context = self.context();
        let signature = self.function_in(context)?;
        verify_constraints(signature, constraints)?;
        let dialect = match options.dialect {
            InlineAsmDialect::Att => LLVMInlineAsmDialectATT,
            InlineAsmDialect::Intel => LLVMInlineAsmDialectIntel,
        };
        // SAFETY: LLVM's parser validated the constraint string against the
        // signature's input count and output shape. Both byte slices remain live.
        let raw = unsafe {
            LLVMGetInlineAsm(
                signature.as_type_ref(),
                assembly.as_ptr().cast(),
                assembly.len(),
                constraints.as_ptr().cast(),
                constraints.len(),
                options.side_effects.into(),
                options.align_stack.into(),
                dialect,
                options.unwind.into(),
            )
        };
        Ok(Value::constant(raw, context, None))
    }
}

fn verify_constraints(signature: FunctionType<'_>, constraints: &[u8]) -> Result<()> {
    if signature.is_var_arg() {
        return Err(Error::new(
            "LLVM inline asm cannot have a variadic signature",
        ));
    }
    // The LLVM IR parser calls InlineAsm::verify before constructing the value.
    // That check uses arity and return shape, so neutral types avoid importing
    // named types into the scratch context. Label and operand attributes are
    // checked on the real call by the module verifier.
    let result = match function_result(signature)? {
        None => "void".to_owned(),
        Some(BasicTypeEnum::StructType(ty)) => {
            let fields = vec!["i8"; ty.count_fields() as usize].join(", ");
            format!("{{ {fields} }}")
        }
        Some(_) => "i8".to_owned(),
    };
    let arguments = vec!["i8 poison"; signature.count_param_types() as usize].join(", ");
    let mut escaped = String::new();
    for &byte in constraints {
        write!(escaped, "\\{byte:02X}").expect("write to String");
    }
    let source = format!(
        "define void @check() {{\n  call {result} asm \"\", \"{escaped}\"({arguments})\n  ret void\n}}\n"
    );
    let context = Context::default();
    let buffer = MemoryBuffer::create_from_memory_range_copy(source.as_bytes(), "inline asm");
    context.llvm().create_module_from_ir(buffer)?;
    Ok(())
}

impl Value {
    /// Read the inline assembly callee's signature, bytes, and flags.
    pub fn inline_asm_info(&self) -> Option<InlineAsmInfo<'_>> {
        // SAFETY: the context keeps this value live. The predicate limits all
        // subsequent accessors to inline asm. LLVM owns both byte arrays.
        unsafe {
            let raw = LLVMIsAInlineAsm(self.raw);
            if raw.is_null() {
                return None;
            }
            let mut assembly_len = 0;
            let mut constraints_len = 0;
            let assembly = LLVMGetInlineAsmAsmString(raw, &mut assembly_len);
            let constraints = LLVMGetInlineAsmConstraintString(raw, &mut constraints_len);
            Some(InlineAsmInfo {
                signature: self.context().type_ref(LLVMGetInlineAsmFunctionType(raw)),
                assembly: slice::from_raw_parts(assembly.cast(), assembly_len),
                constraints: slice::from_raw_parts(constraints.cast(), constraints_len),
                options: InlineAsmOptions {
                    dialect: match LLVMGetInlineAsmDialect(raw) {
                        LLVMInlineAsmDialectATT => InlineAsmDialect::Att,
                        LLVMInlineAsmDialectIntel => InlineAsmDialect::Intel,
                    },
                    side_effects: LLVMGetInlineAsmHasSideEffects(raw) != 0,
                    align_stack: LLVMGetInlineAsmNeedsAlignedStack(raw) != 0,
                    unwind: LLVMGetInlineAsmCanUnwind(raw) != 0,
                },
            })
        }
    }

    /// Return the label named by this blockaddress constant.
    pub fn block_address_target(&self) -> Option<Block> {
        let module = match &self.owner {
            ValueOwner::Module(module) => module,
            ValueOwner::Local(function) => function.module(),
            ValueOwner::Context(_) => return None,
        };
        // SAFETY: the retained module owns both references in a blockaddress.
        unsafe {
            let raw = LLVMIsABlockAddress(self.raw);
            if raw.is_null() {
                return None;
            }
            let function = FunctionValue::new(LLVMGetBlockAddressFunction(raw))?;
            let block = BasicBlock::new(LLVMGetBlockAddressBasicBlock(raw))?;
            Some(module.function(function).block(block))
        }
    }
}

impl Block {
    /// Return the module-owned blockaddress constant for a non-entry block.
    pub fn address(&self) -> Result<Value> {
        self.check_live()?;
        let function = self.function();
        if function
            .first_block()
            .is_some_and(|entry| entry.same_as(self))
        {
            return Err(Error::new("cannot take the address of an LLVM entry block"));
        }
        let module = function.module();
        // SAFETY: the function and its non-entry label belong to the live module.
        let raw = unsafe { LLVMBlockAddress(function.raw, self.raw) };
        Ok(Value::constant(raw, module.context(), Some(module)))
    }
}

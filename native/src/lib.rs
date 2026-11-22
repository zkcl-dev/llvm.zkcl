//! Owned LLVM handles for use by compiler tools and language bindings.
//!
//! Build a lookup table as a module-owned constant and verify the module.
//!
//! ```
//! use zkcl_llvm_api::{Context, GlobalOptions, Linkage};
//!
//! # fn main() -> zkcl_llvm_api::Result<()> {
//! let context = Context::default();
//! let module = context.module("lookup-table")?;
//! let i32 = context.int_type(32)?;
//! let entries = i32.const_array(&[i32.const_int(&7.into())?, i32.const_int(&13.into())?])?;
//! let table = module.add_global("table", &entries.ty(), GlobalOptions {
//!     initializer: Some(&entries),
//!     linkage: Linkage::Private,
//!     constant: true,
//! })?;
//! module.verify()?;
//! assert_eq!(table.storage_type()?.ir(), "[2 x i32]");
//! # Ok(())
//! # }
//! ```
mod builder;
mod debug;
mod ir;
mod jit;
mod native;
mod target;

use std::ffi::CStr;

use inkwell::llvm_sys::error::{LLVMDisposeErrorMessage, LLVMErrorRef, LLVMGetErrorMessage};

pub use builder::GepFlags;
pub use builder::{
    AtomicOptions, AtomicOrdering, AtomicRmwOp, AtomicRmwOptions, Builder, CastOp, CmpXchgOptions,
    FloatOp, FloatPredicate, IntFlags, IntOp, IntPredicate, MemoryOptions, OperandBundle,
    SyncScope,
};
pub use debug::{
    DebugCompileUnitOptions, DebugEmission, DebugEncoding, DebugFile, DebugFunctionOptions,
    DebugInfo, DebugLanguage, DebugLocation, DebugMember, DebugOptions, DebugScope, DebugType,
    DebugVariable,
};
pub use inkwell::values::FastMathFlags;
pub use ir::{
    AttributeData, AttributeInfo, AttributeLocation, AttributeValue, Block, Comdat,
    ComdatSelectionKind, Context, DLLStorageClass, Edit, Function, Global, GlobalAlias,
    GlobalOptions, GlobalVisibility, InlineAsmDialect, InlineAsmInfo, InlineAsmOptions,
    Instruction, Linkage, Metadata, Module, ModuleFlagBehavior, TailCallKind, ThreadLocalMode,
    Type, TypeKind, UnnamedAddress, Value, ValueKind, ValueUse,
};
pub use jit::{HostSymbolKind, Jit, JitSymbol, ResourceTracker};
pub use native::visit_host_symbols;
pub use target::{
    CodeModel, DataLayout, OptimizationLevel, PassOptions, RelocMode, TargetMachine, TargetOptions,
};

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct Error(String);

impl Error {
    fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

impl From<inkwell::builder::BuilderError> for Error {
    fn from(error: inkwell::builder::BuilderError) -> Self {
        Self(error.to_string())
    }
}

impl From<inkwell::support::LLVMString> for Error {
    fn from(error: inkwell::support::LLVMString) -> Self {
        Self(error.to_string())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

fn c_text(text: &str) -> Result<&str> {
    if text.contains('\0') {
        return Err(Error::new("LLVM text contains an embedded NUL"));
    }
    Ok(text)
}

/// Consume the result of an LLVM C API call that returns an owned error.
///
/// # Safety
/// A non-null error must be live and uniquely owned by the caller.
unsafe fn llvm_result(error: LLVMErrorRef) -> Result<()> {
    if error.is_null() {
        return Ok(());
    }
    // SAFETY: LLVMGetErrorMessage consumes this owned error. Copy the message
    // before releasing the buffer with its matching disposer.
    unsafe {
        let message = LLVMGetErrorMessage(error);
        let text = CStr::from_ptr(message).to_string_lossy().into_owned();
        LLVMDisposeErrorMessage(message);
        Err(Error::new(text))
    }
}

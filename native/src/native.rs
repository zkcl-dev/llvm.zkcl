//! C boundary for the ASDF library. The Lisp adapter owns each returned handle.
//! Give an export a distinct symbol name when its C prototype changes.
//! Return integers and predicates as `u64`. SBCL/CFFI on ARM64 can consume all
//! of x0, whose upper half a `u32` return leaves unspecified.

use std::ffi::{CStr, CString, c_char, c_void};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::{
    ptr::{self, NonNull},
    slice,
};

use num_bigint::BigInt;

use crate::debug::DebugCompileUnitOptions;
use crate::{
    AtomicOptions, AtomicOrdering, AtomicRmwOp, AtomicRmwOptions, AttributeData, AttributeInfo,
    AttributeLocation, AttributeValue, Block, Builder, CastOp, CmpXchgOptions, CodeModel, Comdat,
    ComdatSelectionKind, Context, DLLStorageClass, DataLayout, DebugEmission, DebugEncoding,
    DebugFile, DebugFunctionOptions, DebugInfo, DebugLanguage, DebugLocation, DebugMember,
    DebugOptions, DebugScope, DebugType, DebugVariable, Edit, Error, FastMathFlags, FloatOp,
    FloatPredicate, Function, GepFlags, Global, GlobalAlias, GlobalOptions, GlobalVisibility,
    HostSymbolKind, Instruction, IntFlags, IntOp, IntPredicate, Jit, JitSymbol, Linkage,
    MemoryOptions, Metadata, Module, ModuleFlagBehavior, OptimizationLevel, RelocMode,
    ResourceTracker, Result, SyncScope, TailCallKind, TargetMachine, TargetOptions,
    ThreadLocalMode, Type, UnnamedAddress, Value, ValueUse,
};

type Handle = *mut c_void;
type ErrorSlot = *mut *mut c_char;

macro_rules! objects {
    ($($variant:ident($ty:ty)),+ $(,)?) => {
        enum Object {
            $($variant($ty)),+
        }

        $(impl NativeObject for $ty {
            fn wrap(self) -> Object {
                Object::$variant(self)
            }

            fn borrow(object: &Object) -> Option<&Self> {
                match object {
                    Object::$variant(value) => Some(value),
                    _ => None,
                }
            }

            const NAME: &'static str = stringify!($variant);
        })+
    };
}

trait NativeObject: Sized {
    const NAME: &'static str;
    fn wrap(self) -> Object;
    fn borrow(object: &Object) -> Option<&Self>;
}

objects! {
    Context(Context), Module(Module), Edit(Edit), Builder(Builder), Type(Type), Value(Value),
    Function(Function), Block(Block), Instruction(Instruction), Global(Global), Machine(TargetMachine), Jit(Jit), JitSymbol(JitSymbol),
    DebugInfo(DebugInfo), DebugFile(DebugFile), DebugScope(DebugScope), DebugLocation(DebugLocation),
    DebugType(DebugType), DebugMember(DebugMember), DebugVariable(DebugVariable), ResourceTracker(ResourceTracker),
    DataLayout(DataLayout), Metadata(Metadata), Comdat(Comdat), GlobalAlias(GlobalAlias),
    AttributeInfo(AttributeInfo), Attributes(Vec<AttributeInfo>), Text(CString), Bytes(Vec<u8>),
    ValueUses(Vec<ValueUse>), Blocks(Vec<Block>), OperandBundle(crate::OperandBundle),
}

fn owned(value: impl NativeObject) -> Result<Handle> {
    Ok(Box::into_raw(Box::new(value.wrap())).cast())
}

fn optional(value: Option<impl NativeObject>) -> Result<Handle> {
    value.map_or(Ok(ptr::null_mut()), owned)
}

fn unit(result: Result<()>) -> Result<Handle> {
    result.map(|()| ptr::null_mut())
}

fn text(value: String) -> Result<Handle> {
    owned(CString::new(value).map_err(|_| Error::new("LLVM returned text containing NUL"))?)
}

fn optional_text(value: Option<String>) -> Result<Handle> {
    value.map_or(Ok(ptr::null_mut()), text)
}

unsafe fn borrow<'a, T: NativeObject>(handle: Handle) -> Result<&'a T> {
    // SAFETY: the caller retains a live handle created by this library.
    let value = unsafe { handle.cast::<Object>().as_ref() }
        .ok_or_else(|| Error::new(format!("expected an LLVM {} handle", T::NAME)))?;
    let typed = T::borrow(value)
        .ok_or_else(|| Error::new(format!("expected an LLVM {} handle", T::NAME)))?;
    value.check_live()?;
    Ok(typed)
}

impl Object {
    fn check_live(&self) -> Result<()> {
        match self {
            Self::Block(block) => block.check_live(),
            Self::Instruction(instruction) => instruction.block().check_live(),
            Self::Value(value) => value.check_live(),
            Self::DebugScope(scope) => scope.check_live(),
            Self::DebugLocation(location) => location.check_live(),
            Self::DebugVariable(variable) => variable.check_live(),
            _ => Ok(()),
        }
    }
}

unsafe fn nullable<'a, T: NativeObject>(handle: Handle) -> Result<Option<&'a T>> {
    // SAFETY: either this is a live borrowed handle or null.
    (!handle.is_null())
        .then(|| unsafe { borrow(handle) })
        .transpose()
}

unsafe fn input<'a, T>(data: *const T, len: usize) -> Result<&'a [T]> {
    if len == 0 {
        return Ok(&[]);
    }
    if data.is_null() || len > isize::MAX as usize / size_of::<T>() {
        return Err(Error::new("invalid native buffer"));
    }
    // SAFETY: the caller supplied `len` initialized elements valid for this call.
    Ok(unsafe { slice::from_raw_parts(data, len) })
}

unsafe fn handles<T: NativeObject + Clone>(data: *const Handle, len: usize) -> Result<Vec<T>> {
    // SAFETY: both the temporary array and each borrowed object are live.
    unsafe { input(data, len)? }
        .iter()
        .map(|&handle| unsafe { borrow::<T>(handle).cloned() })
        .collect()
}

unsafe fn string<'a>(value: *const c_char) -> Result<&'a str> {
    if value.is_null() {
        return Err(Error::new("expected a UTF-8 string"));
    }
    // SAFETY: the caller passes a NUL-terminated string for this call.
    unsafe { CStr::from_ptr(value) }
        .to_str()
        .map_err(|error| Error::new(error.to_string()))
}

fn choice<T: Copy>(values: &[T], index: u32) -> Result<T> {
    values
        .get(index as usize)
        .copied()
        .ok_or_else(|| Error::new("invalid LLVM operation"))
}

fn debug_source_language(name: &str) -> Result<DebugLanguage> {
    use DebugLanguage::*;
    Ok(match name {
        "c89" => C89,
        "c" => C,
        "ada83" => Ada83,
        "c++" => CPlusPlus,
        "cobol74" => Cobol74,
        "cobol85" => Cobol85,
        "fortran77" => Fortran77,
        "fortran90" => Fortran90,
        "pascal83" => Pascal83,
        "modula2" => Modula2,
        "java" => Java,
        "c99" => C99,
        "ada95" => Ada95,
        "fortran95" => Fortran95,
        "pli" => PLI,
        "objective-c" => ObjC,
        "objective-c++" => ObjCPlusPlus,
        "upc" => UPC,
        "d" => D,
        "python" => Python,
        "opencl" => OpenCL,
        "go" => Go,
        "modula3" => Modula3,
        "haskell" => Haskell,
        "c++03" => CPlusPlus03,
        "c++11" => CPlusPlus11,
        "ocaml" => OCaml,
        "rust" => Rust,
        "c11" => C11,
        "swift" => Swift,
        "julia" => Julia,
        "dylan" => Dylan,
        "c++14" => CPlusPlus14,
        "fortran03" => Fortran03,
        "fortran08" => Fortran08,
        "render-script" => RenderScript,
        "bliss" => BLISS,
        "kotlin" => Kotlin,
        "zig" => Zig,
        "crystal" => Crystal,
        "c++17" => CPlusPlus17,
        "c++20" => CPlusPlus20,
        "c17" => C17,
        "fortran18" => Fortran18,
        "ada2005" => Ada2005,
        "ada2012" => Ada2012,
        "hip" => Hip,
        "assembly" => Assembly,
        "c#" => Csharp,
        "mojo" => Mojo,
        "glsl" => Glsl,
        "glsl-es" => GlslEs,
        "hlsl" => Hlsl,
        "opencl-c++" => OpenClCpp,
        "c++-for-opencl" => CppForOpenCl,
        "sycl" => Sycl,
        "metal" => Metal,
        "mips-assembler" => MipsAssembler,
        "google-render-script" => GOOGLERenderScript,
        "borland-delphi" => BORLANDDelphi,
        "common-lisp" | "lisp" => {
            return Err(Error::new(
                "LLVM 21 has no DWARF source language for Common Lisp",
            ));
        }
        _ => {
            return Err(Error::new(format!(
                "unknown LLVM debug source language '{name}'"
            )));
        }
    })
}

const LINKAGES: &[Linkage] = &[
    Linkage::External,
    Linkage::Internal,
    Linkage::Private,
    Linkage::AvailableExternally,
    Linkage::LinkOnceAny,
    Linkage::LinkOnceODR,
    Linkage::WeakAny,
    Linkage::WeakODR,
    Linkage::Appending,
    Linkage::ExternalWeak,
    Linkage::Common,
];

const UNNAMED_ADDRESSES: &[UnnamedAddress] = &[
    UnnamedAddress::None,
    UnnamedAddress::Local,
    UnnamedAddress::Global,
];

const INT_PREDICATES: &[IntPredicate] = {
    use IntPredicate::*;
    &[EQ, NE, UGT, UGE, ULT, ULE, SGT, SGE, SLT, SLE]
};

const FLOAT_PREDICATES: &[FloatPredicate] = {
    use FloatPredicate::*;
    &[
        OEQ,
        OGT,
        OGE,
        OLT,
        OLE,
        ONE,
        ORD,
        UNO,
        UEQ,
        UGT,
        UGE,
        ULT,
        ULE,
        UNE,
        PredicateFalse,
        PredicateTrue,
    ]
};

const FLOAT_OPS: &[FloatOp] = {
    use FloatOp::*;
    &[Add, Sub, Mul, Div, Rem]
};

fn fast_math_flags(bits: u32) -> Result<FastMathFlags> {
    FastMathFlags::from_bits(bits).ok_or_else(|| Error::new("invalid LLVM fast-math flags"))
}

fn predicate_index<T: PartialEq>(choices: &[T], predicate: Option<T>) -> Result<u64> {
    predicate.map_or(Ok(0), |predicate| Ok(1 + choice_index(choices, predicate)?))
}

fn linkage(index: u32) -> Result<Linkage> {
    choice(LINKAGES, index)
}

fn linkage_index(linkage: Linkage) -> Result<u64> {
    LINKAGES
        .iter()
        .position(|&candidate| candidate == linkage)
        .map(|index| index as u64)
        .ok_or_else(|| Error::new("unrecognized LLVM linkage"))
}

fn attribute_location(index: u32) -> AttributeLocation {
    match index {
        u32::MAX => AttributeLocation::Function,
        0 => AttributeLocation::Return,
        index => AttributeLocation::Param(index - 1),
    }
}

unsafe fn attribute_value<'a>(
    payload: u32,
    number: u64,
    ty: Handle,
    contents: *const c_char,
) -> Result<AttributeValue<'a>> {
    // SAFETY: the tag identifies which payload the caller keeps live.
    unsafe {
        match payload {
            0 => Ok(AttributeValue::Flag),
            1 => Ok(AttributeValue::Integer(number)),
            2 => Ok(AttributeValue::Type(borrow(ty)?)),
            3 => Ok(AttributeValue::String(string(contents)?)),
            4 => {
                let bit_width = u32::try_from(number)
                    .map_err(|_| Error::new("LLVM range width is out of bounds"))?;
                let (lower, upper) = string(contents)?
                    .split_once(',')
                    .ok_or_else(|| Error::new("expected two LLVM range bounds"))?;
                let lower = lower
                    .parse()
                    .map_err(|_| Error::new("invalid LLVM range lower bound"))?;
                let upper = upper
                    .parse()
                    .map_err(|_| Error::new("invalid LLVM range upper bound"))?;
                Ok(AttributeValue::Range {
                    bit_width,
                    lower,
                    upper,
                })
            }
            _ => Err(Error::new("invalid LLVM attribute payload")),
        }
    }
}

const TAIL_CALL_KINDS: &[TailCallKind] = &[
    TailCallKind::None,
    TailCallKind::Tail,
    TailCallKind::MustTail,
    TailCallKind::NoTail,
];

const VISIBILITIES: &[GlobalVisibility] = &[
    GlobalVisibility::Default,
    GlobalVisibility::Hidden,
    GlobalVisibility::Protected,
];

const DLL_STORAGE_CLASSES: &[DLLStorageClass] = &[
    DLLStorageClass::Default,
    DLLStorageClass::Import,
    DLLStorageClass::Export,
];

const TLS_MODES: &[Option<ThreadLocalMode>] = &[
    None,
    Some(ThreadLocalMode::GeneralDynamicTLSModel),
    Some(ThreadLocalMode::LocalDynamicTLSModel),
    Some(ThreadLocalMode::InitialExecTLSModel),
    Some(ThreadLocalMode::LocalExecTLSModel),
];

const COMDAT_KINDS: &[ComdatSelectionKind] = &[
    ComdatSelectionKind::Any,
    ComdatSelectionKind::ExactMatch,
    ComdatSelectionKind::Largest,
    ComdatSelectionKind::NoDuplicates,
    ComdatSelectionKind::SameSize,
];

fn choice_index<T: PartialEq>(values: &[T], value: T) -> Result<u64> {
    values
        .iter()
        .position(|candidate| candidate == &value)
        .map(|index| index as u64)
        .ok_or_else(|| Error::new("unrecognized LLVM option"))
}

const ATOMIC_ORDERINGS: &[AtomicOrdering] = &[
    AtomicOrdering::Unordered,
    AtomicOrdering::Monotonic,
    AtomicOrdering::Acquire,
    AtomicOrdering::Release,
    AtomicOrdering::AcquireRelease,
    AtomicOrdering::SequentiallyConsistent,
];

const ATOMIC_RMW_OPERATIONS: &[AtomicRmwOp] = &[
    AtomicRmwOp::Xchg,
    AtomicRmwOp::Add,
    AtomicRmwOp::Sub,
    AtomicRmwOp::And,
    AtomicRmwOp::Nand,
    AtomicRmwOp::Or,
    AtomicRmwOp::Xor,
    AtomicRmwOp::Max,
    AtomicRmwOp::Min,
    AtomicRmwOp::UMax,
    AtomicRmwOp::UMin,
    AtomicRmwOp::FAdd,
    AtomicRmwOp::FSub,
    AtomicRmwOp::FMax,
    AtomicRmwOp::FMin,
    AtomicRmwOp::UIncWrap,
    AtomicRmwOp::UDecWrap,
    AtomicRmwOp::USubCond,
    AtomicRmwOp::USubSat,
    AtomicRmwOp::FMaximum,
    AtomicRmwOp::FMinimum,
];

fn atomic_ordering(code: u32) -> Result<AtomicOrdering> {
    choice(ATOMIC_ORDERINGS, code.wrapping_sub(1))
}

fn atomic_ordering_index(ordering: Option<AtomicOrdering>) -> Result<u64> {
    ordering.map_or(Ok(0), |ordering| {
        Ok(1 + choice_index(ATOMIC_ORDERINGS, ordering)?)
    })
}

fn sync_scope(code: u32) -> Result<SyncScope> {
    choice(&[SyncScope::System, SyncScope::SingleThread], code)
}

fn memory_alignment(alignment: u64) -> Result<Option<u32>> {
    (alignment != 0)
        .then(|| {
            u32::try_from(alignment)
                .map_err(|_| Error::new("LLVM memory alignment exceeds the C API range"))
        })
        .transpose()
}

fn memory_options(
    alignment: u64,
    volatile: u8,
    ordering: u32,
    scope: u32,
) -> Result<MemoryOptions> {
    let scope = sync_scope(scope)?;
    let atomic = if ordering == 0 {
        if scope != SyncScope::System {
            return Err(Error::new(
                "LLVM synchronization scope requires atomic memory access",
            ));
        }
        None
    } else {
        Some(AtomicOptions {
            ordering: atomic_ordering(ordering)?,
            scope,
        })
    };
    Ok(MemoryOptions {
        alignment: memory_alignment(alignment)?,
        volatile: volatile != 0,
        atomic,
    })
}

fn int_op(code: u32) -> Result<IntOp> {
    use IntOp::*;
    choice(
        &[
            Add, Sub, Mul, UDiv, SDiv, URem, SRem, And, Or, Xor, Shl, LShr, AShr,
        ],
        code,
    )
}

fn respond<T: Default>(error: ErrorSlot, call: impl FnOnce() -> Result<T>) -> T {
    let result = match catch_unwind(AssertUnwindSafe(call)) {
        Ok(result) => result,
        Err(_) => Err(Error::new("native LLVM binding panicked")),
    };
    let (value, failure) = match result {
        Ok(value) => (value, ptr::null_mut()),
        Err(error) => {
            let message = error.to_string().replace('\0', "\\0");
            let message = CString::new(message).expect("NUL was escaped");
            (T::default(), message.into_raw())
        }
    };
    // SAFETY: every entry point receives a writable error slot from the caller.
    unsafe { error.write(failure) };
    value
}

macro_rules! api {
    (@return_type) => { Handle };
    (@return_type $ty:ty) => { $ty };
    ($($name:ident($($arg:ident : $ty:ty),*) $(-> $result:ty)? $body:block)*) => {
        $(
            #[unsafe(no_mangle)]
            unsafe extern "C" fn $name(error: ErrorSlot, $($arg: $ty),*) -> api!(@return_type $($result)?) {
                // SAFETY: the FFI caller retains all borrowed handles and memory for the call.
                #[allow(unused_unsafe)]
                respond(error, || unsafe { $body })
            }
        )*

        pub fn visit_host_symbols(mut register: impl FnMut(&'static str, NonNull<c_void>)) {
            let mut add = |name: &'static str, address: *const ()| {
                register(name, NonNull::new(address.cast_mut().cast()).expect("function address"));
            };
            add("zk_llvm_release", zk_llvm_release as *const ());
            add("zk_llvm_free_error", zk_llvm_free_error as *const ());
            add("zk_llvm_bytes_len", zk_llvm_bytes_len as *const ());
            $(add(stringify!($name), $name as *const ());)*
        }
    };
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zk_llvm_release(handle: Handle) {
    if !handle.is_null() {
        // SAFETY: ownership is returned exactly once by the Lisp handle or finalizer.
        drop(unsafe { Box::from_raw(handle.cast::<Object>()) });
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zk_llvm_free_error(error: *mut c_char) {
    // SAFETY: ownership is returned once for a message allocated by respond.
    drop(unsafe { CString::from_raw(error) });
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zk_llvm_bytes_len(handle: Handle) -> usize {
    unsafe { borrow::<Vec<u8>>(handle) }.map_or(0, Vec::len)
}

fn pass_options(
    flags: u32,
    aa_pipeline: &str,
    opt_cap: i64,
    promotion_cap: i64,
    inliner_threshold: i32,
) -> Result<crate::target::PassOptions<'_>> {
    // The first three bits represent verify-each, debug logging and AA presence.
    // Bits 3..16 encode default, disabled, or enabled in pairs.
    // Bit 17 records the presence of a signed inliner threshold.
    let toggle = |shift: u32| match (flags >> shift) & 3_u32 {
        0 => Ok(None),
        1 => Ok(Some(false)),
        2 => Ok(Some(true)),
        _ => Err(Error::new("invalid LLVM pass option")),
    };
    let cap = |value| {
        (value != -1)
            .then(|| u32::try_from(value).map_err(|_| Error::new("invalid LLVM LICM cap")))
            .transpose()
    };
    Ok(crate::target::PassOptions {
        verify_each: flags & 1 != 0,
        debug_logging: flags & 2 != 0,
        aa_pipeline: (flags & 4 != 0).then_some(aa_pipeline),
        loop_interleaving: toggle(3)?,
        loop_vectorization: toggle(5)?,
        slp_vectorization: toggle(7)?,
        loop_unrolling: toggle(9)?,
        forget_all_scev_in_loop_unroll: toggle(11)?,
        call_graph_profile: toggle(13)?,
        merge_functions: toggle(15)?,
        licm_mssa_opt_cap: cap(opt_cap)?,
        licm_mssa_no_acc_for_promotion_cap: cap(promotion_cap)?,
        inliner_threshold: (flags & (1 << 17) != 0).then_some(inliner_threshold),
    })
}

api! {
    zk_llvm_text_data(value: Handle) {
        Ok(borrow::<CString>(value)?.as_ptr().cast_mut().cast())
    }
    zk_llvm_bytes_data(value: Handle) {
        Ok(borrow::<Vec<u8>>(value)?.as_ptr().cast_mut().cast())
    }
    zk_llvm_handle_alive_p(handle: Handle) -> u64 {
        Ok(handle.cast::<Object>().as_ref().is_some_and(|object| object.check_live().is_ok()).into())
    }
    zk_llvm_make_context() { owned(Context::default()) }
    zk_llvm_make_module(ctx: Handle, name: *const c_char) {
        owned(borrow::<Context>(ctx)?.module(string(name)?)?)
    }
    zk_llvm_make_builder(ctx: Handle) { owned(Builder::new(borrow(ctx)?)) }
    zk_llvm_parse_ir(ctx: Handle, source: *const c_char) {
        owned(borrow::<Context>(ctx)?.parse_ir(string(source)?)?)
    }
    zk_llvm_parse_bitcode(ctx: Handle, data: *const u8, length: usize) {
        owned(borrow::<Context>(ctx)?.parse_bitcode(input(data, length)?)?)
    }
    zk_llvm_read_module(ctx: Handle, path: *const c_char) {
        owned(borrow::<Context>(ctx)?.read_module(std::path::Path::new(string(path)?))?)
    }
    zk_llvm_module_context(module: Handle) { owned(borrow::<Module>(module)?.context().clone()) }
    zk_llvm_module_ir(module: Handle) { text(borrow::<Module>(module)?.ir()) }
    zk_llvm_copy_module(module: Handle) { owned(borrow::<Module>(module)?.copy()?) }
    zk_llvm_module_bitcode(module: Handle) { owned(borrow::<Module>(module)?.bitcode()?) }
    zk_llvm_module_identifier(module: Handle) { text(borrow::<Module>(module)?.identifier()?) }
    zk_llvm_module_set_identifier(module: Handle, name: *const c_char) {
        unit(borrow::<Module>(module)?.set_identifier(string(name)?))
    }
    zk_llvm_module_source_filename(module: Handle) { text(borrow::<Module>(module)?.source_filename()?) }
    zk_llvm_module_set_source_filename(module: Handle, name: *const c_char) {
        borrow::<Module>(module)?.set_source_filename(string(name)?);
        unit(Ok(()))
    }
    zk_llvm_module_inline_asm(module: Handle) { text(borrow::<Module>(module)?.inline_asm()?) }
    zk_llvm_module_set_inline_asm(module: Handle, source: *const c_char) {
        borrow::<Module>(module)?.set_inline_asm(string(source)?);
        unit(Ok(()))
    }
    zk_llvm_module_append_inline_asm(module: Handle, source: *const c_char) {
        borrow::<Module>(module)?.append_inline_asm(string(source)?);
        unit(Ok(()))
    }
    zk_llvm_metadata_string(context: Handle, contents: *const c_char) {
        owned(borrow::<Context>(context)?.metadata_string(string(contents)?))
    }
    zk_llvm_metadata_node(context: Handle, operands: *const Handle, count: usize) {
        let operands = input(operands, count)?.iter().map(|&handle| {
            Ok(nullable::<Metadata>(handle)?.cloned())
        }).collect::<Result<Vec<_>>>()?;
        owned(borrow::<Context>(context)?.metadata_node(&operands)?)
    }
    zk_llvm_value_metadata(value: Handle) { owned(borrow::<Value>(value)?.as_metadata()?) }
    zk_llvm_metadata_ir(metadata: Handle) { text(borrow::<Metadata>(metadata)?.ir()) }
    zk_llvm_metadata_string_value(metadata: Handle) { optional_text(borrow::<Metadata>(metadata)?.string()?) }
    zk_llvm_metadata_integer_value(metadata: Handle) {
        optional_text(borrow::<Metadata>(metadata)?.integer().map(|number| number.to_string()))
    }
    zk_llvm_metadata_operand_count(metadata: Handle) -> u64 {
        Ok(borrow::<Metadata>(metadata)?.operands()?.len() as u64)
    }
    zk_llvm_metadata_operand(metadata: Handle, index: u32) {
        let operands = borrow::<Metadata>(metadata)?.operands()?;
        let operand = operands.into_iter().nth(index as usize)
            .ok_or_else(|| Error::new("LLVM metadata operand index is out of range"))?;
        optional(operand)
    }
    zk_llvm_module_add_named_metadata(module: Handle, name: *const c_char, metadata: Handle) {
        unit(borrow::<Module>(module)?.add_named_metadata(string(name)?, borrow(metadata)?))
    }
    zk_llvm_module_named_metadata_count(module: Handle, name: *const c_char) -> u64 {
        Ok(borrow::<Module>(module)?.named_metadata_count(string(name)?)?.into())
    }
    zk_llvm_module_named_metadata_operand(module: Handle, name: *const c_char, index: u32) {
        optional(borrow::<Module>(module)?.named_metadata(string(name)?)?.into_iter().nth(index as usize))
    }
    zk_llvm_module_flag(module: Handle, name: *const c_char, behavior: *mut u32) {
        let flag = borrow::<Module>(module)?.module_flag(string(name)?)?;
        let Some((mode, value)) = flag else { return Ok(ptr::null_mut()); };
        if behavior.is_null() {
            return Err(Error::new("expected a module flag behavior output slot"));
        }
        behavior.write(mode as u32);
        owned(value)
    }
    zk_llvm_module_add_flag(module: Handle, name: *const c_char, behavior: u32, metadata: Handle) {
        unit(borrow::<Module>(module)?.add_module_flag(
            string(name)?, ModuleFlagBehavior::try_from(behavior)?, borrow(metadata)?))
    }
    zk_llvm_module_strip_debug_info(module: Handle) { owned(borrow::<Module>(module)?.strip_debug_info()?) }
    zk_llvm_verify_module(module: Handle) { unit(borrow::<Module>(module)?.verify()) }
    zk_llvm_link_modules(primary: Handle, inputs: *const Handle, count: usize) {
        owned(borrow::<Module>(primary)?.link(&handles(inputs, count)?)?)
    }
    zk_llvm_int_type(ctx: Handle, bits: u32) { owned(borrow::<Context>(ctx)?.int_type(bits)?) }
    zk_llvm_float_type(ctx: Handle) { owned(borrow::<Context>(ctx)?.float_type()) }
    zk_llvm_double_type(ctx: Handle) { owned(borrow::<Context>(ctx)?.double_type()) }
    zk_llvm_extended_float_type(ctx: Handle, kind: u32) {
        use crate::TypeKind::*;
        let kind = choice(&[LLVMHalfTypeKind, LLVMBFloatTypeKind, LLVMX86_FP80TypeKind,
                            LLVMFP128TypeKind, LLVMPPC_FP128TypeKind], kind)?;
        owned(borrow::<Context>(ctx)?.floating_type(kind)?)
    }
    zk_llvm_void_type(ctx: Handle) { owned(borrow::<Context>(ctx)?.void_type()) }
    zk_llvm_label_type(ctx: Handle) { owned(borrow::<Context>(ctx)?.label_type()) }
    zk_llvm_metadata_type(ctx: Handle) { owned(borrow::<Context>(ctx)?.metadata_type()) }
    zk_llvm_token_type(ctx: Handle) { owned(borrow::<Context>(ctx)?.token_type()) }
    zk_llvm_pointer_type_in_address_space(ctx: Handle, address_space: u32) {
        owned(borrow::<Context>(ctx)?.pointer_type_in(address_space)?)
    }
    zk_llvm_array_type_with_length(ty: Handle, count: u64) {
        owned(borrow::<Type>(ty)?.array_with_length(count)?)
    }
    zk_llvm_vector_type(ty: Handle, count: u32, scalable: u8) {
        owned(borrow::<Type>(ty)?.vector(count, scalable != 0)?)
    }
    zk_llvm_struct_type(ctx: Handle, fields: *const Handle, count: usize, packed: u8) {
        owned(borrow::<Context>(ctx)?.struct_type(&handles(fields, count)?, packed != 0)?)
    }
    zk_llvm_opaque_struct_type(ctx: Handle, name: *const c_char) {
        owned(borrow::<Context>(ctx)?.opaque_struct_type(string(name)?)?)
    }
    zk_llvm_find_struct_type(ctx: Handle, name: *const c_char) {
        optional(borrow::<Context>(ctx)?.find_struct_type(string(name)?)?)
    }
    zk_llvm_set_struct_body(ty: Handle, fields: *const Handle, count: usize, packed: u8) {
        unit(borrow::<Type>(ty)?.set_struct_body(&handles(fields, count)?, packed != 0))
    }
    zk_llvm_function_type(ty: Handle, params: *const Handle, count: usize, variadic: u8) {
        owned(borrow::<Type>(ty)?.function(&handles(params, count)?, variadic != 0)?)
    }
    zk_llvm_type_ir(ty: Handle) { text(borrow::<Type>(ty)?.ir()) }
    zk_llvm_type_context(ty: Handle) { owned(borrow::<Type>(ty)?.context().clone()) }
    zk_llvm_type_eq(left: Handle, right: Handle) -> u64 {
        Ok(borrow::<Type>(left)?.same_as(borrow(right)?).into())
    }
    zk_llvm_type_kind_id(ty: Handle) -> u64 { Ok(borrow::<Type>(ty)?.kind() as u64) }
    zk_llvm_type_is_sized(ty: Handle) -> u64 { Ok(borrow::<Type>(ty)?.is_sized().into()) }
    zk_llvm_type_is_floating_point(ty: Handle) -> u64 {
        Ok(borrow::<Type>(ty)?.is_floating_point().into())
    }
    zk_llvm_type_floating_point_width(ty: Handle) -> u64 {
        Ok(borrow::<Type>(ty)?.floating_point_width()?.into())
    }
    zk_llvm_type_subtype_count(ty: Handle) -> u64 { Ok(borrow::<Type>(ty)?.subtype_count().into()) }
    zk_llvm_scalar_type(ty: Handle) { owned(borrow::<Type>(ty)?.scalar_type()) }
    zk_llvm_type_subtype(ty: Handle, index: u32) { optional(borrow::<Type>(ty)?.subtype(index)) }
    zk_llvm_integer_bit_width(ty: Handle) -> u64 { Ok(borrow::<Type>(ty)?.integer_width()?.into()) }
    zk_llvm_pointer_address_space_id(ty: Handle) -> u64 {
        Ok(borrow::<Type>(ty)?.pointer_address_space()?.into())
    }
    zk_llvm_array_length(ty: Handle) -> u64 { borrow::<Type>(ty)?.array_length() }
    zk_llvm_vector_length(ty: Handle) -> u64 { Ok(borrow::<Type>(ty)?.vector_length()?.into()) }
    zk_llvm_struct_name(ty: Handle) { optional_text(borrow::<Type>(ty)?.struct_name()?) }
    zk_llvm_struct_is_opaque(ty: Handle) -> u64 { Ok(borrow::<Type>(ty)?.is_opaque_struct()?.into()) }
    zk_llvm_struct_is_packed(ty: Handle) -> u64 { Ok(borrow::<Type>(ty)?.is_packed_struct()?.into()) }
    zk_llvm_function_type_is_variadic(ty: Handle) -> u64 {
        Ok(borrow::<Type>(ty)?.is_variadic_function()?.into())
    }
    zk_llvm_value_type(value: Handle) { owned(borrow::<Value>(value)?.ty()) }
    zk_llvm_value_is_constant(value: Handle) -> u64 {
        Ok(borrow::<Value>(value)?.is_constant().into())
    }
    zk_llvm_value_eq(left: Handle, right: Handle) -> u64 {
        Ok(borrow::<Value>(left)?.same_as(borrow(right)?).into())
    }
    zk_llvm_value_ir(value: Handle) { text(borrow::<Value>(value)?.ir()) }
    zk_llvm_value_name(value: Handle) { text(borrow::<Value>(value)?.name()?) }
    zk_llvm_set_value_name(value: Handle, name: *const c_char) {
        unit(borrow::<Value>(value)?.set_name(string(name)?))
    }
    zk_llvm_constant_integer(value: Handle, signed: u8) {
        text(borrow::<Value>(value)?.constant_integer(signed != 0)?.to_string())
    }
    zk_llvm_const_int(ty: Handle, number: *const c_char) {
        let number: BigInt = string(number)?.parse().map_err(|_| Error::new("invalid integer"))?;
        owned(borrow::<Type>(ty)?.const_int(&number)?)
    }
    zk_llvm_const_float(ty: Handle, number: f64) { owned(borrow::<Type>(ty)?.const_float(number)?) }
    zk_llvm_const_float_from_string(ty: Handle, literal: *const c_char) {
        owned(borrow::<Type>(ty)?.const_float_from_string(string(literal)?)?)
    }
    zk_llvm_const_zero(ty: Handle) { owned(borrow::<Type>(ty)?.const_zero()?) }
    zk_llvm_const_undef(ty: Handle) { owned(borrow::<Type>(ty)?.const_undef()?) }
    zk_llvm_const_poison(ty: Handle) { owned(borrow::<Type>(ty)?.const_poison()?) }
    zk_llvm_const_bytes(ctx: Handle, bytes: *const u8, len: usize, terminated: u8) {
        owned(borrow::<Context>(ctx)?.const_bytes(input(bytes, len)?, terminated != 0))
    }
    zk_llvm_const_array(ty: Handle, values: *const Handle, count: usize) {
        owned(borrow::<Type>(ty)?.const_array(&handles(values, count)?)?)
    }
    zk_llvm_const_struct(ty: Handle, fields: *const Handle, count: usize) {
        owned(borrow::<Type>(ty)?.const_struct(&handles(fields, count)?)?)
    }
    zk_llvm_const_vector(ty: Handle, elements: *const Handle, count: usize) {
        owned(borrow::<Type>(ty)?.const_vector(&handles(elements, count)?)?)
    }
    zk_llvm_const_splat(ty: Handle, element: Handle) {
        owned(borrow::<Type>(ty)?.const_splat(borrow(element)?)?)
    }
    zk_llvm_const_gep(ty: Handle, address: Handle, indices: *const Handle, count: usize, in_bounds: u8) {
        owned(borrow::<Type>(ty)?.const_gep(borrow(address)?, &handles(indices, count)?, in_bounds != 0)?)
    }
    zk_llvm_add_global(module: Handle, name: *const c_char, storage_type: Handle, initializer: Handle, constant: u8, link: u32) {
        owned(borrow::<Module>(module)?.add_global(string(name)?, borrow(storage_type)?, GlobalOptions {
            initializer: nullable(initializer)?, constant: constant != 0, linkage: linkage(link)?,
        })?)
    }
    zk_llvm_add_global_in_address_space(module: Handle, name: *const c_char, storage_type: Handle,
                                        initializer: Handle, constant: u8, link: u32, address_space: u32) {
        owned(borrow::<Module>(module)?.add_global_in(string(name)?, borrow(storage_type)?, address_space,
            GlobalOptions { initializer: nullable(initializer)?, constant: constant != 0, linkage: linkage(link)? })?)
    }
    zk_llvm_add_global_string(module: Handle, name: *const c_char, bytes: *const u8, len: usize, nul_terminated: u8) {
        owned(borrow::<Module>(module)?.add_global_string(string(name)?, input(bytes, len)?, nul_terminated != 0)?)
    }
    zk_llvm_find_global(module: Handle, name: *const c_char) {
        optional(borrow::<Module>(module)?.find_global(string(name)?)?)
    }
    zk_llvm_first_global(module: Handle) { optional(borrow::<Module>(module)?.first_global()) }
    zk_llvm_next_global(global: Handle) { optional(borrow::<Global>(global)?.next_global()) }
    zk_llvm_global_name(global: Handle) { text(borrow::<Global>(global)?.name()?) }
    zk_llvm_global_eq(left: Handle, right: Handle) -> u64 {
        Ok(borrow::<Global>(left)?.same_as(borrow(right)?).into())
    }
    zk_llvm_set_global_name(global: Handle, name: *const c_char) {
        unit(borrow::<Global>(global)?.set_name(string(name)?))
    }
    zk_llvm_global_storage_type(global: Handle) { owned(borrow::<Global>(global)?.storage_type()?) }
    zk_llvm_global_address(global: Handle) { owned(borrow::<Global>(global)?.address()) }
    zk_llvm_add_global_alias(module: Handle, name: *const c_char, value_type: Handle, target: Handle, link: u32) {
        owned(borrow::<Module>(module)?.add_global_alias(string(name)?, borrow(value_type)?, borrow(target)?, linkage(link)?)?)
    }
    zk_llvm_find_global_alias(module: Handle, name: *const c_char) {
        optional(borrow::<Module>(module)?.find_global_alias(string(name)?)?)
    }
    zk_llvm_first_global_alias(module: Handle) { optional(borrow::<Module>(module)?.first_global_alias()) }
    zk_llvm_next_global_alias(alias: Handle) { optional(borrow::<GlobalAlias>(alias)?.next_global_alias()) }
    zk_llvm_global_alias_eq(left: Handle, right: Handle) -> u64 {
        Ok(borrow::<GlobalAlias>(left)?.same_as(borrow(right)?).into())
    }
    zk_llvm_global_alias_module(alias: Handle) { owned(borrow::<GlobalAlias>(alias)?.module().clone()) }
    zk_llvm_global_alias_name(alias: Handle) { text(borrow::<GlobalAlias>(alias)?.name()?) }
    zk_llvm_set_global_alias_name(alias: Handle, name: *const c_char) {
        unit(borrow::<GlobalAlias>(alias)?.set_name(string(name)?))
    }
    zk_llvm_global_alias_value_type(alias: Handle) { owned(borrow::<GlobalAlias>(alias)?.value_type()) }
    zk_llvm_global_alias_address(alias: Handle) { owned(borrow::<GlobalAlias>(alias)?.address()) }
    zk_llvm_global_alias_address_space(alias: Handle) -> u64 {
        Ok(borrow::<GlobalAlias>(alias)?.address_space().into())
    }
    zk_llvm_global_alias_target(alias: Handle) { owned(borrow::<GlobalAlias>(alias)?.target()) }
    zk_llvm_set_global_alias_target(alias: Handle, target: Handle) {
        unit(borrow::<GlobalAlias>(alias)?.set_target(borrow(target)?))
    }
    zk_llvm_global_alias_linkage(alias: Handle) -> u64 { linkage_index(borrow::<GlobalAlias>(alias)?.linkage()) }
    zk_llvm_global_is_constant(global: Handle) -> u64 { Ok(borrow::<Global>(global)?.is_constant().into()) }
    zk_llvm_global_initializer(global: Handle) { optional(borrow::<Global>(global)?.initializer()?) }
    zk_llvm_set_global_initializer(global: Handle, value: Handle) {
        unit(borrow::<Global>(global)?.set_initializer(borrow(value)?))
    }
    zk_llvm_global_alignment(global: Handle) -> u64 { Ok(borrow::<Global>(global)?.alignment()) }
    zk_llvm_set_global_alignment(global: Handle, alignment: u64) {
        unit(borrow::<Global>(global)?.set_alignment(alignment))
    }
    zk_llvm_global_linkage(global: Handle) -> u64 {
        linkage_index(borrow::<Global>(global)?.linkage())
    }
    zk_llvm_set_global_linkage(global: Handle, link: u32) {
        unit(borrow::<Global>(global)?.set_linkage(linkage(link)?))
    }
    zk_llvm_global_unnamed_address(global: Handle) -> u64 {
        let address = borrow::<Global>(global)?.unnamed_address();
        UNNAMED_ADDRESSES.iter().position(|&candidate| candidate == address).map(|index| index as u64)
            .ok_or_else(|| Error::new("unrecognized LLVM unnamed address mode"))
    }
    zk_llvm_set_global_unnamed_address(global: Handle, address: u32) {
        borrow::<Global>(global)?.set_unnamed_address(choice(UNNAMED_ADDRESSES, address)?);
        unit(Ok(()))
    }
    zk_llvm_global_is_declaration(global: Handle) -> u64 {
        Ok(borrow::<Global>(global)?.is_declaration().into())
    }
    zk_llvm_global_address_space(global: Handle) -> u64 {
        Ok(borrow::<Global>(global)?.address_space().into())
    }
    zk_llvm_set_global_constant(global: Handle, constant: u8) {
        unit(borrow::<Global>(global)?.set_constant(constant != 0))
    }
    zk_llvm_clear_global_initializer(global: Handle) { unit(borrow::<Global>(global)?.clear_initializer()) }
    zk_llvm_global_is_externally_initialized(global: Handle) -> u64 {
        Ok(borrow::<Global>(global)?.is_externally_initialized().into())
    }
    zk_llvm_set_global_externally_initialized(global: Handle, initialized: u8) {
        borrow::<Global>(global)?.set_externally_initialized(initialized != 0);
        unit(Ok(()))
    }
    zk_llvm_global_thread_local_mode(global: Handle) -> u64 {
        choice_index(TLS_MODES, borrow::<Global>(global)?.thread_local_mode())
    }
    zk_llvm_set_global_thread_local_mode(global: Handle, mode: u32) {
        borrow::<Global>(global)?.set_thread_local_mode(choice(TLS_MODES, mode)?);
        unit(Ok(()))
    }
    zk_llvm_global_visibility(global: Handle) -> u64 {
        choice_index(VISIBILITIES, borrow::<Global>(global)?.visibility())
    }
    zk_llvm_set_global_visibility(global: Handle, visibility: u32) {
        unit(borrow::<Global>(global)?.set_visibility(choice(VISIBILITIES, visibility)?))
    }
    zk_llvm_global_dll_storage_class(global: Handle) -> u64 {
        choice_index(DLL_STORAGE_CLASSES, borrow::<Global>(global)?.dll_storage_class())
    }
    zk_llvm_set_global_dll_storage_class(global: Handle, class: u32) {
        unit(borrow::<Global>(global)?.set_dll_storage_class(choice(DLL_STORAGE_CLASSES, class)?))
    }
    zk_llvm_global_section(global: Handle) { optional_text(borrow::<Global>(global)?.section()?) }
    zk_llvm_set_global_section(global: Handle, name: *const c_char, present: u8) {
        let name = (present != 0).then(|| string(name)).transpose()?;
        unit(borrow::<Global>(global)?.set_section(name))
    }
    zk_llvm_global_comdat(global: Handle) { optional(borrow::<Global>(global)?.comdat()) }
    zk_llvm_set_global_comdat(global: Handle, comdat: Handle) {
        unit(borrow::<Global>(global)?.set_comdat(nullable(comdat)?))
    }
    zk_llvm_get_or_insert_comdat(module: Handle, name: *const c_char) {
        owned(borrow::<Module>(module)?.get_or_insert_comdat(string(name)?)?)
    }
    zk_llvm_comdat_selection_kind(comdat: Handle) -> u64 {
        choice_index(COMDAT_KINDS, borrow::<Comdat>(comdat)?.selection_kind())
    }
    zk_llvm_set_comdat_selection_kind(comdat: Handle, kind: u32) {
        borrow::<Comdat>(comdat)?.set_selection_kind(choice(COMDAT_KINDS, kind)?);
        unit(Ok(()))
    }
    zk_llvm_add_function_with_linkage(module: Handle, name: *const c_char, ty: Handle, link: u32) {
        owned(borrow::<Module>(module)?.add_function_with_linkage(string(name)?, borrow(ty)?, linkage(link)?)?)
    }
    zk_llvm_declare_function(module: Handle, name: *const c_char, result: Handle,
                             parameters: *const Handle, count: usize, variadic: u8, link: u32) {
        let signature = borrow::<Type>(result)?.function(&handles(parameters, count)?, variadic != 0)?;
        let link = (link != u32::MAX).then(|| linkage(link)).transpose()?;
        owned(borrow::<Module>(module)?.declare_function(string(name)?, &signature, link)?)
    }
    zk_llvm_find_function(module: Handle, name: *const c_char) {
        optional(borrow::<Module>(module)?.find_function(string(name)?)?)
    }
    zk_llvm_declare_intrinsic(module: Handle, name: *const c_char, signature: Handle) {
        owned(borrow::<Module>(module)?.declare_intrinsic(string(name)?, borrow(signature)?)?)
    }
    zk_llvm_function_linkage(function: Handle) -> u64 {
        linkage_index(borrow::<Function>(function)?.linkage())
    }
    zk_llvm_set_function_linkage(function: Handle, link: u32) {
        unit(borrow::<Function>(function)?.set_linkage(linkage(link)?))
    }
    zk_llvm_function_is_declaration(function: Handle) -> u64 {
        Ok(borrow::<Function>(function)?.is_declaration().into())
    }
    zk_llvm_function_visibility(function: Handle) -> u64 {
        choice_index(VISIBILITIES, borrow::<Function>(function)?.visibility())
    }
    zk_llvm_set_function_visibility(function: Handle, visibility: u32) {
        unit(borrow::<Function>(function)?.set_visibility(choice(VISIBILITIES, visibility)?))
    }
    zk_llvm_function_dll_storage_class(function: Handle) -> u64 {
        choice_index(DLL_STORAGE_CLASSES, borrow::<Function>(function)?.dll_storage_class())
    }
    zk_llvm_set_function_dll_storage_class(function: Handle, class: u32) {
        unit(borrow::<Function>(function)?.set_dll_storage_class(choice(DLL_STORAGE_CLASSES, class)?))
    }
    zk_llvm_function_section(function: Handle) { optional_text(borrow::<Function>(function)?.section()?) }
    zk_llvm_set_function_section(function: Handle, name: *const c_char, present: u8) {
        let name = (present != 0).then(|| string(name)).transpose()?;
        unit(borrow::<Function>(function)?.set_section(name))
    }
    zk_llvm_function_comdat(function: Handle) { optional(borrow::<Function>(function)?.comdat()) }
    zk_llvm_set_function_comdat(function: Handle, comdat: Handle) {
        unit(borrow::<Function>(function)?.set_comdat(nullable(comdat)?))
    }
    zk_llvm_first_function(module: Handle) { optional(borrow::<Module>(module)?.first_function()) }
    zk_llvm_next_function(function: Handle) { optional(borrow::<Function>(function)?.next_function()) }
    zk_llvm_function_name(function: Handle) { text(borrow::<Function>(function)?.name()?) }
    zk_llvm_function_eq(left: Handle, right: Handle) -> u64 {
        Ok(borrow::<Function>(left)?.same_as(borrow(right)?).into())
    }
    zk_llvm_set_function_name(function: Handle, name: *const c_char) {
        unit(borrow::<Function>(function)?.set_name(string(name)?))
    }
    zk_llvm_function_unnamed_address(function: Handle) -> u64 {
        choice_index(UNNAMED_ADDRESSES, borrow::<Function>(function)?.unnamed_address())
    }
    zk_llvm_set_function_unnamed_address(function: Handle, address: u32) {
        borrow::<Function>(function)?.set_unnamed_address(choice(UNNAMED_ADDRESSES, address)?);
        unit(Ok(()))
    }
    zk_llvm_function_signature(function: Handle) { owned(borrow::<Function>(function)?.signature()) }
    zk_llvm_function_address(function: Handle) { owned(borrow::<Function>(function)?.address()?) }
    zk_llvm_function_calling_convention_id(function: Handle) -> u64 {
        Ok(borrow::<Function>(function)?.calling_convention().into())
    }
    zk_llvm_set_function_calling_convention(function: Handle, convention: u32) {
        unit(borrow::<Function>(function)?.set_calling_convention(convention))
    }
    zk_llvm_set_function_attribute(function: Handle, index: u32, name: *const c_char,
                                   payload: u32, number: u64, ty: Handle, contents: *const c_char) {
        unit(borrow::<Function>(function)?.set_attribute(
            attribute_location(index), string(name)?, attribute_value(payload, number, ty, contents)?))
    }
    zk_llvm_function_has_attribute(function: Handle, index: u32, name: *const c_char, is_string: u8) -> u64 {
        Ok(borrow::<Function>(function)?.has_attribute(attribute_location(index), string(name)?, is_string != 0)?.into())
    }
    zk_llvm_function_attribute(function: Handle, index: u32, name: *const c_char, is_string: u8) {
        optional(borrow::<Function>(function)?.attribute(attribute_location(index), string(name)?, is_string != 0)?)
    }
    zk_llvm_function_attributes(function: Handle, index: u32) {
        owned(borrow::<Function>(function)?.attributes(attribute_location(index))?)
    }
    zk_llvm_remove_function_attribute(function: Handle, index: u32, name: *const c_char, is_string: u8) {
        unit(borrow::<Function>(function)?.remove_attribute(attribute_location(index), string(name)?, is_string != 0))
    }
    zk_llvm_parameter(function: Handle, index: u32) { optional(borrow::<Function>(function)?.parameter(index)?) }
    zk_llvm_function_module(function: Handle) { owned(borrow::<Function>(function)?.module().clone()) }
    zk_llvm_named_parameter(function: Handle, index: u32, name: *const c_char) {
        owned(borrow::<Function>(function)?.named_parameter(index, string(name)?)?)
    }
    zk_llvm_begin_function_body(function: Handle) { owned(Builder::begin_function_body(borrow(function)?)?) }
    zk_llvm_begin_named_function_body(module: Handle, name: *const c_char, result: Handle,
                                      parameters: *const Handle, count: usize, variadic: u8, link: u32) {
        let signature = borrow::<Type>(result)?.function(&handles(parameters, count)?, variadic != 0)?;
        let link = (link != u32::MAX).then(|| linkage(link)).transpose()?;
        owned(Builder::begin_named_function_body(borrow(module)?, string(name)?, &signature, link)?)
    }
    zk_llvm_body_function(builder: Handle) { owned(borrow::<Builder>(builder)?.body_function()?) }
    zk_llvm_finish_function_body(builder: Handle) {
        unit(borrow::<Builder>(builder)?.finish_function_body())
    }
    zk_llvm_append_block(function: Handle, name: *const c_char) {
        owned(borrow::<Function>(function)?.append_block(string(name)?)?)
    }
    zk_llvm_insert_block_before(function: Handle, anchor: Handle, name: *const c_char) {
        owned(borrow::<Function>(function)?.insert_block_before(borrow(anchor)?, string(name)?)?)
    }
    zk_llvm_first_block(function: Handle) { optional(borrow::<Function>(function)?.first_block()) }
    zk_llvm_next_block(block: Handle) { optional(borrow::<Block>(block)?.next_block()) }
    zk_llvm_previous_block(block: Handle) { optional(borrow::<Block>(block)?.previous_block()) }
    zk_llvm_block_name(block: Handle) { text(borrow::<Block>(block)?.name()?) }
    zk_llvm_set_block_name(block: Handle, name: *const c_char) {
        unit(borrow::<Block>(block)?.set_name(string(name)?))
    }
    zk_llvm_block_function(block: Handle) { owned(borrow::<Block>(block)?.function().clone()) }
    zk_llvm_block_eq(left: Handle, right: Handle) -> u64 {
        Ok(borrow::<Block>(left)?.same_as(borrow(right)?).into())
    }
    zk_llvm_block_is_terminated(block: Handle) -> u64 {
        Ok(borrow::<Block>(block)?.is_terminated().into())
    }
    zk_llvm_block_first_instruction(block: Handle) {
        optional(borrow::<Block>(block)?.first_instruction())
    }
    zk_llvm_block_terminator(block: Handle) { optional(borrow::<Block>(block)?.terminator()) }
    zk_llvm_block_last_instruction(block: Handle) { optional(borrow::<Block>(block)?.last_instruction()) }
    zk_llvm_instruction_next(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.next_instruction())
    }
    zk_llvm_instruction_previous(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.previous_instruction())
    }
    zk_llvm_instruction_opcode(instruction: Handle) {
        text(borrow::<Instruction>(instruction)?.opcode().to_owned())
    }
    zk_llvm_instruction_ir(instruction: Handle) { text(borrow::<Instruction>(instruction)?.ir()) }
    zk_llvm_instruction_eq(left: Handle, right: Handle) -> u64 {
        Ok(borrow::<Instruction>(left)?.same_as(borrow(right)?).into())
    }
    zk_llvm_instruction_name(instruction: Handle) { text(borrow::<Instruction>(instruction)?.name()?) }
    zk_llvm_set_instruction_name(instruction: Handle, name: *const c_char) {
        unit(borrow::<Instruction>(instruction)?.set_name(string(name)?))
    }
    zk_llvm_instruction_icmp_predicate(instruction: Handle) -> u64 {
        predicate_index(INT_PREDICATES, borrow::<Instruction>(instruction)?.icmp_predicate())
    }
    zk_llvm_instruction_fcmp_predicate(instruction: Handle) -> u64 {
        predicate_index(FLOAT_PREDICATES, borrow::<Instruction>(instruction)?.fcmp_predicate())
    }
    zk_llvm_instruction_fast_math_flags(instruction: Handle) -> u64 {
        let flags = borrow::<Instruction>(instruction)?.fast_math_flags();
        Ok(u64::from(flags.map_or(u32::MAX, |flags| flags.bits())))
    }
    zk_llvm_add_instruction_fast_math_flags(instruction: Handle, flags: u32) {
        unit(borrow::<Instruction>(instruction)?.add_fast_math_flags(fast_math_flags(flags)?))
    }
    zk_llvm_instruction_int_flags(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.int_flags().map_or(u64::MAX, |flags| {
            u64::from(flags.nsw) | (u64::from(flags.nuw) << 1) | (u64::from(flags.exact) << 2)
        }))
    }
    zk_llvm_set_instruction_int_flags(instruction: Handle, flags: u32) {
        if flags & !0b111 != 0 {
            return Err(Error::new("invalid LLVM integer flags"));
        }
        unit(borrow::<Instruction>(instruction)?.set_int_flags(IntFlags {
            nsw: flags & 0b001 != 0,
            nuw: flags & 0b010 != 0,
            exact: flags & 0b100 != 0,
        }))
    }
    zk_llvm_instruction_block(instruction: Handle) {
        owned(borrow::<Instruction>(instruction)?.block().clone())
    }
    zk_llvm_instruction_context(instruction: Handle) {
        let function = borrow::<Instruction>(instruction)?.block().function();
        owned(function.module().context().clone())
    }
    zk_llvm_instruction_tail_call_kind(instruction: Handle) -> u64 {
        let kind = borrow::<Instruction>(instruction)?.tail_call_kind()?;
        Ok(TAIL_CALL_KINDS.iter().position(|&candidate| candidate == kind).expect("LLVM tail call kind") as u64)
    }
    zk_llvm_set_instruction_tail_call_kind(instruction: Handle, kind: u32) {
        unit(borrow::<Instruction>(instruction)?.set_tail_call_kind(choice(TAIL_CALL_KINDS, kind)?))
    }
    zk_llvm_call_calling_convention_id(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.call_calling_convention()?.into())
    }
    zk_llvm_set_call_calling_convention(instruction: Handle, convention: u32) {
        unit(borrow::<Instruction>(instruction)?.set_call_calling_convention(convention))
    }
    zk_llvm_set_call_attribute(instruction: Handle, index: u32, name: *const c_char,
                               payload: u32, number: u64, ty: Handle, contents: *const c_char) {
        unit(borrow::<Instruction>(instruction)?.set_call_attribute(
            attribute_location(index), string(name)?, attribute_value(payload, number, ty, contents)?))
    }
    zk_llvm_call_has_attribute(instruction: Handle, index: u32, name: *const c_char, is_string: u8) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.has_call_attribute(attribute_location(index), string(name)?, is_string != 0)?.into())
    }
    zk_llvm_call_attribute(instruction: Handle, index: u32, name: *const c_char, is_string: u8) {
        optional(borrow::<Instruction>(instruction)?.call_attribute(attribute_location(index), string(name)?, is_string != 0)?)
    }
    zk_llvm_call_attributes(instruction: Handle, index: u32) {
        owned(borrow::<Instruction>(instruction)?.call_attributes(attribute_location(index))?)
    }
    zk_llvm_remove_call_attribute(instruction: Handle, index: u32, name: *const c_char, is_string: u8) {
        unit(borrow::<Instruction>(instruction)?.remove_call_attribute(attribute_location(index), string(name)?, is_string != 0))
    }
    zk_llvm_attribute_count(attributes: Handle) -> u64 { Ok(borrow::<Vec<AttributeInfo>>(attributes)?.len() as u64) }
    zk_llvm_attribute_at(attributes: Handle, index: usize) {
        let attribute = borrow::<Vec<AttributeInfo>>(attributes)?.get(index)
            .ok_or_else(|| Error::new("LLVM attribute index is out of range"))?;
        owned(attribute.clone())
    }
    zk_llvm_attribute_name(attribute: Handle) {
        optional(borrow::<AttributeInfo>(attribute)?.name.as_ref().map(|name| name.as_bytes().to_vec()))
    }
    zk_llvm_attribute_kind_id(attribute: Handle) -> u64 {
        Ok(borrow::<AttributeInfo>(attribute)?.kind_id.unwrap_or_default().into())
    }
    zk_llvm_attribute_payload(attribute: Handle) -> u64 {
        use AttributeData::*;
        Ok(match &borrow::<AttributeInfo>(attribute)?.value {
            Flag => 0, Integer(_) => 1, Type(_) => 2, String(_) => 3, Opaque => 4,
            Range { .. } => 5,
        })
    }
    zk_llvm_attribute_integer(attribute: Handle) -> u64 {
        match &borrow::<AttributeInfo>(attribute)?.value {
            AttributeData::Integer(value) => Ok(*value),
            _ => Err(Error::new("expected an LLVM integer attribute")),
        }
    }
    zk_llvm_attribute_type(attribute: Handle) {
        match &borrow::<AttributeInfo>(attribute)?.value {
            AttributeData::Type(value) => owned(value.clone()),
            _ => Err(Error::new("expected an LLVM type attribute")),
        }
    }
    zk_llvm_attribute_string(attribute: Handle) {
        match &borrow::<AttributeInfo>(attribute)?.value {
            AttributeData::String(value) => owned(value.as_bytes().to_vec()),
            _ => Err(Error::new("expected an LLVM string attribute")),
        }
    }
    zk_llvm_attribute_range_bit_width(attribute: Handle) -> u64 {
        match &borrow::<AttributeInfo>(attribute)?.value {
            AttributeData::Range { bit_width, .. } => Ok((*bit_width).into()),
            _ => Err(Error::new("expected an LLVM range attribute")),
        }
    }
    zk_llvm_attribute_range_bound(attribute: Handle, upper_bound: u8) {
        match &borrow::<AttributeInfo>(attribute)?.value {
            AttributeData::Range { lower, upper, .. } => {
                let bound = if upper_bound == 0 { lower } else { upper };
                owned(bound.to_string().into_bytes())
            }
            _ => Err(Error::new("expected an LLVM range attribute")),
        }
    }
    zk_llvm_instruction_result(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.result())
    }
    zk_llvm_value_kind(value: Handle) {
        use crate::ValueKind::*;
        let value = borrow::<Value>(value)?;
        let Some(kind) = value.kind() else {
            return text(if value.is_constant() { "opaque-constant" } else { "unknown" }.to_owned());
        };
        let kind = match kind {
            LLVMArgumentValueKind => "argument",
            LLVMBasicBlockValueKind => "basic-block",
            LLVMMemoryUseValueKind => "memory-use",
            LLVMMemoryDefValueKind => "memory-def",
            LLVMMemoryPhiValueKind => "memory-phi",
            LLVMFunctionValueKind => "function",
            LLVMGlobalAliasValueKind => "global-alias",
            LLVMGlobalIFuncValueKind => "global-ifunc",
            LLVMGlobalVariableValueKind => "global-variable",
            LLVMBlockAddressValueKind => "block-address",
            LLVMConstantExprValueKind => "constant-expr",
            LLVMConstantArrayValueKind => "constant-array",
            LLVMConstantStructValueKind => "constant-struct",
            LLVMConstantVectorValueKind => "constant-vector",
            LLVMUndefValueValueKind => "undef",
            LLVMConstantAggregateZeroValueKind => "constant-aggregate-zero",
            LLVMConstantDataArrayValueKind => "constant-data-array",
            LLVMConstantDataVectorValueKind => "constant-data-vector",
            LLVMConstantIntValueKind => "constant-int",
            LLVMConstantFPValueKind => "constant-fp",
            LLVMConstantPointerNullValueKind => "constant-pointer-null",
            LLVMConstantTokenNoneValueKind => "constant-token-none",
            LLVMMetadataAsValueValueKind => "metadata-as-value",
            LLVMInlineAsmValueKind => "inline-asm",
            LLVMInstructionValueKind => "instruction",
            LLVMPoisonValueKind => "poison",
            LLVMConstantTargetNoneValueKind => "constant-target-none",
            LLVMConstantPtrAuthValueKind => "constant-ptr-auth",
        };
        text(kind.to_owned())
    }
    zk_llvm_value_instruction(value: Handle) { optional(borrow::<Value>(value)?.as_instruction()) }
    zk_llvm_value_block(value: Handle) { optional(borrow::<Value>(value)?.as_block()) }
    zk_llvm_value_function(value: Handle) { optional(borrow::<Value>(value)?.as_function()) }
    zk_llvm_value_argument_function(value: Handle) {
        optional(borrow::<Value>(value)?.argument_function())
    }
    zk_llvm_value_argument_index(value: Handle) -> u64 {
        Ok(borrow::<Value>(value)?.argument_index().map_or(u64::MAX, u64::from))
    }
    zk_llvm_value_global(value: Handle) { optional(borrow::<Value>(value)?.as_global()) }
    zk_llvm_value_global_alias(value: Handle) { optional(borrow::<Value>(value)?.as_global_alias()) }
    zk_llvm_value_operand_count(value: Handle) -> u64 {
        Ok(borrow::<Value>(value)?.operand_count().into())
    }
    zk_llvm_value_operand(value: Handle, index: u32) {
        optional(borrow::<Value>(value)?.operand_at(index))
    }
    zk_llvm_value_uses(value: Handle, module: Handle) {
        owned(borrow::<Value>(value)?.uses(nullable(module)?)?)
    }
    zk_llvm_value_use_count(uses: Handle) -> u64 {
        Ok(borrow::<Vec<ValueUse>>(uses)?.len() as u64)
    }
    zk_llvm_value_use_user(uses: Handle, index: usize) {
        let usage = borrow::<Vec<ValueUse>>(uses)?.get(index)
            .ok_or_else(|| Error::new("LLVM value use index is out of range"))?;
        owned(usage.user.clone())
    }
    zk_llvm_value_use_operand_index(uses: Handle, index: usize) -> u64 {
        let usage = borrow::<Vec<ValueUse>>(uses)?.get(index)
            .ok_or_else(|| Error::new("LLVM value use index is out of range"))?;
        Ok(usage.operand_index.into())
    }
    zk_llvm_block_successors(block: Handle) {
        owned(borrow::<Block>(block)?.successors().collect::<Vec<_>>())
    }
    zk_llvm_block_predecessors(block: Handle) {
        owned(borrow::<Block>(block)?.predecessors().collect::<Vec<_>>())
    }
    zk_llvm_function_reverse_postorder(function: Handle) {
        owned(borrow::<Function>(function)?.reverse_postorder())
    }
    zk_llvm_block_list_count(blocks: Handle) -> u64 {
        Ok(borrow::<Vec<Block>>(blocks)?.len() as u64)
    }
    zk_llvm_block_list_at(blocks: Handle, index: usize) {
        optional(borrow::<Vec<Block>>(blocks)?.get(index).cloned())
    }
    zk_llvm_call_target(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.call_target())
    }
    zk_llvm_call_signature(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.call_signature())
    }
    zk_llvm_call_argument_count(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.call_argument_count().map_or(u64::MAX, u64::from))
    }
    zk_llvm_call_argument(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.call_argument(index))
    }
    zk_llvm_call_operand_bundle_count(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.call_operand_bundle_count().map_or(u64::MAX, u64::from))
    }
    zk_llvm_call_operand_bundle(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.call_operand_bundle(index))
    }
    zk_llvm_make_operand_bundle(tag: *const u8, tag_len: usize, operands: *const Handle, count: usize) {
        owned(crate::OperandBundle::new(input(tag, tag_len)?, handles(operands, count)?))
    }
    zk_llvm_operand_bundle_tag(bundle: Handle) {
        owned(borrow::<crate::OperandBundle>(bundle)?.tag().to_vec())
    }
    zk_llvm_operand_bundle_operand_count(bundle: Handle) -> u64 {
        Ok(borrow::<crate::OperandBundle>(bundle)?.operands().len() as u64)
    }
    zk_llvm_operand_bundle_operand(bundle: Handle, index: usize) {
        optional(borrow::<crate::OperandBundle>(bundle)?.operands().get(index).cloned())
    }
    zk_llvm_gep_pointer(instruction: Handle) { optional(borrow::<Instruction>(instruction)?.gep_pointer()) }
    zk_llvm_gep_source_type(instruction: Handle) { optional(borrow::<Instruction>(instruction)?.gep_source_type()) }
    zk_llvm_gep_index_count(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.gep_index_count().map_or(u64::MAX, u64::from))
    }
    zk_llvm_gep_index(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.gep_index(index))
    }
    zk_llvm_gep_is_in_bounds(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.gep_in_bounds().unwrap_or(false).into())
    }
    zk_llvm_instruction_gep_flags(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.gep_flags().map_or(u64::MAX, |flags| flags.bits().into()))
    }
    zk_llvm_set_instruction_gep_flags(instruction: Handle, flags: u32) {
        if flags & !GepFlags::ALL_BITS != 0 {
            return Err(Error::new("invalid LLVM GEP flags"));
        }
        unit(borrow::<Instruction>(instruction)?.set_gep_flags(GepFlags::from_bits(flags)))
    }
    zk_llvm_load_pointer(instruction: Handle) { optional(borrow::<Instruction>(instruction)?.load_pointer()) }
    zk_llvm_store_pointer(instruction: Handle) { optional(borrow::<Instruction>(instruction)?.store_pointer()) }
    zk_llvm_stored_value(instruction: Handle) { optional(borrow::<Instruction>(instruction)?.stored_value()) }
    zk_llvm_alloca_allocated_type(instruction: Handle) { optional(borrow::<Instruction>(instruction)?.allocated_type()) }
    zk_llvm_alloca_element_count(instruction: Handle) { optional(borrow::<Instruction>(instruction)?.alloca_count()) }
    zk_llvm_instruction_is_volatile(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.is_volatile().unwrap_or(false).into())
    }
    zk_llvm_instruction_icmp_is_same_sign(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.icmp_same_sign().unwrap_or(false).into())
    }
    zk_llvm_set_instruction_icmp_same_sign(instruction: Handle, same_sign: u8) {
        unit(borrow::<Instruction>(instruction)?.set_icmp_same_sign(same_sign != 0))
    }
    zk_llvm_instruction_operand_count(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.operand_count().into())
    }
    zk_llvm_instruction_operand(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.operand(index))
    }
    zk_llvm_instruction_successor_count(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.successor_count().into())
    }
    zk_llvm_instruction_successor(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.successor(index))
    }
    zk_llvm_instruction_switch_case_count(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.switch_case_count().into())
    }
    zk_llvm_instruction_switch_case_value(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.switch_case(index).map(|(key, _)| key))
    }
    zk_llvm_instruction_switch_case_block(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.switch_case(index).map(|(_, block)| block))
    }
    zk_llvm_instruction_phi_incoming_block(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.phi_incoming_block(index))
    }
    zk_llvm_instruction_replace_operand(instruction: Handle, index: u32, value: Handle) {
        unit(borrow::<Instruction>(instruction)?.replace_operand(index, borrow(value)?))
    }
    zk_llvm_instruction_alignment(instruction: Handle) -> u64 {
        borrow::<Instruction>(instruction)?.alignment()
    }
    zk_llvm_set_instruction_alignment(instruction: Handle, alignment: u64) {
        unit(borrow::<Instruction>(instruction)?.set_alignment(alignment))
    }
    zk_llvm_begin_edit(module: Handle) { owned(borrow::<Module>(module)?.edit()?) }
    zk_llvm_edit_draft_instruction(edit: Handle, instruction: Handle) {
        owned(borrow::<Edit>(edit)?.draft_instruction(borrow(instruction)?)?)
    }
    zk_llvm_edit_draft_value(edit: Handle, value: Handle) {
        owned(borrow::<Edit>(edit)?.draft_value(borrow(value)?)?)
    }
    zk_llvm_edit_replace_operand(edit: Handle, instruction: Handle, index: u32, value: Handle) {
        unit(borrow::<Edit>(edit)?.replace_operand(borrow(instruction)?, index, borrow(value)?))
    }
    zk_llvm_edit_replace_all_uses(edit: Handle, source: Handle, replacement: Handle) -> u64 {
        borrow::<Edit>(edit)?.replace_all_uses(borrow(source)?, borrow(replacement)?)
    }
    zk_llvm_edit_move_before(edit: Handle, instruction: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.move_before(borrow(instruction)?, borrow(anchor)?))
    }
    zk_llvm_edit_move_after(edit: Handle, instruction: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.move_after(borrow(instruction)?, borrow(anchor)?))
    }
    zk_llvm_edit_detach_instruction(edit: Handle, instruction: Handle) {
        unit(borrow::<Edit>(edit)?.detach_instruction(borrow(instruction)?))
    }
    zk_llvm_edit_insert_instruction_before(edit: Handle, instruction: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.insert_instruction_before(borrow(instruction)?, borrow(anchor)?))
    }
    zk_llvm_edit_insert_instruction_after(edit: Handle, instruction: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.insert_instruction_after(borrow(instruction)?, borrow(anchor)?))
    }
    zk_llvm_edit_erase_instruction(edit: Handle, instruction: Handle) {
        unit(borrow::<Edit>(edit)?.erase_instruction(borrow(instruction)?))
    }
    zk_llvm_edit_move_block_before(edit: Handle, block: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.move_block_before(borrow(block)?, borrow(anchor)?))
    }
    zk_llvm_edit_move_block_after(edit: Handle, block: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.move_block_after(borrow(block)?, borrow(anchor)?))
    }
    zk_llvm_edit_detach_block(edit: Handle, block: Handle) {
        unit(borrow::<Edit>(edit)?.detach_block(borrow(block)?))
    }
    zk_llvm_edit_insert_block_before(edit: Handle, block: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.insert_block_before(borrow(block)?, borrow(anchor)?))
    }
    zk_llvm_edit_insert_block_after(edit: Handle, block: Handle, anchor: Handle) {
        unit(borrow::<Edit>(edit)?.insert_block_after(borrow(block)?, borrow(anchor)?))
    }
    zk_llvm_edit_erase_block(edit: Handle, block: Handle) {
        unit(borrow::<Edit>(edit)?.erase_block(borrow(block)?))
    }
    zk_llvm_edit_redirect_successor(edit: Handle, terminator: Handle, index: u32,
                                   destination: Handle, incoming: *const Handle, count: usize) {
        unit(borrow::<Edit>(edit)?.redirect_successor(borrow(terminator)?, index,
             borrow(destination)?, &handles(incoming, count)?))
    }
    zk_llvm_edit_add_switch_case(edit: Handle, instruction: Handle, key: Handle,
                                destination: Handle, incoming: *const Handle, count: usize) {
        unit(borrow::<Edit>(edit)?.add_switch_case(
            borrow(instruction)?, borrow(key)?, borrow(destination)?, &handles(incoming, count)?))
    }
    zk_llvm_edit_remove_switch_case(edit: Handle, instruction: Handle, key: Handle) {
        unit(borrow::<Edit>(edit)?.remove_switch_case(borrow(instruction)?, borrow(key)?))
    }
    zk_llvm_finish_edit(edit: Handle) { owned(borrow::<Edit>(edit)?.finish()?) }
    zk_llvm_builder_context(builder: Handle) { owned(borrow::<Builder>(builder)?.context().clone()) }
    zk_llvm_builder_block(builder: Handle) { optional(borrow::<Builder>(builder)?.block()?) }
    zk_llvm_position_at_end(builder: Handle, block: Handle) {
        unit(borrow::<Builder>(builder)?.position_at_end(borrow(block)?))
    }
    zk_llvm_position_before_instruction(builder: Handle, instruction: Handle) {
        unit(borrow::<Builder>(builder)?.position_before(borrow(instruction)?))
    }
    zk_llvm_clear_position(builder: Handle) {
        borrow::<Builder>(builder)?.clear_position();
        unit(Ok(()))
    }
    zk_llvm_int_binop(builder: Handle, op: u32, lhs: Handle, rhs: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.int_binop(int_op(op)?, borrow(lhs)?, borrow(rhs)?, string(name)?)?)
    }
    zk_llvm_int_binop_flags(builder: Handle, op: u32, lhs: Handle, rhs: Handle,
                          nsw: u8, nuw: u8, exact: u8, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.int_binop_with_flags(
            int_op(op)?, borrow(lhs)?, borrow(rhs)?, IntFlags { nsw: nsw != 0, nuw: nuw != 0, exact: exact != 0 },
            string(name)?)?)
    }
    zk_llvm_float_binop(builder: Handle, op: u32, lhs: Handle, rhs: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.float_binop(choice(FLOAT_OPS, op)?, borrow(lhs)?, borrow(rhs)?, string(name)?)?)
    }
    zk_llvm_float_binop_with_flags(builder: Handle, op: u32, lhs: Handle, rhs: Handle,
                                 flags: u32, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.float_binop_with_flags(
            choice(FLOAT_OPS, op)?, borrow(lhs)?, borrow(rhs)?, fast_math_flags(flags)?, string(name)?)?)
    }
    zk_llvm_fneg(builder: Handle, value: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.fneg(borrow(value)?, string(name)?)?)
    }
    zk_llvm_fneg_with_flags(builder: Handle, value: Handle, flags: u32, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.fneg_with_flags(borrow(value)?, fast_math_flags(flags)?, string(name)?)?)
    }
    zk_llvm_icmp(builder: Handle, predicate: u32, lhs: Handle, rhs: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.icmp(choice(INT_PREDICATES, predicate)?, borrow(lhs)?, borrow(rhs)?, string(name)?)?)
    }
    zk_llvm_fcmp(builder: Handle, predicate: u32, lhs: Handle, rhs: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.fcmp(choice(FLOAT_PREDICATES, predicate)?, borrow(lhs)?, borrow(rhs)?, string(name)?)?)
    }
    zk_llvm_fcmp_with_flags(builder: Handle, predicate: u32, lhs: Handle, rhs: Handle,
                           flags: u32, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.fcmp_with_flags(
            choice(FLOAT_PREDICATES, predicate)?, borrow(lhs)?, borrow(rhs)?, fast_math_flags(flags)?, string(name)?)?)
    }
    zk_llvm_int_cast(builder: Handle, value: Handle, ty: Handle, signed: u8, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.int_cast(borrow(value)?, borrow(ty)?, signed != 0, string(name)?)?)
    }
    zk_llvm_cast(builder: Handle, op: u32, value: Handle, ty: Handle, name: *const c_char) {
        use CastOp::*;
        let op = choice(&[Trunc, ZExt, SExt, FpToUi, FpToSi, UiToFp, SiToFp, FpTrunc, FpExt,
                          PtrToInt, IntToPtr, BitCast, AddrSpaceCast], op)?;
        owned(borrow::<Builder>(builder)?.cast(op, borrow(value)?, borrow(ty)?, string(name)?)?)
    }
    zk_llvm_phi(builder: Handle, ty: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.phi(borrow(ty)?, string(name)?)?)
    }
    zk_llvm_add_incoming(phi: Handle, value: Handle, block: Handle) {
        unit(borrow::<Value>(phi)?.add_incoming(borrow(value)?, borrow(block)?))
    }
    zk_llvm_ret(builder: Handle, value: Handle) { unit(borrow::<Builder>(builder)?.ret(nullable(value)?)) }
    zk_llvm_br(builder: Handle, target: Handle) { unit(borrow::<Builder>(builder)?.br(borrow(target)?)) }
    zk_llvm_cond_br(builder: Handle, test: Handle, yes: Handle, no: Handle) {
        unit(borrow::<Builder>(builder)?.cond_br(borrow(test)?, borrow(yes)?, borrow(no)?))
    }
    zk_llvm_select(builder: Handle, condition: Handle, yes: Handle, no: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.select(borrow(condition)?, borrow(yes)?, borrow(no)?, string(name)?)?)
    }
    zk_llvm_switch(builder: Handle, condition: Handle, default: Handle, cases: *const Handle, count: usize) {
        let (pairs, remainder) = input(cases, count)?.as_chunks::<2>();
        if !remainder.is_empty() {
            return Err(Error::new("expected a pair of LLVM handles for each switch case"));
        }
        let cases = pairs.iter().map(|&[key, block]| Ok((borrow(key)?, borrow(block)?)))
            .collect::<Result<Vec<_>>>()?;
        unit(borrow::<Builder>(builder)?.switch(borrow(condition)?, borrow(default)?, &cases))
    }
    zk_llvm_unreachable(builder: Handle) { unit(borrow::<Builder>(builder)?.unreachable()) }
    zk_llvm_call(builder: Handle, function: Handle, args: *const Handle, count: usize, name: *const c_char) {
        optional(borrow::<Builder>(builder)?.call(borrow(function)?, &handles(args, count)?, string(name)?)?)
    }
    zk_llvm_call_instruction(builder: Handle, function: Handle, args: *const Handle, count: usize, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.call_instruction(borrow(function)?, &handles(args, count)?, string(name)?)?)
    }
    zk_llvm_call_indirect(builder: Handle, signature: Handle, pointer: Handle, args: *const Handle, count: usize, name: *const c_char) {
        let builder = borrow::<Builder>(builder)?;
        let pointer = borrow::<Value>(pointer)?;
        let args = handles(args, count)?;
        let name = string(name)?;
        let value = match signature.cast::<Object>().as_ref() {
            Some(Object::Type(signature)) => builder.call_indirect(signature, pointer, &args, name),
            Some(Object::Function(prototype)) => builder.call_indirect_with_prototype(prototype, pointer, &args, name),
            _ => return Err(Error::new("expected an LLVM function type or prototype")),
        }?;
        optional(value)
    }
    zk_llvm_call_indirect_instruction(builder: Handle, signature: Handle, pointer: Handle,
                                      args: *const Handle, count: usize, name: *const c_char) {
        let builder = borrow::<Builder>(builder)?;
        let pointer = borrow::<Value>(pointer)?;
        let args = handles(args, count)?;
        let name = string(name)?;
        let instruction = match signature.cast::<Object>().as_ref() {
            Some(Object::Type(signature)) => builder.call_indirect_instruction(signature, pointer, &args, name),
            Some(Object::Function(prototype)) => builder.call_indirect_instruction_with_prototype(prototype, pointer, &args, name),
            _ => return Err(Error::new("expected an LLVM function type or prototype")),
        }?;
        owned(instruction)
    }
    zk_llvm_call_instruction_with_bundles(builder: Handle, function: Handle,
                                          args: *const Handle, count: usize, bundles: *const Handle,
                                          bundle_count: usize, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.call_instruction_with_bundles(borrow(function)?,
            &handles(args, count)?, &handles(bundles, bundle_count)?, string(name)?)?)
    }
    zk_llvm_call_indirect_instruction_with_bundles(builder: Handle, signature: Handle,
                                                   pointer: Handle, args: *const Handle, count: usize,
                                                   bundles: *const Handle, bundle_count: usize,
                                                   name: *const c_char) {
        let builder = borrow::<Builder>(builder)?;
        let pointer = borrow::<Value>(pointer)?;
        let args = handles(args, count)?;
        let bundles = handles(bundles, bundle_count)?;
        let name = string(name)?;
        let instruction = match signature.cast::<Object>().as_ref() {
            Some(Object::Type(signature)) => builder.call_indirect_instruction_with_bundles(signature,
                pointer, &args, &bundles, name),
            Some(Object::Function(prototype)) => builder.call_indirect_instruction_with_prototype_and_bundles(
                prototype, pointer, &args, &bundles, name),
            _ => return Err(Error::new("expected an LLVM function type or prototype")),
        }?;
        owned(instruction)
    }
    zk_llvm_alloca(builder: Handle, ty: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.alloca(borrow(ty)?, string(name)?)?)
    }
    zk_llvm_load(builder: Handle, ty: Handle, address: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.load(borrow(ty)?, borrow(address)?, string(name)?)?)
    }
    zk_llvm_store(builder: Handle, value: Handle, address: Handle) {
        unit(borrow::<Builder>(builder)?.store(borrow(value)?, borrow(address)?))
    }
    zk_llvm_alloca_options(builder: Handle, ty: Handle, count: Handle, alignment: u64, name: *const c_char) {
        let builder = borrow::<Builder>(builder)?;
        let ty = borrow::<Type>(ty)?;
        let alignment = memory_alignment(alignment)?;
        let name = string(name)?;
        owned(match nullable::<Value>(count)? {
            Some(count) => builder.array_alloca_with_alignment(ty, count, alignment, name)?,
            None => builder.alloca_with_alignment(ty, alignment, name)?,
        })
    }
    zk_llvm_load_options(builder: Handle, ty: Handle, address: Handle, alignment: u64,
                         volatile: u8, ordering: u32, scope: u32, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.load_with_options(borrow(ty)?, borrow(address)?,
            memory_options(alignment, volatile, ordering, scope)?, string(name)?)?)
    }
    zk_llvm_store_options(builder: Handle, value: Handle, address: Handle, alignment: u64,
                          volatile: u8, ordering: u32, scope: u32) {
        unit(borrow::<Builder>(builder)?.store_with_options(borrow(value)?, borrow(address)?,
            memory_options(alignment, volatile, ordering, scope)?))
    }
    zk_llvm_instruction_memory_ordering(instruction: Handle) -> u64 {
        atomic_ordering_index(borrow::<Instruction>(instruction)?.memory_ordering())
    }
    zk_llvm_set_instruction_memory_ordering(instruction: Handle, ordering: u32) {
        let ordering = (ordering != 0).then(|| atomic_ordering(ordering)).transpose()?;
        unit(borrow::<Instruction>(instruction)?.set_memory_ordering(ordering))
    }
    zk_llvm_instruction_atomic_sync_scope(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.atomic_sync_scope_id().map_or(0, |id| u64::from(id) + 1))
    }
    zk_llvm_set_instruction_atomic_sync_scope(instruction: Handle, scope: u32) {
        unit(borrow::<Instruction>(instruction)?.set_atomic_sync_scope(sync_scope(scope)?))
    }
    zk_llvm_instruction_cmpxchg_success_ordering(instruction: Handle) -> u64 {
        let orderings = borrow::<Instruction>(instruction)?.cmpxchg_orderings();
        atomic_ordering_index(orderings.map(|(success, _)| success))
    }
    zk_llvm_instruction_cmpxchg_failure_ordering(instruction: Handle) -> u64 {
        let orderings = borrow::<Instruction>(instruction)?.cmpxchg_orderings();
        atomic_ordering_index(orderings.map(|(_, failure)| failure))
    }
    zk_llvm_set_instruction_cmpxchg_orderings(instruction: Handle, success: u32, failure: u32) {
        unit(borrow::<Instruction>(instruction)?.set_cmpxchg_orderings(
            atomic_ordering(success)?, atomic_ordering(failure)?))
    }
    zk_llvm_instruction_cmpxchg_is_weak(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.cmpxchg_is_weak().unwrap_or(false).into())
    }
    zk_llvm_set_instruction_cmpxchg_weak(instruction: Handle, weak: u8) {
        unit(borrow::<Instruction>(instruction)?.set_cmpxchg_weak(weak != 0))
    }
    zk_llvm_instruction_atomic_rmw_operation(instruction: Handle) -> u64 {
        borrow::<Instruction>(instruction)?.atomic_rmw_operation()
            .map_or(Ok(0), |operation| Ok(1 + choice_index(ATOMIC_RMW_OPERATIONS, operation)?))
    }
    zk_llvm_set_instruction_atomic_rmw_operation(instruction: Handle, operation: u32) {
        unit(borrow::<Instruction>(instruction)?.set_atomic_rmw_operation(choice(ATOMIC_RMW_OPERATIONS, operation)?))
    }
    zk_llvm_set_instruction_volatile(instruction: Handle, volatile: u8) {
        unit(borrow::<Instruction>(instruction)?.set_volatile(volatile != 0))
    }
    zk_llvm_fence(builder: Handle, ordering: u32, scope: u32) {
        unit(borrow::<Builder>(builder)?.fence(AtomicOptions {
            ordering: atomic_ordering(ordering)?, scope: sync_scope(scope)?,
        }))
    }
    zk_llvm_atomic_rmw(builder: Handle, op: u32, address: Handle, value: Handle, ordering: u32,
                      scope: u32, alignment: u64, volatile: u8, name: *const c_char) {
        let op = choice(ATOMIC_RMW_OPERATIONS, op)?;
        owned(borrow::<Builder>(builder)?.atomic_rmw(op, borrow(address)?, borrow(value)?, AtomicRmwOptions {
            ordering: atomic_ordering(ordering)?, scope: sync_scope(scope)?,
            alignment: memory_alignment(alignment)?, volatile: volatile != 0,
        }, string(name)?)?)
    }
    zk_llvm_cmpxchg(builder: Handle, address: Handle, expected: Handle, replacement: Handle,
                   success: u32, failure: u32, scope: u32, alignment: u64,
                   volatile: u8, weak: u8, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.cmpxchg(borrow(address)?, borrow(expected)?, borrow(replacement)?, CmpXchgOptions {
            success: atomic_ordering(success)?, failure: atomic_ordering(failure)?, scope: sync_scope(scope)?,
            alignment: memory_alignment(alignment)?, volatile: volatile != 0, weak: weak != 0,
        }, string(name)?)?)
    }
    zk_llvm_memcpy(builder: Handle, dest: Handle, source: Handle, len: Handle,
                  dest_alignment: u64, source_alignment: u64, volatile: u8) {
        owned(borrow::<Builder>(builder)?.memcpy(borrow(dest)?, borrow(source)?, borrow(len)?,
            (dest_alignment != 0).then_some(dest_alignment), (source_alignment != 0).then_some(source_alignment),
            volatile != 0)?)
    }
    zk_llvm_memmove(builder: Handle, dest: Handle, source: Handle, len: Handle,
                   dest_alignment: u64, source_alignment: u64, volatile: u8) {
        owned(borrow::<Builder>(builder)?.memmove(borrow(dest)?, borrow(source)?, borrow(len)?,
            (dest_alignment != 0).then_some(dest_alignment), (source_alignment != 0).then_some(source_alignment),
            volatile != 0)?)
    }
    zk_llvm_memset(builder: Handle, dest: Handle, byte: Handle, len: Handle,
                  dest_alignment: u64, volatile: u8) {
        owned(borrow::<Builder>(builder)?.memset(borrow(dest)?, borrow(byte)?, borrow(len)?,
            (dest_alignment != 0).then_some(dest_alignment), volatile != 0)?)
    }
    zk_llvm_malloc(builder: Handle, allocator: Handle, ty: Handle, count: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.malloc(borrow(allocator)?, borrow(ty)?, borrow(count)?, string(name)?)?)
    }
    zk_llvm_free(builder: Handle, deallocator: Handle, address: Handle) {
        owned(borrow::<Builder>(builder)?.free(borrow(deallocator)?, borrow(address)?)?)
    }
    zk_llvm_ptr_diff(builder: Handle, ty: Handle, left: Handle, right: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.ptr_diff(borrow(ty)?, borrow(left)?, borrow(right)?, string(name)?)?)
    }
    zk_llvm_gep(builder: Handle, ty: Handle, address: Handle, indices: *const Handle, count: usize, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.gep(borrow(ty)?, borrow(address)?, &handles(indices, count)?, string(name)?)?)
    }
    zk_llvm_gep_in_bounds(builder: Handle, ty: Handle, address: Handle, indices: *const Handle, count: usize, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.gep_in_bounds(borrow(ty)?, borrow(address)?, &handles(indices, count)?, string(name)?)?)
    }
    zk_llvm_gep_flags(builder: Handle, ty: Handle, address: Handle, indices: *const Handle,
                     count: usize, in_bounds: u8, nuw: u8, nusw: u8, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.gep_with_flags(borrow(ty)?, borrow(address)?,
            &handles(indices, count)?, GepFlags { in_bounds: in_bounds != 0,
                no_unsigned_wrap: nuw != 0, no_unsigned_signed_wrap: nusw != 0 }, string(name)?)?)
    }
    zk_llvm_extract_value(builder: Handle, aggregate: Handle, index: u32, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.extract_value(borrow(aggregate)?, index, string(name)?)?)
    }
    zk_llvm_insert_value(builder: Handle, aggregate: Handle, value: Handle, index: u32, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.insert_value(borrow(aggregate)?, borrow(value)?, index, string(name)?)?)
    }
    zk_llvm_extract_element(builder: Handle, vector: Handle, index: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.extract_element(borrow(vector)?, borrow(index)?, string(name)?)?)
    }
    zk_llvm_insert_element(builder: Handle, vector: Handle, value: Handle, index: Handle,
                           name: *const c_char) {
        owned(borrow::<Builder>(builder)?.insert_element(borrow(vector)?, borrow(value)?, borrow(index)?, string(name)?)?)
    }
    zk_llvm_shuffle_vector(builder: Handle, left: Handle, right: Handle, mask: Handle,
                           name: *const c_char) {
        owned(borrow::<Builder>(builder)?.shuffle_vector(borrow(left)?, borrow(right)?, borrow(mask)?, string(name)?)?)
    }
    zk_llvm_freeze(builder: Handle, value: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.freeze(borrow(value)?, string(name)?)?)
    }
    zk_llvm_not(builder: Handle, value: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.not(borrow(value)?, string(name)?)?)
    }
    zk_llvm_host_triple() { text(TargetMachine::host_triple()) }
    zk_llvm_host_cpu() { text(TargetMachine::host_cpu()) }
    zk_llvm_host_features() { text(TargetMachine::host_features()) }
    zk_llvm_make_target_machine_with_options(triple: *const c_char, opt: u32,
                                           cpu: *const c_char, features: *const c_char,
                                           relocation: u32, code_model: u32, abi: *const c_char) {
        use OptimizationLevel::*;
        owned(TargetMachine::with_options(string(triple)?, TargetOptions {
            optimization: choice(&[None, Less, Default, Aggressive], opt)?,
            cpu: string(cpu)?, features: string(features)?,
            relocation: choice(&[RelocMode::Default, RelocMode::Static, RelocMode::PIC,
                                RelocMode::DynamicNoPic, RelocMode::Ropi, RelocMode::Rwpi,
                                RelocMode::RopiRwpi], relocation)?,
            code_model: choice(&[CodeModel::Default, CodeModel::Tiny, CodeModel::Small,
                                 CodeModel::Kernel, CodeModel::Medium, CodeModel::Large,
                                 CodeModel::JitDefault], code_model)?,
            abi: string(abi)?,
        })?)
    }
    zk_llvm_target_triple(machine: Handle) { text(borrow::<TargetMachine>(machine)?.triple()) }
    zk_llvm_target_cpu(machine: Handle) { text(borrow::<TargetMachine>(machine)?.cpu()) }
    zk_llvm_target_features(machine: Handle) { text(borrow::<TargetMachine>(machine)?.features()) }
    zk_llvm_target_abi(machine: Handle) { optional_text(borrow::<TargetMachine>(machine)?.abi().map(str::to_owned)) }
    zk_llvm_target_backend_name(machine: Handle) { text(borrow::<TargetMachine>(machine)?.backend_name()) }
    zk_llvm_target_backend_description(machine: Handle) { text(borrow::<TargetMachine>(machine)?.backend_description()) }
    zk_llvm_make_data_layout(contents: *const c_char) { owned(DataLayout::parse(string(contents)?)?) }
    zk_llvm_target_layout(machine: Handle) { owned(borrow::<TargetMachine>(machine)?.target_data()) }
    zk_llvm_module_layout(module: Handle) { owned(borrow::<Module>(module)?.target_data()) }
    zk_llvm_layout_text(layout: Handle) { text(borrow::<DataLayout>(layout)?.text()) }
    zk_llvm_layout_little_endian_p(layout: Handle) -> u64 {
        Ok(borrow::<DataLayout>(layout)?.is_little_endian().into())
    }
    zk_llvm_layout_pointer_size_bits(layout: Handle, address_space: u32) -> u64 {
        Ok(borrow::<DataLayout>(layout)?.pointer_size_bits(address_space).into())
    }
    zk_llvm_layout_pointer_index_size_bits(layout: Handle, address_space: u32) -> u64 {
        Ok(borrow::<DataLayout>(layout)?.pointer_index_size_bits(address_space).into())
    }
    zk_llvm_layout_non_integral_pointer_p(layout: Handle, address_space: u32) -> u64 {
        Ok(borrow::<DataLayout>(layout)?.is_non_integral_pointer(address_space).into())
    }
    zk_llvm_layout_size_in_bits(layout: Handle, ty: Handle) -> u64 { borrow::<DataLayout>(layout)?.size_in_bits(borrow(ty)?) }
    zk_llvm_layout_store_size(layout: Handle, ty: Handle) -> u64 { borrow::<DataLayout>(layout)?.store_size(borrow(ty)?) }
    zk_llvm_layout_alloc_size(layout: Handle, ty: Handle) -> u64 { borrow::<DataLayout>(layout)?.alloc_size(borrow(ty)?) }
    zk_llvm_layout_abi_alignment(layout: Handle, ty: Handle) -> u64 { Ok(borrow::<DataLayout>(layout)?.abi_alignment(borrow(ty)?)?.into()) }
    zk_llvm_layout_preferred_alignment(layout: Handle, ty: Handle) -> u64 { Ok(borrow::<DataLayout>(layout)?.preferred_alignment(borrow(ty)?)?.into()) }
    zk_llvm_layout_field_offset(layout: Handle, ty: Handle, index: u32) -> u64 { borrow::<DataLayout>(layout)?.field_offset(borrow(ty)?, index) }
    zk_llvm_target_data_layout(machine: Handle) { text(borrow::<TargetMachine>(machine)?.data_layout()) }
    zk_llvm_module_target_triple(module: Handle) { optional_text(borrow::<Module>(module)?.target_triple()) }
    zk_llvm_module_data_layout(module: Handle) { optional_text(borrow::<Module>(module)?.data_layout()) }
    zk_llvm_set_module_layout(module: Handle, layout: Handle) {
        borrow::<Module>(module)?.set_target_data(borrow(layout)?);
        unit(Ok(()))
    }
    zk_llvm_set_module_target_triple(module: Handle, triple: *const c_char) {
        unit(borrow::<Module>(module)?.set_target_triple(string(triple)?))
    }
    zk_llvm_configure_module_target(module: Handle, machine: Handle) {
        unit(borrow::<TargetMachine>(machine)?.configure_module(borrow(module)?))
    }
    zk_llvm_target_abi_size(machine: Handle, ty: Handle) -> u64 { borrow::<TargetMachine>(machine)?.abi_size(borrow(ty)?) }
    zk_llvm_target_abi_alignment_bytes(machine: Handle, ty: Handle) -> u64 {
        Ok(borrow::<TargetMachine>(machine)?.abi_alignment(borrow(ty)?)?.into())
    }
    zk_llvm_target_field_offset(machine: Handle, ty: Handle, index: u32) -> u64 {
        borrow::<TargetMachine>(machine)?.field_offset(borrow(ty)?, index)
    }
    zk_llvm_optimize_module(module: Handle, pipeline: *const c_char, machine: Handle) {
        owned(borrow::<Module>(module)?.optimize(string(pipeline)?, nullable(machine)?)?)
    }
    zk_llvm_optimize_module_with_options(module: Handle, pipeline: *const c_char, machine: Handle,
        flags: u32, aa_pipeline: *const c_char, opt_cap: i64, promotion_cap: i64, inliner_threshold: i32) {
        let options = pass_options(flags, string(aa_pipeline)?, opt_cap, promotion_cap, inliner_threshold)?;
        owned(borrow::<Module>(module)?.optimize_with_options(string(pipeline)?, nullable(machine)?, options)?)
    }
    zk_llvm_optimize_function(function: Handle, pipeline: *const c_char, machine: Handle,
        flags: u32, aa_pipeline: *const c_char, opt_cap: i64, promotion_cap: i64, inliner_threshold: i32) {
        let options = pass_options(flags, string(aa_pipeline)?, opt_cap, promotion_cap, inliner_threshold)?;
        owned(borrow::<Function>(function)?.optimize(string(pipeline)?, nullable(machine)?, options)?)
    }
    zk_llvm_emit_object(module: Handle, machine: Handle) { owned(borrow::<Module>(module)?.emit_object(nullable(machine)?)?) }
    zk_llvm_emit_assembly(module: Handle, machine: Handle) { text(borrow::<Module>(module)?.emit_assembly(nullable(machine)?)?) }
    zk_llvm_create_debug_unit(module: Handle, filename: *const c_char, directory: *const c_char, producer: *const c_char, language: u32, optimized: u8, emission: u32) {
        use DebugLanguage::*;
        owned(DebugInfo::new(borrow(module)?, string(filename)?, DebugOptions {
            directory: string(directory)?, producer: string(producer)?,
            language: choice(&[C, CPlusPlus, Rust, Swift, Python], language)?, optimized: optimized != 0,
            emission: choice(&[DebugEmission::LineTables, DebugEmission::Full], emission)?,
        })?)
    }
    zk_llvm_create_debug_compile_unit(module: Handle, filename: *const c_char, directory: *const c_char,
        producer: *const c_char, language: *const c_char, optimized: u8, emission: u32,
        flags: *const c_char, runtime_version: u32, debug_info_for_profiling: u8,
        sysroot: *const c_char, sdk: *const c_char) {
        owned(DebugInfo::with_compile_unit_options(borrow(module)?, string(filename)?, DebugOptions {
            directory: string(directory)?, producer: string(producer)?,
            language: debug_source_language(string(language)?)?, optimized: optimized != 0,
            emission: choice(&[DebugEmission::LineTables, DebugEmission::Full], emission)?,
        }, DebugCompileUnitOptions {
            flags: string(flags)?, runtime_version, debug_info_for_profiling: debug_info_for_profiling != 0,
            sysroot: string(sysroot)?, sdk: string(sdk)?,
        })?)
    }
    zk_llvm_make_debug_file(info: Handle, filename: *const c_char, directory: *const c_char) {
        owned(borrow::<DebugInfo>(info)?.file(string(filename)?, string(directory)?)?)
    }
    zk_llvm_attach_debug_subprogram(info: Handle, function: Handle, line: u32, name: *const c_char, file: Handle, result: Handle, parameters: *const Handle, count: usize, variadic: u8) {
        let name = string(name)?;
        let parameters = handles(parameters, count)?;
        owned(borrow::<DebugInfo>(info)?.attach_function(borrow(function)?, line, DebugFunctionOptions {
            name: (!name.is_empty()).then_some(name), file: nullable(file)?, result: nullable(result)?,
            parameters: &parameters, variadic: variadic != 0,
        })?)
    }
    zk_llvm_make_debug_lexical_block(info: Handle, scope: Handle, line: u32, column: u16, file: Handle) {
        owned(borrow::<DebugInfo>(info)?.lexical_block(borrow(scope)?, line, column, nullable(file)?)?)
    }
    zk_llvm_function_debug_scope(function: Handle) {
        optional(borrow::<Function>(function)?.debug_scope())
    }
    zk_llvm_debug_scope_line(scope: Handle) -> u64 { Ok(borrow::<DebugScope>(scope)?.line().into()) }
    zk_llvm_debug_scope_file(scope: Handle) { optional(borrow::<DebugScope>(scope)?.file()) }
    zk_llvm_make_debug_location(scope: Handle, line: u32, column: u16) {
        owned(borrow::<DebugScope>(scope)?.location(line, column))
    }
    zk_llvm_make_debug_inlined_location(scope: Handle, line: u32, column: u16, inlined_at: Handle) {
        owned(borrow::<DebugScope>(scope)?.inlined_location(line, column, borrow(inlined_at)?)?)
    }
    zk_llvm_debug_location_line(location: Handle) -> u64 { Ok(borrow::<DebugLocation>(location)?.line().into()) }
    zk_llvm_debug_location_column(location: Handle) -> u64 { Ok(borrow::<DebugLocation>(location)?.column().into()) }
    zk_llvm_debug_location_file(location: Handle) { optional(borrow::<DebugLocation>(location)?.file()) }
    zk_llvm_debug_location_inlined_at(location: Handle) { optional(borrow::<DebugLocation>(location)?.inlined_at()) }
    zk_llvm_debug_file_filename(file: Handle) { text(borrow::<DebugFile>(file)?.filename()?) }
    zk_llvm_debug_file_directory(file: Handle) { text(borrow::<DebugFile>(file)?.directory()?) }
    zk_llvm_instruction_debug_location(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.debug_location())
    }
    zk_llvm_set_instruction_debug_location(instruction: Handle, location: Handle) {
        unit(borrow::<Instruction>(instruction)?.set_debug_location(nullable(location)?))
    }
    zk_llvm_make_debug_pointer_type(info: Handle, pointee: Handle, bits: u64, align_bits: u32,
                                    address_space: u32, name: *const c_char) {
        owned(borrow::<DebugInfo>(info)?.pointer_type(nullable(pointee)?, bits, align_bits, address_space, string(name)?)?)
    }
    zk_llvm_make_debug_array_type(info: Handle, element: Handle, bits: u64, align_bits: u32, count: u64) {
        owned(borrow::<DebugInfo>(info)?.array_type(borrow(element)?, bits, align_bits, count)?)
    }
    zk_llvm_make_debug_member(name: *const c_char, ty: Handle, bits: u64, align_bits: u32, offset_bits: u64) {
        owned(DebugMember::new(string(name)?, borrow(ty)?, bits, align_bits, offset_bits)?)
    }
    zk_llvm_make_debug_struct_type(info: Handle, name: *const c_char, bits: u64, align_bits: u32,
                                   line: u32, file: Handle, members: *const Handle, count: usize) {
        owned(borrow::<DebugInfo>(info)?.struct_type(string(name)?, bits, align_bits, line, nullable(file)?, &handles(members, count)?)?)
    }
    zk_llvm_make_debug_union_type(info: Handle, name: *const c_char, bits: u64, align_bits: u32,
                                  line: u32, file: Handle, members: *const Handle, count: usize) {
        owned(borrow::<DebugInfo>(info)?.union_type(string(name)?, bits, align_bits, line, nullable(file)?, &handles(members, count)?)?)
    }
    zk_llvm_insert_debug_address_before(info: Handle, block: Handle, variable: Handle, address: Handle, location: Handle, before: Handle) {
        unit(borrow::<DebugInfo>(info)?.record_address(borrow(block)?, borrow(variable)?, borrow(address)?, borrow(location)?, nullable(before)?))
    }
    zk_llvm_set_debug_location(builder: Handle, location: Handle) {
        let builder = borrow::<Builder>(builder)?;
        if let Some(location) = nullable(location)? {
            builder.set_debug_location(location)?;
        } else {
            builder.clear_debug_location();
        }
        unit(Ok(()))
    }
    zk_llvm_builder_debug_location(builder: Handle) {
        optional(borrow::<Builder>(builder)?.debug_location())
    }
    zk_llvm_finish_debug_function(info: Handle, function: Handle) {
        unit(borrow::<DebugInfo>(info)?.finish_function(borrow(function)?))
    }
    zk_llvm_finish_debug_info(info: Handle) {
        borrow::<DebugInfo>(info)?.finish();
        unit(Ok(()))
    }
    zk_llvm_make_debug_type(info: Handle, name: *const c_char, bits: u64, encoding: u32) {
        use DebugEncoding::*;
        let encoding = choice(&[Address, Boolean, Float, Signed, Unsigned], encoding)?;
        owned(borrow::<DebugInfo>(info)?.basic_type(string(name)?, bits, encoding)?)
    }
    zk_llvm_describe_debug_variable(info: Handle, scope: Handle, name: *const c_char, ty: Handle, line: u32, parameter: u16) {
        owned(borrow::<DebugInfo>(info)?.variable(borrow(scope)?, string(name)?, borrow(ty)?, line, parameter.checked_sub(1))?)
    }
    zk_llvm_insert_debug_value_before(info: Handle, block: Handle, variable: Handle, value: Handle, location: Handle, before: Handle) {
        unit(borrow::<DebugInfo>(info)?.record_value(borrow(block)?, borrow(variable)?, borrow(value)?, borrow(location)?, nullable(before)?))
    }
    zk_llvm_make_jit() { owned(Jit::new()?) }
    zk_llvm_make_jit_dylib(parent: Handle, name: *const c_char) {
        owned(borrow::<Jit>(parent)?.create_dylib(string(name)?)?)
    }
    zk_llvm_jit_add_module(jit: Handle, module: Handle) { unit(borrow::<Jit>(jit)?.add_module(borrow(module)?)) }
    zk_llvm_jit_add_ir_module(jit: Handle, module: Handle, tracker: Handle) {
        let jit = borrow::<Jit>(jit)?;
        let module = borrow::<Module>(module)?;
        unit(match nullable::<ResourceTracker>(tracker)? {
            Some(tracker) => jit.add_ir_module_with_tracker(module, tracker),
            None => jit.add_ir_module(module),
        })
    }
    zk_llvm_jit_add_object(jit: Handle, bytes: *const u8, count: usize) {
        unit(borrow::<Jit>(jit)?.add_object(input(bytes, count)?))
    }
    zk_llvm_make_resource_tracker(jit: Handle) {
        owned(borrow::<Jit>(jit)?.create_resource_tracker()?)
    }
    zk_llvm_remove_resource_tracker(tracker: Handle) {
        unit(borrow::<ResourceTracker>(tracker)?.remove())
    }
    zk_llvm_jit_add_module_with_tracker(jit: Handle, module: Handle, tracker: Handle) {
        unit(borrow::<Jit>(jit)?.add_module_with_tracker(borrow(module)?, borrow(tracker)?))
    }
    zk_llvm_jit_add_object_with_tracker(jit: Handle, bytes: *const u8, count: usize, tracker: Handle) {
        unit(borrow::<Jit>(jit)?.add_object_with_tracker(input(bytes, count)?, borrow(tracker)?))
    }
    zk_llvm_jit_define_symbol(jit: Handle, name: *const c_char, address: *mut c_void,
                              kind: u32) {
        let address = NonNull::new(address).ok_or_else(|| Error::new("ORC symbol address is null"))?;
        let kind = choice(&[HostSymbolKind::Function, HostSymbolKind::Data], kind)?;
        unit(borrow::<Jit>(jit)?.define_symbol(string(name)?, address, kind))
    }
    zk_llvm_jit_triple(jit: Handle) { text(borrow::<Jit>(jit)?.triple()?) }
    zk_llvm_jit_data_layout(jit: Handle) { text(borrow::<Jit>(jit)?.data_layout()?) }
    zk_llvm_jit_configure_module(jit: Handle, module: Handle) {
        unit(borrow::<Jit>(jit)?.configure_module(borrow(module)?))
    }
    zk_llvm_jit_lookup(jit: Handle, name: *const c_char) { owned(borrow::<Jit>(jit)?.lookup(string(name)?)?) }
    zk_llvm_jit_add_module_and_lookup(jit: Handle, module: Handle, name: *const c_char) {
        owned(borrow::<Jit>(jit)?.add_module_and_lookup(borrow(module)?, string(name)?)?)
    }
    zk_llvm_jit_remove_compilation(symbol: Handle) {
        let result = borrow::<JitSymbol>(symbol)?.remove_compilation()?;
        // Let Lisp invalidate its callable before signaling a committed cleanup error.
        optional_text(result.err().map(|error| error.to_string()))
    }
    zk_llvm_jit_symbol_address(symbol: Handle) { Ok(borrow::<JitSymbol>(symbol)?.address().as_ptr()) }
    zk_llvm_close_jit(jit: Handle) {
        borrow::<Jit>(jit)?.close();
        unit(Ok(()))
    }

    zk_llvm_function_personality(function: Handle) {
        optional(borrow::<Function>(function)?.personality()?)
    }
    zk_llvm_set_function_personality(function: Handle, personality: Handle) {
        unit(borrow::<Function>(function)?.set_personality(nullable(personality)?))
    }
    zk_llvm_invoke(builder: Handle, function: Handle, args: *const Handle, count: usize,
                   normal: Handle, unwind: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.invoke(borrow(function)?, &handles(args, count)?,
            borrow(normal)?, borrow(unwind)?, string(name)?)?)
    }
    zk_llvm_invoke_indirect(builder: Handle, signature: Handle, pointer: Handle,
                            args: *const Handle, count: usize,
                            normal: Handle, unwind: Handle, name: *const c_char) {
        let builder = borrow::<Builder>(builder)?;
        let pointer = borrow::<Value>(pointer)?;
        let args = handles(args, count)?;
        let normal = borrow::<Block>(normal)?;
        let unwind = borrow::<Block>(unwind)?;
        let name = string(name)?;
        let instruction = match signature.cast::<Object>().as_ref() {
            Some(Object::Type(signature)) => builder.invoke_indirect(signature, pointer,
                &args, normal, unwind, name),
            Some(Object::Function(prototype)) => builder.invoke_indirect_with_prototype(prototype,
                pointer, &args, normal, unwind, name),
            _ => return Err(Error::new("expected an LLVM function type or prototype")),
        }?;
        owned(instruction)
    }
    zk_llvm_invoke_with_bundles(builder: Handle, function: Handle, args: *const Handle, count: usize,
                                normal: Handle, unwind: Handle, bundles: *const Handle,
                                bundle_count: usize, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.invoke_with_bundles(borrow(function)?, &handles(args, count)?,
            borrow(normal)?, borrow(unwind)?, &handles(bundles, bundle_count)?, string(name)?)?)
    }
    zk_llvm_invoke_indirect_with_bundles(builder: Handle, signature: Handle, pointer: Handle,
                                         args: *const Handle, count: usize, normal: Handle, unwind: Handle,
                                         bundles: *const Handle, bundle_count: usize, name: *const c_char) {
        let builder = borrow::<Builder>(builder)?;
        let pointer = borrow::<Value>(pointer)?;
        let args = handles(args, count)?;
        let destinations = (borrow::<Block>(normal)?, borrow::<Block>(unwind)?);
        let bundles = handles(bundles, bundle_count)?;
        let name = string(name)?;
        let instruction = match signature.cast::<Object>().as_ref() {
            Some(Object::Type(signature)) => builder.invoke_indirect_with_bundles(signature, pointer,
                &args, destinations, &bundles, name),
            Some(Object::Function(prototype)) => builder.invoke_indirect_with_prototype_and_bundles(
                prototype, pointer, &args, destinations, &bundles, name),
            _ => return Err(Error::new("expected an LLVM function type or prototype")),
        }?;
        owned(instruction)
    }
    zk_llvm_landing_pad(builder: Handle, ty: Handle, cleanup: u8,
                        clauses: *const Handle, count: usize, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.landing_pad(borrow(ty)?, cleanup != 0,
            &handles(clauses, count)?, string(name)?)?)
    }
    zk_llvm_resume(builder: Handle, exception: Handle) {
        unit(borrow::<Builder>(builder)?.resume(borrow(exception)?))
    }
    zk_llvm_catch_switch(builder: Handle, parent: Handle, unwind: Handle, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.catch_switch(nullable(parent)?, nullable(unwind)?, string(name)?)?)
    }
    zk_llvm_catch_pad(builder: Handle, parent: Handle, args: *const Handle, count: usize,
                      name: *const c_char) {
        owned(borrow::<Builder>(builder)?.catch_pad(borrow(parent)?, &handles(args, count)?, string(name)?)?)
    }
    zk_llvm_cleanup_pad(builder: Handle, parent: Handle, args: *const Handle, count: usize,
                        name: *const c_char) {
        owned(borrow::<Builder>(builder)?.cleanup_pad(nullable(parent)?, &handles(args, count)?, string(name)?)?)
    }
    zk_llvm_catch_ret(builder: Handle, pad: Handle, destination: Handle) {
        owned(borrow::<Builder>(builder)?.catch_ret(borrow(pad)?, borrow(destination)?)?)
    }
    zk_llvm_cleanup_ret(builder: Handle, pad: Handle, unwind: Handle) {
        owned(borrow::<Builder>(builder)?.cleanup_ret(borrow(pad)?, nullable(unwind)?)?)
    }
    zk_llvm_add_catch_switch_handler(instruction: Handle, handler: Handle) {
        unit(borrow::<Instruction>(instruction)?.add_catch_switch_handler(borrow(handler)?))
    }
    zk_llvm_catch_switch_handler_count(instruction: Handle) -> u64 {
        Ok(borrow::<Instruction>(instruction)?.catch_switch_handler_count().into())
    }
    zk_llvm_catch_switch_handler(instruction: Handle, index: u32) {
        optional(borrow::<Instruction>(instruction)?.catch_switch_handler(index))
    }
    zk_llvm_exception_parent_pad(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.exception_parent_pad())
    }
    zk_llvm_exception_unwind_destination(instruction: Handle) {
        optional(borrow::<Instruction>(instruction)?.exception_unwind_destination())
    }
    zk_llvm_make_inline_asm(signature: Handle, assembly: *const u8, assembly_len: usize,
                            constraints: *const u8, constraints_len: usize, options: u32) {
        if options & !15 != 0 { return Err(Error::new("invalid inline asm options")); }
        owned(borrow::<Type>(signature)?.inline_asm(input(assembly, assembly_len)?,
            input(constraints, constraints_len)?, crate::InlineAsmOptions {
                dialect: if options & 1 == 0 { crate::InlineAsmDialect::Att } else { crate::InlineAsmDialect::Intel },
                side_effects: options & 2 != 0,
                align_stack: options & 4 != 0,
                unwind: options & 8 != 0,
            })?)
    }
    zk_llvm_inline_asm_signature(value: Handle) {
        optional(borrow::<Value>(value)?.inline_asm_info().map(|info| info.signature))
    }
    zk_llvm_inline_asm_assembly(value: Handle) {
        let info = borrow::<Value>(value)?.inline_asm_info()
            .ok_or_else(|| Error::new("expected an LLVM inline asm callee"))?;
        owned(info.assembly.to_vec())
    }
    zk_llvm_inline_asm_constraints(value: Handle) {
        let info = borrow::<Value>(value)?.inline_asm_info()
            .ok_or_else(|| Error::new("expected an LLVM inline asm callee"))?;
        owned(info.constraints.to_vec())
    }
    zk_llvm_inline_asm_options(value: Handle) -> u64 {
        let info = borrow::<Value>(value)?.inline_asm_info()
            .ok_or_else(|| Error::new("expected an LLVM inline asm callee"))?;
        Ok(u64::from(info.options.dialect == crate::InlineAsmDialect::Intel)
            | (u64::from(info.options.side_effects) << 1)
            | (u64::from(info.options.align_stack) << 2)
            | (u64::from(info.options.unwind) << 3))
    }
    zk_llvm_block_address(block: Handle) { owned(borrow::<Block>(block)?.address()?) }
    zk_llvm_block_address_target(value: Handle) { optional(borrow::<Value>(value)?.block_address_target()) }
    zk_llvm_indirect_br(builder: Handle, address: Handle, destinations: *const Handle, count: usize) {
        owned(borrow::<Builder>(builder)?.indirect_br(borrow(address)?, &handles(destinations, count)?)?)
    }
    zk_llvm_call_br(builder: Handle, asm: Handle, args: *const Handle, count: usize,
                     normal: Handle, indirect: *const Handle, indirect_count: usize,
                     bundles: *const Handle, bundle_count: usize, name: *const c_char) {
        owned(borrow::<Builder>(builder)?.call_br(borrow(asm)?, &handles(args, count)?, borrow(normal)?,
            &handles(indirect, indirect_count)?, &handles(bundles, bundle_count)?, string(name)?)?)
    }
    zk_llvm_edit_set_fast_math_flags(edit: Handle, instruction: Handle, flags: u32) {
        unit(borrow::<Edit>(edit)?.set_fast_math_flags(borrow(instruction)?, fast_math_flags(flags)?))
    }
}

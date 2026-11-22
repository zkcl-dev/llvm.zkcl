use std::collections::HashMap;
use std::ffi::c_char;
use std::sync::LazyLock;
use std::{slice, str};

use inkwell::attributes::Attribute;
pub use inkwell::attributes::AttributeLoc as AttributeLocation;
use inkwell::context::AsContextRef;
use inkwell::llvm_sys::core::{
    LLVMCreateConstantRangeAttribute, LLVMGetEnumAttributeKind, LLVMGetOperand, LLVMGetParam,
    LLVMGetReturnType, LLVMGetStringAttributeKind, LLVMGetStringAttributeValue,
    LLVMGetTypeAttributeValue, LLVMTypeOf,
};
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::types::{AnyType, AsTypeRef};
use inkwell::values::{
    AnyValue, AsValueRef, CallSiteValue, FunctionValue, InstructionOpcode, LLVMTailCallKind,
};
use num_bigint::{BigInt, Sign};

use super::{Context, Function, Instruction, Type};
use crate::{Error, Result};

/// Payload for an LLVM function, return or parameter attribute.
pub enum AttributeValue<'a> {
    Flag,
    /// A byte count or an LLVM-encoded `memory` or `nofpclass` mask.
    Integer(u64),
    Type(&'a Type),
    String(&'a str),
    /// A half-open, possibly wrapping range over `bit_width`-bit integers.
    /// Each bound may use a signed or unsigned spelling of its bit pattern.
    /// Two zero bounds represent the empty range.
    Range {
        bit_width: u32,
        lower: BigInt,
        upper: BigInt,
    },
}

/// A snapshot of an attribute attached at one LLVM attribute location.
#[derive(Clone)]
pub struct AttributeInfo {
    /// The built-in spelling or string key. A built-in that this binding does
    /// not recognize by ID may have no name when returned by enumeration.
    pub name: Option<String>,
    /// LLVM's kind ID for a built-in attribute. String keys have no kind ID.
    pub kind_id: Option<u32>,
    pub value: AttributeData,
}

/// An attribute payload read directly from LLVM.
#[derive(Clone)]
pub enum AttributeData {
    Flag,
    /// LLVM's integer payload. For attributes such as `memory`, this value
    /// retains LLVM's encoding.
    Integer(u64),
    Type(Type),
    String(String),
    /// A half-open range with bounds in signed two's-complement notation.
    Range {
        bit_width: u32,
        lower: BigInt,
        upper: BigInt,
    },
    /// A payload that the binding does not decode, such as `initializes`.
    Opaque,
}

/// The tail marker attached to an LLVM `call` instruction.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TailCallKind {
    None,
    Tail,
    MustTail,
    NoTail,
}

// LLVM 21.1's llvm/IR/CallingConv.h defines the largest encodable convention.
const MAX_CALLING_CONVENTION: u32 = 1023;

impl Function {
    pub fn calling_convention(&self) -> u32 {
        self.llvm().get_call_conventions()
    }

    /// Set the LLVM calling convention before constructing calls to this function.
    pub fn set_calling_convention(&self, convention: u32) -> Result<()> {
        check_calling_convention(convention)?;
        self.llvm().set_call_conventions(convention);
        Ok(())
    }

    /// Set an attribute by its LLVM spelling, replacing one of the same kind at
    /// this location. Set ABI attributes before emitting calls. Module verification
    /// checks the resulting signature and attribute combinations.
    pub fn set_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        value: AttributeValue<'_>,
    ) -> Result<()> {
        let function = self.attribute_function(location)?;
        let attribute = make_attribute(self.module.context(), location, name, value, || {
            // SAFETY: the function is live and the parameter index was checked above.
            unsafe {
                match location {
                    AttributeLocation::Return => {
                        Some(LLVMGetReturnType(function.get_type().as_type_ref()))
                    }
                    AttributeLocation::Param(index) => {
                        Some(LLVMTypeOf(LLVMGetParam(function.as_value_ref(), index)))
                    }
                    AttributeLocation::Function => None,
                }
            }
        })?;
        function.add_attribute(location, attribute);
        Ok(())
    }

    /// Look up a built-in attribute by its LLVM spelling or a custom string
    /// attribute by its key. Set `string` to true to select a string attribute
    /// whose key also names a built-in attribute.
    pub fn has_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<bool> {
        Ok(self.find_attribute(location, name, string)?.is_some())
    }

    /// Read an attached attribute using the same lookup as [`Self::has_attribute`].
    pub fn attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<Option<AttributeInfo>> {
        let context = self.module.context();
        self.find_attribute(location, name, string)?
            .map(|attribute| AttributeInfo::read(context, attribute, Some(name)))
            .transpose()
    }

    /// List the attributes attached at this location in LLVM's attribute order.
    pub fn attributes(&self, location: AttributeLocation) -> Result<Vec<AttributeInfo>> {
        let context = self.module.context();
        self.attribute_function(location)?
            .attributes(location)
            .into_iter()
            .map(|attribute| AttributeInfo::read(context, attribute, None))
            .collect()
    }

    /// Remove the named attribute, using the same lookup as [`Self::has_attribute`].
    pub fn remove_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<()> {
        let function = self.attribute_function(location)?;
        match AttributeName::new(name, string)? {
            AttributeName::BuiltIn(kind) => function.remove_enum_attribute(location, kind),
            AttributeName::String(key) => function.remove_string_attribute(location, key),
        }
        Ok(())
    }

    fn attribute_function(&self, location: AttributeLocation) -> Result<FunctionValue<'_>> {
        let function = self.llvm();
        check_attribute_parameter(location, function.count_params())?;
        Ok(function)
    }

    fn find_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<Option<Attribute>> {
        let function = self.attribute_function(location)?;
        Ok(match AttributeName::new(name, string)? {
            AttributeName::BuiltIn(kind) => function.get_enum_attribute(location, kind),
            AttributeName::String(key) => function.get_string_attribute(location, key),
        })
    }
}

impl Instruction {
    fn call_site(&self) -> Result<CallSiteValue<'_>> {
        let instruction = self.llvm();
        match instruction.get_opcode() {
            InstructionOpcode::Call | InstructionOpcode::Invoke => {
                // SAFETY: call and invoke are both LLVM CallBase instructions.
                Ok(unsafe { CallSiteValue::new(instruction.as_value_ref()) })
            }
            _ => Err(Error::new("expected an LLVM call or invoke instruction")),
        }
    }

    pub fn tail_call_kind(&self) -> Result<TailCallKind> {
        use LLVMTailCallKind::*;

        let kind = self
            .llvm()
            .get_tail_call_kind()
            .map_err(|_| Error::new("expected an LLVM call instruction"))?;
        Ok(match kind {
            LLVMTailCallKindNone => TailCallKind::None,
            LLVMTailCallKindTail => TailCallKind::Tail,
            LLVMTailCallKindMustTail => TailCallKind::MustTail,
            LLVMTailCallKindNoTail => TailCallKind::NoTail,
        })
    }

    /// Set the marker on this call. Module verification checks restrictions
    /// such as the signature and following return instruction for `musttail`.
    pub fn set_tail_call_kind(&self, kind: TailCallKind) -> Result<()> {
        use LLVMTailCallKind::*;

        let kind = match kind {
            TailCallKind::None => LLVMTailCallKindNone,
            TailCallKind::Tail => LLVMTailCallKindTail,
            TailCallKind::MustTail => LLVMTailCallKindMustTail,
            TailCallKind::NoTail => LLVMTailCallKindNoTail,
        };
        self.llvm()
            .set_tail_call_kind(kind)
            .map_err(|_| Error::new("expected an LLVM call instruction"))
    }

    pub fn call_calling_convention(&self) -> Result<u32> {
        Ok(self.call_site()?.get_call_convention())
    }

    /// Set the convention on this call. It must agree with the callee's ABI.
    pub fn set_call_calling_convention(&self, convention: u32) -> Result<()> {
        check_calling_convention(convention)?;
        self.call_site()?.set_call_convention(convention);
        Ok(())
    }

    /// Set a call-site attribute, replacing one of the same kind and location.
    /// Parameter indices refer to actual arguments, including variadic arguments.
    /// Module verification checks restrictions on the resulting IR.
    pub fn set_call_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        value: AttributeValue<'_>,
    ) -> Result<()> {
        let call = self.attribute_call(location)?;
        let context = self.block().function.module.context();
        let attribute = make_attribute(context, location, name, value, || {
            let call = call.as_value_ref();
            // SAFETY: the call is live and its first operands are the actual
            // arguments. The argument index was checked above.
            unsafe {
                match location {
                    AttributeLocation::Return => Some(LLVMTypeOf(call)),
                    AttributeLocation::Param(index) => {
                        Some(LLVMTypeOf(LLVMGetOperand(call, index)))
                    }
                    AttributeLocation::Function => None,
                }
            }
        })?;
        call.add_attribute(location, attribute);
        Ok(())
    }

    /// Check attributes explicitly attached to this call. Name selection is
    /// the same as [`Function::has_attribute`].
    pub fn has_call_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<bool> {
        Ok(self.find_call_attribute(location, name, string)?.is_some())
    }

    /// Read an attribute explicitly attached to this call using the same lookup
    /// as [`Self::has_call_attribute`].
    pub fn call_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<Option<AttributeInfo>> {
        let context = self.block().function.module.context();
        self.find_call_attribute(location, name, string)?
            .map(|attribute| AttributeInfo::read(context, attribute, Some(name)))
            .transpose()
    }

    /// List the attributes explicitly attached to this call at the given location.
    pub fn call_attributes(&self, location: AttributeLocation) -> Result<Vec<AttributeInfo>> {
        let context = self.block().function.module.context();
        self.attribute_call(location)?
            .attributes(location)
            .into_iter()
            .map(|attribute| AttributeInfo::read(context, attribute, None))
            .collect()
    }

    /// Remove the named attribute from this call.
    pub fn remove_call_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<()> {
        let call = self.attribute_call(location)?;
        match AttributeName::new(name, string)? {
            AttributeName::BuiltIn(kind) => call.remove_enum_attribute(location, kind),
            AttributeName::String(key) => call.remove_string_attribute(location, key),
        }
        Ok(())
    }

    fn attribute_call(&self, location: AttributeLocation) -> Result<CallSiteValue<'_>> {
        let call = self.call_site()?;
        check_attribute_parameter(location, call.count_arguments())?;
        if location == AttributeLocation::Return && self.result().is_none() {
            return Err(Error::new("a void LLVM call has no return attributes"));
        }
        Ok(call)
    }

    fn find_call_attribute(
        &self,
        location: AttributeLocation,
        name: &str,
        string: bool,
    ) -> Result<Option<Attribute>> {
        let call = self.attribute_call(location)?;
        Ok(match AttributeName::new(name, string)? {
            AttributeName::BuiltIn(kind) => call.get_enum_attribute(location, kind),
            AttributeName::String(key) => call.get_string_attribute(location, key),
        })
    }
}

impl AttributeInfo {
    fn read(context: &Context, attribute: Attribute, queried_name: Option<&str>) -> Result<Self> {
        let raw = attribute.as_mut_ptr();
        if attribute.is_string() {
            let mut name_len = 0;
            let mut value_len = 0;
            // SAFETY: the context retains this string attribute and both byte slices.
            let (name, value) = unsafe {
                let name = LLVMGetStringAttributeKind(raw, &mut name_len);
                let value = LLVMGetStringAttributeValue(raw, &mut value_len);
                (
                    attribute_text(name, name_len)?,
                    attribute_text(value, value_len)?,
                )
            };
            return Ok(Self {
                name: Some(name),
                kind_id: None,
                value: AttributeData::String(value),
            });
        }

        // SAFETY: LLVMGetEnumAttributeKind accepts every non-string attribute,
        // including kinds without a public C API payload accessor.
        let kind_id = unsafe { LLVMGetEnumAttributeKind(raw) };
        let spec = ATTRIBUTE_SPECS.get(&kind_id);
        let value = if attribute.is_type() {
            // SAFETY: the context owns the type stored by this type attribute.
            AttributeData::Type(context.type_ref(unsafe { LLVMGetTypeAttributeValue(raw) }))
        } else if attribute.is_enum() {
            match spec.map(|spec| spec.payload) {
                Some(Payload::Flag) => AttributeData::Flag,
                Some(Payload::ByteCount | Payload::Integer) => {
                    AttributeData::Integer(attribute.get_enum_value())
                }
                _ => AttributeData::Opaque,
            }
        } else if matches!(spec.map(|spec| spec.payload), Some(Payload::Range)) {
            read_range(context, attribute)?
        } else {
            AttributeData::Opaque
        };
        Ok(Self {
            name: spec
                .map(|spec| spec.name)
                .or(queried_name)
                .map(str::to_owned),
            kind_id: Some(kind_id),
            value,
        })
    }
}

fn read_range(context: &Context, attribute: Attribute) -> Result<AttributeData> {
    // LLVM 21's C API can create range attributes but has no accessor for their
    // bounds. Print just this attribute on an otherwise empty declaration. The
    // printer formats the range from its own bit width, independent of the
    // placeholder return type, and does not change the user's module.
    let llvm = context.llvm();
    let module = llvm.create_module("attribute");
    let probe = module.add_function("attribute", llvm.bool_type().fn_type(&[], false), None);
    probe.add_attribute(AttributeLocation::Return, attribute);
    let text = probe.print_to_string();
    let decoded = || {
        let (range, _) = text
            .to_str()
            .ok()?
            .trim()
            .strip_prefix("declare range(i")?
            .split_once(") ")?;
        let (width, bounds) = range.split_once(' ')?;
        let (lower, upper) = bounds.split_once(", ")?;
        Some(AttributeData::Range {
            bit_width: width.parse().ok()?,
            lower: lower.parse().ok()?,
            upper: upper.parse().ok()?,
        })
    };
    decoded().ok_or_else(|| attribute_error("range", "has an unrecognized LLVM representation"))
}

/// `text` must be readable for `len` bytes unless `len` is zero.
unsafe fn attribute_text(text: *const c_char, len: u32) -> Result<String> {
    if len == 0 {
        return Ok(String::new());
    }
    // SAFETY: the caller supplies LLVM's pointer and exact byte length.
    let bytes = unsafe { slice::from_raw_parts(text.cast(), len as usize) };
    str::from_utf8(bytes)
        .map(str::to_owned)
        .map_err(|_| Error::new("LLVM string attribute is not valid UTF-8"))
}

enum AttributeName<'a> {
    BuiltIn(u32),
    String(&'a str),
}

impl<'a> AttributeName<'a> {
    fn new(name: &'a str, string: bool) -> Result<Self> {
        if name.is_empty() {
            return Err(Error::new("LLVM attribute name is empty"));
        }
        if !string {
            let kind = Attribute::get_named_enum_kind_id(name);
            if kind != 0 {
                return Ok(Self::BuiltIn(kind));
            }
        }
        if u32::try_from(name.len()).is_err() {
            return Err(attribute_error(
                name,
                "exceeds the LLVM string attribute length limit",
            ));
        }
        Ok(Self::String(name))
    }
}

fn check_calling_convention(convention: u32) -> Result<()> {
    if convention > MAX_CALLING_CONVENTION {
        return Err(Error::new("LLVM calling convention ID is out of range"));
    }
    Ok(())
}

fn check_attribute_parameter(location: AttributeLocation, argument_count: u32) -> Result<()> {
    if let AttributeLocation::Param(index) = location
        && (index >= argument_count || index > u32::MAX - 2)
    {
        return Err(Error::new("LLVM attribute parameter index is out of range"));
    }
    Ok(())
}

fn make_attribute(
    context: &Context,
    location: AttributeLocation,
    name: &str,
    value: AttributeValue<'_>,
    target_type: impl FnOnce() -> Option<LLVMTypeRef>,
) -> Result<Attribute> {
    if name.is_empty() {
        return Err(Error::new("LLVM attribute name is empty"));
    }
    let llvm = context.llvm();
    if let AttributeValue::String(text) = value {
        if u32::try_from(name.len().max(text.len())).is_err() {
            return Err(attribute_error(
                name,
                "exceeds the LLVM string attribute length limit",
            ));
        }
        return Ok(llvm.create_string_attribute(name, text));
    }

    let kind = Attribute::get_named_enum_kind_id(name);
    let Some(spec) = ATTRIBUTE_SPECS.get(&kind) else {
        return Err(attribute_error(name, "is unknown"));
    };
    let site = match location {
        AttributeLocation::Function => FUNCTION,
        AttributeLocation::Return => RETURN,
        AttributeLocation::Param(_) => PARAMETER,
    };
    if spec.locations & site == 0 {
        return Err(attribute_error(name, "is invalid at this location"));
    }

    match (value, spec.payload) {
        (AttributeValue::Flag, Payload::Flag) => Ok(llvm.create_enum_attribute(kind, 0)),
        (AttributeValue::Type(ty), Payload::Type) => {
            let ty = ty.basic_in(context)?.as_any_type_enum();
            Ok(llvm.create_type_attribute(kind, ty))
        }
        (AttributeValue::Integer(bytes), Payload::ByteCount) => {
            validate_byte_count(name, bytes)?;
            Ok(llvm.create_enum_attribute(kind, bytes))
        }
        (AttributeValue::Integer(value), Payload::Integer) => {
            validate_encoded_integer(name, value)?;
            Ok(llvm.create_enum_attribute(kind, value))
        }
        (
            AttributeValue::Range {
                bit_width,
                lower,
                upper,
            },
            Payload::Range,
        ) => {
            let width = target_type()
                .and_then(|ty| context.type_ref(ty).scalar_type().integer_width().ok());
            if width != Some(bit_width) {
                return Err(attribute_error(
                    name,
                    "requires the bit width of the integer or integer vector",
                ));
            }
            make_range(context, kind, bit_width, lower, upper)
        }
        (AttributeValue::Flag, _) => Err(attribute_error(name, "requires a payload")),
        (AttributeValue::Integer(_), _) => Err(attribute_error(name, "does not accept an integer")),
        (AttributeValue::Type(_), _) => Err(attribute_error(name, "does not accept an LLVM type")),
        (AttributeValue::Range { .. }, _) => Err(attribute_error(name, "does not accept a range")),
        (AttributeValue::String(_), _) => unreachable!("a string attribute was handled above"),
    }
}

fn make_range(
    context: &Context,
    kind: u32,
    bit_width: u32,
    lower: BigInt,
    upper: BigInt,
) -> Result<Attribute> {
    let modulus = BigInt::from(1) << bit_width;
    let signed_minimum = -(&modulus >> 1u32);
    let word_count = bit_width.div_ceil(64) as usize;
    let pack = |bound: BigInt| {
        if bound < signed_minimum || bound >= modulus {
            return Err(attribute_error(
                "range",
                "has a bound outside its bit width",
            ));
        }
        let unsigned = if bound.sign() == Sign::Minus {
            bound + &modulus
        } else {
            bound
        };
        let (_, mut words) = unsigned.to_u64_digits();
        words.resize(word_count, 0);
        Ok(words)
    };
    let lower = pack(lower)?;
    let upper = pack(upper)?;
    if lower == upper && lower.iter().any(|&word| word != 0) {
        return Err(attribute_error(
            "range",
            "requires distinct bounds or two zero bounds",
        ));
    }
    // SAFETY: the bit width comes from a live integer type, both arrays have
    // LLVM's required number of words, and the bounds represent a non-full range.
    let raw = unsafe {
        LLVMCreateConstantRangeAttribute(
            context.llvm().as_ctx_ref(),
            kind,
            bit_width,
            lower.as_ptr(),
            upper.as_ptr(),
        )
    };
    // SAFETY: LLVM created a live attribute retained by the context.
    Ok(unsafe { Attribute::new(raw) })
}

fn attribute_error(name: &str, reason: &str) -> Error {
    Error::new(format!("LLVM attribute '{name}' {reason}"))
}

fn validate_byte_count(name: &str, bytes: u64) -> Result<()> {
    let alignment_limit = match name {
        "align" => 1_u64 << 32, // llvm/IR/Value.h, Value::MaximumAlignment
        "alignstack" => 256,    // llvm/IR/Attributes.h, Attribute::getWithStackAlignment
        _ if bytes > 0 => return Ok(()),
        _ => return Err(attribute_error(name, "requires a positive byte count")),
    };
    if !bytes.is_power_of_two() || bytes > alignment_limit {
        return Err(attribute_error(
            name,
            "requires a representable power-of-two alignment",
        ));
    }
    Ok(())
}

fn validate_encoded_integer(name: &str, value: u64) -> Result<()> {
    match name {
        // llvm/Support/ModRef.h encodes two bits per each of four locations.
        "memory" if value <= 0xff => Ok(()),
        "memory" => Err(attribute_error(
            name,
            "requires an eight-bit memory effects mask",
        )),
        // llvm/ADT/FloatingPointMode.h defines ten FPClassTest flags.
        "nofpclass" if (1..=0x3ff).contains(&value) => Ok(()),
        "nofpclass" => Err(attribute_error(
            name,
            "requires a nonzero ten-bit floating-point class mask",
        )),
        _ => Err(attribute_error(
            name,
            "does not support setting an encoded integer",
        )),
    }
}

#[derive(Clone, Copy)]
enum Payload {
    Flag,
    ByteCount,
    Type,
    Integer,
    Range,
    Other,
}

struct AttributeSpec {
    name: &'static str,
    payload: Payload,
    locations: u8,
}

const FUNCTION: u8 = 1;
const RETURN: u8 = 2;
const PARAMETER: u8 = 4;

// LLVM 21.1's llvm/IR/Attributes.td defines these payloads and locations.
// Resolve kind IDs against the linked LLVM since the IDs can change.
static ATTRIBUTE_SPECS: LazyLock<HashMap<u32, AttributeSpec>> = LazyLock::new(|| {
    const GROUPS: &[(Payload, u8, &[&str])] = &[
        (
            Payload::Flag,
            FUNCTION,
            &[
                "alwaysinline",
                "builtin",
                "cold",
                "convergent",
                "coro_only_destroy_when_complete",
                "coro_elide_safe",
                "disable_sanitizer_instrumentation",
                "fn_ret_thunk_extern",
                "hot",
                "hybrid_patchable",
                "inlinehint",
                "jumptable",
                "minsize",
                "mustprogress",
                "naked",
                "nobuiltin",
                "nocallback",
                "nocf_check",
                "nodivergencesource",
                "noduplicate",
                "noimplicitfloat",
                "noinline",
                "nomerge",
                "noprofile",
                "norecurse",
                "noredzone",
                "noreturn",
                "nosanitize_bounds",
                "nosanitize_coverage",
                "nosync",
                "nounwind",
                "nonlazybind",
                "null_pointer_is_valid",
                "optforfuzzing",
                "optdebug",
                "optsize",
                "optnone",
                "presplitcoroutine",
                "returns_twice",
                "safestack",
                "sanitize_address",
                "sanitize_hwaddress",
                "sanitize_memtag",
                "sanitize_memory",
                "sanitize_numerical_stability",
                "sanitize_realtime",
                "sanitize_realtime_blocking",
                "sanitize_thread",
                "sanitize_type",
                "shadowcallstack",
                "skipprofile",
                "speculatable",
                "speculative_load_hardening",
                "ssp",
                "sspreq",
                "sspstrong",
                "strictfp",
                "willreturn",
            ],
        ),
        (
            Payload::Flag,
            PARAMETER,
            &[
                "allocalign",
                "allocptr",
                "dead_on_return",
                "dead_on_unwind",
                "immarg",
                "nest",
                "readnone",
                "readonly",
                "returned",
                "swiftasync",
                "swifterror",
                "swiftself",
                "writable",
                "writeonly",
            ],
        ),
        (
            Payload::Flag,
            PARAMETER | RETURN,
            &[
                "inreg", "noalias", "noext", "noundef", "nonnull", "signext", "zeroext",
            ],
        ),
        (Payload::Flag, FUNCTION | PARAMETER, &["nofree"]),
        (
            Payload::Type,
            PARAMETER,
            &["byref", "byval", "elementtype", "inalloca", "sret"],
        ),
        (Payload::Type, FUNCTION | PARAMETER, &["preallocated"]),
        (
            Payload::ByteCount,
            PARAMETER | RETURN,
            &["align", "dereferenceable", "dereferenceable_or_null"],
        ),
        (
            Payload::ByteCount,
            FUNCTION | PARAMETER | RETURN,
            &["alignstack"],
        ),
        (
            Payload::Integer,
            FUNCTION,
            &[
                "allockind",
                "allocsize",
                "memory",
                "uwtable",
                "vscale_range",
            ],
        ),
        (Payload::Integer, PARAMETER, &["captures"]),
        (Payload::Integer, PARAMETER | RETURN, &["nofpclass"]),
        (Payload::Other, PARAMETER, &["initializes"]),
        (Payload::Range, PARAMETER | RETURN, &["range"]),
    ];
    let mut specs = HashMap::new();
    for &(payload, locations, names) in GROUPS {
        for &name in names {
            let kind_id = Attribute::get_named_enum_kind_id(name);
            if kind_id != 0 {
                specs.insert(
                    kind_id,
                    AttributeSpec {
                        name,
                        payload,
                        locations,
                    },
                );
            }
        }
    }
    specs
});

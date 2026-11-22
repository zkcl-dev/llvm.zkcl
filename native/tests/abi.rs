use num_bigint::BigInt;
use zkcl_llvm_api::{
    AttributeData, AttributeLocation as At, AttributeValue as Attr, Builder, Context, TailCallKind,
};

#[test]
fn attribute_payloads_and_locations_are_checked_before_applying_them() {
    let context = Context::default();
    let module = context.module("attributes").unwrap();
    let ptr = context.pointer_type();
    let signature = context.void_type().function(&[ptr], false).unwrap();
    let function = module.add_function("write_record", &signature).unwrap();
    let foreign = Context::default().int_type(64).unwrap();
    let before = module.ir();

    for (location, name, value) in [
        (At::Function, "unknown-attribute", Attr::Flag),
        (At::Function, "sret", Attr::Type(&foreign)),
        (At::Param(1), "noundef", Attr::Flag),
        (At::Param(0), "noinline", Attr::Flag),
        (At::Param(0), "sret", Attr::Flag),
        (At::Param(0), "sret", Attr::Type(&foreign)),
        (At::Param(0), "align", Attr::Integer(3)),
        (At::Function, "alignstack", Attr::Integer(512)),
    ] {
        assert!(
            function.set_attribute(location, name, value).is_err(),
            "{name}"
        );
    }
    assert!(function.set_calling_convention(1024).is_err());
    assert_eq!(module.ir(), before);

    function
        .set_attribute(At::Param(0), "align", Attr::Integer(4))
        .unwrap();
    function
        .set_attribute(At::Param(0), "align", Attr::Integer(8))
        .unwrap();
    function
        .set_attribute(At::Function, "noinline", Attr::Flag)
        .unwrap();
    function
        .set_attribute(At::Function, "frame-pointer", Attr::String("all"))
        .unwrap();
    assert!(
        function
            .has_attribute(At::Param(0), "align", false)
            .unwrap()
    );
    assert!(
        function
            .has_attribute(At::Function, "frame-pointer", false)
            .unwrap()
    );
    function
        .set_attribute(At::Function, "noinline", Attr::String("märk\0er"))
        .unwrap();
    assert!(
        function
            .has_attribute(At::Function, "noinline", false)
            .unwrap()
    );
    assert!(
        function
            .has_attribute(At::Function, "noinline", true)
            .unwrap()
    );
    let flag = function
        .attribute(At::Function, "noinline", false)
        .unwrap()
        .unwrap();
    let text = function
        .attribute(At::Function, "noinline", true)
        .unwrap()
        .unwrap();
    assert!(matches!(flag.value, AttributeData::Flag));
    assert!(flag.kind_id.is_some());
    assert!(matches!(text.value, AttributeData::String(value) if value == "märk\0er"));
    assert_eq!(text.kind_id, None);
    let parameter = function.attributes(At::Param(0)).unwrap();
    assert_eq!(parameter.len(), 1);
    assert_eq!(parameter[0].name.as_deref(), Some("align"));
    assert!(matches!(parameter[0].value, AttributeData::Integer(8)));
    assert!(function.attributes(At::Param(1)).is_err());
    function
        .remove_attribute(At::Function, "noinline", true)
        .unwrap();
    assert!(
        function
            .attribute(At::Function, "noinline", true)
            .unwrap()
            .is_none()
    );
    assert!(
        function
            .has_attribute(At::Function, "noinline", false)
            .unwrap()
    );
    assert!(
        !function
            .has_attribute(At::Function, "noinline", true)
            .unwrap()
    );
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("ptr align 8"));
    assert!(!ir.contains("ptr align 4"));
    assert!(ir.contains("\"frame-pointer\"=\"all\""));
}

#[test]
fn parsed_attributes_preserve_integer_range_and_opaque_payloads() {
    let module = Context::default()
        .parse_ir("declare range(i32 0, 8) i32 @bounded(ptr initializes((0, 4))) memory(none)")
        .unwrap();
    let function = module.find_function("bounded").unwrap().unwrap();
    let range = function
        .attribute(At::Return, "range", false)
        .unwrap()
        .unwrap();
    assert!(matches!(
        range.value,
        AttributeData::Range { bit_width: 32, lower, upper }
            if lower == BigInt::from(0) && upper == BigInt::from(8)
    ));
    assert_eq!(range.name.as_deref(), Some("range"));
    assert!(range.kind_id.is_some());
    let attributes = function.attributes(At::Return).unwrap();
    assert_eq!(attributes.len(), 1);
    assert_eq!(attributes[0].kind_id, range.kind_id);
    let memory = function
        .attribute(At::Function, "memory", false)
        .unwrap()
        .unwrap();
    assert!(matches!(memory.value, AttributeData::Integer(0)));
    let initializes = function
        .attribute(At::Param(0), "initializes", false)
        .unwrap()
        .unwrap();
    assert!(matches!(initializes.value, AttributeData::Opaque));
    assert_eq!(initializes.name.as_deref(), Some("initializes"));

    function
        .set_attribute(At::Function, "memory", Attr::Integer(0x55))
        .unwrap();
    assert!(matches!(
        function
            .attribute(At::Function, "memory", false)
            .unwrap()
            .unwrap()
            .value,
        AttributeData::Integer(0x55)
    ));
    assert!(
        function
            .set_attribute(At::Function, "memory", Attr::Integer(0x100))
            .is_err()
    );
}

#[test]
fn wide_range_attributes_round_trip_on_functions_and_calls() {
    let context = Context::default();
    let module = context.module("wide-range").unwrap();
    let i130 = context.int_type(130).unwrap();
    let signature = i130.function(&[i130.clone()], false).unwrap();
    let callee = module.add_function("callee", &signature).unwrap();
    let make_range = || Attr::Range {
        bit_width: 130,
        lower: (BigInt::from(1) << 130u32) - 9u32,
        upper: (BigInt::from(1) << 100u32) + 7u32,
    };
    let before = module.ir();
    for (bit_width, lower, upper) in [
        (129, BigInt::from(0), BigInt::from(1)),
        (130, BigInt::from(1) << 130u32, BigInt::from(1)),
        (130, BigInt::from(-1), (BigInt::from(1) << 130u32) - 1u32),
    ] {
        assert!(
            callee
                .set_attribute(
                    At::Param(0),
                    "range",
                    Attr::Range {
                        bit_width,
                        lower,
                        upper
                    }
                )
                .is_err()
        );
    }
    assert_eq!(module.ir(), before);
    callee
        .set_attribute(At::Param(0), "range", make_range())
        .unwrap();

    let caller = module.add_function("forward", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&caller.append_block("entry").unwrap())
        .unwrap();
    let argument = caller.parameter(0).unwrap().unwrap();
    let call = builder
        .call_instruction(&callee, &[argument], "result")
        .unwrap();
    call.set_call_attribute(At::Return, "range", make_range())
        .unwrap();
    builder.ret(call.result().as_ref()).unwrap();
    let copy = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let callee = copy.find_function("callee").unwrap().unwrap();
    let caller = copy.find_function("forward").unwrap().unwrap();
    let call = caller
        .blocks()
        .next()
        .unwrap()
        .instructions()
        .next()
        .unwrap();
    for attribute in [
        callee
            .attribute(At::Param(0), "range", false)
            .unwrap()
            .unwrap(),
        call.call_attribute(At::Return, "range", false)
            .unwrap()
            .unwrap(),
    ] {
        assert!(matches!(
            attribute.value,
            AttributeData::Range { bit_width: 130, lower, upper }
                if lower == BigInt::from(-9) && upper == (BigInt::from(1) << 100u32) + 7u32
        ));
    }
}

#[test]
fn an_indirect_call_uses_the_prototype_abi_without_referring_to_its_symbol() {
    let context = Context::default();
    let module = context.module("dispatch").unwrap();
    let declarations = context.module("prototypes").unwrap();
    let ptr = context.pointer_type();
    let i8 = context.int_type(8).unwrap();
    let i64 = context.int_type(64).unwrap();
    let record = context.opaque_struct_type("Record").unwrap();
    record
        .set_struct_body(&[i64.clone(), i64.clone(), i64], false)
        .unwrap();
    let void = context.void_type();
    let signature = void.function(&[ptr.clone(), i8.clone()], false).unwrap();
    let prototype = declarations
        .add_function("record_factory", &signature)
        .unwrap();
    prototype.set_calling_convention(8).unwrap();
    assert_eq!(prototype.calling_convention(), 8);
    prototype
        .set_attribute(At::Param(0), "sret", Attr::Type(&record))
        .unwrap();
    prototype
        .set_attribute(At::Param(0), "align", Attr::Integer(8))
        .unwrap();
    prototype
        .set_attribute(At::Param(1), "signext", Attr::Flag)
        .unwrap();
    prototype
        .set_attribute(At::Function, "noinline", Attr::Flag)
        .unwrap();

    let caller = module
        .add_function(
            "dispatch",
            &void.function(&[ptr.clone(), ptr, i8], false).unwrap(),
        )
        .unwrap();
    let incompatible = module.add_function("different_abi", &signature).unwrap();
    let compatible = module.add_function("same_abi", &signature).unwrap();
    for callee in [&incompatible, &compatible] {
        callee.set_calling_convention(8).unwrap();
        callee
            .set_attribute(At::Param(0), "sret", Attr::Type(&record))
            .unwrap();
        callee
            .set_attribute(At::Param(0), "align", Attr::Integer(8))
            .unwrap();
    }
    incompatible
        .set_attribute(At::Param(1), "zeroext", Attr::Flag)
        .unwrap();
    compatible
        .set_attribute(At::Param(1), "signext", Attr::Flag)
        .unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&caller.append_block("entry").unwrap())
        .unwrap();
    let callback = caller.parameter(0).unwrap().unwrap();
    let arguments = [
        caller.parameter(1).unwrap().unwrap(),
        caller.parameter(2).unwrap().unwrap(),
    ];
    let before = module.ir();
    let error = builder
        .call_indirect_with_prototype(&prototype, &incompatible.address().unwrap(), &arguments, "")
        .err()
        .unwrap();
    assert!(error.to_string().contains("does not match"));
    assert_eq!(module.ir(), before);

    assert!(
        builder
            .call_indirect_with_prototype(
                &prototype,
                &compatible.address().unwrap(),
                &arguments,
                ""
            )
            .unwrap()
            .is_none()
    );

    let call = builder
        .call_indirect_instruction_with_prototype(&prototype, &callback, &arguments, "")
        .unwrap();
    assert!(call.result().is_none());
    assert_eq!(call.call_calling_convention().unwrap(), 8);
    assert!(
        call.has_call_attribute(At::Param(0), "sret", false)
            .unwrap()
    );
    let sret = call
        .call_attribute(At::Param(0), "sret", false)
        .unwrap()
        .unwrap();
    assert!(
        matches!(sret.value, AttributeData::Type(ty) if ty.struct_name().unwrap().as_deref() == Some("Record"))
    );
    assert!(
        !call
            .has_call_attribute(At::Function, "noinline", false)
            .unwrap()
    );
    call.set_call_attribute(At::Param(0), "align", Attr::Integer(16))
        .unwrap();
    call.set_call_attribute(At::Function, "nounwind", Attr::Flag)
        .unwrap();
    builder.ret(None).unwrap();
    drop(prototype);
    drop(declarations);
    let bitcode = module.bitcode().unwrap();
    let copy = Context::default().parse_bitcode(&bitcode).unwrap();
    let ir = copy.ir();
    assert!(
        ir.contains("call fastcc void %0(ptr sret(%Record) align 16 %1, i8 signext %2)"),
        "{ir}"
    );
    assert!(ir.contains("@same_abi(ptr sret(%Record) align 8"), "{ir}");
    assert!(ir.contains("nounwind"), "{ir}");
    assert!(!ir.contains("record_factory"));
    assert!(!ir.contains("noinline"));
}

#[test]
fn direct_call_instruction_supports_each_tail_marker() {
    let context = Context::default();
    let module = context.module("tail-markers").unwrap();
    let i32 = context.int_type(32).unwrap();
    let signature = i32.function(&[], false).unwrap();
    let callee = module.add_function("callee", &signature).unwrap();
    let caller = module.add_function("forward", &signature).unwrap();
    for function in [&callee, &caller] {
        function.set_calling_convention(8).unwrap();
        function
            .set_attribute(At::Return, "signext", Attr::Flag)
            .unwrap();
    }

    let builder = Builder::new(&context);
    let block = caller.append_block("entry").unwrap();
    builder.position_at_end(&block).unwrap();
    let call = builder.call_instruction(&callee, &[], "answer").unwrap();
    assert_eq!(call.opcode(), "call");
    assert_eq!(call.call_calling_convention().unwrap(), 8);
    assert_eq!(call.tail_call_kind().unwrap(), TailCallKind::None);
    for (kind, spelling) in [
        (TailCallKind::Tail, "tail "),
        (TailCallKind::NoTail, "notail "),
        (TailCallKind::None, ""),
        (TailCallKind::MustTail, "musttail "),
    ] {
        call.set_tail_call_kind(kind).unwrap();
        assert_eq!(call.tail_call_kind().unwrap(), kind);
        let ir = call.ir();
        assert!(
            ir.contains(&format!("= {spelling}call fastcc signext i32 @callee()")),
            "{ir}"
        );
    }
    builder.ret(call.result().as_ref()).unwrap();

    let bitcode = module.bitcode().unwrap();
    let copy = Context::default().parse_bitcode(&bitcode).unwrap();
    let forward = copy.find_function("forward").unwrap().unwrap();
    let entry = forward.blocks().next().unwrap();
    let call = entry.instructions().next().unwrap();
    assert_eq!(call.tail_call_kind().unwrap(), TailCallKind::MustTail);
    assert_eq!(call.call_calling_convention().unwrap(), 8);
    let attributes = call.call_attributes(At::Return).unwrap();
    assert_eq!(attributes.len(), 1);
    assert_eq!(attributes[0].name.as_deref(), Some("signext"));
    assert!(matches!(attributes[0].value, AttributeData::Flag));
    let ret = entry.terminator().unwrap();
    assert!(ret.tail_call_kind().is_err());
    assert!(ret.set_tail_call_kind(TailCallKind::Tail).is_err());
    assert!(ret.call_calling_convention().is_err());
    assert!(ret.set_call_calling_convention(8).is_err());
    assert!(ret.call_attributes(At::Function).is_err());
    assert!(
        ret.set_call_attribute(At::Function, "nounwind", Attr::Flag)
            .is_err()
    );
}

#[test]
fn call_site_attributes_apply_to_actual_arguments_and_leave_the_callee_unchanged() {
    let context = Context::default();
    let module = context.module("call-site-attributes").unwrap();
    let ptr = context.pointer_type();
    let i8 = context.int_type(8).unwrap();
    let i32 = context.int_type(32).unwrap();
    let signature = i32.function(&[ptr.clone()], true).unwrap();
    let caller = module
        .add_function(
            "dispatch",
            &i32.function(&[ptr.clone(), ptr, i8], false).unwrap(),
        )
        .unwrap();
    let callee = module.add_function("callee", &signature).unwrap();
    let void = module
        .add_function("effect", &context.void_type().function(&[], false).unwrap())
        .unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&caller.append_block("entry").unwrap())
        .unwrap();
    let arguments = [
        caller.parameter(1).unwrap().unwrap(),
        caller.parameter(2).unwrap().unwrap(),
    ];
    let call = builder
        .call_indirect_instruction(&signature, &callee.address().unwrap(), &arguments, "result")
        .unwrap();
    let effect = builder.call_instruction(&void, &[], "").unwrap();
    assert!(effect.result().is_none());
    let before = module.ir();
    let foreign = Context::default().int_type(64).unwrap();
    for (location, name, value) in [
        (At::Param(2), "noundef", Attr::Flag),
        (At::Param(u32::MAX), "noundef", Attr::Flag),
        (At::Function, "sret", Attr::Type(&i32)),
        (At::Param(0), "noinline", Attr::Flag),
        (At::Param(0), "sret", Attr::Type(&foreign)),
        (At::Param(0), "align", Attr::Integer(3)),
    ] {
        assert!(
            call.set_call_attribute(location, name, value).is_err(),
            "{name}"
        );
    }
    assert!(
        effect
            .set_call_attribute(At::Return, "noundef", Attr::Flag)
            .is_err()
    );
    assert!(call.set_call_calling_convention(1024).is_err());
    assert_eq!(module.ir(), before);

    call.set_call_calling_convention(8).unwrap();
    assert_eq!(call.call_calling_convention().unwrap(), 8);
    assert_eq!(callee.calling_convention(), 0);
    call.set_call_calling_convention(0).unwrap();
    call.set_call_attribute(At::Return, "zeroext", Attr::Flag)
        .unwrap();
    call.set_call_attribute(At::Param(0), "align", Attr::Integer(8))
        .unwrap();
    call.set_call_attribute(At::Param(0), "align", Attr::Integer(16))
        .unwrap();
    call.set_call_attribute(At::Param(1), "signext", Attr::Flag)
        .unwrap();
    call.set_call_attribute(At::Function, "noinline", Attr::Flag)
        .unwrap();
    call.set_call_attribute(At::Function, "note", Attr::String("example"))
        .unwrap();
    assert!(
        call.has_call_attribute(At::Function, "note", false)
            .unwrap()
    );
    call.remove_call_attribute(At::Function, "note", false)
        .unwrap();
    assert!(
        !call
            .has_call_attribute(At::Function, "note", false)
            .unwrap()
    );
    assert!(
        call.has_call_attribute(At::Param(1), "signext", false)
            .unwrap()
    );
    call.remove_call_attribute(At::Param(1), "signext", false)
        .unwrap();
    assert!(
        !call
            .has_call_attribute(At::Param(1), "signext", false)
            .unwrap()
    );
    call.set_call_attribute(At::Param(1), "signext", Attr::Flag)
        .unwrap();
    builder.ret(call.result().as_ref()).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("declare i32 @callee(ptr, ...)"), "{ir}");
    assert!(
        ir.contains("call zeroext i32 (ptr, ...) @callee(ptr align 16 %1, i8 signext %2)"),
        "{ir}"
    );
    assert!(ir.contains("noinline"), "{ir}");

    call.set_tail_call_kind(TailCallKind::MustTail).unwrap();
    assert!(module.verify().is_err());
    call.set_tail_call_kind(TailCallKind::NoTail).unwrap();
    module.verify().unwrap();
}

use zkcl_llvm_api::{
    AttributeLocation as At, AttributeValue as Attr, Builder, Context, TailCallKind,
};

#[test]
fn direct_invoke_routes_both_edges_and_resumes_after_cleanup() {
    let context = Context::default();
    let module = context.module("direct-invoke").unwrap();
    let i32 = context.int_type(32).unwrap();
    let exception = context
        .struct_type(&[context.pointer_type(), i32.clone()], false)
        .unwrap();
    let no_args = context.void_type().function(&[], false).unwrap();
    let personality_type = i32.function(&[], true).unwrap();
    let personality = module
        .declare_function("__gxx_personality_v0", &personality_type, None)
        .unwrap();
    let callee = module.add_function("may_throw", &no_args).unwrap();
    let main_type = i32.function(&[], false).unwrap();
    let main = module.add_function("main", &main_type).unwrap();
    let personality = personality.address().unwrap();
    assert!(main.personality().unwrap().is_none());
    main.set_personality(Some(&personality)).unwrap();
    assert!(main.personality().unwrap().unwrap().same_as(&personality));

    let entry = main.append_block("entry").unwrap();
    let normal = main.append_block("normal").unwrap();
    let unwind = main.append_block("unwind").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    let invoke = builder.invoke(&callee, &[], &normal, &unwind, "").unwrap();
    assert_eq!(invoke.opcode(), "invoke");
    assert!(invoke.result().is_none());
    assert!(invoke.successor(0).unwrap().same_as(&normal));
    assert!(invoke.successor(1).unwrap().same_as(&unwind));
    assert!(invoke.tail_call_kind().is_err());
    assert!(invoke.set_tail_call_kind(TailCallKind::Tail).is_err());

    builder.position_at_end(&normal).unwrap();
    builder
        .ret(Some(&i32.const_int(&42.into()).unwrap()))
        .unwrap();

    builder.position_at_end(&unwind).unwrap();
    let landing = builder
        .landing_pad(&exception, true, &[], "exception")
        .unwrap();
    builder.resume(&landing).unwrap();
    module.verify().unwrap();

    let ir = module.ir();
    assert!(ir.contains("invoke void @may_throw()"));
    assert!(ir.contains("landingpad { ptr, i32 }"));
    assert!(ir.contains("cleanup"));
    assert!(ir.contains("resume { ptr, i32 }"));
    main.set_personality(None).unwrap();
    assert!(main.personality().unwrap().is_none());
    main.set_personality(Some(&personality)).unwrap();
    module.verify().unwrap();
}

#[test]
fn indirect_invoke_preserves_prototype_calling_convention_and_attributes() {
    let context = Context::default();
    let module = context.module("indirect-invoke").unwrap();
    let prototypes = context.module("prototypes").unwrap();
    let i32 = context.int_type(32).unwrap();
    let pointer = context.pointer_type();
    let exception = context
        .struct_type(&[pointer.clone(), i32.clone()], false)
        .unwrap();
    let signature = i32.function(std::slice::from_ref(&i32), false).unwrap();
    let personality_type = i32.function(&[], true).unwrap();
    let personality = module
        .declare_function("__gxx_personality_v0", &personality_type, None)
        .unwrap();
    let prototype = prototypes.add_function("prototype", &signature).unwrap();
    prototype.set_calling_convention(8).unwrap();
    prototype
        .set_attribute(At::Return, "signext", Attr::Flag)
        .unwrap();
    prototype
        .set_attribute(At::Param(0), "noundef", Attr::Flag)
        .unwrap();
    prototype
        .set_attribute(At::Function, "nounwind", Attr::Flag)
        .unwrap();

    let wrapper = i32.function(std::slice::from_ref(&pointer), false).unwrap();
    let main = module.add_function("wrapper", &wrapper).unwrap();
    main.set_personality(Some(&personality.address().unwrap()))
        .unwrap();
    let entry = main.append_block("entry").unwrap();
    let normal = main.append_block("normal").unwrap();
    let unwind = main.append_block("unwind").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    let callee = main.parameter(0).unwrap().unwrap();
    let invoke = builder
        .invoke_indirect_with_prototype(
            &prototype,
            &callee,
            &[i32.const_int(&18.into()).unwrap()],
            &normal,
            &unwind,
            "result",
        )
        .unwrap();
    assert_eq!(invoke.call_calling_convention().unwrap(), 8);
    assert!(
        invoke
            .has_call_attribute(At::Return, "signext", false)
            .unwrap()
    );
    assert!(
        invoke
            .has_call_attribute(At::Param(0), "noundef", false)
            .unwrap()
    );
    assert!(
        !invoke
            .has_call_attribute(At::Function, "nounwind", false)
            .unwrap()
    );

    builder.position_at_end(&normal).unwrap();
    builder.ret(invoke.result().as_ref()).unwrap();
    builder.position_at_end(&unwind).unwrap();
    let type_info = module
        .add_function("type_info", &personality_type)
        .unwrap()
        .address()
        .unwrap();
    let filter = pointer.const_array(&[type_info]).unwrap();
    let catch_all = pointer.const_zero().unwrap();
    let landing = builder
        .landing_pad(&exception, false, &[filter, catch_all], "exception")
        .unwrap();
    builder.resume(&landing).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("invoke fastcc signext i32"));
    assert!(ir.find("filter [1 x ptr]").unwrap() < ir.find("catch ptr null").unwrap());
    assert!(!ir.contains("@prototype"));
}

#[test]
fn exception_builders_check_scope_and_clause_types_before_mutation() {
    let context = Context::default();
    let module = context.module("invalid-invoke").unwrap();
    let other = context.module("other").unwrap();
    let ptr = context.pointer_type();
    let i32 = context.int_type(32).unwrap();
    let exception = context
        .struct_type(&[ptr.clone(), i32.clone()], false)
        .unwrap();
    let signature = context.void_type().function(&[ptr], false).unwrap();
    let callee = module.add_function("callee", &signature).unwrap();
    let caller = module.add_function("caller", &signature).unwrap();
    let foreign = other.add_function("foreign", &signature).unwrap();
    let foreign_address = foreign.address().unwrap();
    let entry = caller.append_block("entry").unwrap();
    let normal = caller.append_block("normal").unwrap();
    let unwind = caller.append_block("unwind").unwrap();
    let foreign_block = foreign.append_block("foreign-entry").unwrap();
    let argument = caller.parameter(0).unwrap().unwrap();
    let arguments = std::slice::from_ref(&argument);
    let builder = Builder::new(&context);
    assert!(caller.set_personality(Some(&foreign_address)).is_err());
    assert!(caller.set_personality(Some(&argument)).is_err());
    builder.position_at_end(&entry).unwrap();
    assert!(
        builder
            .invoke(&callee, arguments, &normal, &normal, "")
            .is_err()
    );
    assert!(
        builder
            .invoke(&callee, arguments, &normal, &foreign_block, "")
            .is_err()
    );
    assert!(
        builder
            .invoke(&callee, arguments, &entry, &unwind, "")
            .is_err()
    );
    assert!(builder.invoke(&callee, &[], &normal, &unwind, "").is_err());
    assert_eq!(entry.instructions().count(), 0);

    caller
        .set_personality(Some(&callee.address().unwrap()))
        .unwrap();
    builder.position_at_end(&unwind).unwrap();
    assert!(builder.landing_pad(&exception, false, &[], "").is_err());
    for clause in [argument, foreign_address, i32.const_int(&0.into()).unwrap()] {
        assert!(
            builder
                .landing_pad(&exception, true, &[clause], "")
                .is_err()
        );
    }
    assert_eq!(unwind.instructions().count(), 0);
}

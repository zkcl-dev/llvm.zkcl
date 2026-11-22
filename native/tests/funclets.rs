use object::Object;
use zkcl_llvm_api::{
    Builder, Context, GlobalOptions, OptimizationLevel, Result, TargetMachine, TypeKind,
};

#[test]
fn builds_windows_funclets_and_emits_unwind_tables() -> Result<()> {
    let context = Context::default();
    let module = context.module("funclets")?;
    let target = TargetMachine::new("x86_64-pc-windows-msvc", OptimizationLevel::None)?;
    target.configure_module(&module)?;

    let i32 = context.int_type(32)?;
    let signature = context.void_type().function(&[], false)?;
    let personality = module.add_function("__CxxFrameHandler3", &i32.function(&[], true)?)?;
    let throwing = module.add_function("may_throw", &signature)?;
    let function = module.add_function("run", &signature)?;
    function.set_personality(Some(&personality.address()?))?;
    let descriptor = module.add_global(
        "type_descriptor",
        &context.int_type(8)?,
        GlobalOptions::default(),
    )?;

    let entry = function.append_block("entry")?;
    let first_cleanup = function.append_block("before.dispatch")?;
    let dispatch = function.append_block("dispatch")?;
    let handler = function.append_block("handler")?;
    let handler_body = function.append_block("handler.body")?;
    let fallback = function.append_block("fallback")?;
    let final_cleanup = function.append_block("after.dispatch")?;
    let normal = function.append_block("normal")?;

    let builder = Builder::new(&context);
    builder.position_at_end(&entry)?;
    let invoke = builder.invoke(&throwing, &[], &normal, &first_cleanup, "")?;
    assert!(
        invoke
            .exception_unwind_destination()
            .unwrap()
            .same_as(&first_cleanup)
    );

    builder.position_at_end(&first_cleanup)?;
    let cleanup = builder.cleanup_pad(None, &[], "pre")?;
    assert!(cleanup.exception_parent_pad().is_none());
    let cleanup_ret = builder.cleanup_ret(&cleanup, Some(&dispatch))?;
    assert!(
        cleanup_ret
            .exception_unwind_destination()
            .unwrap()
            .same_as(&dispatch)
    );

    builder.position_at_end(&dispatch)?;
    let switch = builder.catch_switch(None, Some(&final_cleanup), "matches")?;
    switch.add_catch_switch_handler(&handler)?;
    assert_eq!(
        switch.result().unwrap().ty().kind(),
        TypeKind::LLVMTokenTypeKind
    );
    assert!(
        switch
            .exception_unwind_destination()
            .unwrap()
            .same_as(&final_cleanup)
    );

    let null = context.pointer_type().const_zero()?;
    builder.position_at_end(&handler)?;
    let typed_args = [
        descriptor.address(),
        i32.const_int(&0.into())?,
        null.clone(),
    ];
    let pad = builder.catch_pad(&switch, &typed_args, "typed")?;
    assert!(pad.exception_parent_pad().unwrap().same_as(&switch));
    assert!(pad.operand(0).unwrap().same_as(&typed_args[0]));
    assert_eq!(
        pad.result().unwrap().ty().kind(),
        TypeKind::LLVMTokenTypeKind
    );
    builder.br(&handler_body)?;
    builder.position_at_end(&handler_body)?;
    let catch_ret = builder.catch_ret(&pad, &normal)?;
    assert_eq!(catch_ret.opcode(), "catchret");
    assert!(catch_ret.successor(0).unwrap().same_as(&normal));

    builder.position_at_end(&fallback)?;
    builder.unreachable()?;
    builder.position_before(&fallback.terminator().unwrap())?;
    builder.catch_pad(
        &switch,
        &[null.clone(), i32.const_int(&64.into())?, null],
        "all",
    )?;
    switch.add_catch_switch_handler(&fallback)?;
    assert_eq!(switch.catch_switch_handler_count(), 2);
    assert!(switch.catch_switch_handler(0).unwrap().same_as(&handler));
    assert!(switch.catch_switch_handler(1).unwrap().same_as(&fallback));
    assert!(switch.catch_switch_handler(u32::MAX).is_none());

    builder.position_at_end(&final_cleanup)?;
    let cleanup = builder.cleanup_pad(None, &[], "post")?;
    let cleanup_ret = builder.cleanup_ret(&cleanup, None)?;
    assert_eq!(cleanup_ret.opcode(), "cleanupret");
    assert!(cleanup_ret.exception_unwind_destination().is_none());
    builder.position_at_end(&normal)?;
    builder.ret(None)?;

    module.verify()?;
    let object = target.object(&module)?;
    let object = object::File::parse(object.as_slice()).unwrap();
    assert_eq!(object.format(), object::BinaryFormat::Coff);
    assert!(object.section_by_name(".pdata").is_some());
    assert!(object.section_by_name(".xdata").is_some());
    Ok(())
}

#[test]
fn rejects_invalid_funclet_operands_before_changing_ir() -> Result<()> {
    let context = Context::default();
    let module = context.module("invalid-funclets")?;
    let signature = context
        .void_type()
        .function(&[context.pointer_type()], false)?;
    let function = module.add_function("f", &signature)?;
    let other = module.add_function("g", &signature)?;
    let entry = function.append_block("entry")?;
    let dispatch = function.append_block("dispatch")?;
    let handler = function.append_block("handler")?;
    let cleanup = function.append_block("cleanup")?;
    let other_block = other.append_block("other")?;
    let builder = Builder::new(&context);

    builder.position_at_end(&dispatch)?;
    assert!(builder.catch_switch(None, None, "").is_err());
    let i32 = context.int_type(32)?;
    let personality = module.add_function("personality", &i32.function(&[], true)?)?;
    function.set_personality(Some(&personality.address()?))?;
    builder.position_at_end(&entry)?;
    assert!(builder.cleanup_pad(None, &[], "").is_err());
    builder.position_at_end(&dispatch)?;
    assert!(builder.catch_switch(None, Some(&other_block), "").is_err());
    assert!(builder.catch_switch(None, Some(&entry), "").is_err());
    let switch = builder.catch_switch(None, Some(&cleanup), "dispatch")?;

    let before = module.ir();
    assert!(switch.add_catch_switch_handler(&other_block).is_err());
    assert!(switch.add_catch_switch_handler(&cleanup).is_err());
    builder.position_at_end(&handler)?;
    assert!(builder.cleanup_pad(Some(&switch), &[], "").is_err());
    assert!(builder.catch_ret(&switch, &cleanup).is_err());
    assert!(builder.cleanup_ret(&switch, None).is_err());
    let foreign = other.parameter(0)?.unwrap();
    assert!(builder.catch_pad(&switch, &[foreign], "").is_err());
    builder.position_at_end(&cleanup)?;
    assert!(builder.catch_pad(&switch, &[], "").is_err());
    assert_eq!(module.ir(), before);

    let pad = builder.cleanup_pad(None, &[], "cleanup")?;
    assert!(pad.add_catch_switch_handler(&handler).is_err());
    assert!(builder.cleanup_pad(None, &[], "").is_err());
    assert!(builder.cleanup_ret(&pad, Some(&other_block)).is_err());
    assert_eq!(cleanup.instructions().count(), 1);
    Ok(())
}

use zkcl_llvm_api::{Builder, Context, GlobalOptions};

#[test]
fn checks_intrinsic_signatures_and_reuses_llvm_declarations() {
    let context = Context::default();
    let module = context.module("intrinsics").unwrap();
    let i32 = context.int_type(32).unwrap();
    let i1 = context.int_type(1).unwrap();
    let output = context.struct_type(&[i32.clone(), i1], false).unwrap();
    let arguments = [i32.clone(), i32.clone()];
    let signature = output.function(&arguments, false).unwrap();
    let base = "llvm.uadd.with.overflow";
    let symbol = "llvm.uadd.with.overflow.i32";
    assert!(
        module
            .declare_intrinsic(base, &i32.function(&arguments, false).unwrap())
            .is_err()
    );
    assert!(
        module
            .declare_intrinsic(base, &output.function(&arguments[..1], false).unwrap())
            .is_err()
    );
    assert!(module.declare_intrinsic(symbol, &signature).is_err());
    assert!(
        module
            .declare_intrinsic("llvm.no.such.intrinsic", &signature)
            .is_err()
    );
    assert_eq!(module.functions().count(), 0);

    let declaration = module.declare_intrinsic(base, &signature).unwrap();
    assert_eq!(declaration.name().unwrap(), symbol);
    assert_eq!(
        module
            .declare_intrinsic(base, &signature)
            .unwrap()
            .name()
            .unwrap(),
        symbol
    );
    assert_eq!(module.functions().count(), 1);
    let trap = module
        .declare_intrinsic(
            "llvm.trap",
            &context.void_type().function(&[], false).unwrap(),
        )
        .unwrap();
    assert_eq!(trap.name().unwrap(), "llvm.trap");
    module.verify().unwrap();

    let collision = context.module("collision").unwrap();
    collision
        .add_global(symbol, &i32, GlobalOptions::default())
        .unwrap();
    let error = collision.declare_intrinsic(base, &signature).err().unwrap();
    assert!(error.to_string().contains("already in use"));
    assert_eq!(collision.globals().next().unwrap().name().unwrap(), symbol);
    assert_eq!(collision.functions().count(), 0);
}

#[test]
fn infers_multiple_intrinsic_overloads_from_a_memory_copy_signature() {
    let context = Context::default();
    let module = context.module("copy").unwrap();
    let destination = context.pointer_type_in(3).unwrap();
    let source = context.pointer_type();
    let i64 = context.int_type(64).unwrap();
    let i1 = context.int_type(1).unwrap();
    let parameters = [destination, source, i64];
    let mut memcpy_parameters = parameters.to_vec();
    memcpy_parameters.push(i1.clone());
    let void = context.void_type();
    let memcpy = module
        .declare_intrinsic(
            "llvm.memcpy",
            &void.function(&memcpy_parameters, false).unwrap(),
        )
        .unwrap();
    assert_eq!(memcpy.name().unwrap(), "llvm.memcpy.p3.p0.i64");
    let copy = module
        .add_function("copy", &void.function(&parameters, false).unwrap())
        .unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&copy.append_block("entry").unwrap())
        .unwrap();
    let args = [
        copy.parameter(0).unwrap().unwrap(),
        copy.parameter(1).unwrap().unwrap(),
        copy.parameter(2).unwrap().unwrap(),
        i1.const_zero().unwrap(),
    ];
    assert!(builder.call(&memcpy, &args, "").unwrap().is_none());
    builder.ret(None).unwrap();
    module.verify().unwrap();
    assert!(module.ir().contains("i1 immarg false"));
}

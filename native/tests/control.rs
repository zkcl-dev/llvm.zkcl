use std::ffi::c_void;

use zkcl_llvm_api::{Builder, Context, IntOp, IntPredicate, Jit, Linkage};

#[test]
fn calls_through_a_module_owned_function_table() {
    let context = Context::default();
    let module = context.module("dispatch").unwrap();
    let jit = Jit::new().unwrap();
    jit.configure_module(&module).unwrap();
    let i32 = context.int_type(32).unwrap();
    let i64 = context.int_type(64).unwrap();
    let ptr = context.pointer_type();
    let signature = i32.function(std::slice::from_ref(&i32), false).unwrap();
    let builder = Builder::new(&context);
    let zero = i32.const_zero().unwrap();
    let one = i32.const_int(&1.into()).unwrap();
    let add_step = |name, op| {
        let function = module
            .add_function_with_linkage(name, &signature, Linkage::Internal)
            .unwrap();
        builder
            .position_at_end(&function.append_block("entry").unwrap())
            .unwrap();
        let argument = function.parameter(0).unwrap().unwrap();
        let result = builder.int_binop(op, &argument, &one, "result").unwrap();
        builder.ret(Some(&result)).unwrap();
        function
    };
    let increment = add_step("increment", IntOp::Add);
    let decrement = add_step("decrement", IntOp::Sub);
    let dead = decrement.append_block("dead").unwrap();
    let entries = ptr
        .const_array(&[increment.address().unwrap(), decrement.address().unwrap()])
        .unwrap();
    let table = module
        .add_global(
            "operations",
            &entries.ty(),
            zkcl_llvm_api::GlobalOptions {
                initializer: Some(&entries),
                constant: true,
                linkage: Linkage::Private,
            },
        )
        .unwrap();
    let dispatcher = module
        .add_function(
            "dispatch",
            &i32.function(&[i32.clone(), i32.clone()], false).unwrap(),
        )
        .unwrap();
    let selector = dispatcher.parameter(0).unwrap().unwrap();
    let input = dispatcher.parameter(1).unwrap().unwrap();
    let entry = dispatcher.append_block("entry").unwrap();
    let run = dispatcher.append_block("run").unwrap();
    let reject = dispatcher.append_block("reject").unwrap();
    builder.position_at_end(&entry).unwrap();

    assert!(
        builder
            .switch(&selector, &reject, &[(&selector, &run)])
            .is_err()
    );
    assert!(
        builder
            .switch(&selector, &reject, &[(&i64.const_zero().unwrap(), &run)])
            .is_err()
    );
    assert!(
        builder
            .switch(&selector, &reject, &[(&zero, &run), (&zero, &run)])
            .is_err()
    );
    assert!(
        builder
            .switch(&selector, &reject, &[(&zero, &dead)])
            .is_err()
    );
    builder
        .switch(&selector, &reject, &[(&zero, &run), (&one, &run)])
        .unwrap();

    builder.position_at_end(&run).unwrap();
    let slot = builder
        .gep(
            &entries.ty(),
            &table.address(),
            &[zero.clone(), selector],
            "slot",
        )
        .unwrap();
    let callee = builder.load(&ptr, &slot, "callee").unwrap();
    let negative = builder
        .icmp(IntPredicate::SLT, &input, &zero, "negative")
        .unwrap();
    assert!(builder.select(&input, &zero, &input, "bad").is_err());
    let argument = builder.select(&negative, &zero, &input, "clamped").unwrap();
    assert!(
        builder
            .call_indirect(&signature, &callee, &[], "bad")
            .is_err()
    );
    assert!(
        builder
            .call_indirect(&signature, &callee, std::slice::from_ref(&callee), "bad")
            .is_err()
    );
    let result = builder
        .call_indirect(&signature, &callee, &[argument], "result")
        .unwrap()
        .unwrap();
    builder.ret(Some(&result)).unwrap();

    builder.position_at_end(&reject).unwrap();
    builder
        .ret(Some(&i32.const_int(&(-99).into()).unwrap()))
        .unwrap();
    builder.position_at_end(&dead).unwrap();
    builder.unreachable().unwrap();
    assert!(module.ir().contains("call i32 %callee(i32 %clamped)"));
    assert!(!module.ir().contains("%bad"));
    let optimized = module.optimize("default<O2>", None).unwrap();
    // SAFETY: all reachable paths select validated functions of the same signature.
    unsafe { jit.add_module(&optimized) }.unwrap();
    let symbol = jit.lookup("dispatch").unwrap();
    // SAFETY: the symbol retains the session and the public function uses exactly this C ABI.
    let dispatch = unsafe {
        std::mem::transmute::<*mut c_void, extern "C" fn(i32, i32) -> i32>(
            symbol.address().as_ptr(),
        )
    };
    assert_eq!(dispatch(0, 41), 42);
    assert_eq!(dispatch(1, 43), 42);
    assert_eq!(dispatch(0, -4), 1);
    assert_eq!(dispatch(2, 1), -99);
}

#[test]
fn known_callees_preserve_their_abi_and_reject_mismatched_signatures() {
    let context = Context::default();
    let module = context
        .parse_ir("define internal fastcc signext i32 @callee(i32 signext %x) { ret i32 %x }")
        .unwrap();
    let i32 = context.int_type(32).unwrap();
    let callee = module.find_function("callee").unwrap().unwrap();
    let signature = callee.signature();
    let caller = module.add_function("caller", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&caller.append_block("entry").unwrap())
        .unwrap();
    let arg = caller.parameter(0).unwrap().unwrap();
    let callee = callee.address().unwrap();
    assert!(
        builder
            .call_indirect(&i32.function(&[], false).unwrap(), &callee, &[], "bad")
            .is_err()
    );
    let result = builder
        .call_indirect(&signature, &callee, &[arg], "result")
        .unwrap()
        .unwrap();
    builder.ret(Some(&result)).unwrap();
    module.verify().unwrap();
    assert!(
        module
            .ir()
            .contains("call fastcc signext i32 @callee(i32 signext")
    );
}

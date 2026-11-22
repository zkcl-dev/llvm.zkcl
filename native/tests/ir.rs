use std::ffi::c_void;

use zkcl_llvm_api::{
    Builder, Context, IntOp, IntPredicate, Jit, Module, OptimizationLevel, TargetMachine,
};

fn constant_module(value: u64) -> (Module, Context) {
    let context = Context::default();
    let module = context.module("answer").unwrap();
    let i64 = context.int_type(64).unwrap();
    let signature = i64.function(&[], false).unwrap();
    let function = module.add_function("answer", &signature).unwrap();
    let entry = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    builder
        .ret(Some(&i64.const_int(&value.into()).unwrap()))
        .unwrap();
    (module, context)
}

#[test]
fn emits_verifiable_ir_and_round_trips_bitcode() {
    let (module, context) = constant_module(42);
    module.verify().unwrap();
    assert!(module.ir().contains("ret i64 42"));

    let bitcode = module.bitcode().unwrap();
    let decoded = context.parse_bitcode(&bitcode).unwrap();
    assert!(decoded.ir().contains("ret i64 42"));
}

#[test]
fn optimization_uses_a_copy_of_the_original_module() {
    let context = Context::default();
    let module = context
        .parse_ir("define i64 @identity(i64 %x) { %sum = add i64 %x, 0\n ret i64 %sum }")
        .unwrap();
    let optimized = module.optimize("default<O2>", None).unwrap();
    optimized.verify().unwrap();
    assert!(optimized.ir().contains("ret i64 %x"));
    assert!(module.ir().contains("add i64 %x, 0"));
}

#[test]
fn optimization_can_rename_types_without_mutating_the_original_context() {
    let context = Context::default();
    let module = context
        .parse_ir("%Point = type { i32 }\n@point = global %Point zeroinitializer")
        .unwrap();
    let optimized = module.optimize("strip", None).unwrap();
    assert!(!optimized.ir().contains("%Point"));
    assert!(module.ir().contains("%Point = type"));
}

#[test]
fn target_machine_emits_a_native_object_file() {
    let (module, _) = constant_module(42);
    let target =
        TargetMachine::new(&TargetMachine::host_triple(), OptimizationLevel::Default).unwrap();
    let object = target.object(&module).unwrap();

    assert!(!object.is_empty());
    assert!(target.assembly(&module).unwrap().contains("answer"));
}

#[test]
fn lljit_executes_a_verified_native_module() {
    let (module, _) = constant_module(42);
    let jit = Jit::new().unwrap();
    // SAFETY: the module defines `answer` as `i64 ()`, matching the function pointer below.
    unsafe { jit.add_module(&module) }.unwrap();
    let symbol = jit.lookup("answer").unwrap();
    // SAFETY: lookup resolves the verified zero-argument function with the exact C ABI signature.
    let answer = unsafe {
        std::mem::transmute::<*mut c_void, extern "C" fn() -> u64>(symbol.address().as_ptr())
    };

    jit.close();
    assert!(jit.lookup("answer").is_err());
    assert_eq!(answer(), 42);
}

#[test]
fn builder_rejects_operands_owned_by_a_different_function() {
    let context = Context::default();
    let module = context.module("functions").unwrap();
    let i64 = context.int_type(64).unwrap();
    let signature = i64.function(std::slice::from_ref(&i64), false).unwrap();
    let first = module.add_function("first", &signature).unwrap();
    let second = module.add_function("second", &signature).unwrap();
    let first_block = first.append_block("entry").unwrap();
    let second_block = second.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&first_block).unwrap();
    let lhs = first.parameter(0).unwrap().unwrap();
    let rhs = second.parameter(0).unwrap().unwrap();

    assert!(builder.int_binop(IntOp::Add, &lhs, &rhs, "sum").is_err());
    builder.position_at_end(&second_block).unwrap();
    builder
        .ret(Some(&i64.const_int(&0.into()).unwrap()))
        .unwrap();
}

#[test]
fn builds_control_flow_with_memory_and_a_direct_call() {
    let context = Context::default();
    let module = context.module("dispatch").unwrap();
    let i32 = context.int_type(32).unwrap();
    let pair = context
        .struct_type(&[i32.clone(), i32.clone()], false)
        .unwrap();
    let signature = i32.function(std::slice::from_ref(&i32), false).unwrap();
    let twice = module.add_function("twice", &signature).unwrap();
    let main = module.add_function("compute", &signature).unwrap();
    let builder = Builder::new(&context);
    let zero = i32.const_int(&0.into()).unwrap();
    let one = i32.const_int(&1.into()).unwrap();

    builder
        .position_at_end(&twice.append_block("entry").unwrap())
        .unwrap();
    let parameter = twice.parameter(0).unwrap().unwrap();
    let double = builder
        .int_binop(IntOp::Add, &parameter, &parameter, "double")
        .unwrap();
    builder.ret(Some(&double)).unwrap();

    let entry = main.append_block("entry").unwrap();
    let positive = main.append_block("positive").unwrap();
    let merge = main.append_block("merge").unwrap();
    builder.position_at_end(&entry).unwrap();
    let parameter = main.parameter(0).unwrap().unwrap();
    let condition = builder
        .icmp(IntPredicate::SGT, &parameter, &zero, "condition")
        .unwrap();
    builder.cond_br(&condition, &positive, &merge).unwrap();

    builder.position_at_end(&positive).unwrap();
    let memory = builder.alloca(&pair, "pair").unwrap();
    let field = builder
        .gep(&pair, &memory, &[zero.clone(), one], "field")
        .unwrap();
    builder.store(&parameter, &field).unwrap();
    let loaded = builder.load(&i32, &field, "loaded").unwrap();
    let result = builder.call(&twice, &[loaded], "result").unwrap().unwrap();
    builder.br(&merge).unwrap();

    builder.position_at_end(&merge).unwrap();
    let phi = builder.phi(&i32, "selected").unwrap();
    phi.add_incoming(&zero, &entry).unwrap();
    phi.add_incoming(&result, &positive).unwrap();
    builder.ret(Some(&phi)).unwrap();
    module.verify().unwrap();
    assert!(module.ir().contains("phi i32"));
}

#[test]
fn malformed_ir_bitcode_and_pipelines_return_errors() {
    let (module, context) = constant_module(42);
    assert!(context.parse_ir("define i64 @broken(").is_err());
    assert!(context.parse_bitcode(b"invalid bitcode").is_err());
    assert!(module.optimize("no-such-pass", None).is_err());
    module.verify().unwrap();
}

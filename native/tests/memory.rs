use zkcl_llvm_api::{
    AtomicOptions, AtomicOrdering, AtomicRmwOp, AtomicRmwOptions, Builder, CmpXchgOptions, Context,
    IntFlags, IntOp, MemoryOptions, SyncScope,
};

#[test]
fn memory_options_preserve_alignment_volatility_and_atomic_order() {
    let context = Context::default();
    let module = context.module("memory_options").unwrap();
    let i32 = context.int_type(32).unwrap();
    let signature = i32.function(std::slice::from_ref(&i32), false).unwrap();
    let function = module.add_function("round_trip", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let input = function.parameter(0).unwrap().unwrap();
    let slot = builder
        .alloca_with_alignment(&i32, Some(32), "slot")
        .unwrap();

    let volatile = MemoryOptions {
        alignment: Some(16),
        volatile: true,
        ..MemoryOptions::default()
    };
    builder.store_with_options(&input, &slot, volatile).unwrap();
    let first = builder
        .load_with_options(&i32, &slot, volatile, "first")
        .unwrap();
    let release = AtomicOptions {
        ordering: AtomicOrdering::Release,
        scope: SyncScope::SingleThread,
    };
    builder
        .store_with_options(
            &first,
            &slot,
            MemoryOptions {
                atomic: Some(release),
                ..volatile
            },
        )
        .unwrap();
    builder.fence(release).unwrap();
    let acquire = AtomicOptions {
        ordering: AtomicOrdering::Acquire,
        ..release
    };
    let loaded = builder
        .load_with_options(
            &i32,
            &slot,
            MemoryOptions {
                atomic: Some(acquire),
                ..MemoryOptions::default()
            },
            "loaded",
        )
        .unwrap();
    builder.ret(Some(&loaded)).unwrap();
    module.verify().unwrap();

    let module = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let ir = module.ir();
    assert!(ir.contains("%slot = alloca i32, align 32"), "{ir}");
    assert!(
        ir.contains("store volatile i32 %0, ptr %slot, align 16"),
        "{ir}"
    );
    assert!(
        ir.contains("load volatile i32, ptr %slot, align 16"),
        "{ir}"
    );
    assert!(ir.contains("store atomic volatile i32 %first, ptr %slot syncscope(\"singlethread\") release, align 16"), "{ir}");
    assert!(
        ir.contains("fence syncscope(\"singlethread\") release"),
        "{ir}"
    );
    assert!(
        ir.contains("load atomic i32, ptr %slot syncscope(\"singlethread\") acquire, align 4"),
        "{ir}"
    );
}

#[test]
fn atomic_updates_support_pointer_and_float_values_and_cmpxchg_results() {
    let context = Context::default();
    let module = context.module("atomic_updates").unwrap();
    let ptr = context.pointer_type();
    let f32 = context.float_type();
    let fp_vector = f32.vector(2, false).unwrap();
    let flag = context.int_type(1).unwrap();
    let signature = flag
        .function(&[ptr.clone(), ptr, f32, fp_vector], false)
        .unwrap();
    let function = module.add_function("update", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let address = function.parameter(0).unwrap().unwrap();
    let replacement = function.parameter(1).unwrap().unwrap();
    let number = function.parameter(2).unwrap().unwrap();
    let vector = function.parameter(3).unwrap().unwrap();
    let rmw = AtomicRmwOptions {
        ordering: AtomicOrdering::Monotonic,
        scope: SyncScope::System,
        alignment: Some(32),
        volatile: true,
    };
    let old = builder
        .atomic_rmw(AtomicRmwOp::Xchg, &address, &replacement, rmw, "old")
        .unwrap();
    builder
        .atomic_rmw(AtomicRmwOp::FAdd, &address, &number, rmw, "scalar")
        .unwrap();
    builder
        .atomic_rmw(AtomicRmwOp::FAdd, &address, &vector, rmw, "vector")
        .unwrap();
    let result = builder
        .cmpxchg(
            &address,
            &replacement,
            &old,
            CmpXchgOptions {
                success: AtomicOrdering::Release,
                failure: AtomicOrdering::Acquire,
                scope: SyncScope::SingleThread,
                alignment: Some(16),
                volatile: true,
                weak: true,
            },
            "exchanged",
        )
        .unwrap();
    let success = builder.extract_value(&result, 1, "success").unwrap();
    assert_eq!(success.ty().ir(), "i1");
    builder.ret(Some(&success)).unwrap();
    module.verify().unwrap();

    let ir = module.ir();
    assert!(
        ir.contains("%old = atomicrmw volatile xchg ptr %0, ptr %1 monotonic, align 32"),
        "{ir}"
    );
    assert!(
        ir.contains("%scalar = atomicrmw volatile fadd ptr %0, float %2 monotonic, align 32"),
        "{ir}"
    );
    assert!(
        ir.contains("%vector = atomicrmw volatile fadd ptr %0, <2 x float> %3 monotonic, align 32"),
        "{ir}"
    );
    assert!(ir.contains("%exchanged = cmpxchg weak volatile ptr %0, ptr %1, ptr %old syncscope(\"singlethread\") release acquire, align 16"), "{ir}");
    assert!(
        ir.contains("extractvalue { ptr, i1 } %exchanged, 1"),
        "{ir}"
    );
}

#[test]
fn invalid_memory_options_leave_the_block_unchanged() {
    let context = Context::default();
    let module = context.module("bad_memory_options").unwrap();
    let i24 = context.int_type(24).unwrap();
    let i32 = context.int_type(32).unwrap();
    let signature = i32
        .function(&[context.pointer_type(), i24.clone(), i32.clone()], false)
        .unwrap();
    let function = module.add_function("invalid", &signature).unwrap();
    let block = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&block).unwrap();
    let ptr = function.parameter(0).unwrap().unwrap();
    let odd_size = function.parameter(1).unwrap().unwrap();
    let int = function.parameter(2).unwrap().unwrap();
    let access = |ordering| MemoryOptions {
        atomic: Some(AtomicOptions {
            ordering,
            scope: SyncScope::System,
        }),
        ..MemoryOptions::default()
    };
    assert!(
        builder
            .alloca_with_alignment(&i32, Some(3), "invalid")
            .is_err()
    );
    assert!(
        builder
            .load_with_options(&i32, &ptr, access(AtomicOrdering::Release), "invalid")
            .is_err()
    );
    assert!(
        builder
            .load_with_options(&i24, &ptr, access(AtomicOrdering::Monotonic), "invalid")
            .is_err()
    );
    assert!(
        builder
            .store_with_options(&int, &ptr, access(AtomicOrdering::Acquire))
            .is_err()
    );
    assert!(
        builder
            .store_with_options(
                &int,
                &ptr,
                MemoryOptions {
                    alignment: Some(0),
                    ..MemoryOptions::default()
                }
            )
            .is_err()
    );
    assert!(
        builder
            .fence(access(AtomicOrdering::Monotonic).atomic.unwrap())
            .is_err()
    );

    let rmw = AtomicRmwOptions {
        ordering: AtomicOrdering::Monotonic,
        alignment: None,
        volatile: false,
        scope: SyncScope::System,
    };
    assert!(
        builder
            .atomic_rmw(AtomicRmwOp::Add, &ptr, &odd_size, rmw, "invalid")
            .is_err()
    );
    assert!(
        builder
            .atomic_rmw(AtomicRmwOp::FAdd, &ptr, &int, rmw, "invalid")
            .is_err()
    );
    assert!(
        builder
            .atomic_rmw(
                AtomicRmwOp::Add,
                &ptr,
                &int,
                AtomicRmwOptions {
                    ordering: AtomicOrdering::Unordered,
                    ..rmw
                },
                "invalid"
            )
            .is_err()
    );

    let cmp = CmpXchgOptions {
        success: AtomicOrdering::AcquireRelease,
        failure: AtomicOrdering::Monotonic,
        scope: SyncScope::System,
        alignment: None,
        volatile: false,
        weak: false,
    };
    assert!(
        builder
            .cmpxchg(&ptr, &odd_size, &int, cmp, "invalid")
            .is_err()
    );
    assert!(
        builder
            .cmpxchg(
                &ptr,
                &int,
                &int,
                CmpXchgOptions {
                    failure: AtomicOrdering::Release,
                    ..cmp
                },
                "invalid"
            )
            .is_err()
    );
    assert_eq!(block.instructions().count(), 0);
    builder.ret(Some(&int)).unwrap();
    module.verify().unwrap();
}

#[test]
fn integer_flags_survive_constant_operands_and_apply_to_vectors() {
    let context = Context::default();
    let module = context.module("integer_flags").unwrap();
    let i32 = context.int_type(32).unwrap();
    let vector = i32.vector(2, true).unwrap();
    let signature = vector
        .function(std::slice::from_ref(&vector), false)
        .unwrap();
    let function = module.add_function("flagged", &signature).unwrap();
    let builder = Builder::new(&context);
    let block = function.append_block("entry").unwrap();
    builder.position_at_end(&block).unwrap();
    let input = function.parameter(0).unwrap().unwrap();
    let exact = IntFlags {
        exact: true,
        ..IntFlags::default()
    };
    let no_wrap = IntFlags {
        nsw: true,
        nuw: true,
        exact: false,
    };
    assert!(
        builder
            .int_binop_with_flags(IntOp::Add, &input, &input, exact, "invalid")
            .is_err()
    );
    assert!(
        builder
            .int_binop_with_flags(IntOp::And, &input, &input, no_wrap, "invalid")
            .is_err()
    );
    assert_eq!(block.instructions().count(), 0);

    let three = i32.const_int(&3.into()).unwrap();
    let two = i32.const_int(&2.into()).unwrap();
    let exact_divide = builder
        .int_binop_with_flags(IntOp::UDiv, &three, &two, exact, "non_exact")
        .unwrap();
    assert!(!exact_divide.is_constant());
    let constant_shift = builder
        .int_binop_with_flags(IntOp::Shl, &three, &two, no_wrap, "shifted")
        .unwrap();
    let result = builder
        .int_binop_with_flags(IntOp::Add, &input, &input, no_wrap, "result")
        .unwrap();
    builder.ret(Some(&result)).unwrap();
    module.verify().unwrap();
    assert_eq!(block.instructions().count(), 4);
    let ir = module.ir();
    assert!(ir.contains("%non_exact = udiv exact i32 3, 2"), "{ir}");
    assert!(ir.contains("%shifted = shl nuw nsw i32 3, 2"), "{ir}");
    assert!(
        ir.contains("%result = add nuw nsw <vscale x 2 x i32> %0, %0"),
        "{ir}"
    );
    assert!(!constant_shift.is_constant());
}

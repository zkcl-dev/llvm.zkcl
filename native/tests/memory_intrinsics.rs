use zkcl_llvm_api::{Builder, Context, GlobalOptions, Linkage, UnnamedAddress};

#[test]
fn emits_typed_alloca_and_overloaded_memory_intrinsics() {
    let context = Context::default();
    let module = context
        .parse_ir(r#"target datalayout = "e-p:32:32-A5""#)
        .unwrap();
    let i32 = context.int_type(32).unwrap();
    let i8 = context.int_type(8).unwrap();
    let source_type = context.pointer_type_in(3).unwrap();
    let signature = context
        .void_type()
        .function(&[source_type, i32.clone()], false)
        .unwrap();
    let function = module.add_function("copy", &signature).unwrap();
    let source = function.parameter(0).unwrap().unwrap();
    let count = function.parameter(1).unwrap().unwrap();
    let entry = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    let dest = builder
        .array_alloca_with_alignment(&i32, &count, Some(16), "dest")
        .unwrap();
    assert_eq!(dest.ty().pointer_address_space().unwrap(), 5);
    builder
        .memcpy(&dest, &source, &count, Some(16), Some(1 << 32), false)
        .unwrap();
    builder
        .memmove(&dest, &source, &count, None, None, true)
        .unwrap();
    builder
        .memset(&dest, &i8.const_zero().unwrap(), &count, Some(8), false)
        .unwrap();
    builder.ret(None).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("%dest = alloca i32, i32 %1, align 16, addrspace(5)"));
    assert!(ir.contains("call void @llvm.memcpy.p5.p3.i32(ptr addrspace(5) align 16 %dest, ptr addrspace(3) align 4294967296 %0, i32 %1, i1 false)"));
    assert!(ir.contains("call void @llvm.memmove.p5.p3.i32(ptr addrspace(5) %dest, ptr addrspace(3) %0, i32 %1, i1 true)"));
    assert!(ir.contains(
        "call void @llvm.memset.p5.i32(ptr addrspace(5) align 8 %dest, i8 0, i32 %1, i1 false)"
    ));
}

#[test]
fn checks_inputs_and_intrinsic_symbol_collisions_before_mutating_ir() {
    let context = Context::default();
    let module = context.module("bad memory operations").unwrap();
    let other = context.module("other").unwrap();
    let i32 = context.int_type(32).unwrap();
    let len = i32.const_zero().unwrap();
    let i8 = context.int_type(8).unwrap();
    let source = other
        .add_global_string("other", b"other", true)
        .unwrap()
        .address();
    let text = module.add_global_string("text", b"Z\0\xFF", true).unwrap();
    assert_eq!(text.linkage(), Linkage::Private);
    assert_eq!(text.unnamed_address(), UnnamedAddress::Global);
    assert_eq!(text.alignment(), 1);
    assert_eq!(text.storage_type().unwrap().ir(), "[4 x i8]");
    assert!(module.ir().contains(r#"c"Z\00\FF\00""#));
    let dest = text.address();
    module
        .add_global("llvm.memcpy.p0.p0.i32", &i8, GlobalOptions::default())
        .unwrap();
    let signature = context.void_type().function(&[], false).unwrap();
    let function = module.add_function("copy", &signature).unwrap();
    let entry = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    let ir = module.ir();
    assert!(builder.array_alloca(&i8, &dest, "bad count").is_err());
    assert!(
        builder
            .array_alloca_with_alignment(&i8, &len, Some(3), "bad alignment")
            .is_err()
    );
    assert!(
        builder
            .memcpy(&dest, &source, &len, None, None, false)
            .is_err()
    );
    assert!(
        builder
            .memcpy(&dest, &dest, &len, None, None, false)
            .is_err()
    );
    assert!(
        builder
            .memmove(&dest, &dest, &len, None, Some(3), false)
            .is_err()
    );
    assert!(builder.memset(&dest, &len, &len, None, false).is_err());
    assert_eq!(module.ir(), ir);
    builder.ret(None).unwrap();
    module.verify().unwrap();
}

#[test]
fn typed_allocator_uses_explicit_abi_and_pointer_difference_uses_index_width() {
    let context = Context::default();
    let module = context
        .parse_ir(r#"target datalayout = "e-p:64:64:64:32-p1:64:64-ni:1""#)
        .unwrap();
    let i32 = context.int_type(32).unwrap();
    let i64 = context.int_type(64).unwrap();
    let ptr = context.pointer_type();
    let ptr1 = context.pointer_type_in(1).unwrap();
    let void = context.void_type();
    let allocator = module
        .add_function(
            "allocate",
            &ptr.function(std::slice::from_ref(&i64), false).unwrap(),
        )
        .unwrap();
    let deallocator = module
        .add_function(
            "release",
            &void.function(std::slice::from_ref(&ptr), false).unwrap(),
        )
        .unwrap();
    for function in [&allocator, &deallocator] {
        function.set_calling_convention(8).unwrap();
    }
    let signature = i32.function(&[i64, ptr, ptr1.clone()], false).unwrap();
    let function = module.add_function("distance", &signature).unwrap();
    let count = function.parameter(0).unwrap().unwrap();
    let first = function.parameter(1).unwrap().unwrap();
    let non_integral = function.parameter(2).unwrap().unwrap();
    let entry = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    let ir = module.ir();
    let i32_zero = i32.const_zero().unwrap();
    let empty = i32.array(0).unwrap();
    assert!(
        builder
            .malloc(&allocator, &i32, &i32_zero, "bad width")
            .is_err()
    );
    assert!(
        builder
            .malloc(&deallocator, &i32, &count, "bad allocator")
            .is_err()
    );
    assert!(builder.free(&allocator, &first).is_err());
    assert!(builder.free(&deallocator, &non_integral).is_err());
    assert!(
        builder
            .ptr_diff(&i32, &non_integral, &non_integral, "nonintegral")
            .is_err()
    );
    assert!(
        builder
            .ptr_diff(&i32, &first, &non_integral, "different space")
            .is_err()
    );
    assert!(
        builder
            .ptr_diff(&empty, &first, &first, "zero stride")
            .is_err()
    );
    assert_eq!(module.ir(), ir);

    let allocation = builder
        .malloc(&allocator, &ptr1, &count, "allocated")
        .unwrap();
    assert!(
        builder
            .free(&deallocator, &allocation)
            .unwrap()
            .result()
            .is_none()
    );
    let next = builder
        .gep(&i32, &first, &[i32.const_int(&2.into()).unwrap()], "next")
        .unwrap();
    let distance = builder.ptr_diff(&i32, &next, &first, "elements").unwrap();
    assert_eq!(distance.ty().ir(), "i32");
    builder.ret(Some(&distance)).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("mul nuw i64 %0, 8"));
    assert!(ir.contains("%allocated = call fastcc ptr @allocate(i64"));
    assert!(ir.contains("call fastcc void @release(ptr %allocated)"));
    assert!(ir.contains("ptrtoint ptr %next to i32"));
    assert!(ir.contains("%elements = sdiv exact i32"));
}

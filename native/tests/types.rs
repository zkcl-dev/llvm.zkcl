use num_bigint::BigInt;
use zkcl_llvm_api::{
    Builder, CastOp, Context, FastMathFlags, FloatOp, FloatPredicate, GlobalOptions, IntOp,
    IntPredicate, TypeKind,
};

#[test]
fn constant_operands_keep_floating_point_promises_in_the_ir() {
    let context = Context::default();
    let module = context.module("fast_math").unwrap();
    let float = context.double_type();
    let signature = float.function(&[], false).unwrap();
    let function = module.add_function("arithmetic", &signature).unwrap();
    let entry = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    let nan = float.const_float(f64::NAN).unwrap();
    let inf = float.const_float(f64::INFINITY).unwrap();
    let nnan = FastMathFlags::NoNaNs;
    let ninf = FastMathFlags::NoInfs;
    let sum = builder
        .float_binop_with_flags(FloatOp::Add, &nan, &inf, nnan, "sum")
        .unwrap();
    builder.fneg_with_flags(&inf, ninf, "neg").unwrap();
    builder
        .fcmp_with_flags(
            FloatPredicate::PredicateFalse,
            &nan,
            &inf,
            nnan | ninf,
            "cmp",
        )
        .unwrap();
    builder.ret(Some(&sum)).unwrap();

    let instructions: Vec<_> = entry.instructions().collect();
    assert_eq!(instructions.len(), 4);
    assert_eq!(instructions[0].fast_math_flags(), Some(nnan));
    assert_eq!(instructions[1].fast_math_flags(), Some(ninf));
    assert_eq!(instructions[2].fast_math_flags(), Some(nnan | ninf));
    assert_eq!(instructions[3].fast_math_flags(), None);
    instructions[0].add_fast_math_flags(ninf).unwrap();
    assert_eq!(instructions[0].fast_math_flags(), Some(nnan | ninf));
    module.verify().unwrap();
}

#[test]
fn identified_structs_reject_value_cycles_before_setting_the_body() {
    let context = Context::default();
    let outer = context.opaque_struct_type("Outer").unwrap();
    let inner = context.opaque_struct_type("Inner").unwrap();
    outer
        .set_struct_body(std::slice::from_ref(&inner), false)
        .unwrap();

    let cycle = outer.array(1).unwrap();
    let failure = inner.set_struct_body(&[cycle], false).unwrap_err();
    assert!(failure.to_string().contains("recursive"));
    assert!(inner.is_opaque_struct().unwrap());
    assert!(inner.subtype(0).is_none());
    assert!(
        inner
            .set_struct_body(&[Context::default().pointer_type()], false)
            .is_err()
    );
    assert!(inner.is_opaque_struct().unwrap());

    inner
        .set_struct_body(
            &[
                context.pointer_type_in(3).unwrap(),
                context.int_type(16).unwrap(),
            ],
            true,
        )
        .unwrap();
    assert!(inner.set_struct_body(&[], false).is_err());
    let module = context.module("layouts").unwrap();
    module
        .add_global("data", &outer, GlobalOptions::default())
        .unwrap();
    let decoded = Context::default();
    let module = decoded.parse_bitcode(&module.bitcode().unwrap()).unwrap();
    assert_eq!(module.globals().next().unwrap().name().unwrap(), "data");
    let inner = decoded.find_struct_type("Inner").unwrap().unwrap();
    assert!(inner.is_packed_struct().unwrap());
    assert_eq!(inner.struct_name().unwrap().as_deref(), Some("Inner"));
    assert_eq!(
        inner.subtype(0).unwrap().pointer_address_space().unwrap(),
        3
    );
    assert_eq!(inner.subtype(1).unwrap().integer_width().unwrap(), 16);
    assert!(inner.subtype(2).is_none());
}

#[test]
fn inspection_handles_full_array_lengths_and_intrinsic_signatures() {
    let module = Context::default()
        .parse_ir(
            "@large = external global [4294967297 x ptr addrspace(3)]
             declare token @llvm.coro.id(i32, ptr, ptr, ptr)
             declare void @variadic(i16, ...)
             define void @walk() {
             entry: br label %exit
             exit: ret void
             }",
        )
        .unwrap();
    let array = module.globals().next().unwrap().storage_type().unwrap();
    assert_eq!(array.array_length().unwrap(), 4_294_967_297);
    assert_eq!(
        module
            .context()
            .pointer_type_in(3)
            .unwrap()
            .array_with_length(4_294_967_297)
            .unwrap()
            .ir(),
        array.ir()
    );
    assert_eq!(
        array.subtype(0).unwrap().pointer_address_space().unwrap(),
        3
    );

    let mut functions = module.functions();
    let intrinsic = functions.next().unwrap().signature().subtype(0).unwrap();
    assert_eq!(intrinsic.kind(), TypeKind::LLVMTokenTypeKind);
    assert_eq!(intrinsic.ir(), "token");
    assert_eq!(intrinsic.ir(), module.context().token_type().ir());
    assert!(intrinsic.const_zero().is_err());
    assert!(!intrinsic.is_sized());
    for (ty, kind) in [
        (module.context().label_type(), TypeKind::LLVMLabelTypeKind),
        (
            module.context().metadata_type(),
            TypeKind::LLVMMetadataTypeKind,
        ),
    ] {
        assert_eq!(ty.kind(), kind);
        assert_eq!(ty.subtype_count(), 0);
        assert!(!ty.is_sized());
        assert!(!ty.is_floating_point());
        assert!(ty.const_zero().is_err());
    }
    let variadic = functions.next().unwrap();
    assert_eq!(variadic.signature().subtype_count(), 2);
    assert!(variadic.signature().is_variadic_function().unwrap());
    assert!(variadic.blocks().next().is_none());
    let blocks = functions
        .next()
        .unwrap()
        .blocks()
        .map(|block| block.name().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(blocks, ["entry", "exit"]);
}

#[test]
fn vector_types_preserve_element_type_and_scalability() {
    let context = Context::default();
    let half = context.floating_type(TypeKind::LLVMHalfTypeKind).unwrap();
    let fixed = half.vector(4, false).unwrap();
    let scalable = half.vector(2, true).unwrap();
    assert_eq!(fixed.ir(), "<4 x half>");
    assert_eq!(scalable.ir(), "<vscale x 2 x half>");
    assert_eq!(fixed.vector_length().unwrap(), 4);
    assert_eq!(scalable.vector_length().unwrap(), 2);
    assert_eq!(fixed.subtype(0).unwrap().kind(), TypeKind::LLVMHalfTypeKind);
    assert_eq!(fixed.subtype_count(), 1);
    assert_eq!(scalable.scalar_type().kind(), TypeKind::LLVMHalfTypeKind);
    assert!(scalable.is_sized());
    assert!(!scalable.is_floating_point());
    assert!(scalable.floating_point_width().is_err());
    assert!(half.vector(0, false).is_err());
    assert!(fixed.vector(4, false).is_err());
}

#[test]
fn decimal_float_constants_keep_the_destination_precision_in_bitcode() {
    let context = Context::default();
    let module = context.module("floating_point").unwrap();
    let precise = "1.000000000000000000867361737988403547205962240695953369140625";
    let cases = [
        (TypeKind::LLVMHalfTypeKind, "half", 16),
        (TypeKind::LLVMBFloatTypeKind, "bfloat", 16),
        (TypeKind::LLVMFloatTypeKind, "float", 32),
        (TypeKind::LLVMDoubleTypeKind, "double", 64),
        (TypeKind::LLVMX86_FP80TypeKind, "x86_fp80", 80),
        (TypeKind::LLVMFP128TypeKind, "fp128", 128),
        (TypeKind::LLVMPPC_FP128TypeKind, "ppc_fp128", 128),
    ];
    let mut expected = Vec::new();
    for (kind, name, width) in cases {
        let ty = context.floating_type(kind).unwrap();
        assert!(ty.is_floating_point());
        assert_eq!(ty.floating_point_width().unwrap(), width);
        let rounded = ty.const_float(1.0).unwrap();
        let value = ty.const_float_from_string(precise).unwrap();
        assert_eq!(value.ty().kind(), kind);
        assert_eq!(value.ir() != rounded.ir(), width >= 80);
        assert_eq!(
            ty.const_float_from_string("+.125E+1").unwrap().ir(),
            ty.const_float(1.25).unwrap().ir()
        );
        for malformed in ["", "-", "1.2foo", "1e-", "0x20", "1\0", "1.1.1"] {
            assert!(ty.const_float_from_string(malformed).is_err());
        }
        module
            .add_global(
                name,
                &ty,
                GlobalOptions {
                    initializer: Some(&value),
                    ..GlobalOptions::default()
                },
            )
            .unwrap();
        expected.push(value.ir());
    }
    let decoded = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let actual = decoded
        .globals()
        .map(|global| global.initializer().unwrap().unwrap().ir())
        .collect::<Vec<_>>();
    assert_eq!(actual, expected);
}

#[test]
fn integer_constants_read_back_exact_bits_with_either_signedness() {
    let context = Context::default();
    let i1 = context.int_type(1).unwrap();
    let boolean = i1.const_int(&1.into()).unwrap();
    assert_eq!(boolean.constant_integer(false).unwrap(), 1.into());
    assert_eq!(boolean.constant_integer(true).unwrap(), (-1).into());

    let ty = context.int_type(137).unwrap();
    let limit = BigInt::from(1) << 137usize;
    let large = BigInt::from(1) << 120usize;
    let value = ty.const_int(&large).unwrap();
    assert_eq!(value.constant_integer(false).unwrap(), large);
    assert_eq!(value.constant_integer(true).unwrap(), large);
    let value = ty.const_int(&(-&large)).unwrap();
    assert_eq!(value.constant_integer(false).unwrap(), limit - &large);
    assert_eq!(value.constant_integer(true).unwrap(), -large);
    assert!(ty.const_poison().unwrap().constant_integer(false).is_err());
    assert!(
        context
            .double_type()
            .const_float(1.0)
            .unwrap()
            .constant_integer(false)
            .is_err()
    );
}

#[test]
fn fixed_vectors_support_typed_lane_access_and_constant_shuffle_masks() {
    let context = Context::default();
    let module = context.module("fixed_vectors").unwrap();
    let i32 = context.int_type(32).unwrap();
    let vector = i32.vector(4, false).unwrap();
    let pair = i32.vector(2, false).unwrap();
    let signature = pair
        .function(&[vector.clone(), vector, i32.clone()], false)
        .unwrap();
    let function = module.add_function("shuffle", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let left = function.parameter(0).unwrap().unwrap();
    let right = function.parameter(1).unwrap().unwrap();
    let index = function.parameter(2).unwrap().unwrap();
    let lane = builder.extract_element(&left, &index, "lane").unwrap();
    assert_eq!(lane.ty().integer_width().unwrap(), 32);
    assert!(builder.extract_element(&left, &right, "rejected").is_err());
    assert!(
        builder
            .insert_element(&left, &right, &index, "rejected")
            .is_err()
    );
    let updated = builder
        .insert_element(&right, &lane, &index, "updated")
        .unwrap();
    assert!(
        builder
            .shuffle_vector(&left, &updated, &left, "rejected")
            .is_err()
    );
    let mask = |first, second| {
        pair.const_vector(&[
            i32.const_int(&num_bigint::BigInt::from(first)).unwrap(),
            i32.const_int(&num_bigint::BigInt::from(second)).unwrap(),
        ])
        .unwrap()
    };
    assert!(
        builder
            .shuffle_vector(&left, &updated, &mask(8, 5), "rejected")
            .is_err()
    );
    assert!(
        builder
            .shuffle_vector(&left, &updated, &mask(-1, 0), "rejected")
            .is_err()
    );
    let one = i32.const_int(&1.into()).unwrap();
    builder
        .shuffle_vector(&left, &updated, &pair.const_splat(&one).unwrap(), "repeat")
        .unwrap();
    let poison_mask = pair
        .const_vector(&[i32.const_poison().unwrap(), one])
        .unwrap();
    builder
        .shuffle_vector(&left, &updated, &poison_mask, "poison.lane")
        .unwrap();
    let shuffled = builder
        .shuffle_vector(&left, &updated, &mask(0, 5), "shuffled")
        .unwrap();
    builder.ret(Some(&shuffled)).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("shufflevector <4 x i32>"));
    assert!(!ir.contains("rejected"));
}

#[test]
fn scalable_vector_arithmetic_preserves_lane_types_and_shape() {
    let context = Context::default();
    let module = context.module("scalable_vectors").unwrap();
    let i32 = context.int_type(32).unwrap();
    let ints = i32.vector(2, true).unwrap();
    let fixed = i32.vector(2, false).unwrap();
    let floats = context.float_type().vector(2, true).unwrap();
    let wide = context.int_type(64).unwrap().vector(2, true).unwrap();
    let signature = wide
        .function(&[ints.clone(), ints.clone(), fixed.clone()], false)
        .unwrap();
    let function = module.add_function("choose", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let left = function.parameter(0).unwrap().unwrap();
    let right = function.parameter(1).unwrap().unwrap();
    let fixed_arg = function.parameter(2).unwrap().unwrap();
    assert!(
        builder
            .int_binop(IntOp::Add, &left, &fixed_arg, "rejected")
            .is_err()
    );
    let sum = builder.int_binop(IntOp::Add, &left, &right, "sum").unwrap();
    let inverted = builder.not(&sum, "inverted").unwrap();
    let sumf = builder.cast(CastOp::SiToFp, &sum, &floats, "sumf").unwrap();
    let negative = builder.fneg(&sumf, "negative").unwrap();
    let product = builder
        .float_binop(FloatOp::Mul, &sumf, &negative, "product")
        .unwrap();
    assert!(
        builder
            .icmp(IntPredicate::EQ, &product, &negative, "rejected")
            .is_err()
    );
    let negative_mask = builder
        .fcmp(FloatPredicate::OLT, &product, &sumf, "negative.mask")
        .unwrap();
    let int_mask = builder
        .icmp(IntPredicate::SLT, &sum, &right, "int.mask")
        .unwrap();
    assert_eq!(negative_mask.ty().ir(), "<vscale x 2 x i1>");
    assert_eq!(int_mask.ty().ir(), "<vscale x 2 x i1>");
    assert!(
        builder
            .select(&negative_mask, &fixed_arg, &fixed_arg, "rejected")
            .is_err()
    );
    let selected = builder
        .select(&negative_mask, &sum, &inverted, "selected")
        .unwrap();
    assert!(
        builder
            .shuffle_vector(&selected, &right, &fixed.const_zero().unwrap(), "rejected")
            .is_err()
    );
    assert!(
        builder
            .shuffle_vector(
                &selected,
                &right,
                &ints
                    .const_splat(&i32.const_int(&1.into()).unwrap())
                    .unwrap(),
                "rejected"
            )
            .is_err()
    );
    let shuffled = builder
        .shuffle_vector(&selected, &right, &ints.const_zero().unwrap(), "splat")
        .unwrap();
    assert!(
        builder
            .int_cast(&shuffled, &fixed, true, "rejected")
            .is_err()
    );
    let widened = builder.int_cast(&shuffled, &wide, true, "widened").unwrap();
    let frozen = builder.freeze(&widened, "frozen").unwrap();
    builder.ret(Some(&frozen)).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("sext <vscale x 2 x i32>"));
    assert!(ir.contains("freeze <vscale x 2 x i64>"));
    assert!(!ir.contains("rejected"));
}

#[test]
fn gep_checks_llvm_struct_and_scalable_vector_index_paths() {
    let context = Context::default();
    let module = context.module("gep_paths").unwrap();
    let ptr = context.pointer_type();
    let i32 = context.int_type(32).unwrap();
    let vector = i32.vector(2, true).unwrap();
    let plain_struct = context
        .struct_type(std::slice::from_ref(&i32), false)
        .unwrap();
    let scalable_struct = context
        .struct_type(std::slice::from_ref(&vector), false)
        .unwrap();
    let signature = ptr.function(&[ptr.clone(), i32.clone()], false).unwrap();
    let function = module.add_function("index_vector", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let address = function.parameter(0).unwrap().unwrap();
    let index = function.parameter(1).unwrap().unwrap();
    let zero = i32.const_zero().unwrap();
    let indices = [zero, index];
    assert!(
        builder
            .gep(&plain_struct, &address, &indices, "rejected")
            .is_err()
    );
    assert!(
        builder
            .gep(&scalable_struct, &address, &indices[..1], "rejected")
            .is_err()
    );
    let result = builder
        .gep_in_bounds(&vector, &address, &indices, "lane.address")
        .unwrap();
    builder.ret(Some(&result)).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("getelementptr inbounds <vscale x 2 x i32>"));
    assert!(!ir.contains("rejected"));
}

#[test]
fn cast_uses_llvm_legality_rules_before_emitting_an_instruction() {
    let context = Context::default();
    let module = context.module("cast").unwrap();
    let i32 = context.int_type(32).unwrap();
    let f32 = context.float_type();
    let ptr = context.pointer_type();
    let shared = context.pointer_type_in(3).unwrap();
    let fixed = context.int_type(16).unwrap().vector(2, false).unwrap();
    let scalable = context.int_type(16).unwrap().vector(2, true).unwrap();
    let shared_vector = shared.vector(1, false).unwrap();
    let signature = i32.function(&[i32.clone(), ptr], false).unwrap();
    let function = module.add_function("cast_values", &signature).unwrap();
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let number = function.parameter(0).unwrap().unwrap();
    let address = function.parameter(1).unwrap().unwrap();
    assert!(
        builder
            .cast(CastOp::BitCast, &number, &context.double_type(), "rejected")
            .is_err()
    );
    assert!(
        builder
            .cast(CastOp::BitCast, &number, &scalable, "rejected")
            .is_err()
    );
    let lanes = builder
        .cast(CastOp::BitCast, &number, &fixed, "lanes")
        .unwrap();
    assert_eq!(lanes.ty().vector_length().unwrap(), 2);
    let number = builder
        .cast(CastOp::BitCast, &number, &f32, "number")
        .unwrap();
    let address = builder
        .cast(CastOp::AddrSpaceCast, &address, &shared, "shared")
        .unwrap();
    assert_eq!(address.ty().pointer_address_space().unwrap(), 3);
    let shared_lane = builder
        .cast(CastOp::BitCast, &address, &shared_vector, "shared.lane")
        .unwrap();
    assert_eq!(shared_lane.ty().vector_length().unwrap(), 1);
    let rounded = builder
        .cast(CastOp::FpToSi, &number, &i32, "rounded")
        .unwrap();
    builder.ret(Some(&rounded)).unwrap();
    module.verify().unwrap();
    assert!(!module.ir().contains("rejected"));
}

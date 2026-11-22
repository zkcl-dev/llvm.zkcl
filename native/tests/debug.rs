use zkcl_llvm_api::{
    Builder, Context, DebugCompileUnitOptions, DebugEmission, DebugEncoding, DebugFunctionOptions,
    DebugInfo, DebugLanguage, DebugMember, DebugOptions, IntOp, OptimizationLevel, TargetMachine,
};

#[test]
fn line_tables_survive_bitcode_optimization_and_object_emission() {
    let context = Context::default();
    let module = context.module("debug").unwrap();
    let di = DebugInfo::new(&module, "demo.lisp", DebugOptions::default()).unwrap();
    assert!(DebugInfo::new(&module, "duplicate.lisp", DebugOptions::default()).is_err());

    let i32 = context.int_type(32).unwrap();
    let signature = i32.function(std::slice::from_ref(&i32), false).unwrap();
    let function = module.add_function("add_one", &signature).unwrap();
    assert!(function.debug_scope().is_none());
    di.attach_function(
        &function,
        4,
        DebugFunctionOptions {
            name: Some("add-one"),
            ..Default::default()
        },
    )
    .unwrap();
    let scope = function.debug_scope().unwrap();
    assert_eq!(scope.line(), 4);
    assert!(
        di.attach_function(&function, 4, DebugFunctionOptions::default())
            .is_err()
    );
    assert!(di.basic_type("int", 32, DebugEncoding::Signed).is_err());
    let included = di.file("included.lisp", "source").unwrap();
    let body = di.lexical_block(&scope, 10, 2, Some(&included)).unwrap();
    assert_eq!(body.line(), 10);
    assert_eq!(body.file().unwrap().filename().unwrap(), "included.lisp");

    let builder = Builder::new(&context);
    let entry = function.append_block("entry").unwrap();
    builder.position_at_end(&entry).unwrap();
    assert!(builder.debug_location().is_none());
    builder.set_debug_location(&body.location(11, 5)).unwrap();
    assert_eq!(builder.debug_location().unwrap().line(), 11);
    let value = function.parameter(0).unwrap().unwrap();
    let one = i32.const_int(&1.into()).unwrap();
    let result = builder
        .int_binop(IntOp::Add, &value, &one, "result")
        .unwrap();
    builder.set_debug_location(&body.location(12, 3)).unwrap();
    builder.ret(Some(&result)).unwrap();
    assert_eq!(builder.debug_location().unwrap().column(), 3);
    builder.set_debug_location(&body.location(11, 5)).unwrap();
    assert_eq!(builder.debug_location().unwrap().line(), 11);
    assert_eq!(builder.debug_location().unwrap().column(), 5);
    builder
        .position_before(&entry.terminator().unwrap())
        .unwrap();
    assert!(builder.debug_location().is_none());

    assert!(module.verify().unwrap_err().to_string().contains("finish"));
    assert!(module.bitcode().is_err());
    assert!(module.emit_object(None).is_err());
    di.finish();
    assert!(di.file("closed.lisp", "").is_err());

    let saved = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let saved_function = saved.find_function("add_one").unwrap().unwrap();
    let saved_scope = saved_function.debug_scope().unwrap();
    assert_eq!(saved_scope.line(), 4);
    assert_eq!(saved_scope.file().unwrap().filename().unwrap(), "demo.lisp");
    let optimized = saved.optimize("default<O2>", None).unwrap();
    let ir = optimized.ir();
    assert!(ir.contains("emissionKind: LineTablesOnly"));
    assert!(ir.contains("filename: \"included.lisp\""));
    assert!(ir.contains("DILocation(line: 11, column: 5"));
    assert!(ir.contains("linkageName: \"add_one\""));
    assert!(!optimized.emit_object(None).unwrap().is_empty());
}

#[test]
fn debug_compile_unit_records_source_language_and_options() {
    let context = Context::default();
    let module = context.module("compile-unit-settings").unwrap();
    assert!(
        DebugInfo::new(
            &module,
            "source.rb",
            DebugOptions {
                language: DebugLanguage::Ruby,
                ..DebugOptions::default()
            },
        )
        .is_err()
    );
    let debug = DebugInfo::with_compile_unit_options(
        &module,
        "source.ml",
        DebugOptions {
            language: DebugLanguage::OCaml,
            ..DebugOptions::default()
        },
        DebugCompileUnitOptions {
            flags: "-g -O2",
            runtime_version: 5,
            debug_info_for_profiling: true,
            sysroot: "/example",
            sdk: "ExampleSDK",
        },
    )
    .unwrap();
    debug.finish();
    let copy = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let ir = copy.ir();
    for field in [
        "language: DW_LANG_OCaml",
        "flags: \"-g -O2\"",
        "runtimeVersion: 5",
        "debugInfoForProfiling: true",
        "sysroot: \"/example\"",
        "sdk: \"ExampleSDK\"",
    ] {
        assert!(ir.contains(field), "missing {field} in {ir}");
    }
}

#[test]
fn debug_scopes_respect_module_and_function_ownership() {
    let context = Context::default();
    let module = context.module("work").unwrap();
    let other = context.module("foreign").unwrap();
    let di = DebugInfo::new(&module, "one.lisp", DebugOptions::default()).unwrap();
    let other_di = DebugInfo::new(&other, "two.lisp", DebugOptions::default()).unwrap();
    let foreign = other_di.file("foreign.lisp", "").unwrap();
    let ty = context.int_type(32).unwrap();
    let signature = ty.function(&[], false).unwrap();
    let traced = module.add_function("traced", &signature).unwrap();
    let plain = module.add_function("plain", &signature).unwrap();

    assert!(
        di.attach_function(
            &traced,
            1,
            DebugFunctionOptions {
                file: Some(&foreign),
                ..Default::default()
            }
        )
        .is_err()
    );
    let scope = di
        .attach_function(&traced, 1, DebugFunctionOptions::default())
        .unwrap();
    assert!(other_di.lexical_block(&scope, 1, 0, None).is_err());
    assert!(other_di.finish_function(&traced).is_err());
    assert!(di.finish_function(&plain).is_err());
    let location = scope.location(2, 1);
    let builder = Builder::new(&context);
    let zero = ty.const_zero().unwrap();

    builder
        .position_at_end(&traced.append_block("entry").unwrap())
        .unwrap();
    builder.set_debug_location(&location).unwrap();
    builder.ret(Some(&zero)).unwrap();
    let plain_entry = plain.append_block("entry").unwrap();
    builder.position_at_end(&plain_entry).unwrap();
    assert!(builder.set_debug_location(&location).is_err());
    builder.ret(Some(&zero)).unwrap();

    let plain_return = plain_entry.terminator().unwrap();
    assert!(plain_return.debug_location().is_none());
    assert!(plain_return.set_debug_location(Some(&location)).is_err());
    let caller_file = di.file("outer.lisp", "source").unwrap();
    let caller_scope = di
        .attach_function(
            &plain,
            5,
            DebugFunctionOptions {
                file: Some(&caller_file),
                ..Default::default()
            },
        )
        .unwrap();
    let callsite = caller_scope.location(6, 3);
    let inlined = scope.inlined_location(3, 2, &callsite).unwrap();
    plain_return.set_debug_location(Some(&inlined)).unwrap();

    // The module and locations continue to own the LLVM context after finalization.
    drop(di);
    module.verify().unwrap();
    let ir = module.ir();
    assert_eq!(ir.matches("ret i32 0, !dbg").count(), 2);
    assert!(ir.contains("inlinedAt:"));
    let location = plain_return.debug_location().unwrap();
    assert_eq!((location.line(), location.column()), (3, 2));
    assert_eq!(location.file().unwrap().filename().unwrap(), "one.lisp");
    let caller = location.inlined_at().unwrap();
    assert_eq!((caller.line(), caller.column()), (6, 3));
    let file = caller.file().unwrap();
    assert_eq!(file.filename().unwrap(), "outer.lisp");
    assert_eq!(file.directory().unwrap(), "source");
    assert!(caller.inlined_at().is_none());
    plain_return.set_debug_location(None).unwrap();
    assert!(plain_return.debug_location().is_none());
    module.verify().unwrap();
}

#[test]
fn full_debug_info_tracks_a_parameter_and_local_value() {
    let context = Context::default();
    let module = context.module("locals").unwrap();
    let debug = DebugInfo::new(
        &module,
        "locals.lisp",
        DebugOptions {
            emission: DebugEmission::Full,
            ..DebugOptions::default()
        },
    )
    .unwrap();
    let int_debug = debug.basic_type("int", 32, DebugEncoding::Signed).unwrap();
    let i32 = context.int_type(32).unwrap();
    let signature = i32.function(std::slice::from_ref(&i32), false).unwrap();
    let function = module.add_function("increment", &signature).unwrap();
    let scope = debug
        .attach_function(
            &function,
            5,
            DebugFunctionOptions {
                result: Some(&int_debug),
                parameters: std::slice::from_ref(&int_debug),
                ..Default::default()
            },
        )
        .unwrap();
    assert!(
        debug
            .variable(&scope, "missing", &int_debug, 5, Some(1))
            .is_err()
    );
    let argument = debug
        .variable(&scope, "input", &int_debug, 5, Some(0))
        .unwrap();
    let body = debug.lexical_block(&scope, 6, 3, None).unwrap();
    assert!(
        debug
            .variable(&body, "bad-scope", &int_debug, 6, Some(0))
            .is_err()
    );
    let local = debug
        .variable(&body, "result", &int_debug, 6, None)
        .unwrap();
    let entry = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    let input = function.parameter(0).unwrap().unwrap();
    let argument_location = scope.location(5, 1);
    assert!(
        debug
            .record_value(&entry, &argument, &input, &argument_location, None)
            .is_err()
    );
    let location = body.location(6, 3);
    builder.set_debug_location(&location).unwrap();
    let one = i32.const_int(&1.into()).unwrap();
    let sum = builder.int_binop(IntOp::Add, &input, &one, "sum").unwrap();
    debug
        .record_value(&entry, &argument, &input, &argument_location, Some(&sum))
        .unwrap();
    builder.ret(Some(&sum)).unwrap();
    debug.finish_function(&function).unwrap();
    assert!(module.ir().contains("retainedNodes:"));
    debug.finish_function(&function).unwrap();
    assert!(
        debug
            .variable(&body, "late", &int_debug, 6, None)
            .is_err_and(|error| error.to_string().contains("already finished"))
    );
    debug
        .record_value(&entry, &local, &sum, &location, None)
        .unwrap();
    debug.finish();
    assert!(debug.finish_function(&function).is_err());
    assert!(
        debug
            .record_value(&entry, &local, &sum, &location, None)
            .is_err()
    );
    module.verify().unwrap();
    let copy = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let copied_function = copy.find_function("increment").unwrap().unwrap();
    let new_unit = DebugInfo::new(&copy, "second.lisp", DebugOptions::default()).unwrap();
    assert!(new_unit.finish_function(&copied_function).is_err());
    assert!(
        new_unit
            .lexical_block(&copied_function.debug_scope().unwrap(), 4, 1, None)
            .is_err()
    );
    new_unit.finish();
    copy.verify().unwrap();
    let ir = copy.ir();
    assert!(ir.contains("DILocalVariable(name: \"input\", arg: 1"));
    assert!(ir.contains("DILocalVariable(name: \"result\""));
    assert!(ir.contains("DIBasicType(name: \"int\", size: 32, encoding: DW_ATE_signed"));
    assert_eq!(ir.matches("#dbg_value(").count(), 2);
    assert!(!copy.emit_object(None).unwrap().is_empty());
}

#[test]
fn debug_addresses_describe_memory_locals_with_composite_types() {
    let context = Context::default();
    let module = context.module("storage").unwrap();
    let machine = TargetMachine::new("x86_64-unknown-linux-gnu", OptimizationLevel::None).unwrap();
    machine.configure_module(&module).unwrap();
    let debug = DebugInfo::new(
        &module,
        "storage.lisp",
        DebugOptions {
            emission: DebugEmission::Full,
            ..DebugOptions::default()
        },
    )
    .unwrap();
    let int_debug = debug.basic_type("int", 32, DebugEncoding::Signed).unwrap();
    let array_debug = debug.array_type(&int_debug, 64, 32, 2).unwrap();
    let pointer_debug = debug.pointer_type(Some(&int_debug), 64, 64, 0, "").unwrap();
    let array_member = DebugMember::new("entries", &array_debug, 64, 32, 0).unwrap();
    let pointer_member = DebugMember::new("next", &pointer_debug, 64, 64, 64).unwrap();
    let definition = debug.file("storage-types.lisp", "source").unwrap();
    assert!(debug.array_type(&int_debug, 64, 32, u64::MAX).is_err());
    assert!(
        debug
            .union_type(
                "invalid",
                128,
                64,
                3,
                None,
                std::slice::from_ref(&pointer_member)
            )
            .is_err()
    );
    let struct_debug = debug
        .struct_type(
            "table",
            128,
            64,
            3,
            Some(&definition),
            &[array_member, pointer_member],
        )
        .unwrap();
    let unsigned_debug = debug
        .basic_type("unsigned int", 32, DebugEncoding::Unsigned)
        .unwrap();
    let union_debug = debug
        .union_type(
            "number",
            32,
            32,
            4,
            Some(&definition),
            &[
                DebugMember::new("signed", &int_debug, 32, 32, 0).unwrap(),
                DebugMember::new("unsigned", &unsigned_debug, 32, 32, 0).unwrap(),
            ],
        )
        .unwrap();

    let i32 = context.int_type(32).unwrap();
    let signature = i32.function(&[], false).unwrap();
    let function = module.add_function("test_storage", &signature).unwrap();
    let scope = debug
        .attach_function(&function, 5, DebugFunctionOptions::default())
        .unwrap();
    let location = scope.location(6, 3);
    let table = debug
        .variable(&scope, "table", &struct_debug, 6, None)
        .unwrap();
    let number = debug
        .variable(&scope, "number", &union_debug, 6, None)
        .unwrap();
    let entry = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&entry).unwrap();
    builder.set_debug_location(&location).unwrap();
    let storage = context
        .struct_type(&[i32.array(2).unwrap(), context.pointer_type()], false)
        .unwrap();
    let table_address = builder.alloca(&storage, "table.address").unwrap();
    let number_address = builder.alloca(&i32, "number.address").unwrap();
    let zero = i32.const_zero().unwrap();
    builder
        .store(&storage.const_zero().unwrap(), &table_address)
        .unwrap();
    builder.store(&zero, &number_address).unwrap();
    builder.ret(Some(&zero)).unwrap();
    assert!(
        debug
            .record_address(&entry, &table, &zero, &location, None)
            .is_err()
    );
    debug
        .record_address(
            &entry,
            &table,
            &table_address,
            &location,
            Some(&number_address),
        )
        .unwrap();
    debug
        .record_address(&entry, &number, &number_address, &location, None)
        .unwrap();
    debug.finish();
    module.verify().unwrap();

    let copy = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let ir = copy.ir();
    assert_eq!(ir.matches("#dbg_declare(").count(), 2);
    assert!(ir.contains("DW_TAG_structure_type, name: \"table\""));
    assert!(ir.contains("DW_TAG_union_type, name: \"number\""));
    assert!(ir.contains("DW_TAG_member, name: \"next\""));
    assert!(ir.contains("offset: 64"));
    assert!(ir.contains("DW_TAG_pointer_type"));
    assert!(ir.contains("DW_TAG_array_type"));
    assert!(ir.contains("DISubrange(count: 2"));
    assert!(!copy.emit_object(Some(&machine)).unwrap().is_empty());
}

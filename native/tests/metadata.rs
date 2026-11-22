use num_bigint::BigInt;
use zkcl_llvm_api::{
    Builder, Context, DebugFunctionOptions, DebugInfo, DebugOptions, ModuleFlagBehavior,
};

#[test]
fn named_metadata_round_trips_through_bitcode() {
    let context = Context::default();
    let module = context.module("metadata").unwrap();
    let text = context.metadata_string("source\0file λ");
    let wide: BigInt = (BigInt::from(1) << 100) + BigInt::from(39);
    let integer = context
        .int_type(128)
        .unwrap()
        .const_int(&wide)
        .unwrap()
        .as_metadata()
        .unwrap();
    let tuple = context
        .metadata_node(&[Some(text.clone()), None, Some(integer.clone())])
        .unwrap();
    let singleton = context.metadata_node(&[Some(integer.clone())]).unwrap();
    assert!(module.add_named_metadata("frontend", &text).is_err());
    module.add_named_metadata("frontend", &tuple).unwrap();
    module.add_named_metadata("frontend", &singleton).unwrap();
    assert_eq!(module.named_metadata_count("frontend").unwrap(), 2);

    let copy = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let nodes = copy.named_metadata("frontend").unwrap();
    let values = nodes[0].operands().unwrap();
    assert_eq!(
        values[0].as_ref().unwrap().string().unwrap().as_deref(),
        Some("source\0file λ")
    );
    assert!(values[1].is_none());
    assert_eq!(values[2].as_ref().unwrap().integer(), Some(wide.clone()));
    assert_eq!(
        nodes[1].operands().unwrap()[0].as_ref().unwrap().integer(),
        Some(wide)
    );
    assert!(copy.named_metadata("missing").unwrap().is_empty());
}

#[test]
fn module_metadata_retains_the_owner_and_rejects_foreign_operands() {
    let context = Context::default();
    let module = context
        .parse_ir("@first = global i32 1\ndefine i32 @f(i32 %v) { ret i32 %v }")
        .unwrap();
    let other = context.parse_ir("@second = global i32 2").unwrap();
    let global = module
        .find_global("first")
        .unwrap()
        .unwrap()
        .address()
        .as_metadata()
        .unwrap();
    let foreign_global = other
        .find_global("second")
        .unwrap()
        .unwrap()
        .address()
        .as_metadata()
        .unwrap();
    let parameter = module
        .find_function("f")
        .unwrap()
        .unwrap()
        .parameter(0)
        .unwrap()
        .unwrap();
    assert!(parameter.as_metadata().is_err());
    assert!(
        context
            .metadata_node(&[Some(global.clone()), Some(foreign_global)])
            .is_err()
    );
    assert!(
        Context::default()
            .metadata_node(&[Some(global.clone())])
            .is_err()
    );

    let entry = context.metadata_node(&[Some(global)]).unwrap();
    assert!(other.add_named_metadata("globals", &entry).is_err());
    module.add_named_metadata("globals", &entry).unwrap();
    module.verify().unwrap();
    drop(module);
    assert!(
        entry.operands().unwrap()[0]
            .as_ref()
            .unwrap()
            .ir()
            .contains("@first")
    );
}

#[test]
fn module_flags_link_using_llvm_merge_behavior() {
    let context = Context::default();
    let left = context.module("left").unwrap();
    let right = context.module("right").unwrap();
    let int = context.int_type(32).unwrap();
    let low = int.const_int(&3.into()).unwrap().as_metadata().unwrap();
    let high = int.const_int(&9.into()).unwrap().as_metadata().unwrap();
    let left_values = context.metadata_node(&[Some(low.clone())]).unwrap();
    let right_values = context.metadata_node(&[Some(high.clone())]).unwrap();
    for (module, integer, tuple) in [(&left, &low, &left_values), (&right, &high, &right_values)] {
        module
            .add_module_flag("maximum", ModuleFlagBehavior::Max, integer)
            .unwrap();
        module
            .add_module_flag("minimum", ModuleFlagBehavior::Min, integer)
            .unwrap();
        module
            .add_module_flag("list", ModuleFlagBehavior::AppendUnique, tuple)
            .unwrap();
    }
    assert!(
        left.add_module_flag("maximum", ModuleFlagBehavior::Max, &high)
            .is_err()
    );
    assert!(
        left.add_module_flag("bad", ModuleFlagBehavior::Append, &low)
            .is_err()
    );
    assert!(
        left.add_module_flag("bad", ModuleFlagBehavior::Require, &left_values)
            .is_err()
    );
    assert!(left.module_flag("missing").unwrap().is_none());
    let negative = int.const_int(&(-2).into()).unwrap().as_metadata().unwrap();
    assert!(
        left.add_module_flag("bad", ModuleFlagBehavior::Min, &negative)
            .is_err()
    );
    let require = context
        .metadata_node(&[Some(context.metadata_string("maximum")), Some(high.clone())])
        .unwrap();
    right
        .add_module_flag("require-maximum", ModuleFlagBehavior::Require, &require)
        .unwrap();

    let linked = left.link(&[right]).unwrap();
    let (behavior, value) = linked.module_flag("maximum").unwrap().unwrap();
    assert_eq!(behavior, ModuleFlagBehavior::Max);
    assert_eq!(value.integer(), Some(9.into()));
    let (behavior, value) = linked.module_flag("minimum").unwrap().unwrap();
    assert_eq!(behavior, ModuleFlagBehavior::Min);
    assert_eq!(value.integer(), Some(3.into()));
    let (behavior, value) = linked.module_flag("list").unwrap().unwrap();
    assert_eq!(behavior, ModuleFlagBehavior::AppendUnique);
    let numbers: Vec<_> = value
        .operands()
        .unwrap()
        .iter()
        .map(|item| item.as_ref().unwrap().integer().unwrap())
        .collect();
    assert_eq!(numbers, [3.into(), 9.into()]);
    assert_eq!(
        linked.module_flag("require-maximum").unwrap().unwrap().0,
        ModuleFlagBehavior::Require
    );
}

#[test]
fn parsed_flags_with_null_values_or_byte_strings_do_not_block_other_flags() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
!llvm.module.flags = !{!0, !1, !2}
!0 = !{i32 1, !"raw\FF", i32 0}
!1 = !{i32 1, !"null", null}
!2 = !{i32 1, !"number", i32 4}
"#,
        )
        .unwrap();
    assert!(module.module_flag("null").unwrap().is_none());
    let (_, integer) = module.module_flag("number").unwrap().unwrap();
    assert_eq!(integer.integer(), Some(4.into()));
    assert!(
        module
            .add_module_flag("null", ModuleFlagBehavior::Error, &integer)
            .is_err()
    );
}

#[test]
fn stripping_debug_info_preserves_the_module_and_its_instruction_handles() {
    let context = Context::default();
    let module = context.module("original").unwrap();
    assert!(module.set_identifier("malformed\0module").is_err());
    module.set_identifier("renamed λ").unwrap();
    module.set_source_filename("source.lisp");
    module.set_inline_asm(".text");
    module.append_inline_asm(".p2align 2");
    assert_eq!(module.inline_asm().unwrap(), ".text\n.p2align 2\n");
    let debug = DebugInfo::new(&module, "source.lisp", DebugOptions::default()).unwrap();
    let signature = context.void_type().function(&[], false).unwrap();
    let function = module.add_function("main", &signature).unwrap();
    let scope = debug
        .attach_function(&function, 1, DebugFunctionOptions::default())
        .unwrap();
    let block = function.append_block("entry").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&block).unwrap();
    builder.set_debug_location(&scope.location(2, 1)).unwrap();
    builder.ret(None).unwrap();
    let original_instruction = block.terminator().unwrap();
    assert!(module.strip_debug_info().is_err());
    debug.finish();

    let snapshot = module.strip_debug_info().unwrap();
    assert_eq!(snapshot.identifier().unwrap(), "renamed λ");
    assert_eq!(snapshot.source_filename().unwrap(), "source.lisp");
    assert_eq!(snapshot.inline_asm().unwrap(), ".text\n.p2align 2\n");
    assert!(!snapshot.ir().contains("!dbg"));
    assert!(!snapshot.ir().contains("!llvm.dbg.cu"));
    assert!(original_instruction.ir().contains("!dbg"));
    assert!(module.ir().contains("!llvm.dbg.cu"));
}

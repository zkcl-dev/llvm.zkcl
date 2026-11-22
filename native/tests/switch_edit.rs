use zkcl_llvm_api::{Context, DebugEmission, DebugEncoding, DebugInfo, DebugOptions};

#[test]
fn edits_switch_cases_with_parallel_edges_and_preserves_metadata() {
    let context = Context::default();
    let source = context
        .parse_ir(
            r#"
            define i32 @choose(i128 %key, i1 %test, i32 %value) {
            entry: br i1 %test, label %dispatch, label %other
            other: br label %join
            dispatch:
              switch i128 %key, label %join [ i128 1, label %join
                                              i128 2, label %cold ], !prof !0, !annotation !1
            join:
              %chosen = phi i32 [ %value, %dispatch ], [ 3, %other ], [ %value, %dispatch ]
              ret i32 %chosen
            cold:
              ret i32 0
            }
            !0 = !{!"branch_weights", i32 8, i32 13, i32 21}
            !1 = !{!"dispatch"}
            "#,
        )
        .unwrap();
    let function = source.find_function("choose").unwrap().unwrap();
    let blocks: Vec<_> = function.blocks().collect();
    let [entry, _, dispatch, join, cold] = blocks.as_slice() else {
        panic!("expected five blocks");
    };
    let switch = dispatch.terminator().unwrap();
    let old_phi = join.first_instruction().unwrap();
    let value = function.parameter(2).unwrap().unwrap();

    let debug = DebugInfo::new(
        &source,
        "switch.lisp",
        DebugOptions {
            emission: DebugEmission::Full,
            ..Default::default()
        },
    )
    .unwrap();
    let scope = debug
        .attach_function(&function, 1, Default::default())
        .unwrap();
    let int_debug = debug.basic_type("int", 32, DebugEncoding::Signed).unwrap();
    let variable = debug
        .variable(&scope, "value", &int_debug, 2, None)
        .unwrap();
    let location = scope.location(2, 3);
    switch.set_debug_location(Some(&location)).unwrap();
    debug
        .record_value(dispatch, &variable, &value, &location, None)
        .unwrap();
    debug.finish();

    let before = source.ir();
    let (one, case_destination) = switch.switch_case(0).unwrap();
    assert!(case_destination.same_as(join));
    let (two, case_destination) = switch.switch_case(1).unwrap();
    assert!(case_destination.same_as(cold));
    assert!(switch.switch_case(2).is_none());
    assert_eq!(entry.terminator().unwrap().switch_case_count(), 0);
    assert!(switch.successor(0).unwrap().same_as(join));

    let i32 = context.int_type(32).unwrap();
    let i128 = context.int_type(128).unwrap();
    let three = i32.const_int(&3.into()).unwrap();
    let wide_key = i128.const_int(&(1_u128 << 96).into()).unwrap();
    let edit = source.edit().unwrap();
    assert!(edit.add_switch_case(&switch, &one, join, &[]).is_err());
    assert!(edit.add_switch_case(&switch, &three, join, &[]).is_err());
    assert!(edit.add_switch_case(&switch, &wide_key, join, &[]).is_err());
    assert!(
        edit.add_switch_case(&switch, &wide_key, join, &[three.clone()])
            .is_err()
    );
    edit.add_switch_case(&switch, &wide_key, join, &[value])
        .unwrap();
    let added = edit.finish().unwrap();
    let blocks: Vec<_> = added
        .find_function("choose")
        .unwrap()
        .unwrap()
        .blocks()
        .collect();
    let added_switch = blocks[2].terminator().unwrap();
    assert_eq!(added_switch.switch_case_count(), 3);
    assert!(added_switch.switch_case(2).unwrap().0.same_as(&wide_key));
    assert_eq!(blocks[3].first_instruction().unwrap().operand_count(), 4);
    assert!(
        added
            .ir()
            .contains("!{!\"branch_weights\", i32 8, i32 13, i32 21, i32 0}")
    );

    let edit = source.edit().unwrap();
    edit.remove_switch_case(&switch, &one).unwrap();
    assert!(edit.remove_switch_case(&switch, &one).is_err());
    edit.replace_operand(&old_phi, 1, &three).unwrap();
    let removed = edit.finish().unwrap();
    let blocks: Vec<_> = removed
        .find_function("choose")
        .unwrap()
        .unwrap()
        .blocks()
        .collect();
    let new_switch = blocks[2].terminator().unwrap();
    assert_eq!(new_switch.switch_case_count(), 1);
    assert!(new_switch.switch_case(0).unwrap().0.same_as(&two));
    assert_eq!(new_switch.debug_location().unwrap().line(), 2);
    let new_phi = blocks[3].first_instruction().unwrap();
    assert_eq!(new_phi.operand_count(), 2);
    assert!(new_phi.phi_incoming_block(0).unwrap().same_as(&blocks[2]));
    assert!(new_phi.phi_incoming_block(1).unwrap().same_as(&blocks[1]));
    let ir = removed.ir();
    assert!(ir.contains("!{!\"branch_weights\", i32 8, i32 21}"));
    assert!(ir.contains("!annotation"));
    assert_eq!(ir.matches("#dbg_value(").count(), 1);
    assert_eq!(source.ir(), before);
}

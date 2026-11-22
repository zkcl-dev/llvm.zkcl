use zkcl_llvm_api::{
    Builder, Context, FloatPredicate, GlobalOptions, IntOp, IntPredicate, TypeKind, UnnamedAddress,
};

#[test]
fn inspects_comparison_predicates_and_preserves_renamed_handles() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            @slot = global i32 0
            declare double @llvm.sqrt.f64(double)

            define i1 @compare(i32 %integer, double %real) {
            entry:
              %int_test = icmp uge i32 %integer, 0
              %float_test = fcmp false double %real, 0.0
              %result = or i1 %int_test, %float_test
              ret i1 %result
            }

            define ptr @address() { ret ptr @slot }
            "#,
        )
        .unwrap();
    let function = module.find_function("compare").unwrap().unwrap();
    let block = function.blocks().next().unwrap();
    let icmp = block.first_instruction().unwrap();
    let fcmp = icmp.next_instruction().unwrap();
    let ret = block.last_instruction().unwrap();
    assert!(fcmp.previous_instruction().unwrap().same_as(&icmp));
    assert!(icmp.previous_instruction().is_none());
    assert!(ret.next_instruction().is_none());
    assert!(ret.same_as(&block.terminator().unwrap()));
    assert_eq!(icmp.icmp_predicate(), Some(IntPredicate::UGE));
    assert_eq!(fcmp.fcmp_predicate(), Some(FloatPredicate::PredicateFalse));
    assert_eq!(icmp.fcmp_predicate(), None);
    assert_eq!(fcmp.icmp_predicate(), None);

    let result = icmp.result().unwrap();
    icmp.set_name("large_enough").unwrap();
    assert_eq!(result.name().unwrap(), "large_enough");
    result.set_name("").unwrap();
    assert_eq!(icmp.name().unwrap(), "");
    assert!(ret.set_name("void_result").is_err());
    assert!(icmp.operand(1).unwrap().set_name("constant").is_err());

    let parameter = function.parameter(0).unwrap().unwrap();
    parameter.set_name("input").unwrap();
    assert_eq!(icmp.operand(0).unwrap().name().unwrap(), "input");
    block.set_name("test").unwrap();
    assert_eq!(icmp.block().name().unwrap(), "test");

    let global = module.find_global("slot").unwrap().unwrap();
    function.set_name("slot").unwrap();
    let actual = function.name().unwrap();
    assert!(actual.starts_with("slot."));
    assert!(function.same_as(&module.find_function(&actual).unwrap().unwrap()));
    assert!(module.find_function("compare").unwrap().is_none());
    function.set_unnamed_address(UnnamedAddress::Local);
    assert_eq!(function.unnamed_address(), UnnamedAddress::Local);

    let address = global.address();
    global.set_name("storage").unwrap();
    assert_eq!(address.name().unwrap(), "storage");
    assert!(global.same_as(&module.find_global("storage").unwrap().unwrap()));
    assert!(module.ir().contains("ret ptr @storage"));

    let intrinsic = module.find_function("llvm.sqrt.f64").unwrap().unwrap();
    assert!(intrinsic.set_name("square_root").is_err());
    assert!(function.set_name("llvm.foo").is_err());
    module.verify().unwrap();
}

#[test]
fn inspects_and_rewrites_parsed_instructions() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            declare token @llvm.coro.id(i32, ptr, ptr, ptr)
            declare i1 @llvm.type.test(ptr, metadata)

            define i32 @transform(i32 %x, i1 %take) {
              entry:
                %sum = add i32 %x, 1
                br i1 %take, label %left, label %right
              left:
                %more = add i32 %sum, 2
                br label %join
              right:
                br label %join
              join:
                %chosen = phi i32 [ %more, %left ], [ %sum, %right ]
                ret i32 %chosen
            }

            define i32 @other(i32 %y) { ret i32 %y }

            define void @special_operands() {
              %id = call token @llvm.coro.id(i32 0, ptr null, ptr null, ptr null)
              %test = call i1 @llvm.type.test(ptr null, metadata !"payload")
              call void asm "", ""()
              ret void
            }
            "#,
        )
        .unwrap();

    let function = module.find_function("transform").unwrap().unwrap();
    let blocks: Vec<_> = function.blocks().collect();
    assert_eq!(
        blocks[0]
            .instructions()
            .map(|inst| inst.opcode())
            .collect::<Vec<_>>(),
        ["add", "br"]
    );
    let add = blocks[0].instructions().next().unwrap();
    let add_result = add.result().unwrap();
    assert_eq!(add.operand_count(), 2);
    assert!(
        add.operand(0)
            .unwrap()
            .same_as(&function.parameter(0).unwrap().unwrap())
    );
    assert!(add.operand(2).is_none());
    assert_eq!(add.successor_count(), 0);
    assert!(add.phi_incoming_block(0).is_none());

    let branch = blocks[0].terminator().unwrap();
    assert!(branch.result().is_none());
    assert_eq!(branch.successor_count(), 2);
    assert!(branch.successor(0).unwrap().same_as(&blocks[1]));
    assert!(branch.successor(1).unwrap().same_as(&blocks[2]));
    assert!(branch.successor(2).is_none());
    let label = branch.operand(1).unwrap();
    assert_eq!(label.ty().kind(), TypeKind::LLVMLabelTypeKind);
    assert!(!label.ir().is_empty());

    let later = blocks[1].instructions().next().unwrap();
    let phi = blocks[3].instructions().next().unwrap();
    assert_eq!(phi.opcode(), "phi");
    assert!(phi.phi_incoming_block(0).unwrap().same_as(&blocks[1]));
    assert!(phi.phi_incoming_block(1).unwrap().same_as(&blocks[2]));
    assert!(phi.phi_incoming_block(2).is_none());
    assert!(phi.operand(1).unwrap().same_as(&add_result));

    let special = module.find_function("special_operands").unwrap().unwrap();
    let special_block = special.blocks().next().unwrap();
    let mut calls = special_block.instructions();
    let token = calls.next().unwrap().result().unwrap();
    assert_eq!(token.ty().kind(), TypeKind::LLVMTokenTypeKind);
    assert!(token.ir().contains("call token"));
    let type_test = calls.next().unwrap();
    let metadata = type_test.operand(1).unwrap();
    assert_eq!(metadata.ty().kind(), TypeKind::LLVMMetadataTypeKind);
    assert!(metadata.ir().contains("payload"));
    let inline_asm = calls.next().unwrap().operand(0).unwrap();
    assert!(inline_asm.ir().contains("asm"));

    let before = module.ir();
    let error = add
        .replace_operand(0, &later.result().unwrap())
        .unwrap_err();
    assert!(error.to_string().contains("does not dominate"));
    assert_eq!(module.ir(), before);
    let other = module.find_function("other").unwrap().unwrap();
    assert!(
        add.replace_operand(0, &other.parameter(0).unwrap().unwrap())
            .is_err()
    );
    assert!(add.replace_operand(0, &label).is_err());
    assert!(
        add.replace_operand(0, &function.parameter(1).unwrap().unwrap())
            .is_err()
    );
    assert!(
        branch
            .replace_operand(0, &function.parameter(1).unwrap().unwrap())
            .is_err()
    );

    let seven = context.int_type(32).unwrap().const_int(&7.into()).unwrap();
    add.replace_operand(1, &seven).unwrap();
    function.module().verify().unwrap();
    drop(module);
    assert!(add.ir().contains("add i32 %x, 7"));
    assert!(add.result().unwrap().same_as(&add_result));
    assert!(add.operand(1).unwrap().same_as(&seven));
}

#[test]
fn edits_a_draft_without_invalidating_source_handles() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            define i32 @transform(i32 %x) {
              %sum = add i32 %x, 1
              %twice = add i32 %sum, %sum
              ret i32 %twice
            }
            "#,
        )
        .unwrap();
    let function = module.find_function("transform").unwrap().unwrap();
    let block = function.blocks().next().unwrap();
    let mut instructions = block.instructions();
    let sum = instructions.next().unwrap();
    let twice = instructions.next().unwrap();
    let ret = instructions.next().unwrap();
    let sum_value = sum.result().unwrap();
    let before = module.ir();
    let edit = module.edit().unwrap();

    assert!(edit.erase_instruction(&sum).is_err());
    edit.move_before(&twice, &sum).unwrap();
    assert!(
        edit.finish()
            .err()
            .unwrap()
            .to_string()
            .contains("does not dominate")
    );
    assert_eq!(module.ir(), before);
    edit.move_before(&sum, &twice).unwrap();

    let foreign = Context::default().int_type(32).unwrap();
    assert!(
        edit.replace_operand(&ret, 0, &foreign.const_int(&7.into()).unwrap())
            .is_err()
    );
    let seven = context.int_type(32).unwrap().const_int(&7.into()).unwrap();
    assert_eq!(edit.replace_all_uses(&sum_value, &seven).unwrap(), 2);
    edit.replace_operand(&ret, 0, &seven).unwrap();
    edit.erase_instruction(&sum).unwrap();
    assert!(edit.erase_instruction(&sum).is_err());
    assert!(edit.replace_operand(&twice, 0, &sum_value).is_err());
    let edited = edit.finish().unwrap();

    assert!(edited.ir().contains("%twice = add i32 7, 7"));
    assert!(edited.ir().contains("ret i32 7"));
    assert!(!edited.ir().contains("%sum"));
    assert!(edit.finish().is_err());
    assert_eq!(module.ir(), before);
    assert_eq!(sum.opcode(), "add");
    assert!(sum.result().unwrap().same_as(&sum_value));
}

#[test]
fn redirects_one_of_parallel_successors_and_repairs_both_phis() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            define i32 @route(i32 %key) {
            entry:
              switch i32 %key, label %left [ i32 0, label %left
                                             i32 1, label %right ]
            left:
              %l = phi i32 [ 5, %entry ], [ 5, %entry ]
              ret i32 %l
            right:
              %r = phi i32 [ 8, %entry ]
              ret i32 %r
            }
            "#,
        )
        .unwrap();
    let route = module.find_function("route").unwrap().unwrap();
    let blocks: Vec<_> = route.blocks().collect();
    let switch = blocks[0].terminator().unwrap();
    let left_phi = blocks[1].instructions().next().unwrap();
    let i32 = context.int_type(32).unwrap();
    let seven = i32.const_int(&7.into()).unwrap();
    let eight = i32.const_int(&8.into()).unwrap();
    let nine = i32.const_int(&9.into()).unwrap();
    let edit = module.edit().unwrap();

    assert!(
        edit.redirect_successor(&switch, 0, &blocks[2], &[])
            .is_err()
    );
    assert!(
        edit.redirect_successor(&switch, 0, &blocks[2], &[seven])
            .is_err()
    );
    edit.redirect_successor(&switch, 0, &blocks[2], &[eight])
        .unwrap();
    edit.replace_operand(&left_phi, 0, &nine).unwrap();
    let edited = edit.finish().unwrap();
    let route = edited.find_function("route").unwrap().unwrap();
    let edited_blocks: Vec<_> = route.blocks().collect();
    let edited_switch = edited_blocks[0].terminator().unwrap();
    assert!(
        edited_switch
            .successor(0)
            .unwrap()
            .same_as(&edited_blocks[2])
    );
    assert!(
        edited_switch
            .successor(1)
            .unwrap()
            .same_as(&edited_blocks[1])
    );
    assert!(switch.successor(0).unwrap().same_as(&blocks[1]));
    assert_eq!(left_phi.operand_count(), 2);
    let edited_left_phi = edited_blocks[1].instructions().next().unwrap();
    assert_eq!(edited_left_phi.operand_count(), 1);
    assert_eq!(edited_left_phi.operand(0).unwrap().ir(), nine.ir());
    assert_eq!(
        edited_blocks[2]
            .instructions()
            .next()
            .unwrap()
            .operand_count(),
        2
    );
}

#[test]
fn maps_only_module_symbols_present_in_the_snapshot() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            @original = global i32 1
            @replacement = global i32 2
            define ptr @address() { ret ptr @original }
            "#,
        )
        .unwrap();
    let address = module.find_function("address").unwrap().unwrap();
    let ret = address.blocks().next().unwrap().terminator().unwrap();
    let edit = module.edit().unwrap();
    let added = module
        .add_global(
            "added",
            &context.int_type(32).unwrap(),
            GlobalOptions::default(),
        )
        .unwrap()
        .address();
    assert!(edit.replace_operand(&ret, 0, &added).is_err());

    let replacement = module
        .find_global("replacement")
        .unwrap()
        .unwrap()
        .address();
    edit.replace_operand(&ret, 0, &replacement).unwrap();
    let edited = edit.finish().unwrap();
    assert!(edited.ir().contains("ret ptr @replacement"));
    assert!(edited.find_global("added").unwrap().is_none());
    let clone = edited
        .find_global("replacement")
        .unwrap()
        .unwrap()
        .address();
    assert!(!clone.same_as(&replacement));
    assert!(ret.ir().contains("ret ptr @original"));
}

#[test]
fn moves_blocks_without_changing_entry_or_phi_edges() {
    let module = Context::default()
        .parse_ir(
            r#"
            define i32 @choose(i1 %test) {
            entry: br i1 %test, label %yes, label %no
            yes: br label %join
            no: br label %join
            join:
              %value = phi i32 [ 1, %yes ], [ 2, %no ]
              ret i32 %value
            }
            define void @other() { ret void }
            "#,
        )
        .unwrap();
    let choose = module.find_function("choose").unwrap().unwrap();
    let blocks: Vec<_> = choose.blocks().collect();
    let [entry, yes, no, join] = blocks.as_slice() else {
        panic!("expected four blocks");
    };
    let other = module.find_function("other").unwrap().unwrap();
    let foreign = other.blocks().next().unwrap();
    let before = module.ir();
    let edit = module.edit().unwrap();

    assert!(edit.move_block_after(yes, &foreign).is_err());
    assert!(edit.move_block_before(no, entry).is_err());
    assert!(edit.move_block_after(entry, join).is_err());
    edit.move_block_before(entry, yes).unwrap();
    edit.move_block_after(yes, yes).unwrap();
    edit.move_block_before(no, yes).unwrap();
    edit.move_block_after(yes, join).unwrap();
    let edited = edit.finish().unwrap();

    let choose = edited.find_function("choose").unwrap().unwrap();
    let blocks: Vec<_> = choose.blocks().collect();
    assert_eq!(
        blocks
            .iter()
            .map(|block| block.name().unwrap())
            .collect::<Vec<_>>(),
        ["entry", "no", "join", "yes"]
    );
    let entry_branch = blocks[0].terminator().unwrap();
    assert!(entry_branch.successor(0).unwrap().same_as(&blocks[3]));
    assert!(entry_branch.successor(1).unwrap().same_as(&blocks[1]));
    let phi = blocks[2].instructions().next().unwrap();
    assert!(phi.phi_incoming_block(0).unwrap().same_as(&blocks[3]));
    assert!(phi.phi_incoming_block(1).unwrap().same_as(&blocks[1]));
    assert_eq!(module.ir(), before);
}

#[test]
fn erases_a_block_and_repairs_all_outgoing_phi_edges() {
    let module = Context::default()
        .parse_ir(
            r#"
            define i32 @choose(i32 %key) {
            entry: br i1 true, label %join, label %dead
            dead:
              %previous = phi i32 [ 1, %entry ], [ %next, %dead ]
              %next = add i32 %previous, 1
              switch i32 %key, label %join [ i32 0, label %join
                                             i32 1, label %dead
                                             i32 2, label %cold ]
            join:
              %value = phi i32 [ 42, %entry ], [ %next, %dead ], [ %next, %dead ]
              ret i32 %value
            cold:
              %other = phi i32 [ %next, %dead ]
              ret i32 %other
            }
            "#,
        )
        .unwrap();
    let choose = module.find_function("choose").unwrap().unwrap();
    let blocks: Vec<_> = choose.blocks().collect();
    let [entry, dead, join, cold] = blocks.as_slice() else {
        panic!("expected four blocks");
    };
    let branch = entry.terminator().unwrap();
    let old_phi = join.instructions().next().unwrap();
    let dead_switch = dead.terminator().unwrap();
    let value = old_phi.operand(0).unwrap();
    let before = module.ir();
    let edit = module.edit().unwrap();

    assert!(edit.erase_block(entry).is_err());
    assert!(edit.erase_block(dead).is_err());
    edit.redirect_successor(&branch, 1, join, &[value.clone()])
        .unwrap();
    edit.erase_block(dead).unwrap();
    assert!(edit.erase_block(dead).is_err());
    assert!(edit.move_block_before(cold, dead).is_err());
    assert!(edit.redirect_successor(&dead_switch, 0, join, &[]).is_err());
    edit.replace_operand(&old_phi, 0, &value).unwrap();
    let edited = edit.finish().unwrap();

    let choose = edited.find_function("choose").unwrap().unwrap();
    let blocks: Vec<_> = choose.blocks().collect();
    assert_eq!(blocks.len(), 3);
    let phi = blocks[1].instructions().next().unwrap();
    assert_eq!(phi.operand_count(), 2);
    assert!(phi.phi_incoming_block(0).unwrap().same_as(&blocks[0]));
    assert!(phi.phi_incoming_block(1).unwrap().same_as(&blocks[0]));
    assert_eq!(blocks[2].instructions().next().unwrap().operand_count(), 0);
    assert_eq!(module.ir(), before);
    assert_eq!(old_phi.operand_count(), 3);
    assert_eq!(dead_switch.successor_count(), 4);
}

#[test]
fn rejects_erasing_a_block_with_external_address_or_ssa_uses() {
    let module = Context::default()
        .parse_ir(
            r#"
            @saved = global ptr blockaddress(@f, %addressed)
            define i32 @f(i32 %n) {
            entry: ret i32 0
            addressed: ret i32 1
            dead:
              %value = add i32 %n, 1
              br label %user
            user: ret i32 %value
            }
            "#,
        )
        .unwrap();
    let f = module.find_function("f").unwrap().unwrap();
    let blocks: Vec<_> = f.blocks().collect();
    let edit = module.edit().unwrap();
    assert!(edit.erase_block(&blocks[1]).is_err());
    assert!(edit.erase_block(&blocks[2]).is_err());
    let edited = edit.finish().unwrap();
    assert_eq!(edited.ir(), module.ir());
}

#[test]
fn adds_instructions_and_blocks_to_an_independent_module_copy() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            target triple = "aarch64-unknown-linux-gnu"
            target datalayout = "e-p:64:64"
            define i32 @forward(i32 %value) {
            entry: br label %exit
            exit: ret i32 %value
            }
            "#,
        )
        .unwrap();
    let original_ir = module.ir();
    let copy = module.copy().unwrap();
    assert_eq!(copy.target_triple(), module.target_triple());
    assert_eq!(copy.data_layout(), module.data_layout());
    let f = copy.find_function("forward").unwrap().unwrap();
    let entry = f.blocks().next().unwrap();
    let branch = entry.terminator().unwrap();
    let argument = f.parameter(0).unwrap().unwrap();
    let alternate = f.append_block("alternate").unwrap();
    let builder = Builder::new(copy.context());
    builder.position_before(&branch).unwrap();
    let one = copy
        .context()
        .int_type(32)
        .unwrap()
        .const_int(&1.into())
        .unwrap();
    let original_one = context.int_type(32).unwrap().const_int(&1.into()).unwrap();
    assert!(
        builder
            .int_binop(IntOp::Add, &argument, &original_one, "invalid")
            .is_err()
    );
    let adjusted = builder
        .int_binop(IntOp::Add, &argument, &one, "adjusted")
        .unwrap();
    builder.position_at_end(&alternate).unwrap();
    builder.ret(Some(&adjusted)).unwrap();
    let edit = copy.edit().unwrap();
    edit.redirect_successor(&branch, 0, &alternate, &[])
        .unwrap();
    let edited = edit.finish().unwrap();
    assert!(edited.ir().contains("br label %alternate"));
    assert!(edited.ir().contains("ret i32 %adjusted"));
    assert_eq!(module.ir(), original_ir);
}

use zkcl_llvm_api::{Builder, Context};

#[test]
fn inserts_a_new_block_before_an_anchor() {
    let module = Context::default()
        .parse_ir("define void @f() { old: ret void } define void @g() { ret void }")
        .unwrap();
    let f = module.find_function("f").unwrap().unwrap();
    let g = module.find_function("g").unwrap().unwrap();
    let old = f.blocks().next().unwrap();
    let foreign = g.blocks().next().unwrap();

    assert!(f.insert_block_before(&foreign, "invalid").is_err());
    assert!(f.insert_block_before(&old, "bad\0name").is_err());
    assert_eq!(f.blocks().count(), 1);

    let entry = f.insert_block_before(&old, "entry").unwrap();
    let builder = Builder::new(module.context());
    builder.position_at_end(&entry).unwrap();
    builder.br(&old).unwrap();
    module.verify().unwrap();
    let blocks: Vec<_> = f.blocks().collect();
    assert!(blocks[0].same_as(&entry));
    assert!(blocks[1].same_as(&old));
}

#[test]
fn reinserts_a_marked_block_without_losing_phi_edges_or_addresses() {
    let module = Context::default()
        .parse_ir(
            r#"
            @saved = global ptr blockaddress(@choose, %yes)
            define i32 @choose(i1 %test) {
            entry: br i1 %test, label %yes, label %no
            yes: br label %join
            no: br label %join
            join:
              %result = phi i32 [ 1, %yes ], [ 2, %no ]
              ret i32 %result
            }
            "#,
        )
        .unwrap();
    let f = module.find_function("choose").unwrap().unwrap();
    let blocks: Vec<_> = f.blocks().collect();
    let [entry, yes, no, join] = blocks.as_slice() else {
        panic!("expected four blocks");
    };
    let original = module.ir();
    let edit = module.edit().unwrap();
    assert!(edit.detach_block(entry).is_err());
    assert!(edit.insert_block_after(yes, join).is_err());
    edit.detach_block(yes).unwrap();
    assert!(edit.detach_block(yes).is_err());
    assert!(edit.move_block_after(no, yes).is_err());
    assert!(edit.erase_block(yes).is_err());
    assert!(edit.insert_block_before(yes, entry).is_err());
    assert!(edit.finish().is_err());

    edit.insert_block_after(yes, join).unwrap();
    edit.detach_block(no).unwrap();
    edit.insert_block_before(no, join).unwrap();
    let edited = edit.finish().unwrap();
    let f = edited.find_function("choose").unwrap().unwrap();
    let blocks: Vec<_> = f.blocks().collect();
    assert_eq!(
        blocks
            .iter()
            .map(|block| block.name().unwrap())
            .collect::<Vec<_>>(),
        ["entry", "no", "join", "yes"]
    );
    let phi = blocks[2].first_instruction().unwrap();
    assert!(phi.phi_incoming_block(0).unwrap().same_as(&blocks[3]));
    assert!(phi.phi_incoming_block(1).unwrap().same_as(&blocks[1]));
    assert!(edited.ir().contains("blockaddress(@choose, %yes)"));
    assert_eq!(module.ir(), original);
}

#[test]
fn reinserts_and_moves_instructions_using_original_handles() {
    let module = Context::default()
        .parse_ir(
            r#"
            define i32 @compute(i32 %n) {
            entry:
              %original = add i32 %n, 1
              %spare = mul i32 %n, 2
              %later = sub i32 %n, 3
              br label %work
            work:
              %first = phi i32 [ %original, %entry ]
              %result = add i32 %first, 4
              ret i32 %result
            }
            "#,
        )
        .unwrap();
    let f = module.find_function("compute").unwrap().unwrap();
    let mut blocks = f.blocks();
    let entry = blocks.next().unwrap();
    let work = blocks.next().unwrap();
    let mut instructions = entry.instructions();
    let original = instructions.next().unwrap();
    let spare = instructions.next().unwrap();
    let later = instructions.next().unwrap();
    let phi = work.first_instruction().unwrap();
    let result = phi.next_instruction().unwrap();
    let ret = work.terminator().unwrap();

    let edit = module.edit().unwrap();
    edit.detach_instruction(&later).unwrap();
    assert!(edit.move_after(&spare, &later).is_err());
    assert!(edit.detach_instruction(&later).is_err());
    assert!(edit.erase_instruction(&later).is_err());
    assert!(edit.insert_instruction_before(&later, &phi).is_err());
    assert!(edit.insert_instruction_after(&later, &ret).is_err());
    assert!(edit.finish().is_err());

    edit.insert_instruction_after(&later, &result).unwrap();
    edit.move_after(&spare, &phi).unwrap();
    edit.detach_instruction(&original).unwrap();
    edit.insert_instruction_before(&original, &entry.terminator().unwrap())
        .unwrap();
    let edited = edit.finish().unwrap();
    let f = edited.find_function("compute").unwrap().unwrap();
    let work = f.blocks().nth(1).unwrap();
    assert_eq!(
        work.instructions()
            .map(|instruction| instruction.name().unwrap())
            .collect::<Vec<_>>(),
        ["first", "spare", "result", "later", ""]
    );
    assert_eq!(entry.instructions().count(), 4);
}

#[test]
fn preserves_musttail_return_adjacency() {
    let module = Context::default()
        .parse_ir(
            r#"
            declare void @target(i32)
            define void @forward(i32 %n) {
              %unused = add i32 %n, 1
              musttail call void @target(i32 %n)
              ret void
            }
            "#,
        )
        .unwrap();
    let f = module.find_function("forward").unwrap().unwrap();
    let block = f.blocks().next().unwrap();
    let mut instructions = block.instructions();
    let unused = instructions.next().unwrap();
    let call = instructions.next().unwrap();
    let edit = module.edit().unwrap();
    assert!(edit.move_after(&unused, &call).is_err());
    assert!(edit.detach_instruction(&call).is_err());
    assert_eq!(edit.finish().unwrap().ir(), module.ir());
}

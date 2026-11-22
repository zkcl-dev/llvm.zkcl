use zkcl_llvm_api::{Block, Context};

fn block_names(blocks: impl IntoIterator<Item = Block>) -> Vec<String> {
    blocks
        .into_iter()
        .map(|block| block.name().unwrap())
        .collect()
}

#[test]
fn cfg_queries_preserve_edge_multiplicity_and_traverse_only_reachable_blocks() {
    let module = Context::default()
        .parse_ir(
            r#"
            @address = global ptr blockaddress(@flow, %dead)
            declare void @prototype()

            define i32 @flow(i32 %selector, i1 %again) {
            entry:
              switch i32 %selector, label %loop [ i32 0, label %other
                                                 i32 1, label %loop ]
            loop:
              br i1 %again, label %loop, label %done
            other:
              br label %loop
            done:
              ret i32 %selector
            dead:
              br label %done
            }
            "#,
        )
        .unwrap();
    let function = module.find_function("flow").unwrap().unwrap();
    let blocks: Vec<_> = function.blocks().collect();
    let [entry, looping, _, done, dead] = blocks.as_slice() else {
        panic!("expected all blocks in function order");
    };

    assert_eq!(block_names(entry.successors()), ["loop", "other", "loop"]);
    assert_eq!(
        block_names(looping.predecessors()),
        ["entry", "entry", "loop", "other"]
    );
    assert_eq!(block_names(done.predecessors()), ["loop", "dead"]);
    assert_eq!(done.successors().count(), 0);
    assert_eq!(dead.predecessors().count(), 0);
    assert_eq!(
        block_names(function.reverse_postorder()),
        ["entry", "other", "loop", "done"]
    );

    let declaration = module.find_function("prototype").unwrap().unwrap();
    assert!(declaration.reverse_postorder().is_empty());
    let unfinished = declaration.append_block("unfinished").unwrap();
    assert_eq!(unfinished.successors().count(), 0);
    assert_eq!(block_names(declaration.reverse_postorder()), ["unfinished"]);
}

#[test]
fn arguments_retain_their_function_and_report_zero_based_indices() {
    let (first, second, constant) = {
        let module = Context::default()
            .parse_ir(
                r#"
                define i32 @args(i1 %flag, i32 %payload) {
                  %result = add i32 %payload, 1
                  ret i32 %result
                }
                "#,
            )
            .unwrap();
        let function = module.find_function("args").unwrap().unwrap();
        let first = function.parameter(0).unwrap().unwrap();
        let add = function
            .blocks()
            .next()
            .unwrap()
            .first_instruction()
            .unwrap();
        let second = add.operand(0).unwrap();
        assert!(second.argument_function().unwrap().same_as(&function));
        (first, second, add.operand(1).unwrap())
    };

    assert_eq!(first.argument_index(), Some(0));
    assert_eq!(second.argument_index(), Some(1));
    let function = first.argument_function().unwrap();
    assert_eq!(function.name().unwrap(), "args");
    assert!(second.argument_function().unwrap().same_as(&function));
    assert!(second.as_function().is_none());
    let function_value = function.address().unwrap();
    assert!(function_value.as_function().unwrap().same_as(&function));
    assert!(function_value.argument_function().is_none());
    assert_eq!(function_value.argument_index(), None);
    assert!(constant.argument_function().is_none());
    assert_eq!(constant.argument_index(), None);
}

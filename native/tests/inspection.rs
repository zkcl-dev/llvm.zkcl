use zkcl_llvm_api::{Context, TypeKind, ValueKind};

#[test]
fn navigates_duplicate_and_constant_uses_in_the_selected_module() {
    let aggregate_user = {
        let context = Context::default();
        let module = context
            .parse_ir(
                r#"
                @slot = global i32 0
                @slot_alias = alias i32, ptr @slot
                @root = global { ptr, i32 } { ptr @slot_alias, i32 7 }
                @opaque = global ptr no_cfi @run

                define i32 @run(i32 %x) {
                  %twice = add i32 %x, %x
                  %sum = add i32 %twice, 7
                  ret i32 %sum
                }
                "#,
            )
            .unwrap();
        let other = context
            .parse_ir("define i32 @other() prefix i32 7 { ret i32 7 }")
            .unwrap();
        let function = module.find_function("run").unwrap().unwrap();
        let block = function.blocks().next().unwrap();
        let mut instructions = block.instructions();
        let twice = instructions.next().unwrap();
        let sum = instructions.next().unwrap();
        let x = function.parameter(0).unwrap().unwrap();
        assert_eq!(x.kind(), Some(ValueKind::LLVMArgumentValueKind));
        assert!(x.as_instruction().is_none());
        assert_eq!(x.operand_count(), 0);
        assert!(x.operand_at(0).is_none());
        let uses = x.uses(None).unwrap();
        assert_eq!(uses.len(), 2);
        for (index, usage) in uses.iter().enumerate() {
            assert_eq!(usage.operand_index, index as u32);
            assert!(usage.user.as_instruction().unwrap().same_as(&twice));
            assert!(
                usage
                    .user
                    .operand_at(usage.operand_index)
                    .unwrap()
                    .same_as(&x)
            );
        }
        assert!(x.uses(Some(&other)).is_err());

        let constant = sum.operand(1).unwrap();
        assert_eq!(constant.kind(), Some(ValueKind::LLVMConstantIntValueKind));
        assert!(constant.uses(None).is_err());
        let other_function = other.find_function("other").unwrap().unwrap();
        let other_return = other_function
            .blocks()
            .next()
            .unwrap()
            .terminator()
            .unwrap();
        assert!(constant.same_as(&other_return.operand(0).unwrap()));
        let other_uses = constant.uses(Some(&other)).unwrap();
        assert_eq!(other_uses.len(), 2);
        assert!(other_uses.iter().any(|usage| {
            usage.operand_index == 1
                && usage
                    .user
                    .as_function()
                    .is_some_and(|function| function.same_as(&other_function))
        }));
        assert!(other_uses.iter().any(|usage| {
            usage.operand_index == 0
                && usage
                    .user
                    .as_instruction()
                    .is_some_and(|ret| ret.same_as(&other_return))
        }));
        let opaque = module
            .find_global("opaque")
            .unwrap()
            .unwrap()
            .initializer()
            .unwrap()
            .unwrap();
        assert!(opaque.is_constant());
        assert!(opaque.kind().is_none());
        assert!(opaque.as_instruction().is_none());
        let uses = constant.uses(Some(&module)).unwrap();
        assert_eq!(uses.len(), 2);
        assert!(uses.iter().any(|usage| {
            usage
                .user
                .as_instruction()
                .is_some_and(|instruction| instruction.same_as(&sum))
        }));
        let aggregate = uses
            .into_iter()
            .find(|usage| usage.user.kind() == Some(ValueKind::LLVMConstantStructValueKind))
            .unwrap();
        assert_eq!(aggregate.operand_index, 1);
        aggregate.user
    };

    let alias = aggregate_user
        .operand_at(0)
        .unwrap()
        .as_global_alias()
        .unwrap();
    assert_eq!(alias.name().unwrap(), "slot_alias");
    let slot = alias.target().as_global().unwrap();
    assert_eq!(slot.name().unwrap(), "slot");
    let uses = aggregate_user.uses(None).unwrap();
    assert_eq!(uses.len(), 1);
    assert_eq!(uses[0].user.as_global().unwrap().name().unwrap(), "root");
    assert!(
        uses[0]
            .user
            .operand_at(uses[0].operand_index)
            .unwrap()
            .same_as(&aggregate_user)
    );
}

#[test]
fn inspects_typed_memory_and_call_operands() {
    let module = Context::default()
        .parse_ir(
            r#"
            declare i32 @callee(i32, ...)
            declare i32 @personality(...)

            define i32 @inspect(ptr %fp, i64 %count, i32 %x) personality ptr @personality {
            entry:
              %slot = alloca [4 x i32], i64 %count, align 16
              %ptr = getelementptr inbounds [4 x i32], ptr %slot, i64 0, i64 2
              store volatile i32 %x, ptr %ptr, align 4
              %read = load volatile i32, ptr %ptr, align 4
              %same = icmp samesign slt i32 %read, %x
              %a = call i32 (i32, ...) @callee(i32 %read, i32 %x) [ "deopt"(i1 %same) ]
              %b = invoke i32 %fp(i32 %a) to label %done unwind label %exception
            done:
              ret i32 %b
            exception:
              %pad = landingpad { ptr, i32 } cleanup
              resume { ptr, i32 } %pad
            }
            "#,
        )
        .unwrap();
    let function = module.find_function("inspect").unwrap().unwrap();
    let personality = function.personality().unwrap().unwrap();
    let personality_uses = personality.uses(None).unwrap();
    assert_eq!(personality_uses.len(), 1);
    assert_eq!(personality_uses[0].operand_index, 0);
    assert!(
        personality_uses[0]
            .user
            .as_function()
            .unwrap()
            .same_as(&function)
    );
    let mut blocks = function.blocks();
    let instructions: Vec<_> = blocks.next().unwrap().instructions().collect();
    let [alloca, gep, store, load, compare, call, invoke] = instructions.as_slice() else {
        panic!("expected all entry instructions");
    };

    assert_eq!(alloca.allocated_type().unwrap().ir(), "[4 x i32]");
    assert_eq!(alloca.alloca_count().unwrap().name().unwrap(), "count");
    assert_eq!(gep.gep_source_type().unwrap().ir(), "[4 x i32]");
    assert_eq!(gep.gep_in_bounds(), Some(true));
    assert_eq!(gep.gep_pointer().unwrap().name().unwrap(), "slot");
    assert_eq!(gep.gep_index_count(), Some(2));
    assert_eq!(
        gep.gep_index(1).unwrap().constant_integer(false).unwrap(),
        2.into()
    );
    assert!(gep.gep_index(u32::MAX).is_none());
    assert!(load.gep_source_type().is_none());

    assert_eq!(load.is_volatile(), Some(true));
    assert_eq!(store.is_volatile(), Some(true));
    assert_eq!(compare.icmp_same_sign(), Some(true));
    assert_eq!(store.stored_value().unwrap().name().unwrap(), "x");
    let pointer = gep.result().unwrap();
    assert!(load.load_pointer().unwrap().same_as(&pointer));
    assert!(store.store_pointer().unwrap().same_as(&pointer));
    assert!(load.store_pointer().is_none());

    let target = call.call_target().unwrap().as_function().unwrap();
    assert_eq!(target.name().unwrap(), "callee");
    assert_eq!(call.call_signature().unwrap().ir(), "i32 (i32, ...)");
    assert_eq!(call.call_argument_count(), Some(2));
    assert_eq!(call.call_argument(0).unwrap().name().unwrap(), "read");
    assert_eq!(call.call_argument(1).unwrap().name().unwrap(), "x");
    assert!(call.call_argument(2).is_none());
    assert!(call.operand_count() > 3);
    assert!(store.call_target().is_none());
    assert!(store.call_signature().is_none());
    assert!(store.call_argument_count().is_none());

    assert_eq!(invoke.call_target().unwrap().name().unwrap(), "fp");
    assert_eq!(
        invoke.call_signature().unwrap().kind(),
        TypeKind::LLVMFunctionTypeKind
    );
    assert_eq!(invoke.call_argument_count(), Some(1));
    assert_eq!(invoke.call_argument(0).unwrap().name().unwrap(), "a");
    assert!(invoke.call_argument(1).is_none());
    let normal = blocks.next().unwrap();
    assert!((0..invoke.operand_count()).any(|index| {
        invoke
            .operand(index)
            .and_then(|operand| operand.as_block())
            .is_some_and(|block| block.same_as(&normal))
    }));
}

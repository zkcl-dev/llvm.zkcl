use zkcl_llvm_api::{Builder, Context, IntOp};

#[test]
fn positions_before_existing_instructions_and_keeps_phis_grouped() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            define i32 @step(i32 %x) {
            entry:
              br label %done
            done:
              %incoming = phi i32 [ %x, %entry ]
              %result = add i32 %incoming, 1
              ret i32 %result
            }
            "#,
        )
        .unwrap();
    let function = module.find_function("step").unwrap().unwrap();
    let mut blocks = function.blocks();
    let entry = blocks.next().unwrap();
    let done = blocks.next().unwrap();
    let mut instructions = done.instructions();
    let original_phi = instructions.next().unwrap();
    let original_add = instructions.next().unwrap();
    let ret = instructions.next().unwrap();
    let x = function.parameter(0).unwrap().unwrap();
    let i32 = x.ty();

    let builder = Builder::new(&context);
    builder.position_before(&original_phi).unwrap();
    assert!(builder.not(&x, "invalid").is_err());
    let front = builder.phi(&i32, "front").unwrap();
    front.add_incoming(&x, &entry).unwrap();

    builder.position_before(&original_add).unwrap();
    let late_phi = builder.phi(&i32, "late_phi").unwrap();
    late_phi.add_incoming(&x, &entry).unwrap();
    let inverted = builder.not(&late_phi, "inverted").unwrap();
    let sum = builder
        .int_binop(IntOp::Add, &inverted, &late_phi, "sum")
        .unwrap();
    assert!(builder.phi(&i32, "invalid").is_err());
    assert!(builder.br(&done).is_err());
    original_add.replace_operand(0, &sum).unwrap();

    builder.position_at_end(&done).unwrap();
    assert!(builder.not(&x, "invalid").is_err());
    builder.position_before(&ret).unwrap();
    assert!(builder.ret(Some(&x)).is_err());
    assert!(builder.unreachable().is_err());

    let foreign = Context::default()
        .parse_ir("define void @foreign() { ret void }")
        .unwrap()
        .find_function("foreign")
        .unwrap()
        .unwrap()
        .blocks()
        .next()
        .unwrap()
        .terminator()
        .unwrap();
    assert!(builder.position_before(&foreign).is_err());
    builder.not(&x, "before_ret").unwrap();
    builder.clear_position();
    assert!(builder.not(&x, "invalid").is_err());

    module.verify().unwrap();
    let ir = module.ir();
    assert!(!ir.contains("%invalid"));
    let mut previous = 0;
    for text in [
        "%front = phi",
        "%incoming = phi",
        "%late_phi = phi",
        "%inverted = xor",
        "%sum = add",
        "%result = add",
        "%before_ret = xor",
        "ret i32 %result",
    ] {
        let position = ir.find(text).unwrap();
        assert!(position > previous, "unexpected position of {text}\n{ir}");
        previous = position;
    }
}

#[test]
fn preserves_musttail_and_exception_pad_placement() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            declare i32 @target(i32)
            declare i32 @personality(...)

            define i32 @tail(i32 %x) {
              %result = musttail call i32 @target(i32 %x)
              ret i32 %result
            }

            define i32 @invoke(i32 %x) personality ptr @personality {
            entry:
              %result = invoke i32 @target(i32 %x) to label %done unwind label %failure
            done:
              ret i32 %result
            failure:
              %exception = landingpad { ptr, i32 } cleanup
              resume { ptr, i32 } %exception
            }
            "#,
        )
        .unwrap();
    let builder = Builder::new(&context);
    let tail = module.find_function("tail").unwrap().unwrap();
    let x = tail.parameter(0).unwrap().unwrap();
    let block = tail.blocks().next().unwrap();
    builder
        .position_before(&block.terminator().unwrap())
        .unwrap();
    assert!(builder.not(&x, "invalid").is_err());
    builder
        .position_before(&block.instructions().next().unwrap())
        .unwrap();
    builder.not(&x, "before_musttail").unwrap();

    let invoke = module.find_function("invoke").unwrap().unwrap();
    let x = invoke.parameter(0).unwrap().unwrap();
    let entry = invoke.blocks().next().unwrap();
    let block = invoke.blocks().nth(2).unwrap();
    builder
        .position_before(&block.instructions().next().unwrap())
        .unwrap();
    assert!(builder.not(&x, "invalid").is_err());
    let phi = builder.phi(&x.ty(), "before_pad").unwrap();
    phi.add_incoming(&x, &entry).unwrap();
    builder
        .position_before(&block.terminator().unwrap())
        .unwrap();
    builder.not(&phi, "before_resume").unwrap();

    module.verify().unwrap();
}

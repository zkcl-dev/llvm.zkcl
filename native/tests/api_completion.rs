use zkcl_llvm_api::{
    AtomicOrdering, AtomicRmwOp, Builder, Context, InlineAsmDialect, InlineAsmOptions,
    OperandBundle, PassOptions, Result, SyncScope,
};

#[test]
fn inline_assembly_checks_constraints_and_round_trips_into_callable_ir() -> Result<()> {
    let context = Context::default();
    let i32 = context.int_type(32)?;
    let signature = i32.function(&[i32.clone()], false)?;
    let asm = signature.inline_asm(b"", b"=r,0", InlineAsmOptions::default())?;
    let info = asm.inline_asm_info().unwrap();
    assert_eq!(info.assembly, b"");
    assert_eq!(info.constraints, b"=r,0");
    assert_eq!(info.signature.ir(), signature.ir());
    let module = context.module("inline-assembly")?;
    let function = module.add_function("identity", &signature)?;
    let entry = function.append_block("entry")?;
    let builder = Builder::new(&context);
    builder.position_at_end(&entry)?;
    let result =
        builder.call_indirect(&signature, &asm, &[function.parameter(0)?.unwrap()], "v")?;
    builder.ret(result.as_ref())?;
    module.verify()?;
    let parsed = Context::default().parse_bitcode(&module.bitcode()?)?;
    let call = parsed
        .find_function("identity")?
        .unwrap()
        .blocks()
        .next()
        .unwrap()
        .instructions()
        .next()
        .unwrap();
    assert_eq!(
        call.call_target()
            .unwrap()
            .inline_asm_info()
            .unwrap()
            .constraints,
        b"=r,0"
    );
    assert!(
        signature
            .inline_asm(b"", b"=r", InlineAsmOptions::default())
            .is_err()
    );
    assert!(
        i32.function(&[], true)?
            .inline_asm(b"", b"=r", InlineAsmOptions::default())
            .is_err()
    );
    let bytes = signature.inline_asm(
        b"\0\xFF",
        b"=r,0",
        InlineAsmOptions {
            dialect: InlineAsmDialect::Intel,
            side_effects: true,
            ..Default::default()
        },
    )?;
    assert_eq!(bytes.inline_asm_info().unwrap().assembly, b"\0\xFF");
    assert!(bytes.inline_asm_info().unwrap().options.side_effects);
    Ok(())
}

#[test]
fn block_addresses_and_asm_goto_preserve_successor_lists() -> Result<()> {
    let context = Context::default();
    let signature = context.void_type().function(&[], false)?;
    let module = context.module("computed-branches")?;
    let function = module.add_function("computed", &signature)?;
    let entry = function.append_block("entry")?;
    let target = function.append_block("target")?;
    let address = target.address()?;
    assert!(address.block_address_target().unwrap().same_as(&target));
    assert!(entry.address().is_err());
    let builder = Builder::new(&context);
    builder.position_at_end(&entry)?;
    assert!(builder.indirect_br(&address, &[]).is_err());
    let branch = builder.indirect_br(&address, &[target.clone()])?;
    assert_eq!(branch.opcode(), "indirectbr");
    builder.position_at_end(&target)?;
    builder.ret(None)?;
    let function = module.add_function("asm_goto", &signature)?;
    let entry = function.append_block("entry")?;
    let normal = function.append_block("normal")?;
    let indirect = function.append_block("indirect")?;
    let asm = signature.inline_asm(
        b"",
        b"!i",
        InlineAsmOptions {
            side_effects: true,
            ..Default::default()
        },
    )?;
    builder.position_at_end(&entry)?;
    let branch = builder.call_br(&asm, &[], &normal, &[indirect.clone()], &[], "")?;
    assert_eq!(branch.opcode(), "callbr");
    assert!(branch.successor(0).unwrap().same_as(&normal));
    assert!(branch.successor(1).unwrap().same_as(&indirect));
    for block in [&normal, &indirect] {
        builder.position_at_end(block)?;
        builder.ret(None)?;
    }
    module.verify()?;
    Ok(())
}

#[test]
fn call_bundles_retain_exact_tags_and_values_after_bitcode_readback() -> Result<()> {
    let context = Context::default();
    let i32 = context.int_type(32)?;
    let signature = i32.function(&[i32.clone()], false)?;
    let module = context.module("bundles")?;
    let callee = module.add_function("callee", &signature)?;
    let function = module.add_function("caller", &signature)?;
    let entry = function.append_block("entry")?;
    let builder = Builder::new(&context);
    builder.position_at_end(&entry)?;
    let input = function.parameter(0)?.unwrap();
    let bundles = [
        OperandBundle::new(b"site\0\xFF", vec![input.clone()]),
        OperandBundle::new(b"deopt", vec![i32.const_int(&7.into())?]),
    ];
    let call = builder.call_instruction_with_bundles(&callee, &[input.clone()], &bundles, "v")?;
    assert_eq!(call.call_argument_count(), Some(1));
    assert_eq!(call.call_operand_bundle_count(), Some(2));
    let recovered = call.call_operand_bundle(0).unwrap();
    assert_eq!(recovered.tag(), b"site\0\xFF");
    assert!(recovered.operands()[0].same_as(&input));
    builder.ret(call.result().as_ref())?;
    module.verify()?;
    let parsed = Context::default().parse_bitcode(&module.bitcode()?)?;
    let call = parsed
        .find_function("caller")?
        .unwrap()
        .blocks()
        .next()
        .unwrap()
        .instructions()
        .next()
        .unwrap();
    assert_eq!(call.call_operand_bundle(0).unwrap().tag(), b"site\0\xFF");
    let other = context
        .module("foreign")?
        .add_function("foreign", &signature)?;
    let builder = Builder::new(&context);
    builder.position_at_end(&other.append_block("entry")?)?;
    let before = other.module().ir();
    assert!(
        builder
            .call_indirect_instruction_with_bundles(
                &signature,
                &other.address()?,
                &[other.parameter(0)?.unwrap()],
                &bundles,
                ""
            )
            .is_err()
    );
    assert_eq!(other.module().ir(), before);
    Ok(())
}

#[test]
fn parsed_atomics_support_validated_property_updates() -> Result<()> {
    let module = Context::default().parse_ir(
        r#"
        define i32 @atomic(ptr %p) {
        entry:
          %load = load atomic volatile i32, ptr %p acquire, align 4
          %old = atomicrmw add ptr %p, i32 1 monotonic, align 4
          %pair = cmpxchg ptr %p, i32 1, i32 2 acq_rel acquire, align 4
          fence acquire
          ret i32 %old
        }
    "#,
    )?;
    let function = module.find_function("atomic")?.unwrap();
    let instructions: Vec<_> = function.blocks().next().unwrap().instructions().collect();
    let [load, rmw, cmpxchg, fence, ret] = instructions.as_slice() else {
        panic!("fixture")
    };
    assert_eq!(load.memory_ordering(), Some(AtomicOrdering::Acquire));
    load.set_memory_ordering(Some(AtomicOrdering::SequentiallyConsistent))?;
    load.set_atomic_sync_scope(SyncScope::SingleThread)?;
    assert_eq!(load.atomic_sync_scope_id(), Some(0));
    load.set_volatile(false)?;
    assert_eq!(load.is_volatile(), Some(false));
    rmw.set_atomic_rmw_operation(AtomicRmwOp::Sub)?;
    assert_eq!(rmw.atomic_rmw_operation(), Some(AtomicRmwOp::Sub));
    cmpxchg.set_cmpxchg_orderings(
        AtomicOrdering::Monotonic,
        AtomicOrdering::SequentiallyConsistent,
    )?;
    cmpxchg.set_cmpxchg_weak(true)?;
    assert_eq!(cmpxchg.cmpxchg_is_weak(), Some(true));
    for instruction in [rmw, cmpxchg] {
        instruction.set_alignment(8)?;
        assert_eq!(instruction.alignment()?, 8);
        assert!(instruction.set_alignment(0).is_err());
        assert!(instruction.set_alignment(3).is_err());
    }
    let before = module.ir();
    assert!(
        load.set_memory_ordering(Some(AtomicOrdering::Release))
            .is_err()
    );
    assert!(
        cmpxchg
            .set_cmpxchg_orderings(AtomicOrdering::Acquire, AtomicOrdering::Release)
            .is_err()
    );
    assert!(ret.set_atomic_sync_scope(SyncScope::System).is_err());
    assert!(fence.set_memory_ordering(None).is_err());
    assert_eq!(module.ir(), before);
    load.set_memory_ordering(None)?;
    assert_eq!(load.memory_ordering(), None);
    assert_eq!(load.atomic_sync_scope_id(), None);
    module.verify()?;
    Ok(())
}

#[test]
fn function_passes_modify_only_the_selected_definition_in_an_independent_copy() -> Result<()> {
    let context = Context::default();
    let module = context.parse_ir(
        r#"
        define i32 @first(i32 %x) { %v = add i32 %x, 0 ret i32 %v }
        define i32 @second(i32 %x) { %v = add i32 %x, 0 ret i32 %v }
    "#,
    )?;
    let first = module.find_function("first")?.unwrap();
    let before = module.ir();
    let optimized = first.optimize(
        "instcombine",
        None,
        PassOptions {
            verify_each: true,
            loop_unrolling: Some(false),
            ..Default::default()
        },
    )?;
    assert!(
        !optimized
            .find_function("first")?
            .unwrap()
            .blocks()
            .next()
            .unwrap()
            .instructions()
            .any(|inst| inst.opcode() == "add")
    );
    assert!(
        optimized
            .find_function("second")?
            .unwrap()
            .blocks()
            .next()
            .unwrap()
            .instructions()
            .any(|inst| inst.opcode() == "add")
    );
    assert_eq!(module.ir(), before);
    assert!(
        first
            .optimize("missing-pass", None, PassOptions::default())
            .is_err()
    );
    assert!(
        module
            .optimize_with_options(
                "default<O2>",
                None,
                PassOptions {
                    aa_pipeline: Some("missing-aa"),
                    ..Default::default()
                }
            )
            .is_err()
    );
    assert_eq!(module.ir(), before);
    Ok(())
}

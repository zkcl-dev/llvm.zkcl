use zkcl_llvm_api::{Context, FastMathFlags};

#[test]
fn replaces_flags_on_floating_casts_without_constant_folding() {
    let module = Context::default()
        .parse_ir(
            r#"
        define double @casts() {
          %small = fptrunc fast double 1.0 to float
          %wide = fpext fast float 1.0 to double
          ret double %wide
        }
    "#,
        )
        .unwrap();
    let function = module.find_function("casts").unwrap().unwrap();
    let casts: Vec<_> = function
        .blocks()
        .next()
        .unwrap()
        .instructions()
        .take(2)
        .collect();
    let edit = module.edit().unwrap();
    for cast in casts {
        edit.set_fast_math_flags(&cast, FastMathFlags::NoNaNs)
            .unwrap();
        edit.set_fast_math_flags(&cast, FastMathFlags::empty())
            .unwrap();
    }
    let edited = edit.finish().unwrap();
    let ir = edited.ir();
    assert!(
        ir.contains("%small = fptrunc double 1.000000e+00 to float"),
        "{ir}"
    );
    assert!(
        ir.contains("%wide = fpext float 1.000000e+00 to double"),
        "{ir}"
    );
}

#[test]
fn replaces_flags_exactly_without_folding_and_keeps_source_handles() {
    let module = Context::default()
        .parse_ir(
            r#"
        define double @math() {
          %a = fadd fast double 1.0, 2.0, !annotation !0
          %b = fsub fast double 1.0, 2.0
          %c = fmul fast double 1.0, 2.0
          %d = fdiv fast double 1.0, 2.0
          %e = frem fast double 1.0, 2.0
          %f = fneg fast double 1.0
          %g = fcmp fast false double 1.0, 2.0
          %h = select fast i1 true, double 1.0, double 1.0
          ret double %a
        }
        !0 = !{!"keep"}
    "#,
        )
        .unwrap();
    let function = module.find_function("math").unwrap().unwrap();
    let instructions: Vec<_> = function.blocks().next().unwrap().instructions().collect();
    let before = module.ir();
    let edit = module.edit().unwrap();
    for instruction in &instructions[..8] {
        edit.set_fast_math_flags(instruction, FastMathFlags::NoNaNs)
            .unwrap();
        // The same source handle must find the newly reconstructed instruction.
        edit.set_fast_math_flags(instruction, FastMathFlags::empty())
            .unwrap();
    }
    assert!(
        edit.set_fast_math_flags(&instructions[8], FastMathFlags::empty())
            .is_err()
    );
    let edited = edit.finish().unwrap();
    let f = edited.find_function("math").unwrap().unwrap();
    let edited_instructions: Vec<_> = f.blocks().next().unwrap().instructions().collect();
    assert_eq!(edited_instructions.len(), instructions.len());
    for instruction in &edited_instructions[..8] {
        assert_eq!(instruction.fast_math_flags(), Some(FastMathFlags::empty()));
    }
    assert!(edited.ir().contains("!annotation"));
    assert!(edited.ir().contains("ret double %a"));
    assert_eq!(module.ir(), before);
    assert_eq!(
        instructions[0].fast_math_flags(),
        Some(FastMathFlags::all())
    );
    assert!(
        edit.set_fast_math_flags(&instructions[0], FastMathFlags::empty())
            .is_err()
    );
}

#[test]
fn preserves_self_referential_phi_and_repeated_replacement() {
    let module = Context::default()
        .parse_ir(
            r#"
        define double @loop(i1 %again) {
        entry: br label %body
        body:
          %value = phi fast double [ 1.0, %entry ], [ %value, %body ]
          br i1 %again, label %body, label %exit
        exit: ret double %value
        }
    "#,
        )
        .unwrap();
    let f = module.find_function("loop").unwrap().unwrap();
    let phi = f.blocks().nth(1).unwrap().first_instruction().unwrap();
    let edit = module.edit().unwrap();
    edit.set_fast_math_flags(&phi, FastMathFlags::NoInfs)
        .unwrap();
    edit.replace_operand(&phi, 0, &phi.operand(0).unwrap())
        .unwrap();
    edit.set_fast_math_flags(&phi, FastMathFlags::empty())
        .unwrap();
    let edited = edit.finish().unwrap();
    assert!(
        edited
            .ir()
            .contains("phi double [ 1.000000e+00, %entry ], [ %value, %body ]")
    );
}

#[test]
fn preserves_call_bundles_attributes_convention_and_tail_kind() {
    let module = Context::default()
        .parse_ir(
            r#"
        declare fastcc double @callee(double)
        define fastcc double @caller(double %x) {
          %r = tail call fast fastcc noundef double @callee(double noundef %x)
              nounwind [ "tag"(double %x) ], !annotation !0
          ret double %r
        }
        !0 = !{!"keep"}
    "#,
        )
        .unwrap();
    let f = module.find_function("caller").unwrap().unwrap();
    let call = f.blocks().next().unwrap().first_instruction().unwrap();
    let edit = module.edit().unwrap();
    edit.set_fast_math_flags(&call, FastMathFlags::NoNaNs)
        .unwrap();
    let edited = edit.finish().unwrap();
    let ir = edited.ir();
    assert!(
        ir.contains("tail call nnan fastcc noundef double @callee(double noundef %x)"),
        "{ir}"
    );
    assert!(ir.contains("[ \"tag\"(double %x) ]"), "{ir}");
    assert!(ir.contains("nounwind"));
    assert!(ir.contains("!annotation"));
}

#[test]
fn rejects_invoke_and_callbr_because_llvm21_excludes_them_from_fp_math_operators() {
    let module = Context::default()
        .parse_ir(
            r#"
        declare i32 @personality(...)
        declare fastcc double @callee(double)
        define double @invoke_test(double %x) personality ptr @personality {
        entry:
          %r = invoke fastcc noundef double @callee(double noundef %x)
              [ "tag"(double %x) ] to label %normal unwind label %exception
        normal: ret double %r
        exception:
          %pad = landingpad { ptr, i32 } cleanup
          resume { ptr, i32 } %pad
        }
        define double @callbr_test(double %x) {
        entry:
          %r = callbr noundef double asm sideeffect "", "=w,w,!i"(double noundef %x)
              [ "tag"(double %x) ] to label %normal [label %indirect]
        normal: ret double %r
        indirect: ret double 0.0
        }
    "#,
        )
        .unwrap();
    let before = module.ir();
    let edit = module.edit().unwrap();
    for name in ["invoke_test", "callbr_test"] {
        let f = module.find_function(name).unwrap().unwrap();
        let instruction = f.blocks().next().unwrap().first_instruction().unwrap();
        let error = edit
            .set_fast_math_flags(&instruction, FastMathFlags::NoNaNs)
            .unwrap_err();
        assert!(error.to_string().contains("LLVM 21 does not support"));
    }
    let edited = edit.finish().unwrap();
    let ir = edited.ir();
    assert_eq!(ir, before);
    assert!(ir.contains("invoke fastcc noundef double @callee(double noundef %x)"));
    assert!(ir.contains("callbr noundef double asm sideeffect"));
    assert!(ir.contains("to label %normal unwind label %exception"));
    assert!(ir.contains("to label %normal [label %indirect]"));
    for name in ["invoke_test", "callbr_test"] {
        let f = edited.find_function(name).unwrap().unwrap();
        let instruction = f.blocks().next().unwrap().first_instruction().unwrap();
        assert_eq!(instruction.call_operand_bundle_count(), Some(1));
        assert_eq!(instruction.call_operand_bundle(0).unwrap().tag(), b"tag");
    }
}

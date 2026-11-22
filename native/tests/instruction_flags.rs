use zkcl_llvm_api::{Context, GepFlags, IntFlags};

#[test]
fn replaces_instruction_flags_without_replacing_ssa_values() {
    let module = Context::default()
        .parse_ir(
            r#"
            define i16 @flagged(ptr %base, i64 %index, i32 %x) {
            entry:
              %addr = getelementptr i32, ptr %base, i64 %index
              %loaded = load i32, ptr %addr
              %sum = add nsw i32 %x, %loaded
              %div = udiv exact i32 %sum, 2
              %small = trunc nuw i32 %div to i16
              %same = icmp samesign slt i16 %small, 0
              %result = select i1 %same, i16 %small, i16 0
              ret i16 %result
            }
            "#,
        )
        .unwrap();
    let function = module.find_function("flagged").unwrap().unwrap();
    let entry = function.blocks().next().unwrap();
    let mut instructions = entry.instructions();
    let gep = instructions.next().unwrap();
    let load = instructions.next().unwrap();
    let sum = instructions.next().unwrap();
    let div = instructions.next().unwrap();
    let trunc = instructions.next().unwrap();
    let icmp = instructions.next().unwrap();

    let address = gep.result().unwrap();
    assert_eq!(gep.gep_flags(), Some(GepFlags::default()));
    let in_bounds_and_nuw = GepFlags {
        in_bounds: true,
        no_unsigned_wrap: true,
        ..GepFlags::default()
    };
    gep.set_gep_flags(in_bounds_and_nuw).unwrap();
    assert_eq!(
        gep.gep_flags(),
        Some(GepFlags {
            no_unsigned_signed_wrap: true,
            ..in_bounds_and_nuw
        })
    );
    assert_eq!(gep.gep_in_bounds(), Some(true));
    let nusw = GepFlags {
        no_unsigned_signed_wrap: true,
        ..GepFlags::default()
    };
    gep.set_gep_flags(nusw).unwrap();
    assert_eq!(gep.gep_flags(), Some(nusw));
    assert_eq!(gep.gep_in_bounds(), Some(false));
    gep.set_gep_flags(GepFlags::default()).unwrap();
    assert_eq!(gep.gep_flags(), Some(GepFlags::default()));
    assert!(address.as_instruction().unwrap().same_as(&gep));
    assert!(load.load_pointer().unwrap().same_as(&address));
    assert_eq!(load.gep_flags(), None);
    assert!(load.set_gep_flags(nusw).is_err());

    let nsw = IntFlags {
        nsw: true,
        ..IntFlags::default()
    };
    let nuw = IntFlags {
        nuw: true,
        ..IntFlags::default()
    };
    let exact = IntFlags {
        exact: true,
        ..IntFlags::default()
    };
    assert_eq!(sum.int_flags(), Some(nsw));
    assert!(sum.set_int_flags(exact).is_err());
    assert_eq!(sum.int_flags(), Some(nsw));
    sum.set_int_flags(nuw).unwrap();
    assert_eq!(sum.int_flags(), Some(nuw));
    assert_eq!(trunc.int_flags(), Some(nuw));
    trunc.set_int_flags(nsw).unwrap();
    assert_eq!(trunc.int_flags(), Some(nsw));
    assert_eq!(div.int_flags(), Some(exact));
    assert!(div.set_int_flags(nuw).is_err());
    assert_eq!(div.int_flags(), Some(exact));
    div.set_int_flags(IntFlags::default()).unwrap();
    assert_eq!(div.int_flags(), Some(IntFlags::default()));
    assert_eq!(load.int_flags(), None);
    assert!(load.set_int_flags(IntFlags::default()).is_err());

    assert_eq!(icmp.icmp_same_sign(), Some(true));
    icmp.set_icmp_same_sign(false).unwrap();
    assert_eq!(icmp.icmp_same_sign(), Some(false));
    icmp.set_icmp_same_sign(true).unwrap();
    assert_eq!(icmp.icmp_same_sign(), Some(true));
    assert!(sum.set_icmp_same_sign(true).is_err());
    module.verify().unwrap();
}

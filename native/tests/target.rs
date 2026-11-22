use std::ffi::c_void;
use std::mem::{align_of, offset_of, size_of};

use zkcl_llvm_api::{
    CodeModel, Context, DataLayout, Jit, OptimizationLevel, RelocMode, TargetMachine, TargetOptions,
};

const POINTER_STRIDE: &str = "define i64 @pointer_stride() {
    ret i64 ptrtoint (ptr getelementptr (ptr, ptr null, i32 1) to i64)
}";

fn host() -> TargetMachine {
    TargetMachine::new(&TargetMachine::host_triple(), OptimizationLevel::Default).unwrap()
}

#[repr(C)]
struct Record {
    tag: u8,
    payload: u64,
    next: *mut c_void,
}

#[test]
fn target_queries_match_the_platform_c_record_layout() {
    let context = Context::default();
    let target = host();
    let fields = [
        context.int_type(8).unwrap(),
        context.int_type(64).unwrap(),
        context.pointer_type(),
    ];
    let record = context.struct_type(&fields, false).unwrap();
    assert_eq!(
        target.abi_size(&record).unwrap(),
        size_of::<Record>() as u64
    );
    assert_eq!(
        target.abi_alignment(&record).unwrap(),
        align_of::<Record>() as u32
    );
    assert_eq!(
        target.field_offset(&record, 0).unwrap(),
        offset_of!(Record, tag) as u64
    );
    assert_eq!(
        target.field_offset(&record, 1).unwrap(),
        offset_of!(Record, payload) as u64
    );
    assert_eq!(
        target.field_offset(&record, 2).unwrap(),
        offset_of!(Record, next) as u64
    );

    let packed = context.struct_type(&fields, true).unwrap();
    assert_eq!(target.abi_alignment(&packed).unwrap(), 1);
    assert_eq!(target.field_offset(&packed, 1).unwrap(), 1);
    assert!(target.field_offset(&record, 3).is_err());
    assert!(
        target
            .field_offset(&context.struct_type(&[], false).unwrap(), 0)
            .is_err()
    );
    assert!(target.abi_size(&context.void_type()).is_err());

    let module = context
        .parse_ir(
            "declare void @vectors(<vscale x 4 x i32>, { <vscale x 4 x i32>, <vscale x 4 x i32> })",
        )
        .unwrap();
    let vectors = module.find_function("vectors").unwrap().unwrap();
    let layout = target.target_data();
    for index in 0..2 {
        let scalable = vectors.parameter(index).unwrap().unwrap().ty();
        for query in [
            layout.size_in_bits(&scalable),
            layout.store_size(&scalable),
            layout.alloc_size(&scalable),
            layout.field_offset(&scalable, 0),
        ] {
            assert!(query.unwrap_err().to_string().contains("fixed-size"));
        }
    }
}

#[test]
fn data_layout_distinguishes_sizes_alignments_and_pointer_index_widths() {
    let context = Context::default();
    let layout = DataLayout::parse("E-p:64:64:64:32-p5:17:32:32:13-i24:32:64-ni:5:7").unwrap();
    let i24 = context.int_type(24).unwrap();
    assert!(!layout.is_little_endian());
    assert_eq!(layout.pointer_size_bits(0), 64);
    assert_eq!(layout.pointer_index_size_bits(0), 32);
    assert_eq!(layout.pointer_size_bits(5), 17);
    assert_eq!(layout.pointer_index_size_bits(5), 13);
    assert_eq!(
        layout
            .size_in_bits(&context.pointer_type_in(5).unwrap())
            .unwrap(),
        17
    );
    assert_eq!(layout.pointer_size_bits(6), 64);
    assert_eq!(layout.pointer_index_size_bits(6), 32);
    assert!(layout.is_non_integral_pointer(5));
    assert!(layout.is_non_integral_pointer(7));
    assert!(!layout.is_non_integral_pointer(6));
    assert_eq!(layout.size_in_bits(&i24).unwrap(), 24);
    assert_eq!(layout.store_size(&i24).unwrap(), 3);
    assert_eq!(layout.alloc_size(&i24).unwrap(), 4);
    assert_eq!(layout.abi_alignment(&i24).unwrap(), 4);
    assert_eq!(layout.preferred_alignment(&i24).unwrap(), 8);
    let record = context
        .struct_type(&[context.int_type(8).unwrap(), i24], false)
        .unwrap();
    assert_eq!(layout.field_offset(&record, 1).unwrap(), 4);
    assert!(layout.field_offset(&record, 2).is_err());
}

#[test]
fn data_layout_snapshots_survive_their_sources() {
    let context = Context::default();
    let module = context.module("layout").unwrap();
    let before = module.target_data();
    assert_eq!(before.text(), "");
    assert!(before.is_little_endian());
    assert_eq!(before.pointer_index_size_bits(0), 64);

    let target = TargetMachine::new("i686-unknown-linux-gnu", OptimizationLevel::Default).unwrap();
    target.configure_module(&module).unwrap();
    let from_target = target.target_data();
    let from_live = module.target_data();
    let from_parsed = context.parse_ir(&module.ir()).unwrap().target_data();
    drop(target);
    drop(module);
    for layout in [from_target, from_live, from_parsed] {
        assert_eq!(layout.pointer_size_bits(0), 32);
        assert_eq!(layout.pointer_index_size_bits(0), 32);
        assert_eq!(layout.alloc_size(&context.pointer_type()).unwrap(), 4);
    }
    assert_eq!(before.pointer_size_bits(0), 64);
}

#[test]
fn data_layout_text_reports_invalid_llvm_grammar() {
    for malformed in [
        "e-p:64:3",
        "e-p:32:32:32:64",
        "e\0-i8:8",
        "e\"\n; sneaky",
        r"e\22",
    ] {
        assert!(
            DataLayout::parse(malformed).is_err(),
            "accepted {malformed:?}"
        );
    }
    let default = DataLayout::parse("").unwrap();
    assert!(default.is_little_endian());
    assert_eq!(default.pointer_size_bits(0), 64);
}

#[test]
fn compatible_layouts_allow_reordered_and_overridden_specs() {
    let context = Context::default();
    let target = host();
    let reordered = target
        .data_layout()
        .split('-')
        .rev()
        .collect::<Vec<_>>()
        .join("-");
    let reordered = format!("p:32:32-p0:64:64-{reordered}");
    let module = context
        .parse_ir(&format!(
            "target datalayout = \"{reordered}\"\n{POINTER_STRIDE}"
        ))
        .unwrap();
    target.configure_module(&module).unwrap();
    assert_eq!(
        module.target_triple().as_deref(),
        Some(target.triple().as_str())
    );
    assert_eq!(module.data_layout().as_deref(), Some(reordered.as_str()));
    assert!(!module.emit_object(None).unwrap().is_empty());
    assert_eq!(module.data_layout().as_deref(), Some(reordered.as_str()));

    let other_triple = context
        .parse_ir(&format!(
            "target triple = \"wasm32-unknown-unknown\"\n{POINTER_STRIDE}"
        ))
        .unwrap();
    assert!(
        target
            .configure_module(&other_triple)
            .unwrap_err()
            .to_string()
            .contains("module target")
    );
    assert!(other_triple.data_layout().is_none());
}

#[test]
fn codegen_rejects_a_conflicting_layout_after_target_dependent_folding() {
    let context = Context::default();
    let target = host();
    let host_size = target.abi_size(&context.pointer_type()).unwrap();
    let other_size = if host_size == 4 { 8 } else { 4 };
    let explicit = format!("e-p:{0}:{0}", other_size * 8);
    let original = context
        .parse_ir(&format!(
            "target datalayout = \"{explicit}\"\n{POINTER_STRIDE}"
        ))
        .unwrap();
    let folded = original.optimize("default<O2>", None).unwrap();
    assert!(folded.ir().contains(&format!("ret i64 {other_size}")));
    assert_eq!(folded.data_layout().as_deref(), Some(explicit.as_str()));
    for error in [
        target.configure_module(&folded).unwrap_err(),
        target.object(&folded).unwrap_err(),
        target.assembly(&folded).unwrap_err(),
        original
            .optimize("default<O2>", Some(&target))
            .err()
            .unwrap(),
    ] {
        assert!(error.to_string().contains("module data layout"), "{error}");
    }
    assert!(folded.target_triple().is_none());

    let unassigned = context.parse_ir(POINTER_STRIDE).unwrap();
    let optimized = unassigned.optimize("default<O2>", None).unwrap();
    assert!(optimized.ir().contains(&format!("ret i64 {host_size}")));
    assert_eq!(
        optimized.data_layout().as_deref(),
        Some(target.data_layout().as_str())
    );
    assert!(unassigned.data_layout().is_none());
}

#[test]
fn a_jit_target_is_retained_by_the_snapshot() {
    let jit = Jit::new().unwrap();
    let module = Context::default().parse_ir(POINTER_STRIDE).unwrap();
    jit.configure_module(&module).unwrap();
    let copy = module.optimize("default<O2>", None).unwrap();
    assert_eq!(copy.target_triple().unwrap(), jit.triple().unwrap());
    assert_eq!(copy.data_layout().unwrap(), jit.data_layout().unwrap());
    // SAFETY: this pure function only computes the LLVM pointer stride.
    unsafe { jit.add_module(&copy) }.unwrap();
    let symbol = jit.lookup("pointer_stride").unwrap();
    // SAFETY: the symbol retains `jit` and the definition is exactly i64 () with the C ABI.
    let stride = unsafe {
        std::mem::transmute::<*mut c_void, extern "C" fn() -> u64>(symbol.address().as_ptr())
    };
    assert_eq!(stride(), size_of::<*mut c_void>() as u64);
}

#[test]
fn codegen_uses_the_explicit_features_relocation_and_code_model() {
    let triple = "x86_64-unknown-linux-gnu";
    let baseline = TargetMachine::new(triple, OptimizationLevel::Default).unwrap();
    let explicit = TargetMachine::with_options(
        triple,
        TargetOptions {
            cpu: "x86-64",
            features: "+popcnt",
            relocation: RelocMode::Static,
            code_model: CodeModel::Large,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(explicit.cpu(), "x86-64");
    assert_eq!(explicit.features(), "+popcnt");
    let module = Context::default()
        .parse_ir(
            "@external_word = external global i64
             declare i64 @llvm.ctpop.i64(i64)
             define i64 @popcount_word() {
                %word = load i64, ptr @external_word
                %count = call i64 @llvm.ctpop.i64(i64 %word)
                ret i64 %count
             }",
        )
        .unwrap();
    let baseline_assembly = baseline.assembly(&module).unwrap();
    let explicit_assembly = explicit.assembly(&module).unwrap();
    assert!(baseline_assembly.contains("external_word@GOTPCREL"));
    assert!(!baseline_assembly.contains("popcntq"));
    assert!(explicit_assembly.contains("popcntq"));
    assert!(explicit_assembly.contains("movabsq"));
    assert!(!explicit_assembly.contains("external_word@GOTPCREL"));
}

#[test]
fn embedded_arm_relocation_models_reach_codegen() {
    let module = Context::default()
        .parse_ir(
            "@writable = global i32 0
             @readonly = constant i32 42
             define ptr @w() { ret ptr @writable }
             define ptr @r() { ret ptr @readonly }",
        )
        .unwrap();
    for (relocation, ropi, rwpi) in [
        (RelocMode::Ropi, true, false),
        (RelocMode::Rwpi, false, true),
        (RelocMode::RopiRwpi, true, true),
    ] {
        let target = TargetMachine::with_options(
            "thumbv7em-none-eabi",
            TargetOptions {
                relocation,
                ..Default::default()
            },
        )
        .unwrap();
        let assembly = target.assembly(&module).unwrap();
        assert_eq!(assembly.contains("readonly-(.LPC"), ropi);
        assert_eq!(assembly.contains("writable(sbrel)"), rwpi);
        assert_eq!(&target.object(&module).unwrap()[..6], b"\x7fELF\x01\x01");
    }
}

#[test]
fn explicitly_selected_riscv_abi_reaches_the_object() {
    let module = Context::default()
        .parse_ir("define double @keep(double %value) { ret double %value }")
        .unwrap();
    for (abi, expected_float_abi) in [("lp64", 0), ("lp64d", 4)] {
        let machine = TargetMachine::with_options(
            "riscv64-unknown-linux-gnu",
            TargetOptions {
                features: "+d",
                abi,
                ..Default::default()
            },
        )
        .unwrap();
        let object = machine.object(&module).unwrap();
        assert_eq!(&object[..6], b"\x7fELF\x02\x01");
        let flags = u32::from_le_bytes(object[48..52].try_into().unwrap());
        assert_eq!(flags & 0x6, expected_float_abi);
    }
}

#[test]
fn explicit_riscv_abi_rejects_a_conflicting_module_flag() {
    let machine = TargetMachine::with_options(
        "riscv64-unknown-linux-gnu",
        TargetOptions {
            features: "+d",
            abi: "lp64d",
            ..Default::default()
        },
    )
    .unwrap();
    let module = Context::default()
        .parse_ir(
            "define double @keep(double %value) { ret double %value }
             !llvm.module.flags = !{!0}
             !0 = !{i32 1, !\"target-abi\", !\"lp64\"}",
        )
        .unwrap();
    for error in [
        machine.configure_module(&module).unwrap_err(),
        machine.object(&module).unwrap_err(),
    ] {
        assert!(error.to_string().contains("module target ABI"), "{error}");
    }
    assert!(module.target_triple().is_none());
    assert!(module.data_layout().is_none());
}

#[test]
fn target_option_errors_are_reported_before_codegen() {
    let triple = "x86_64-unknown-linux-gnu";
    let defaults = TargetOptions::default();
    for (triple, options, expected) in [
        (
            triple,
            TargetOptions {
                cpu: "contains whitespace",
                ..defaults
            },
            "target CPU",
        ),
        (
            triple,
            TargetOptions {
                cpu: "i386",
                ..defaults
            },
            "conflict with target",
        ),
        (
            triple,
            TargetOptions {
                features: "popcnt",
                ..defaults
            },
            "target feature",
        ),
        (
            triple,
            TargetOptions {
                features: "+unknown feature",
                ..defaults
            },
            "target feature",
        ),
        (
            triple,
            TargetOptions {
                features: "+popcnt,",
                ..defaults
            },
            "target feature",
        ),
        (
            triple,
            TargetOptions {
                cpu: "x86-64\0x",
                ..defaults
            },
            "embedded NUL",
        ),
        (
            triple,
            TargetOptions {
                abi: "sysv",
                ..defaults
            },
            "target ABI",
        ),
        (
            triple,
            TargetOptions {
                code_model: CodeModel::Tiny,
                ..defaults
            },
            "code model",
        ),
        (
            triple,
            TargetOptions {
                relocation: RelocMode::DynamicNoPic,
                ..defaults
            },
            "Mach-O",
        ),
        (
            "riscv64-unknown-linux-gnu",
            TargetOptions {
                abi: "lp64d",
                ..defaults
            },
            "target ABI",
        ),
        (
            "riscv64-unknown-linux-gnu",
            TargetOptions {
                features: "-64bit",
                ..defaults
            },
            "conflict with target",
        ),
    ] {
        let error = TargetMachine::with_options(triple, options).err().unwrap();
        assert!(error.to_string().contains(expected), "{error}");
    }
}

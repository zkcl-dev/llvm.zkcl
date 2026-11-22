use std::ffi::c_void;

use zkcl_llvm_api::{Context, Jit};

#[test]
fn links_separate_contexts_and_emits_the_linked_program() {
    let primary = Context::default()
        .parse_ir(
            "%Pair = type { i32, i32 }
             declare i32 @pair_sum(%Pair)
             define i32 @measure() {
               %sum = call i32 @pair_sum(%Pair { i32 19, i32 22 })
               ret i32 %sum
             }",
        )
        .unwrap();
    let library = Context::default()
        .parse_ir(
            "%Pair = type { i32, i32 }
             declare i32 @bias()
             define i32 @pair_sum(%Pair %pair) {
               %left = extractvalue %Pair %pair, 0
               %right = extractvalue %Pair %pair, 1
               %sum = add i32 %left, %right
               %bias = call i32 @bias()
               %biased = add i32 %sum, %bias
               ret i32 %biased
             }",
        )
        .unwrap();
    let bias = Context::default()
        .parse_ir("define i32 @bias() { ret i32 1 }")
        .unwrap();
    let declaration = primary.find_function("pair_sum").unwrap().unwrap();
    let definition = library.find_function("pair_sum").unwrap().unwrap();

    let jit = Jit::new().unwrap();
    jit.configure_module(&library).unwrap();
    let linked = primary.link(&[library.clone(), bias]).unwrap();
    assert_eq!(linked.target_triple(), library.target_triple());
    assert_eq!(linked.data_layout(), library.data_layout());
    assert_eq!(linked.ir().matches(" = type { i32, i32 }").count(), 1);
    assert!(declaration.blocks().next().is_none());
    assert_eq!(definition.blocks().count(), 1);
    assert!(primary.target_triple().is_none());

    let optimized = linked.optimize("default<O2>", None).unwrap();
    let object = optimized.emit_object(None).unwrap();
    // SAFETY: the object is generated from these verified arithmetic functions.
    unsafe { jit.add_object(&object) }.unwrap();
    let symbol = jit.lookup("measure").unwrap();
    // SAFETY: the symbol retains the session, and measure has this exact ABI.
    let measure = unsafe {
        std::mem::transmute::<*mut c_void, extern "C" fn() -> i32>(symbol.address().as_ptr())
    };
    assert_eq!(measure(), 42);
}

#[test]
fn a_late_linker_error_preserves_the_inputs_and_they_can_be_reused() {
    let context = Context::default();
    let primary = context.parse_ir("declare i32 @answer()").unwrap();
    let library = context
        .parse_ir("define i32 @answer() { ret i32 42 }")
        .unwrap();
    let duplicate = context
        .parse_ir("define i32 @answer() { ret i32 41 }")
        .unwrap();
    let expected = [primary.ir(), library.ir(), duplicate.ir()];
    let error = primary
        .link(&[library.clone(), duplicate.clone()])
        .err()
        .unwrap();
    assert!(error.to_string().contains("multiply defined"), "{error}");
    assert!(error.to_string().contains("answer"), "{error}");
    assert_eq!([primary.ir(), library.ir(), duplicate.ir()], expected);
    let linked = primary.link(&[library]).unwrap();
    assert!(linked.ir().contains("ret i32 42"));
}

#[test]
fn links_equivalent_layouts_and_rejects_incompatible_targets() {
    let context = Context::default();
    let input = |triple, layout| {
        context
            .parse_ir(&format!(
                "target triple = \"{triple}\"\ntarget datalayout = \"{layout}\""
            ))
            .unwrap()
    };
    let triple = "x86_64-unknown-linux-gnu";
    let primary = input(triple, "e-p:64:64-i128:128");
    primary
        .link(&[input(triple, "e-i128:128-p:64:64")])
        .unwrap();
    let layout_error = primary
        .link(&[input(triple, "e-p:32:32-i128:128")])
        .err()
        .unwrap();
    assert!(layout_error.to_string().contains("data layouts"));
    let triple_error = primary
        .link(&[input("aarch64-unknown-linux-gnu", "e-p:64:64")])
        .err()
        .unwrap();
    assert!(triple_error.to_string().contains("target triples"));
}

#[test]
fn linking_preserves_llvm_target_compatibility_rules() {
    let context = Context::default();
    let module = |triple| {
        context
            .parse_ir(&format!("target triple = \"{triple}\""))
            .unwrap()
    };
    let macos = module("arm64-apple-macosx14.0.0");
    let newer_macos = module("aarch64-apple-macosx15.0.0");
    let linked = macos.link(std::slice::from_ref(&newer_macos)).unwrap();
    assert_eq!(linked.target_triple(), newer_macos.target_triple());
    module("x86_64-pc-windows-gnu")
        .link(&[module("x86_64-w64-windows-gnu")])
        .unwrap();

    let nvptx = module("nvptx64-nvidia-cuda");
    let libdevice = module("nvptx-nvidia-gpulibs");
    let error = nvptx.link(&[libdevice]).err().unwrap();
    assert!(error.to_string().contains("target triples"), "{error}");
    module("nvptx64-nvidia-unknown")
        .link(&[module("nvptx64-nvidia-gpulibs")])
        .unwrap();
}

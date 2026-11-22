use std::ffi::c_void;
use std::ptr::NonNull;

use zkcl_llvm_api::{Context, HostSymbolKind, Jit, JitSymbol};

#[test]
fn links_a_relocatable_object_and_a_separate_ir_module() {
    let context = Context::default();
    let jit = Jit::new().unwrap();
    let helper = context
        .parse_ir("define i32 @double(i32 %x) { %r = mul i32 %x, 2\n ret i32 %r }")
        .unwrap();
    let program = context
        .parse_ir(
            "declare i32 @double(i32)
        define i32 @compute(i32 %x) { %r = call i32 @double(i32 %x)\n ret i32 %r }",
        )
        .unwrap();
    jit.configure_module(&helper).unwrap();
    jit.configure_module(&program).unwrap();
    let object = helper.emit_object(None).unwrap();

    // SAFETY: the invalid buffer cannot provide executable code, and the valid
    // object and IR are generated from the fixed C ABI functions above.
    unsafe {
        assert!(jit.add_object(b"not an object file").is_err());
        jit.add_object(&object).unwrap();
        let duplicate = jit.add_object(&object).unwrap_err();
        assert!(duplicate.to_string().contains("double"), "{duplicate}");
        jit.add_module(&program).unwrap();
    }
    let symbol = jit.lookup("compute").unwrap();
    jit.close();
    // SAFETY: the symbol retains its JIT session and `compute` uses the C calling convention.
    let compute = unsafe {
        std::mem::transmute::<*mut c_void, extern "C" fn(i32) -> i32>(symbol.address().as_ptr())
    };
    assert_eq!(compute(21), 42);
}

#[test]
fn failed_submission_and_lookup_allow_retry_in_the_same_dylib() {
    let context = Context::default();
    let jit = Jit::new().unwrap();
    let base = context
        .parse_ir("define i32 @existing() { ret i32 40 }")
        .unwrap();
    let duplicate = context
        .parse_ir(
            "define i32 @entry() { ret i32 -1 }
             define i32 @existing() { ret i32 -1 }",
        )
        .unwrap();
    let unresolved = context
        .parse_ir(
            "declare i32 @zkcl_missing_import()
             define i32 @entry() { %r = call i32 @zkcl_missing_import()\n ret i32 %r }",
        )
        .unwrap();
    let working = context
        .parse_ir(
            "declare i32 @existing()
             define i32 @entry() {
               %base = call i32 @existing()
               %r = add i32 %base, 2
               ret i32 %r
             }",
        )
        .unwrap();

    // SAFETY: these modules only call the declared functions and use matching C signatures.
    unsafe {
        let base = jit.add_module_and_lookup(&base, "existing").unwrap();
        let error = jit
            .add_module_and_lookup(&duplicate, "entry")
            .err()
            .unwrap();
        assert!(error.to_string().contains("existing"), "{error}");
        let error = jit
            .add_module_and_lookup(&unresolved, "entry")
            .err()
            .unwrap();
        assert!(
            error.to_string().contains("Failed to materialize"),
            "{error}"
        );
        assert_eq!(execute(&base), 40);
        assert!(jit.lookup("entry").is_err());

        let entry = jit.add_module_and_lookup(&working, "entry").unwrap();
        assert_eq!(execute(&entry), 42);
        assert_eq!(execute(&jit.lookup("entry").unwrap()), 42);
    }
}

#[test]
fn a_pending_object_can_resolve_a_provider_after_its_failed_submission() {
    let context = Context::default();
    let jit = Jit::new().unwrap();
    let caller = context
        .parse_ir(
            "declare i32 @provider()
             define i32 @caller() { %value = call i32 @provider()\n ret i32 %value }",
        )
        .unwrap();
    let invalid_provider = context
        .parse_ir(
            "declare i32 @missing_provider_dependency()
             define i32 @provider() {
               %value = call i32 @missing_provider_dependency()
               ret i32 %value
             }",
        )
        .unwrap();
    let provider = context
        .parse_ir("define i32 @provider() { ret i32 42 }")
        .unwrap();

    // SAFETY: caller and both versions of provider share the i32 () C ABI.
    unsafe {
        jit.add_module(&caller).unwrap();
        let failure = jit
            .add_module_and_lookup(&invalid_provider, "provider")
            .err()
            .expect("unresolved provider import");
        assert!(failure.to_string().contains("missing_provider_dependency"));
        let provider = jit.add_module_and_lookup(&provider, "provider").unwrap();
        assert_eq!(execute(&provider), 42);
        assert_eq!(execute(&jit.lookup("caller").unwrap()), 42);
    }
}

#[test]
fn child_dylibs_keep_imports_and_reclaim_names_after_the_last_symbol() {
    let root = Jit::new().unwrap();
    let context = Context::default();
    let base = context
        .parse_ir("define i32 @base() { ret i32 40 }")
        .unwrap();
    let first = root.create_dylib("generation").unwrap();
    let second = root.create_dylib("replacement").unwrap();
    let nested = second.create_dylib("nested").unwrap();
    let first_impl = context
        .parse_ir(
            "declare i32 @base()
        define i32 @work() { %b = call i32 @base()\n %r = add i32 %b, 1\n ret i32 %r }",
        )
        .unwrap();
    let second_impl = context
        .parse_ir(
            "declare i32 @base()
        define i32 @work() { %b = call i32 @base()\n %r = add i32 %b, 2\n ret i32 %r }",
        )
        .unwrap();
    let nested_impl = context
        .parse_ir(
            "declare i32 @work()
        define i32 @nested_work() { %r = call i32 @work()\n ret i32 %r }",
        )
        .unwrap();

    // SAFETY: these definitions use matching C signatures and constant arithmetic.
    unsafe {
        root.add_module(&base).unwrap();
        first.add_module(&first_impl).unwrap();
        second
            .add_object(&second_impl.emit_object(None).unwrap())
            .unwrap();
        nested.add_module(&nested_impl).unwrap();
    }
    assert!(root.lookup("work").is_err());
    let old = first.lookup("work").unwrap();
    first.close();
    assert!(first.lookup("work").is_err());
    assert!(root.create_dylib("generation").is_err());

    let next = second.lookup("work").unwrap();
    second.close();
    assert_eq!(execute(&old), 41);
    assert_eq!(execute(&next), 42);
    drop(old);
    root.create_dylib("generation").unwrap().close();
    drop(next);
    assert!(root.create_dylib("replacement").is_err());
    root.close();

    // Neither the child's native code nor its parent's relocations depend on
    // keeping a public Jit handle to the imported dylibs open.
    let nested_symbol = nested.lookup("nested_work").unwrap();
    nested.close();
    assert_eq!(execute(&nested_symbol), 42);
}

#[test]
fn child_object_resolves_host_functions_and_data_by_name() {
    extern "C" fn double(input: i32) -> i32 {
        input.wrapping_mul(2)
    }

    static BIAS: i32 = 4;

    let root = Jit::new().unwrap();
    let child = root.create_dylib("host-imports").unwrap();
    let double = NonNull::new(double as *mut c_void).unwrap();
    let bias = NonNull::from(&BIAS).cast();
    // SAFETY: this C function and immutable data have process lifetime. Their
    // IR declarations below use the matching types and C calling convention.
    unsafe {
        root.define_symbol("host_double", double, HostSymbolKind::Function)
            .unwrap();
        root.define_symbol("host_bias", bias, HostSymbolKind::Data)
            .unwrap();
        let duplicate = root
            .define_symbol("host_double", double, HostSymbolKind::Function)
            .unwrap_err();
        assert!(duplicate.to_string().contains("host_double"), "{duplicate}");
    }
    let context = Context::default();
    let module = context
        .parse_ir(
            "@host_bias = external constant i32
         declare i32 @host_double(i32)
         define i32 @measure() {
           %double = call i32 @host_double(i32 19)
           %bias = load i32, ptr @host_bias
           %result = add i32 %double, %bias
           ret i32 %result
         }",
        )
        .unwrap();
    child.configure_module(&module).unwrap();
    let object = module.emit_object(None).unwrap();
    // SAFETY: the object implements the verified function above and imports
    // the live symbols specified by its declarations.
    unsafe { child.add_object(&object) }.unwrap();
    root.close();
    let measure = child.lookup("measure").unwrap();
    child.close();
    assert_eq!(execute(&measure), 42);
}

#[test]
fn removes_ir_and_object_resources_then_reuses_their_names_in_place() {
    let jit = Jit::new().unwrap();
    let child = jit.create_dylib("wrong-owner").unwrap();
    let context = Context::default();
    let original = context
        .parse_ir("define i32 @version() { ret i32 41 }")
        .unwrap();
    let replacement = context
        .parse_ir("define i32 @version() { ret i32 42 }")
        .unwrap();
    let unrelated = context
        .parse_ir("define i32 @unrelated() { ret i32 7 }")
        .unwrap();
    jit.configure_module(&replacement).unwrap();
    let object = replacement.emit_object(None).unwrap();
    let old = jit.create_resource_tracker().unwrap();

    // SAFETY: both functions use the i32 () C signature and return constants.
    unsafe {
        let error = child.add_module_with_tracker(&original, &old).unwrap_err();
        assert!(error.to_string().contains("different JITDylib"), "{error}");
        jit.add_module_with_tracker(&original, &old).unwrap();
        jit.add_module(&unrelated).unwrap();
    }
    let unrelated = jit.lookup("unrelated").unwrap();
    let symbol = jit.lookup("version").unwrap();
    assert_eq!(execute(&symbol), 41);
    let error = old.remove().unwrap_err();
    assert!(error.to_string().contains("live JIT symbols"), "{error}");
    drop(symbol);
    old.remove().unwrap();
    old.remove().unwrap();
    assert!(jit.lookup("version").is_err());
    assert_eq!(execute(&unrelated), 7);

    let new = jit.create_resource_tracker().unwrap();
    // SAFETY: the object is generated from the verified replacement above.
    unsafe {
        let error = jit.add_object_with_tracker(&object, &old).unwrap_err();
        assert!(error.to_string().contains("has been removed"), "{error}");
        jit.add_object_with_tracker(&object, &new).unwrap();
    }
    assert_eq!(execute(&jit.lookup("version").unwrap()), 42);
    new.remove().unwrap();
    assert!(jit.lookup("version").is_err());
}

#[test]
fn linked_groups_and_children_keep_their_tracked_imports() {
    let root = Jit::new().unwrap();
    let child = root.create_dylib("consumer").unwrap();
    let context = Context::default();
    let helper = context
        .parse_ir("define i32 @helper() { ret i32 42 }")
        .unwrap();
    let caller = context
        .parse_ir(
            "declare i32 @helper()
         define i32 @answer() { %answer = call i32 @helper()\n ret i32 %answer }",
        )
        .unwrap();
    let helper_group = root.create_resource_tracker().unwrap();
    let caller_group = root.create_resource_tracker().unwrap();
    let child_group = child.create_resource_tracker().unwrap();
    // SAFETY: all definitions use the i32 () C ABI and match their imports.
    unsafe {
        root.add_module_with_tracker(&helper, &helper_group)
            .unwrap();
        root.add_module_with_tracker(&caller, &caller_group)
            .unwrap();
        child
            .add_module_with_tracker(&caller, &child_group)
            .unwrap();
    }

    assert_eq!(execute(&root.lookup("answer").unwrap()), 42);
    let error = helper_group.remove().unwrap_err();
    assert!(error.to_string().contains("submitted JIT code"), "{error}");
    caller_group.remove().unwrap();
    assert_eq!(execute(&child.lookup("answer").unwrap()), 42);
    // Releasing the tracker transfers the child's caller to its default group.
    drop(child_group);
    let error = helper_group.remove().unwrap_err();
    assert!(error.to_string().contains("live JIT symbols"), "{error}");

    let symbol = child.lookup("answer").unwrap();
    child.close();
    assert_eq!(execute(&symbol), 42);
    assert!(helper_group.remove().is_err());
    drop(symbol);
    helper_group.remove().unwrap();
    assert!(root.lookup("helper").is_err());
}

#[test]
fn child_definitions_can_shadow_an_already_linked_parent_import() {
    let root = Jit::new().unwrap();
    let child = root.create_dylib("shadow").unwrap();
    let context = Context::default();
    let parent = context
        .parse_ir("define i32 @shared() { ret i32 41 }")
        .unwrap();
    let local = context
        .parse_ir("define i32 @shared() { ret i32 42 }")
        .unwrap();
    let caller = context
        .parse_ir(
            "declare i32 @shared()
         define i32 @original() { %r = call i32 @shared()\n ret i32 %r }",
        )
        .unwrap();
    // SAFETY: all definitions use the i32 () C ABI and match their imports.
    unsafe {
        root.add_module(&parent).unwrap();
        child.add_module(&caller).unwrap();
    }
    let original = child.lookup("original").unwrap();
    assert_eq!(execute(&original), 41);
    assert!(child.lookup("shared").is_err());
    // SAFETY: the child defines the same C ABI with a distinct implementation.
    unsafe { child.add_module(&local) }.unwrap();
    assert_eq!(execute(&child.lookup("shared").unwrap()), 42);
    assert_eq!(execute(&original), 41);
}

#[test]
fn on_demand_ir_resolves_a_later_parent_submission_and_holds_its_tracker() {
    let root = Jit::new().unwrap();
    let child = root.create_dylib("on-demand").unwrap();
    let group = root.create_resource_tracker().unwrap();
    {
        let context = Context::default();
        let caller = context
            .parse_ir(
                "declare i32 @on_demand_base()
                 define i32 @on_demand_entry() {
                   %answer = call i32 @on_demand_base()
                   ret i32 %answer
                 }",
            )
            .unwrap();
        let provider = context
            .parse_ir("define i32 @on_demand_base() { ret i32 42 }")
            .unwrap();
        // SAFETY: the caller and its later provider use matching C signatures.
        unsafe {
            child.add_ir_module(&caller).unwrap();
            root.add_ir_module_with_tracker(&provider, &group).unwrap();
            assert!(root.add_ir_module(&provider).is_err());
        }
    }

    let entry = child.lookup("on_demand_entry").unwrap();
    child.close();
    assert_eq!(execute(&entry), 42);
    assert!(
        group
            .remove()
            .unwrap_err()
            .to_string()
            .contains("live JIT symbols")
    );
    drop(entry);
    group.remove().unwrap();
    assert!(root.lookup("on_demand_base").is_err());
}

#[test]
fn failed_on_demand_materialization_leaves_a_removable_group() {
    let jit = Jit::new().unwrap();
    let context = Context::default();
    let unresolved = context
        .parse_ir(
            "declare i32 @zkcl_missing_on_demand_import()
             define i32 @on_demand_retry() {
               %answer = call i32 @zkcl_missing_on_demand_import()
               ret i32 %answer
             }",
        )
        .unwrap();
    let replacement = context
        .parse_ir("define i32 @on_demand_retry() { ret i32 42 }")
        .unwrap();
    let group = jit.create_resource_tracker().unwrap();
    // SAFETY: the missing import cannot execute and both definitions have the i32 () C ABI.
    unsafe { jit.add_ir_module_with_tracker(&unresolved, &group) }.unwrap();
    let error = jit.lookup("on_demand_retry").err().unwrap();
    assert!(
        error.to_string().contains("zkcl_missing_on_demand_import"),
        "{error}"
    );
    group.remove().unwrap();

    let pending = jit.create_resource_tracker().unwrap();
    // SAFETY: the replacement returns a constant using the i32 () C ABI.
    unsafe { jit.add_ir_module_with_tracker(&replacement, &pending) }.unwrap();
    pending.remove().unwrap();
    // SAFETY: same verified replacement and C ABI, now in the default group.
    unsafe { jit.add_ir_module(&replacement) }.unwrap();
    assert_eq!(execute(&jit.lookup("on_demand_retry").unwrap()), 42);
}

#[cfg(any(target_arch = "aarch64", target_arch = "x86_64"))]
#[test]
fn on_demand_ir_reports_assembler_diagnostics_at_lookup() {
    let jit = Jit::new().unwrap();
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"define void @on_demand_bad_assembly() {
               call void asm sideeffect ".error \22zkcl deferred assembler diagnostic\22", ""()
               ret void
             }"#,
        )
        .unwrap();
    let group = jit.create_resource_tracker().unwrap();
    // SAFETY: the assembler diagnostic prevents publication of executable code.
    unsafe { jit.add_ir_module_with_tracker(&module, &group) }.unwrap();
    let error = jit.lookup("on_demand_bad_assembly").err().unwrap();
    assert!(
        error
            .to_string()
            .contains("zkcl deferred assembler diagnostic"),
        "{error}"
    );
    group.remove().unwrap();
}

#[cfg(target_pointer_width = "64")]
#[test]
fn on_demand_resource_tracking_includes_imports_introduced_by_codegen() {
    let jit = Jit::new().unwrap();
    let context = Context::default();
    let provider = context
        .parse_ir("define ptr @memcpy(ptr %dest, ptr %src, i64 %n) { ret ptr %dest }")
        .unwrap();
    let caller = context
        .parse_ir(
            "declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1 immarg)
             define void @on_demand_copy(ptr %dest, ptr %src, i64 %n) {
               call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 %n, i1 false)
               ret void
             }",
        )
        .unwrap();
    let provider_group = jit.create_resource_tracker().unwrap();
    let caller_group = jit.create_resource_tracker().unwrap();
    // SAFETY: these private test addresses will only be resolved and never called.
    unsafe {
        jit.add_module_with_tracker(&provider, &provider_group)
            .unwrap();
        jit.add_ir_module_with_tracker(&caller, &caller_group)
            .unwrap();
    }
    drop(jit.lookup("on_demand_copy").unwrap());
    let error = provider_group.remove().unwrap_err();
    assert!(error.to_string().contains("submitted JIT code"), "{error}");
    caller_group.remove().unwrap();
    provider_group.remove().unwrap();
}

fn execute(symbol: &JitSymbol) -> i32 {
    // SAFETY: all symbols passed by these tests use the exact C signature i32 ().
    let function = unsafe {
        std::mem::transmute::<*mut c_void, extern "C" fn() -> i32>(symbol.address().as_ptr())
    };
    function()
}

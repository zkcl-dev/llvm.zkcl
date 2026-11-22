use zkcl_llvm_api::{Context, Instruction, Module, Value};

fn global_address(module: &Module, name: &str) -> Value {
    module.find_global(name).unwrap().unwrap().address()
}

fn function_address(module: &Module, name: &str) -> Value {
    module
        .find_function(name)
        .unwrap()
        .unwrap()
        .address()
        .unwrap()
}

fn terminator(module: &Module, name: &str) -> Instruction {
    module
        .find_function(name)
        .unwrap()
        .unwrap()
        .blocks()
        .next()
        .unwrap()
        .terminator()
        .unwrap()
}

#[test]
fn replaces_globals_in_aliases_initializers_and_reused_constant_operands() {
    let module = Context::default()
        .parse_ir(
            r#"
            @old = global [4 x i32] zeroinitializer
            @new = global [4 x i32] zeroinitializer
            @last = global [4 x i32] zeroinitializer
            @alias = alias [4 x i32], ptr @old
            @direct = global ptr @old
            @nested = global [1 x ptr] [ptr getelementptr inbounds ([4 x i32], ptr @old, i64 0, i64 1)]

            define ptr @existing() {
              ret ptr getelementptr inbounds ([4 x i32], ptr @old, i64 0, i64 1)
            }

            define ptr @additional() { ret ptr null }
            "#,
        )
        .unwrap();
    let source = global_address(&module, "old");
    let replacement = global_address(&module, "new");
    let last = global_address(&module, "last");
    let expression = terminator(&module, "existing").operand(0).unwrap();
    let original = module.ir();
    let edit = module.edit().unwrap();

    assert_eq!(edit.replace_all_uses(&source, &replacement).unwrap(), 3);
    edit.replace_operand(&terminator(&module, "additional"), 0, &expression)
        .unwrap();
    assert_eq!(edit.replace_all_uses(&replacement, &last).unwrap(), 3);
    assert_eq!(edit.replace_all_uses(&source, &last).unwrap(), 0);
    let edited = edit.finish().unwrap();

    assert!(edited.ir().contains("@alias = alias [4 x i32], ptr @last"));
    assert!(edited.ir().contains("@direct = global ptr @last"));
    let nested = edited.find_global("nested").unwrap().unwrap();
    let initializer = nested.initializer().unwrap().unwrap();
    assert!(initializer.ir().contains("@last"));
    let existing = terminator(&edited, "existing").operand(0).unwrap();
    let additional = terminator(&edited, "additional").operand(0).unwrap();
    assert!(existing.ir().contains("@last"));
    assert!(existing.same_as(&additional));
    assert!(expression.ir().contains("@old"));
    assert_eq!(module.ir(), original);
}

#[test]
fn replaces_function_references_in_calls_initializers_and_metadata() {
    let module = Context::default()
        .parse_ir(
            r#"
            @callback = constant ptr @old
            @entry_alias = alias i32 (i32), ptr @old

            define i32 @old(i32 %x) { ret i32 %x }
            define i32 @new(i32 %x) { ret i32 %x }
            declare void @metadata_only()
            declare void @new_metadata_only()

            define i32 @caller(i32 %x) {
              %result = call i32 @old(i32 %x)
              ret i32 %result
            }

            !symbols = !{!0}
            !0 = !{ptr @old, ptr @metadata_only}
            "#,
        )
        .unwrap();
    let original = module.ir();
    let edit = module.edit().unwrap();
    assert_eq!(
        edit.replace_all_uses(
            &function_address(&module, "old"),
            &function_address(&module, "new"),
        )
        .unwrap(),
        3
    );
    assert_eq!(
        edit.replace_all_uses(
            &function_address(&module, "metadata_only"),
            &function_address(&module, "new_metadata_only"),
        )
        .unwrap(),
        0
    );
    let edited = edit.finish().unwrap();

    let ir = edited.ir();
    assert!(ir.contains("@callback = constant ptr @new"));
    assert!(ir.contains("@entry_alias = alias i32 (i32), ptr @new"));
    assert!(ir.contains("call i32 @new(i32 %x)"));
    assert!(ir.contains("!{ptr @new, ptr @new_metadata_only}"));
    assert_eq!(module.ir(), original);
}

#[test]
fn checks_opaque_pointer_symbol_ownership_and_abi() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
            @source = global i32 0
            @wrong_storage = global i64 0
            @wrong_address = addrspace(1) global i32 0
            @wrong_thread = thread_local global i32 0
            declare i32 @source_fn(i32)
            declare fastcc i32 @wrong_convention(i32)
            declare i32 @wrong_signature(i64)
            declare signext i32 @wrong_return(i32)
            declare void @source_parameter(ptr byval(i32))
            declare void @wrong_parameter(ptr byval(i64))
            "#,
        )
        .unwrap();
    let foreign = context.parse_ir("@foreign = global i32 0").unwrap();
    let source = global_address(&module, "source");
    let source_fn = function_address(&module, "source_fn");
    let edit = module.edit().unwrap();

    for name in ["wrong_storage", "wrong_address", "wrong_thread"] {
        assert!(
            edit.replace_all_uses(&source, &global_address(&module, name))
                .is_err()
        );
    }
    for name in ["wrong_convention", "wrong_signature", "wrong_return"] {
        assert!(
            edit.replace_all_uses(&source_fn, &function_address(&module, name))
                .is_err()
        );
    }
    assert!(
        edit.replace_all_uses(&source, &global_address(&foreign, "foreign"))
            .is_err()
    );
    assert!(edit.replace_all_uses(&source, &source_fn).is_err());
    assert!(
        edit.replace_all_uses(
            &function_address(&module, "source_parameter"),
            &function_address(&module, "wrong_parameter"),
        )
        .is_err()
    );
    edit.finish().unwrap();
}

#[test]
fn rejects_retargeting_block_addresses_and_keeps_failed_verification_repairable() {
    let module = Context::default()
        .parse_ir(
            r#"
            @old = global i32 0
            @new = global i32 1
            @decl = external global i32
            @alias = alias i32, ptr @old
            @address = global ptr blockaddress(@with_address, %target)

            define void @with_address() {
            entry:
              br label %target
            target:
              ret void
            }

            define void @other() { ret void }
            "#,
        )
        .unwrap();
    let original = module.ir();
    let edit = module.edit().unwrap();
    let error = edit
        .replace_all_uses(
            &function_address(&module, "with_address"),
            &function_address(&module, "other"),
        )
        .unwrap_err();
    assert!(error.to_string().contains("block address"));

    let declaration = global_address(&module, "decl");
    edit.replace_all_uses(&global_address(&module, "old"), &declaration)
        .unwrap();
    assert!(edit.finish().is_err());
    assert_eq!(module.ir(), original);

    edit.replace_all_uses(&declaration, &global_address(&module, "new"))
        .unwrap();
    let edited = edit.finish().unwrap();
    assert!(edited.ir().contains("@alias = alias i32, ptr @new"));
    assert!(edited.ir().contains("blockaddress(@with_address, %target)"));
}

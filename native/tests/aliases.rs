use zkcl_llvm_api::{Builder, Context, GlobalOptions, Linkage};

#[test]
fn creates_function_and_offset_aliases_and_reads_them_after_linking() {
    let context = Context::default();
    let module = context.module("definitions").unwrap();
    let i32_type = context.int_type(32).unwrap();
    let zero = i32_type.const_zero().unwrap();
    let one = i32_type.const_int(&1.into()).unwrap();
    let array = i32_type.const_array(&[zero.clone(), one.clone()]).unwrap();
    let storage = module
        .add_global_in(
            "array",
            &array.ty(),
            1,
            GlobalOptions {
                initializer: Some(&array),
                ..GlobalOptions::default()
            },
        )
        .unwrap();
    let offset = array
        .ty()
        .const_gep(&storage.address(), &[zero, one.clone()], true)
        .unwrap();
    let data_alias = module
        .add_global_alias("element", &i32_type, &offset, Linkage::External)
        .unwrap();

    let builder = Builder::new(&context);
    let signature = i32_type.function(&[], false).unwrap();
    let function = module.add_function("get", &signature).unwrap();
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let result = builder
        .load(&i32_type, &data_alias.address(), "element")
        .unwrap();
    builder.ret(Some(&result)).unwrap();
    let function_alias = module
        .add_global_alias(
            "read",
            &signature,
            &function.address().unwrap(),
            Linkage::External,
        )
        .unwrap();

    assert!(data_alias.target().same_as(&offset));
    assert!(
        function_alias
            .target()
            .same_as(&function.address().unwrap())
    );
    assert_eq!(data_alias.address_space(), 1);
    assert_eq!(data_alias.value_type().ir(), "i32");
    assert_eq!(function_alias.value_type().ir(), "i32 ()");
    assert_eq!(function_alias.linkage(), Linkage::External);

    let consumer = Context::default()
        .parse_ir("declare i32 @read()\n define i32 @run() { %x = call i32 @read()\n ret i32 %x }")
        .unwrap();
    let linked = consumer.link(&[module]).unwrap();
    let decoded = Context::default()
        .parse_bitcode(&linked.bitcode().unwrap())
        .unwrap();
    let aliases = decoded.global_aliases().collect::<Vec<_>>();
    assert_eq!(aliases.len(), 2);
    let data = decoded.find_global_alias("element").unwrap().unwrap();
    let read = decoded.find_global_alias("read").unwrap().unwrap();
    assert!(aliases.iter().any(|alias| alias.same_as(&data)));
    assert!(aliases.iter().any(|alias| alias.same_as(&read)));
    assert_eq!(data.address().ty().ir(), "ptr addrspace(1)");
    assert!(data.target().ir().contains("getelementptr inbounds"));
    assert_eq!(read.target().name().unwrap(), "get");
    assert!(decoded.find_function("read").unwrap().is_none());
    assert!(decoded.find_global_alias("missing").unwrap().is_none());

    let aliasee = data.target();
    drop(decoded);
    assert!(read.module().ir().contains("@read = alias"));
    drop(read);
    drop(data);
    drop(aliases);
    assert!(aliasee.ir().contains("@array"));
}

#[test]
fn checks_symbol_collisions_targets_and_ownership_before_mutating() {
    let context = Context::default();
    let module = context
        .parse_ir(
            "@a = global i32 1
             @b = global i32 2
             @special = addrspace(1) global i32 3
             @decl = external global i32
             define i32 @function() { ret i32 4 }",
        )
        .unwrap();
    let i32_type = context.int_type(32).unwrap();
    let pointer = |name| module.find_global(name).unwrap().unwrap().address();
    let a = pointer("a");
    let b = pointer("b");
    let other = context.parse_ir("@foreign = global i32 0").unwrap();
    let foreign = other.find_global("foreign").unwrap().unwrap().address();
    let alias = module
        .add_global_alias("alias", &i32_type, &a, Linkage::Internal)
        .unwrap();
    let chain = module
        .add_global_alias("chain", &i32_type, &alias.address(), Linkage::External)
        .unwrap();
    let weak = module
        .add_global_alias("weak", &i32_type, &a, Linkage::WeakAny)
        .unwrap();
    let before = module.ir();
    for name in ["a", "function", "alias", ""] {
        assert!(
            module
                .add_global_alias(name, &i32_type, &a, Linkage::External)
                .is_err()
        );
    }
    for bad_target in [
        pointer("decl"),
        foreign,
        weak.address(),
        context.pointer_type().const_zero().unwrap(),
        i32_type.const_zero().unwrap(),
    ] {
        assert!(
            module
                .add_global_alias("invalid", &i32_type, &bad_target, Linkage::External)
                .is_err()
        );
        assert!(alias.set_target(&bad_target).is_err());
    }
    for linkage in [
        Linkage::AvailableExternally,
        Linkage::Appending,
        Linkage::Common,
        Linkage::ExternalWeak,
    ] {
        assert!(
            module
                .add_global_alias("invalid", &i32_type, &a, linkage)
                .is_err()
        );
    }
    assert!(alias.set_target(&alias.address()).is_err());
    assert!(alias.set_target(&chain.address()).is_err());
    assert!(alias.set_target(&pointer("special")).is_err());
    assert!(alias.set_name("a").is_err());
    assert!(alias.set_name("chain").is_err());
    assert_eq!(module.ir(), before);

    let old_target = alias.target();
    alias.set_target(&b).unwrap();
    alias.set_name("renamed").unwrap();
    assert!(old_target.same_as(&a));
    assert!(alias.target().same_as(&b));
    assert!(chain.target().same_as(&alias.address()));
    assert_eq!(chain.target().name().unwrap(), "renamed");
    assert!(module.find_global_alias("alias").unwrap().is_none());
    assert!(
        module
            .find_global_alias("renamed")
            .unwrap()
            .unwrap()
            .same_as(&alias)
    );
    let alias_address = alias.address();
    let opts = GlobalOptions {
        initializer: Some(&alias_address),
        ..GlobalOptions::default()
    };
    assert!(
        other
            .add_global("invalid", &context.pointer_type(), opts)
            .is_err()
    );
    module.verify().unwrap();
}

#[test]
fn checks_available_externally_and_module_interposition_rules() {
    let context = Context::default();
    let module = context
        .parse_ir(
            "@storage = global i32 1
             @external_only = available_externally global i32 2
             @interposed = alias i32, ptr @storage
             @local = dso_local alias i32, ptr @storage
             !llvm.module.flags = !{!0}
             !0 = !{i32 1, !\"SemanticInterposition\", i1 true}",
        )
        .unwrap();
    let ty = context.int_type(32).unwrap();
    let available = module.find_global("external_only").unwrap().unwrap();
    assert!(
        module
            .add_global_alias("bad_linkage", &ty, &available.address(), Linkage::External)
            .is_err()
    );
    module
        .add_global_alias(
            "available",
            &ty,
            &available.address(),
            Linkage::AvailableExternally,
        )
        .unwrap();
    let interposed = module.find_global_alias("interposed").unwrap().unwrap();
    assert!(
        module
            .add_global_alias("bad_target", &ty, &interposed.address(), Linkage::Internal)
            .is_err()
    );
    let local = module.find_global_alias("local").unwrap().unwrap();
    module
        .add_global_alias("good_target", &ty, &local.address(), Linkage::External)
        .unwrap();
    module.verify().unwrap();
}

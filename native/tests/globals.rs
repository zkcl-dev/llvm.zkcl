use zkcl_llvm_api::{
    Builder, ComdatSelectionKind, Context, DLLStorageClass, GlobalOptions, GlobalVisibility,
    Linkage, ThreadLocalMode, UnnamedAddress,
};

#[test]
fn globals_preserve_storage_types_and_contents_across_bitcode() {
    let context = Context::default();
    let module = context.module("data").unwrap();
    let i32 = context.int_type(32).unwrap();
    let counter = module
        .add_global("counter", &i32, GlobalOptions::default())
        .unwrap();
    assert!(counter.initializer().unwrap().is_none());
    assert!(!counter.is_constant());
    assert!(module.ir().contains("@counter = external global i32"));

    let one = i32.const_int(&1.into()).unwrap();
    counter.set_initializer(&one).unwrap();
    let old_initializer = counter.initializer().unwrap().unwrap();
    counter.set_initializer(&i32.const_zero().unwrap()).unwrap();
    assert_eq!(old_initializer.ir(), "i32 1");

    let bytes = context.const_bytes(&[b'Z', 0, 255], true);
    let message = module
        .add_global(
            "message",
            &bytes.ty(),
            GlobalOptions {
                initializer: Some(&bytes),
                constant: true,
                linkage: Linkage::Private,
            },
        )
        .unwrap();
    assert!(counter.set_initializer(&bytes).is_err());
    assert_eq!(counter.initializer().unwrap().unwrap().ir(), "i32 0");
    assert_eq!(message.storage_type().unwrap().ir(), "[4 x i8]");
    assert_eq!(message.address().ty().ir(), "ptr");

    let bitcode = module.bitcode().unwrap();
    let copy = Context::default().parse_bitcode(&bitcode).unwrap();
    let global = copy.find_global("message").unwrap().unwrap();
    assert_eq!(global.name().unwrap(), "message");
    assert!(global.is_constant());
    assert!(copy.find_global("missing").unwrap().is_none());
    assert!(
        copy.ir()
            .contains(r#"@message = private constant [4 x i8] c"Z\00\FF\00""#)
    );
    let address = global.address();
    drop(global);
    drop(copy);
    assert!(address.ir().contains("@message"));
}

#[test]
fn aggregate_constants_preserve_module_provenance() {
    let context = Context::default();
    let left = context.module("left").unwrap();
    let right = context.module("right").unwrap();
    let i32 = context.int_type(32).unwrap();
    let ptr = context.pointer_type();
    let first = left
        .add_global("first", &i32, GlobalOptions::default())
        .unwrap();
    let second = right
        .add_global("second", &i32, GlobalOptions::default())
        .unwrap();
    let address = first.address();

    assert!(
        ptr.const_array(&[address.clone(), second.address()])
            .is_err()
    );
    assert!(
        ptr.const_array(&[Context::default().pointer_type().const_zero().unwrap()])
            .is_err()
    );
    let array = ptr.const_array(std::slice::from_ref(&address)).unwrap();
    let record = context
        .struct_type(&[array.ty(), i32.clone()], true)
        .unwrap();
    assert!(record.const_struct(std::slice::from_ref(&array)).is_err());
    let initializer = record
        .const_struct(&[array, i32.const_zero().unwrap()])
        .unwrap();
    let options = || GlobalOptions {
        initializer: Some(&initializer),
        ..GlobalOptions::default()
    };
    assert!(right.add_global("invalid", &record, options()).is_err());
    assert!(right.find_global("invalid").unwrap().is_none());
    left.add_global("valid", &record, options()).unwrap();
    assert!(left.ir().contains("[1 x ptr] [ptr @first]"));

    let signature = i32.function(std::slice::from_ref(&i32), false).unwrap();
    let function = right.add_function("read", &signature).unwrap();
    let argument = function.parameter(0).unwrap().unwrap();
    assert!(i32.const_array(std::slice::from_ref(&argument)).is_err());
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    assert!(builder.load(&i32, &address, "invalid").is_err());
    let value = builder.load(&i32, &second.address(), "value").unwrap();
    builder.ret(Some(&value)).unwrap();
    left.verify().unwrap();
    right.verify().unwrap();
}

#[test]
fn aggregate_instructions_validate_fields_before_inserting_ir() {
    let context = Context::default();
    let module = context.module("aggregates").unwrap();
    let i32 = context.int_type(32).unwrap();
    let i8 = context.int_type(8).unwrap();
    let pair = context
        .struct_type(&[i32.clone(), i8.clone()], false)
        .unwrap();
    let signature = i32.function(&[pair, i32.clone()], false).unwrap();
    let function = module.add_function("replace", &signature).unwrap();
    assert_eq!(function.name().unwrap(), "replace");
    assert_eq!(function.signature().ir(), "i32 ({ i32, i8 }, i32)");
    let builder = Builder::new(&context);
    builder
        .position_at_end(&function.append_block("entry").unwrap())
        .unwrap();
    let aggregate = function.parameter(0).unwrap().unwrap();
    let value = function.parameter(1).unwrap().unwrap();
    assert!(builder.extract_value(&aggregate, 2, "invalid").is_err());
    assert!(
        builder
            .insert_value(&aggregate, &value, 1, "invalid")
            .is_err()
    );
    assert!(builder.extract_value(&value, 0, "invalid").is_err());
    let result = builder
        .insert_value(&aggregate, &value, 0, "updated")
        .unwrap();
    let result = builder.extract_value(&result, 0, "field").unwrap();
    let empty_array = i8.const_array(&[]).unwrap();
    assert_eq!(empty_array.ty().ir(), "[0 x i8]");
    assert!(builder.extract_value(&empty_array, 0, "invalid").is_err());
    builder.ret(Some(&result)).unwrap();
    module.verify().unwrap();
    let ir = module.ir();
    assert!(ir.contains("%updated = insertvalue"));
    assert!(ir.contains("%field = extractvalue"));
    assert!(!ir.contains("invalid"));
}

#[test]
fn constant_gep_validates_the_element_path_and_retains_its_base_module() {
    let context = Context::default();
    let module = context.module("owner").unwrap();
    let other = context.module("other").unwrap();
    let i32 = context.int_type(32).unwrap();
    let i64 = context.int_type(64).unwrap();
    let ptr = context.pointer_type();
    let record = context
        .struct_type(&[i32.clone(), i32.clone()], false)
        .unwrap();
    let object = module
        .add_global("object", &record, GlobalOptions::default())
        .unwrap();
    let address = object.address();
    let first = i32.const_zero().unwrap();
    let second = i32.const_int(&1.into()).unwrap();
    let wide_field = i64.const_int(&1.into()).unwrap();
    let missing_field = i32.const_int(&2.into()).unwrap();
    assert!(record.const_gep(&address, &[], true).is_err());
    assert!(
        record
            .const_gep(&first, std::slice::from_ref(&first), true)
            .is_err()
    );
    assert!(
        record
            .const_gep(&address, &[first.clone(), wide_field], true)
            .is_err()
    );
    assert!(
        record
            .const_gep(&address, &[first.clone(), missing_field], true)
            .is_err()
    );
    assert!(
        i32.const_gep(&address, &[first.clone(), second.clone()], false)
            .is_err()
    );
    let foreign_index = Context::default()
        .int_type(32)
        .unwrap()
        .const_zero()
        .unwrap();
    assert!(record.const_gep(&address, &[foreign_index], true).is_err());

    let field = record.const_gep(&address, &[first, second], true).unwrap();
    assert!(field.ir().contains("getelementptr inbounds"));
    let opts = || GlobalOptions {
        initializer: Some(&field),
        ..GlobalOptions::default()
    };
    assert!(other.add_global("bad", &ptr, opts()).is_err());
    assert!(other.find_global("bad").unwrap().is_none());
    module.add_global("field", &ptr, opts()).unwrap();
    let bitcode = module.bitcode().unwrap();
    let copy = Context::default().parse_bitcode(&bitcode).unwrap();
    assert!(copy.ir().contains("getelementptr inbounds"));
    drop(object);
    drop(address);
    drop(module);
    assert!(field.ir().contains("ptr @object"));
}

#[test]
fn vector_constants_require_exact_types_and_retain_symbol_owners() {
    let context = Context::default();
    let module = context.module("vector").unwrap();
    let other = context.module("other").unwrap();
    let i32 = context.int_type(32).unwrap();
    let fixed = i32.vector(2, false).unwrap();
    let scalable = i32.vector(2, true).unwrap();
    let value = i32.const_int(&3.into()).unwrap();
    assert!(fixed.const_vector(std::slice::from_ref(&value)).is_err());
    assert!(
        scalable
            .const_vector(&[value.clone(), value.clone()])
            .is_err()
    );
    let last = i32.const_undef().unwrap();
    assert_eq!(
        fixed.const_vector(&[value.clone(), last]).unwrap().ir(),
        "<2 x i32> <i32 3, i32 undef>"
    );
    assert_eq!(fixed.const_poison().unwrap().ir(), "<2 x i32> poison");
    assert!(fixed.const_splat(&fixed.const_zero().unwrap()).is_err());
    assert_eq!(fixed.const_splat(&value).unwrap().ty().ir(), "<2 x i32>");
    let vector = scalable.const_splat(&value).unwrap();
    assert_eq!(vector.ty().ir(), "<vscale x 2 x i32>");
    assert!(vector.is_constant());

    let ptr_vector = context.pointer_type().vector(2, false).unwrap();
    let target = module
        .add_global("target", &i32, GlobalOptions::default())
        .unwrap();
    let foreign = other
        .add_global("foreign", &i32, GlobalOptions::default())
        .unwrap();
    assert!(
        ptr_vector
            .const_vector(&[target.address(), foreign.address()])
            .is_err()
    );
    let references = ptr_vector.const_splat(&target.address()).unwrap();
    let options = || GlobalOptions {
        initializer: Some(&references),
        ..GlobalOptions::default()
    };
    assert!(other.add_global("bad", &ptr_vector, options()).is_err());
    module.add_global("refs", &ptr_vector, options()).unwrap();
    module.verify().unwrap();
}

#[test]
fn global_and_memory_properties_preserve_llvm_alignment_and_linkage() {
    let context = Context::default();
    let module = context
        .parse_ir(
            r#"
        @global = weak_odr local_unnamed_addr global i32 0, align 4294967296
        @quoted = global i32 0, section "\22, align 4294967296"
        @declaration = extern_weak global i32
        define i32 @memory() {
          %slot = alloca i32, align 4294967296
          store i32 1, ptr %slot, align 4
          %loaded = load i32, ptr %slot, align 4
          ret i32 %loaded
        }
        "#,
        )
        .unwrap();
    let global = module.find_global("global").unwrap().unwrap();
    assert_eq!(global.linkage(), Linkage::WeakODR);
    assert_eq!(global.unnamed_address(), UnnamedAddress::Local);
    assert_eq!(global.alignment(), 1 << 32);
    assert!(global.set_alignment(1 << 32).is_err());
    assert_eq!(global.alignment(), 1 << 32);
    assert_eq!(
        module.find_global("quoted").unwrap().unwrap().alignment(),
        0
    );
    global.set_alignment(0).unwrap();
    assert_eq!(global.alignment(), 0);
    assert!(global.set_alignment(3).is_err());
    assert!(global.set_alignment(1 << 33).is_err());
    global.set_alignment(1 << 31).unwrap();
    global.set_unnamed_address(UnnamedAddress::Global);
    assert!(global.set_linkage(Linkage::Appending).is_err());
    global.set_linkage(Linkage::Common).unwrap();

    let zero = context.int_type(32).unwrap().const_zero().unwrap();
    let one = zero.ty().const_int(&1.into()).unwrap();
    assert!(global.set_initializer(&one).is_err());
    let declaration = module.find_global("declaration").unwrap().unwrap();
    assert_eq!(declaration.linkage(), Linkage::ExternalWeak);
    assert!(declaration.set_linkage(Linkage::Internal).is_err());
    assert!(declaration.set_initializer(&zero).is_err());
    declaration.set_linkage(Linkage::External).unwrap();
    declaration.set_initializer(&one).unwrap();

    let function = module.find_function("memory").unwrap().unwrap();
    assert_eq!(function.linkage(), Linkage::External);
    assert!(function.set_linkage(Linkage::Common).is_err());
    assert!(function.set_linkage(Linkage::ExternalWeak).is_err());
    function.set_linkage(Linkage::Private).unwrap();
    assert_eq!(function.linkage(), Linkage::Private);
    let block = function.blocks().next().unwrap();
    let mut instructions = block.instructions();
    let alloca = instructions.next().unwrap();
    let store = instructions.next().unwrap();
    let load = instructions.next().unwrap();
    let ret = instructions.next().unwrap();
    assert_eq!(alloca.alignment().unwrap(), 1 << 32);
    assert!(alloca.set_alignment(0).is_err());
    store.set_alignment(2).unwrap();
    assert!(load.set_alignment(1 << 32).is_err());
    load.set_alignment(1 << 31).unwrap();
    assert_eq!(load.alignment().unwrap(), 1 << 31);
    assert_eq!(store.alignment().unwrap(), 2);
    assert!(ret.alignment().is_err());
    assert!(ret.set_alignment(4).is_err());
    module.verify().unwrap();
    assert!(
        module
            .ir()
            .contains("@global = common unnamed_addr global i32 0, align 2147483648")
    );
}

#[test]
fn global_storage_properties_round_trip_and_allow_declaration_transitions() {
    let context = Context::default();
    let module = context.module("storage").unwrap();
    let i32 = context.int_type(32).unwrap();
    let global = module
        .add_global_in("item", &i32, 3, GlobalOptions::default())
        .unwrap();
    assert!(global.is_declaration());
    assert_eq!(global.address().ty().ir(), "ptr addrspace(3)");
    assert!(
        module
            .add_global_in("invalid", &i32, 1 << 24, GlobalOptions::default())
            .is_err()
    );
    assert!(module.find_global("invalid").unwrap().is_none());

    let initializer = i32.const_int(&7.into()).unwrap();
    global
        .set_dll_storage_class(DLLStorageClass::Import)
        .unwrap();
    assert!(global.set_initializer(&initializer).is_err());
    assert!(global.set_visibility(GlobalVisibility::Hidden).is_err());
    global
        .set_dll_storage_class(DLLStorageClass::Default)
        .unwrap();
    global.set_initializer(&initializer).unwrap();
    global.set_constant(true).unwrap();
    global.set_externally_initialized(true);
    global.set_thread_local_mode(Some(ThreadLocalMode::InitialExecTLSModel));
    global.set_visibility(GlobalVisibility::Protected).unwrap();
    global.set_section(Some(".data.zkcl")).unwrap();
    assert!(global.set_section(Some("invalid\0section")).is_err());

    let copy = Context::default()
        .parse_bitcode(&module.bitcode().unwrap())
        .unwrap();
    let recovered = copy.find_global("item").unwrap().unwrap();
    assert_eq!(recovered.address_space(), 3);
    assert!(!recovered.is_declaration());
    assert!(recovered.is_constant());
    assert!(recovered.is_externally_initialized());
    assert_eq!(
        recovered.thread_local_mode(),
        Some(ThreadLocalMode::InitialExecTLSModel)
    );
    assert_eq!(recovered.section().unwrap().as_deref(), Some(".data.zkcl"));
    assert_eq!(recovered.visibility(), GlobalVisibility::Protected);
    recovered.clear_initializer().unwrap();
    recovered.set_section(None).unwrap();
    recovered.set_thread_local_mode(None);
    assert!(recovered.is_declaration());
    assert!(recovered.initializer().unwrap().is_none());
    assert!(recovered.section().unwrap().is_none());
    assert!(recovered.thread_local_mode().is_none());
    copy.verify().unwrap();
}

#[test]
fn comdats_keep_their_module_and_reject_invalid_symbol_changes() {
    let context = Context::default();
    let module = context.module("symbols").unwrap();
    let other = context
        .parse_ir(
            "@\"quoted = dso_local\" = external dso_local global i32\n\
                   declare dso_local void @local_to_dso()",
        )
        .unwrap();
    let dso_global = other.find_global("quoted = dso_local").unwrap().unwrap();
    let dso_function = other.find_function("local_to_dso").unwrap().unwrap();
    assert!(
        dso_global
            .set_dll_storage_class(DLLStorageClass::Import)
            .is_err()
    );
    assert!(
        dso_function
            .set_dll_storage_class(DLLStorageClass::Import)
            .is_err()
    );
    assert_eq!(dso_global.dll_storage_class(), DLLStorageClass::Default);
    other.verify().unwrap();
    let signature = context.void_type().function(&[], false).unwrap();
    let function = module.add_function("entry", &signature).unwrap();
    let group = module.get_or_insert_comdat("group").unwrap();
    group.set_selection_kind(ComdatSelectionKind::NoDuplicates);

    assert!(function.is_declaration());
    assert!(function.set_comdat(Some(&group)).is_err());
    function
        .set_dll_storage_class(DLLStorageClass::Import)
        .unwrap();
    assert_eq!(function.dll_storage_class(), DLLStorageClass::Import);
    assert!(function.append_block("body").is_err());
    function
        .set_dll_storage_class(DLLStorageClass::Default)
        .unwrap();
    let block = function.append_block("body").unwrap();
    let builder = Builder::new(&context);
    builder.position_at_end(&block).unwrap();
    builder.ret(None).unwrap();
    assert!(!function.is_declaration());
    assert!(
        function
            .set_dll_storage_class(DLLStorageClass::Import)
            .is_err()
    );

    let foreign_group = other.get_or_insert_comdat("group").unwrap();
    assert!(function.set_comdat(Some(&foreign_group)).is_err());
    function.set_comdat(Some(&group)).unwrap();
    function.set_section(Some(".text.zkcl")).unwrap();
    assert_eq!(function.section().unwrap().as_deref(), Some(".text.zkcl"));
    assert!(function.set_linkage(Linkage::AvailableExternally).is_err());
    assert_eq!(
        function.comdat().unwrap().selection_kind(),
        ComdatSelectionKind::NoDuplicates
    );

    let i32 = context.int_type(32).unwrap();
    let zero = i32.const_zero().unwrap();
    let global = module
        .add_global(
            "object",
            &i32,
            GlobalOptions {
                initializer: Some(&zero),
                ..GlobalOptions::default()
            },
        )
        .unwrap();
    global.set_comdat(Some(&group)).unwrap();
    assert!(global.clear_initializer().is_err());
    assert!(global.set_linkage(Linkage::Common).is_err());
    global.set_comdat(None).unwrap();
    assert!(global.comdat().is_none());
    global.set_linkage(Linkage::Common).unwrap();
    assert!(global.set_constant(true).is_err());

    function.set_linkage(Linkage::Internal).unwrap();
    assert!(function.set_visibility(GlobalVisibility::Hidden).is_err());
    assert!(
        function
            .set_dll_storage_class(DLLStorageClass::Export)
            .is_err()
    );
    module.verify().unwrap();
    drop(builder);
    drop(block);
    drop(function);
    drop(global);
    drop(module);
    assert_eq!(group.selection_kind(), ComdatSelectionKind::NoDuplicates);
}

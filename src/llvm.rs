use std::path::Path;

use inkwell::{
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    OptimizationLevel,
};

pub fn run() {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let i32_type = context.i32_type();

    // getchar()
    let fn_getchar_type = i32_type.fn_type(&[], false);
    let fn_getchar = module.add_function("getchar", fn_getchar_type, Some(Linkage::External));

    // main()
    let fn_main_type = i32_type.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_type, None);

    let entry = context.append_basic_block(fn_main, "entry");

    builder.position_at_end(entry);
    let a1 = builder
        .build_call(fn_getchar, &[], "")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_int_value();
    let v1 = builder.build_int_add(a1, i32_type.const_int(0, false), "");
    builder.build_return(Some(&v1));

    module.print_to_stderr();

    emit_to_file(&module);
}

fn emit_to_file(module: &Module) {
    Target::initialize_x86(&InitializationConfig::default());

    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let features = TargetMachine::get_host_cpu_features();
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &triple,
            "x86-64",
            features.to_str().unwrap(),
            opt,
            reloc,
            model,
        )
        .unwrap();

    let path = Path::new("/tmp/main.o");
    target_machine
        .write_to_file(module, FileType::Object, &path)
        .unwrap();
}

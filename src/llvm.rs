use inkwell::context::Context;

pub fn run() {
    let context = Context::create();
    let module = context.create_module("main");

    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[i32_type.into()], false);
    let fn_value = module.add_function("main", fn_type, None);

    let entry = context.append_basic_block(fn_value, "entry");

    let builder = context.create_builder();
    builder.position_at_end(entry);
    let a1 = fn_value.get_nth_param(0).unwrap().into_int_value();
    let v1 = builder.build_int_add(a1, i32_type.const_int(0, false), "");
    builder.build_return(Some(&v1));

    module.print_to_stderr();
}

use std::{collections::HashMap, path::Path};

use inkwell::{
    builder::Builder,
    context::Context as LLVMContext,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    OptimizationLevel,
};

use crate::{
    context::Ident,
    quaruple::{self, Quaruple},
};

struct Context<'a> {
    pub pvar: HashMap<Ident, PointerValue<'a>>,
    pub ivar: HashMap<Ident, IntValue<'a>>,
}

impl<'a> Context<'a> {
    fn new() -> Self {
        Context {
            pvar: HashMap::new(),
            ivar: HashMap::new(),
        }
    }
}

fn ident_to_pointer<'a>(
    ident: Ident,
    builder: &Builder<'a>,
    ctx: &mut Context<'a>,
    llctx: &'a LLVMContext,
) -> PointerValue<'a> {
    let i32_type = llctx.i32_type();
    *ctx.pvar
        .entry(ident)
        .or_insert_with(|| builder.build_alloca(i32_type, ""))
}

fn value_to_llvm<'a>(
    value: quaruple::Value,
    builder: &Builder<'a>,
    ctx: &mut Context<'a>,
    llctx: &'a LLVMContext,
) -> BasicValueEnum<'a> {
    let i32_type = llctx.i32_type();
    match value {
        quaruple::Value::Int(v) => i32_type.const_int(v as u64, false).into(),
        quaruple::Value::Reg(r) => {
            if r.is_const {
                todo!()
            } else {
                let ptr = ident_to_pointer(r.sym, builder, ctx, llctx);
                builder.build_load(ptr, "").into()
            }
        }
    }
}

fn trans_one_quaruple<'a>(
    quaruple: Quaruple,
    builder: &Builder<'a>,
    ctx: &mut Context<'a>,
    llctx: &'a LLVMContext,
) {
    use quaruple::{OpArg, UnaryOp};

    match quaruple.op {
        OpArg::Unary { op, arg } => {
            let llvm_value = value_to_llvm(arg, builder, ctx, llctx);
            match op {
                UnaryOp::Assign => {
                    let ident = quaruple.result.expect("Assign must have a result").sym;
                    let ptr = ident_to_pointer(ident, builder, ctx, llctx);
                    builder.build_store(ptr, llvm_value)
                }
                UnaryOp::Ret => builder.build_return(Some(&llvm_value)),
            }
        }
        _ => todo!("Unrecognized opcode"),
    };
    ()
}

fn trans_quaruples<'a>(
    function: FunctionValue,
    quaruples: Vec<Quaruple>,
    ctx: &mut Context<'a>,
    llctx: &'a LLVMContext,
) {
    let builder = llctx.create_builder();

    let entry = llctx.append_basic_block(function, "entry");
    builder.position_at_end(entry);

    for quaruple in quaruples {
        trans_one_quaruple(quaruple, &builder, ctx, llctx);
    }
}

pub fn run(quaruples: Vec<Quaruple>) {
    let mut ctx = Context::new();
    let llctx = LLVMContext::create();
    let module = llctx.create_module("main");

    let i32_type = llctx.i32_type();

    // // getchar()
    // let fn_getchar_type = i32_type.fn_type(&[], false);
    // let fn_getchar = module.add_function("getchar", fn_getchar_type, Some(Linkage::External));

    // main()
    let fn_main_type = i32_type.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_type, None);
    trans_quaruples(fn_main, quaruples, &mut ctx, &llctx);

    // let a1 = builder
    //     .build_call(fn_getchar, &[], "")
    //     .try_as_basic_value()
    //     .left()
    //     .unwrap()
    //     .into_int_value();
    // let v1 = builder.build_int_add(a1, i32_type.const_int(0, false), "");
    // builder.build_return(Some(&v1));

    module.print_to_stderr();

    emit_to_file(&module, &Path::new("/tmp/main.o"));
}

fn emit_to_file(module: &Module, path: &Path) {
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

    target_machine
        .write_to_file(module, FileType::Object, path)
        .unwrap();
}

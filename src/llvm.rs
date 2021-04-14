use std::{collections::HashMap, io::Write, path::Path};

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
    pub ctx: &'a LLVMContext,
    pub builder: Builder<'a>,

    pub pvar: HashMap<Ident, PointerValue<'a>>,
    pub ivar: HashMap<Ident, IntValue<'a>>,
}

impl<'a> Context<'a> {
    fn new(ctx: &'a LLVMContext) -> Self {
        Context {
            ctx,
            builder: ctx.create_builder(),
            pvar: HashMap::new(),
            ivar: HashMap::new(),
        }
    }
}

fn ident_to_pointer<'a>(ident: Ident, ctx: &mut Context<'a>) -> PointerValue<'a> {
    let llctx = ctx.ctx;
    let i32_type = llctx.i32_type();
    let builder = &ctx.builder;
    *ctx.pvar
        .entry(ident)
        .or_insert_with(|| builder.build_alloca(i32_type, ""))
}

fn value_to_llvm<'a>(value: quaruple::Value, ctx: &mut Context<'a>) -> BasicValueEnum<'a> {
    use quaruple::{Reg, Value};

    let llctx = ctx.ctx;
    let i32_type = llctx.i32_type();

    match value {
        Value::Int(v) => i32_type.const_int(v as u64, false).into(),
        Value::Reg(Reg {
            sym,
            is_const: true,
        }) => {
            let value = ctx.ivar.get(&sym).unwrap();
            (*value).into()
        }
        Value::Reg(Reg {
            sym,
            is_const: false,
        }) => {
            let ptr = ident_to_pointer(sym, ctx);
            ctx.builder.build_load(ptr, "").into()
        }
    }
}

fn trans_one_quaruple<'a>(quaruple: Quaruple, ctx: &mut Context<'a>) {
    use quaruple::{OpArg, UnaryOp};

    match quaruple.op {
        OpArg::Unary { op, arg } => {
            let llvm_value = value_to_llvm(arg, ctx);
            match op {
                UnaryOp::Assign => {
                    let ident = quaruple.result.expect("Assign must have a result").sym;
                    let ptr = ident_to_pointer(ident, ctx);
                    ctx.builder.build_store(ptr, llvm_value)
                }
                UnaryOp::Ret => ctx.builder.build_return(Some(&llvm_value)),
            }
        }
        _ => todo!("Unrecognized opcode"),
    };
}

fn trans_quaruples<'a>(function: FunctionValue, quaruples: Vec<Quaruple>, ctx: &mut Context<'a>) {
    let llctx = ctx.ctx;
    let builder = &ctx.builder;

    let entry = llctx.append_basic_block(function, "entry");
    builder.position_at_end(entry);

    for quaruple in quaruples {
        trans_one_quaruple(quaruple, ctx);
    }
}

fn quaruple_to_module<'a>(quaruples: Vec<Quaruple>, ctx: &mut Context<'a>) -> Module<'a> {
    let llctx = ctx.ctx;
    let module = llctx.create_module("main");

    let i32_type = llctx.i32_type();

    // // getchar()
    // let fn_getchar_type = i32_type.fn_type(&[], false);
    // let fn_getchar = module.add_function("getchar", fn_getchar_type, Some(Linkage::External));

    // main()
    let fn_main_type = i32_type.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_type, None);
    trans_quaruples(fn_main, quaruples, ctx);

    // let a1 = builder
    //     .build_call(fn_getchar, &[], "")
    //     .try_as_basic_value()
    //     .left()
    //     .unwrap()
    //     .into_int_value();
    // let v1 = builder.build_int_add(a1, i32_type.const_int(0, false), "");
    // builder.build_return(Some(&v1));

    module
}

fn emit_to_file(module: Module, path: &Path) {
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
        .write_to_file(&module, FileType::Object, path)
        .unwrap();
}

pub fn emit_llvm_ir(quaruples: Vec<Quaruple>, file: &mut dyn Write) -> anyhow::Result<()> {
    let llctx = LLVMContext::create();
    let mut ctx = Context::new(&llctx);

    let module = quaruple_to_module(quaruples, &mut ctx);

    let ir_form = module.print_to_string().to_string();
    writeln!(file, "{}", ir_form)?;
    Ok(())
}

pub fn emit_obj(quaruples: Vec<Quaruple>, path: &Path) {
    let llctx = LLVMContext::create();
    let mut ctx = Context::new(&llctx);

    let module = quaruple_to_module(quaruples, &mut ctx);

    emit_to_file(module, path);
}

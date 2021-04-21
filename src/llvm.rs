use std::{collections::HashMap, io::Write, path::Path};

use inkwell::{
    builder::Builder,
    context::Context as LLVMContext,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate, OptimizationLevel,
};

use crate::{
    context::Context as QContext,
    quaruple::{self, Quaruple},
    sym_table::{SymTable, Symbol},
};

struct Context<'a> {
    pub ctx: &'a LLVMContext,
    pub builder: Builder<'a>,

    pub pvar: HashMap<Symbol, PointerValue<'a>>,
    pub ivar: HashMap<Symbol, BasicValueEnum<'a>>,

    sym_table: SymTable,
}

impl<'a> Context<'a> {
    fn new(llctx: &'a LLVMContext, qctx: QContext) -> Self {
        Context {
            ctx: llctx,
            builder: llctx.create_builder(),
            pvar: HashMap::new(),
            ivar: HashMap::new(),
            sym_table: qctx.sym_table,
        }
    }
}

/// Get a `PointerValue` from a non-const `Ident` in ir.
fn get_pointer<'a>(ident: Symbol, ctx: &mut Context<'a>) -> PointerValue<'a> {
    let llctx = ctx.ctx;
    let i32_type = llctx.i32_type();
    let builder = &ctx.builder;
    *ctx.pvar
        .entry(ident)
        .or_insert_with(|| builder.build_alloca(i32_type, ""))
}

fn store_value<'a>(value: BasicValueEnum<'a>, reg: Symbol, ctx: &mut Context<'a>) {
    if ctx.sym_table.is_const(reg) {
        ctx.ivar.insert(reg, value);
    } else {
        let p = get_pointer(reg, ctx);
        ctx.builder.build_store(p, value);
    }
}

fn get_basic_value<'a>(value: quaruple::Value, ctx: &mut Context<'a>) -> BasicValueEnum<'a> {
    use quaruple::Value;

    let llctx = ctx.ctx;
    let i32_type = llctx.i32_type();
    let i1_type = llctx.bool_type();

    match value {
        Value::Int(v) => i32_type.const_int(v as u64, false).into(),
        Value::Bool(v) => i1_type.const_int(v as u64, false).into(),
        Value::Reg(sym) if ctx.sym_table.is_const(sym) => {
            let value = ctx.ivar.get(&sym).unwrap();
            *value
        }
        Value::Reg(sym) => {
            // is_const -> false
            let ptr = get_pointer(sym, ctx);
            ctx.builder.build_load(ptr, "")
        }
    }
}

fn trans_quaruple(quaruple: Quaruple, ctx: &mut Context) {
    use quaruple::{BinaryOp, OpArg, UnaryOp};

    match quaruple.op {
        OpArg::Unary { op, arg } => {
            let arg_value = get_basic_value(arg, ctx);
            match op {
                UnaryOp::Assign => {
                    let ident = quaruple.result.expect("Assign must have a result");
                    let ptr = get_pointer(ident, ctx);
                    ctx.builder.build_store(ptr, arg_value);
                }
                UnaryOp::Ret => {
                    ctx.builder.build_return(Some(&arg_value));
                }
            }
        }
        OpArg::Binary { op, arg1, arg2 } => {
            let arg1_value = get_basic_value(arg1, ctx);
            let arg2_value = get_basic_value(arg2, ctx);
            let v = match op {
                BinaryOp::Add => ctx.builder.build_int_add(
                    arg1_value.into_int_value(),
                    arg2_value.into_int_value(),
                    "",
                ),
                BinaryOp::Sub => ctx.builder.build_int_sub(
                    arg1_value.into_int_value(),
                    arg2_value.into_int_value(),
                    "",
                ),
                BinaryOp::Mul => ctx.builder.build_int_mul(
                    arg1_value.into_int_value(),
                    arg2_value.into_int_value(),
                    "",
                ),
                BinaryOp::Div => ctx.builder.build_int_signed_div(
                    arg1_value.into_int_value(),
                    arg2_value.into_int_value(),
                    "",
                ),
                ir_op
                @
                (BinaryOp::Eq
                | BinaryOp::Ne
                | BinaryOp::Lt
                | BinaryOp::Le
                | BinaryOp::Gt
                | BinaryOp::Ge) => {
                    let op = match ir_op {
                        BinaryOp::Eq => IntPredicate::EQ,
                        BinaryOp::Ne => IntPredicate::NE,
                        BinaryOp::Lt => IntPredicate::SLT,
                        BinaryOp::Le => IntPredicate::SLE,
                        BinaryOp::Gt => IntPredicate::SGT,
                        BinaryOp::Ge => IntPredicate::SGE,
                        _ => unreachable!(),
                    };
                    ctx.builder.build_int_compare(
                        op,
                        arg1_value.into_int_value(),
                        arg2_value.into_int_value(),
                        "",
                    )
                }
                _ => todo!(),
            };
            quaruple.result.map(|reg| store_value(v.into(), reg, ctx));
        }
    };
}

fn build_function<'a>(function: FunctionValue, quaruples: Vec<Quaruple>, ctx: &mut Context<'a>) {
    let llctx = ctx.ctx;
    let builder = &ctx.builder;

    let entry = llctx.append_basic_block(function, "entry");
    builder.position_at_end(entry);

    for quaruple in quaruples {
        trans_quaruple(quaruple, ctx);
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
    build_function(fn_main, quaruples, ctx);

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

fn emit_obj_file(module: Module, path: &Path) {
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

pub fn emit_llvm_ir(
    quaruples: Vec<Quaruple>,
    file: &mut dyn Write,
    qctx: QContext,
) -> anyhow::Result<()> {
    let llctx = LLVMContext::create();
    let mut ctx = Context::new(&llctx, qctx);

    let module = quaruple_to_module(quaruples, &mut ctx);

    let ir_form = module.print_to_string().to_string();
    writeln!(file, "{}", ir_form)?;
    Ok(())
}

pub fn emit_obj(quaruples: Vec<Quaruple>, path: &Path, qctx: QContext) {
    let llctx = LLVMContext::create();
    let mut ctx = Context::new(&llctx, qctx);

    let module = quaruple_to_module(quaruples, &mut ctx);

    emit_obj_file(module, path);
}

use std::{collections::HashMap, io::Write, path::Path};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context as LLVMContext,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyType, AnyTypeEnum, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate, OptimizationLevel,
};

use crate::{
    context::Context as QContext,
    ir::{self, BranchOp, IrGraph, Label, Quaruple},
    sym_table::{SymTable, Symbol},
    ty::Ty,
};

struct Context<'a> {
    ctx: &'a LLVMContext,
    builder: Builder<'a>,

    pvar: HashMap<Symbol, PointerValue<'a>>,
    ivar: HashMap<Symbol, BasicValueEnum<'a>>,
    label_block: HashMap<Label, BasicBlock<'a>>,
    sym_fn: HashMap<Symbol, FunctionValue<'a>>,

    sym_table: SymTable,
}

impl<'a> Context<'a> {
    fn new(llctx: &'a LLVMContext, qctx: QContext) -> Self {
        Context {
            ctx: llctx,
            builder: llctx.create_builder(),
            pvar: HashMap::new(),
            ivar: HashMap::new(),
            label_block: HashMap::new(),
            sym_fn: HashMap::new(),
            sym_table: qctx.sym_table,
        }
    }
}

trait AnyTypeEnumExt<'ctx>: AnyType<'ctx>
where
    Self: Sized,
{
    fn fn_type(
        &self,
        param_types: &[BasicTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        match self.as_any_type_enum() {
            AnyTypeEnum::IntType(ty) => ty.fn_type(param_types, is_var_args),
            AnyTypeEnum::VoidType(ty) => ty.fn_type(param_types, is_var_args),
            o => todo!("{:?}", o),
        }
    }
}

impl<'a> AnyTypeEnumExt<'a> for AnyTypeEnum<'a> {}

fn llvm_basic_type<'a>(ty: Ty, ctx: &Context<'a>) -> BasicTypeEnum<'a> {
    let llctx = ctx.ctx;
    match ty {
        Ty::Int => llctx.i32_type().into(),
        Ty::Bool => llctx.bool_type().into(),
        Ty::Void | Ty::Fn(_, _) => panic!("{:?}", ty),
        o => unimplemented!("{}", o),
    }
}

fn llvm_type<'a>(ty: Ty, ctx: &Context<'a>) -> AnyTypeEnum<'a> {
    let llctx = ctx.ctx;
    match ty {
        Ty::Int | Ty::Bool => llvm_basic_type(ty, ctx).as_any_type_enum(),
        Ty::Void => llctx.void_type().into(),
        Ty::Fn(arg_ty, ret_ty) => {
            let args = arg_ty
                .into_iter()
                .map(|ty| llvm_basic_type(ty, ctx))
                .collect::<Vec<_>>();
            llvm_type(*ret_ty, ctx).fn_type(&args, false).into()
        }
        o => todo!("todo: {}", o),
    }
}

/// Get a `PointerValue` from a non-const `Ident` in ir.
fn get_pointer<'a>(ident: Symbol, ctx: &mut Context<'a>) -> PointerValue<'a> {
    let sym_type = llvm_basic_type(ctx.sym_table.ty_of(ident).unwrap(), ctx);
    let builder = &ctx.builder;
    *ctx.pvar
        .entry(ident)
        .or_insert_with(|| builder.build_alloca(sym_type, ""))
}

fn store_value<'a>(value: BasicValueEnum<'a>, reg: Symbol, ctx: &mut Context<'a>) {
    if ctx.sym_table.is_const(reg) {
        ctx.ivar.insert(reg, value);
    } else {
        let p = get_pointer(reg, ctx);
        ctx.builder.build_store(p, value);
    }
}

fn get_basic_value<'a>(value: ir::Value, ctx: &mut Context<'a>) -> BasicValueEnum<'a> {
    use ir::Value;

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
    use ir::{BinaryOp, OpArg, UnaryOp};

    let v = match quaruple.op {
        OpArg::Arg(n) => {
            let fn_val = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
            fn_val.get_nth_param(n as u32).unwrap()
        }
        OpArg::Unary { op, arg } => match op {
            UnaryOp::Const => get_basic_value(arg, ctx),
        },
        OpArg::Binary { op, arg1, arg2 } => {
            let arg1_value = get_basic_value(arg1, ctx);
            let arg2_value = get_basic_value(arg2, ctx);
            match op {
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
            }
            .into()
        }
        OpArg::Call { fn_val, args } => {
            let fn_sym = ctx.sym_fn[&fn_val.unwrap_reg()];
            let args = args
                .into_iter()
                .map(|arg| get_basic_value(arg, ctx))
                .collect::<Vec<_>>();
            ctx.builder
                .build_call(fn_sym, &args, "")
                .try_as_basic_value()
                .left()
                .unwrap()
        }
    };
    if let Some(reg) = quaruple.result {
        store_value(v, reg, ctx);
    }
}

fn trans_branch(branch_op: BranchOp, ctx: &mut Context<'_>) {
    match branch_op {
        BranchOp::Ret(val) => {
            let value = get_basic_value(val, ctx);
            ctx.builder.build_return(Some(&value));
        }
        BranchOp::Goto(label) => {
            ctx.builder
                .build_unconditional_branch(ctx.label_block[&label]);
        }
        BranchOp::CondGoto(val, true_label, false_label) => {
            let value = get_basic_value(val, ctx).into_int_value();
            let then_block = ctx.label_block[&true_label];
            let else_block = ctx.label_block[&false_label];
            ctx.builder
                .build_conditional_branch(value, then_block, else_block);
        }
    }
}

fn build_function(function: FunctionValue, mut ir_graph: IrGraph, ctx: &mut Context<'_>) {
    let mut label_block = HashMap::new();
    for &label in &ir_graph.block_order {
        let block = ctx.ctx.append_basic_block(function, &label.to_string());
        label_block.insert(label, block);
    }
    ctx.label_block = label_block;
    for &label in &ir_graph.block_order {
        ctx.builder.position_at_end(ctx.label_block[&label]);
        let ir_block = ir_graph.blocks.remove(&label).unwrap();
        for quaruple in ir_block.ir_list {
            trans_quaruple(quaruple, ctx);
        }
        trans_branch(ir_block.exit, ctx);
    }
}

fn ir_graph_to_module<'a>(
    ir_graph: Vec<(Symbol, Option<IrGraph>)>,
    ctx: &mut Context<'a>,
) -> Module<'a> {
    let llctx = ctx.ctx;
    let module = llctx.create_module("program");

    for (fun_sym, ir_graph) in ir_graph {
        let name = ctx.sym_table.name_of(fun_sym).unwrap();
        let fun_ty = llvm_type(ctx.sym_table.ty_of(fun_sym).unwrap(), ctx).into_function_type();

        let fn_sym = module
            .get_function(name)
            .unwrap_or_else(|| module.add_function(name, fun_ty, None));
        ctx.sym_fn.entry(fun_sym).or_insert(fn_sym);
        if let Some(ir_graph) = ir_graph {
            build_function(fn_sym, ir_graph, ctx);
        }
    }

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
    ir_graph: Vec<(Symbol, Option<IrGraph>)>,
    file: &mut dyn Write,
    qctx: QContext,
) -> anyhow::Result<()> {
    let llctx = LLVMContext::create();
    let mut ctx = Context::new(&llctx, qctx);

    let module = ir_graph_to_module(ir_graph, &mut ctx);

    let ir_form = module.print_to_string().to_string();
    writeln!(file, "{}", ir_form)?;
    Ok(())
}

pub fn emit_obj(ir_graph: Vec<(Symbol, Option<IrGraph>)>, path: &Path, qctx: QContext) {
    let llctx = LLVMContext::create();
    let mut ctx = Context::new(&llctx, qctx);

    let module = ir_graph_to_module(ir_graph, &mut ctx);

    emit_obj_file(module, path);
}

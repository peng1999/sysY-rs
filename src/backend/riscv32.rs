use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    io::Write,
};

use crate::{
    context::Context as QContext,
    ir::{self, BranchOp, IrGraph, Quaruple},
    sym_table::{SymTable, Symbol},
    ty::Ty,
};

use self::RiscVReg::*;
use crate::ir::UnaryOp;

struct Context<'a> {
    sym_table: SymTable,
    file: &'a mut dyn Write,

    frame_size: i32,
    reg_alloc: HashMap<Symbol, AllocReg>,
}

impl<'a> Context<'a> {
    fn new(file: &'a mut dyn Write, ctx: QContext) -> Self {
        Context {
            sym_table: ctx.sym_table,
            file,
            reg_alloc: HashMap::new(),
            frame_size: 0,
        }
    }
}

#[rustfmt::skip]
#[derive(Debug, Copy, Clone)]
enum RiscVReg {
    // t5, t6: 临时寄存器
    A0, A1, A2, A3, A4, A5, A6, A7,
    T0, T1, T2, T3, T4, T5, T6,
}

const ARG_REGS: &[RiscVReg] = &[A0, A1, A2, A3, A4, A5, A6, A7];
//const TMP_REGS: &[RiscVReg] = &[T0, T1, T2, T3, T4, T5, T6];

impl Display for RiscVReg {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let name = format!("{:?}", self);
        write!(fmt, "{}", name.to_ascii_lowercase())
    }
}

#[derive(Debug, Copy, Clone)]
enum AllocReg {
    Reg(RiscVReg),
    Stack(i32, u32), // offset of sp, size in stack
}

#[derive(Debug, Copy, Clone)]
enum AsmValue {
    Reg(RiscVReg),
    Stack(i32, u32),
    Const(i32),
}

fn align_byte_up(pos: i32, align: i32) -> i32 {
    if pos % align == 0 {
        pos
    } else {
        pos + (align - pos % align)
    }
}

#[test]
fn align_byte_works() {
    assert_eq!(align_byte_up(8, 4), 8);
    assert_eq!(align_byte_up(9, 4), 12);
    assert_eq!(align_byte_up(10, 4), 12);
}

fn op_store(size: u32) -> &'static str {
    match size {
        1 => "sb", // byte
        2 => "sh", // half word
        4 => "sw", // word
        _ => unimplemented!(),
    }
}

fn op_load(size: u32) -> &'static str {
    match size {
        1 => "lb", // byte
        2 => "lh", // half word
        4 => "lw", // word
        _ => unimplemented!(),
    }
}

fn extract_const_val(val: ir::Value) -> i32 {
    use ir::Value::*;

    match val {
        Reg(_) => panic!("not a const value"),
        Int(v) => v,
        Bool(v) => v as i32,
    }
}

fn emit_asm_value_to_reg(val: AsmValue, target: RiscVReg, ctx: &mut Context) -> anyhow::Result<()> {
    match val {
        AsmValue::Reg(r) => {
            writeln!(ctx.file, "mv {}, {}", target, r)?;
        }
        AsmValue::Stack(offset, size) => {
            let op = op_load(size);
            writeln!(ctx.file, "{} {}, {}(sp)", op, target, offset)?;
        }
        AsmValue::Const(v) => {
            writeln!(ctx.file, "li {}, {}", target, v)?;
        }
    }
    Ok(())
}

fn emit_asm_value_to_stack(
    val: AsmValue,
    (offset, size): (i32, u32),
    ctx: &mut Context,
) -> anyhow::Result<()> {
    let reg = match val {
        AsmValue::Reg(r) => r,
        val @ (AsmValue::Stack(_, _) | AsmValue::Const(_)) => {
            emit_asm_value_to_reg(val, T6, ctx)?;
            T6
        }
    };
    let op = op_store(size);
    writeln!(ctx.file, "{} {}, {}(sp)", op, reg, offset)?;
    Ok(())
}

fn ir_into_asm_value(val: ir::Value, ctx: &mut Context) -> AsmValue {
    use ir::Value::*;

    match val {
        Reg(r) => match ctx.reg_alloc[&r] {
            AllocReg::Reg(reg) => AsmValue::Reg(reg),
            AllocReg::Stack(offset, size) => AsmValue::Stack(offset, size),
        },
        v @ (Int(_) | Bool(_)) => {
            let v = extract_const_val(v);
            AsmValue::Const(v)
        }
    }
}

fn emit_call(
    fn_sym: Symbol,
    args: Vec<ir::Value>,
    ctx: &mut Context,
) -> anyhow::Result<Option<AsmValue>> {
    let arg_cnt = args.len() as i32;
    // store arguments
    for (i, arg) in args.into_iter().enumerate() {
        let asm_value = ir_into_asm_value(arg, ctx);
        if i < 8 {
            let target = ARG_REGS[i];
            emit_asm_value_to_reg(asm_value, target, ctx)?;
        } else {
            let target_offset = 4 * (i as i32 - arg_cnt);
            emit_asm_value_to_stack(asm_value, (target_offset, 4), ctx)?;
        };
    }
    if arg_cnt >= 8 {
        writeln!(ctx.file, "addi sp, sp, {}", -(arg_cnt - 8) * 4)?;
    }
    let name = ctx.sym_table.name_of(fn_sym).unwrap();
    writeln!(ctx.file, "call {}", name)?;
    if arg_cnt >= 8 {
        writeln!(ctx.file, "addi sp, sp, {}", (arg_cnt - 8) * 4)?;
    }
    let ret_ty = ctx.sym_table.ty_of(fn_sym).unwrap().fn_ret_ty();
    Ok((ret_ty != Ty::Void).then_some(AsmValue::Reg(A0)))
}

fn emit_quaruple(quaruple: Quaruple, ctx: &mut Context) -> anyhow::Result<()> {
    use ir::OpArg;

    dbg!(quaruple.to_string());
    let val = match quaruple.op {
        OpArg::Arg(n) => {
            if n < 8 {
                Some(AsmValue::Reg(ARG_REGS[n]))
            } else {
                let n = n as i32;
                let offset = ctx.frame_size + (n - 8) * 4;
                let ouput_reg = T6;
                writeln!(ctx.file, "lw {}, {}(sp)", ouput_reg, offset)?;
                Some(AsmValue::Reg(ouput_reg))
            }
        }
        OpArg::Unary { op, arg } => match op {
            UnaryOp::Const => {
                let val = ir_into_asm_value(arg, ctx);
                let ouput_reg = T6;
                emit_asm_value_to_reg(val, ouput_reg, ctx)?;
                Some(AsmValue::Reg(ouput_reg))
            },
        },
        OpArg::Binary { .. } => None,
        OpArg::Call { fn_val, args } => emit_call(fn_val, args, ctx)?,
        OpArg::LoadArr { .. } => todo!("riscv array"),
        OpArg::StoreArr { .. } => todo!("riscv array"),
    };
    if let (Some(result), Some(val)) = (quaruple.result, val) {
        match ctx.reg_alloc[&result] {
            AllocReg::Reg(reg) => {
                emit_asm_value_to_reg(val, reg, ctx)?;
            }
            AllocReg::Stack(offset, size) => {
                emit_asm_value_to_stack(val, (offset, size), ctx)?;
            }
        }
    }

    Ok(())
}

fn emit_branch(branch_op: BranchOp, name: &str, ctx: &mut Context) -> anyhow::Result<()> {
    match branch_op {
        BranchOp::Ret(v) => {
            if let Some(v) = v {
                let val = ir_into_asm_value(v, ctx);
                emit_asm_value_to_reg(val, A0, ctx)?;
            }
            writeln!(ctx.file, "j .exit{}", name)?;
        }
        BranchOp::Goto(label) => {
            writeln!(ctx.file, "j .{}", label)?;
        }
        BranchOp::CondGoto(_, _, _) => {}
    }
    Ok(())
}

/// 分配寄存器，确定栈帧结构
fn allocate_register(fn_sym: Symbol, ctx: &mut Context) {
    // stack structure:
    // |   ra   | align 16
    // |  vars  | align sizeof(var)
    //            <- sp

    let mut sp = 0;
    let mut alloc = HashMap::new();

    // allocate local variables
    for &local in ctx.sym_table.locals_of(fn_sym) {
        let ty = ctx.sym_table.ty_of(local).unwrap().into_ty_basic().unwrap();
        let size = ty.byte_size() as i32;
        let item_size = ty.get_elem_ty_rank().0.byte_size();
        let offset = align_byte_up(sp, size);
        alloc.insert(local, AllocReg::Stack(offset, item_size));
        sp = offset + size;
    }
    ctx.reg_alloc = alloc;
    // saved ra at top of the frame
    sp += 4;
    // frame are 16-byte aligned
    ctx.frame_size = align_byte_up(sp, 16);
}

fn emit_function(mut ir_graph: IrGraph, fn_sym: Symbol, ctx: &mut Context) -> anyhow::Result<()> {
    let name = ctx.sym_table.name_of(fn_sym).unwrap().to_owned();
    writeln!(ctx.file, ".globl {0}\n{0}:", name)?;
    allocate_register(fn_sym, ctx);

    // enter frame
    writeln!(ctx.file, "addi sp, sp, {}", -ctx.frame_size)?;
    writeln!(ctx.file, "sw ra, {}(sp)", ctx.frame_size - 4)?;

    for &label in &ir_graph.block_order {
        let ir_block = ir_graph.blocks.remove(&label).unwrap();
        writeln!(ctx.file, ".{}:", label)?;
        for quaruple in ir_block.ir_list {
            emit_quaruple(quaruple, ctx)?;
        }
        emit_branch(ir_block.exit, &name, ctx)?;
    }

    // exit frame
    writeln!(ctx.file, ".exit{}:", name)?;
    writeln!(ctx.file, "lw ra, {}(sp)", ctx.frame_size - 4)?;
    writeln!(ctx.file, "addi sp, sp, {}", ctx.frame_size)?;
    writeln!(ctx.file, "ret")?;
    Ok(())
}

pub fn emit_asm(
    ir_graph: Vec<(Symbol, Option<IrGraph>)>,
    file: &mut dyn Write,
    qctx: QContext,
) -> anyhow::Result<()> {
    let mut ctx = Context::new(file, qctx);

    for (fn_sym, ir_graph) in ir_graph {
        if let Some(ir_graph) = ir_graph {
            emit_function(ir_graph, fn_sym, &mut ctx)?;
        }
    }
    Ok(())
}

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
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum RiscVReg {
    // t5, t6: 临时寄存器
    A0, A1, A2, A3, A4, A5, A6, A7,
    /*T0, T1, T2, T3, T4, */T5, T6,
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

impl AllocReg {
    fn into_reg(self) -> Option<RiscVReg> {
        match self {
            AllocReg::Reg(r) => Some(r),
            AllocReg::Stack(_, _) => None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Operand {
    Reg(RiscVReg),
    Stack(i32, u32),
    Const(i32),
}

impl Operand {
    fn into_const(self) -> Option<i32> {
        match self {
            Operand::Const(v) => Some(v),
            _ => None,
        }
    }
}

impl Display for Operand {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Operand::Reg(r) => write!(fmt, "{}", r),
            Operand::Stack(offset, _) => write!(fmt, "{}(sp)", offset),
            Operand::Const(v) => write!(fmt, "{}", v),
        }
    }
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

fn size_suffix(size: u32) -> &'static str {
    match size {
        1 => "b", // byte
        2 => "h", // half word
        4 => "w", // word
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

/// Convert operand to Reg or Const
fn emit_load_operand(val: Operand, hint: RiscVReg, ctx: &mut Context) -> anyhow::Result<Operand> {
    match val {
        Operand::Stack(offset, size) => {
            let s = size_suffix(size);
            writeln!(ctx.file, "l{} {}, {}(sp)", s, hint, offset)?;
            Ok(Operand::Reg(hint))
        }
        op => Ok(op),
    }
}

/// Convert operand to Reg
fn emit_operand_as_reg(val: Operand, hint: RiscVReg, ctx: &mut Context) -> anyhow::Result<RiscVReg> {
    match emit_load_operand(val, hint, ctx)? {
        Operand::Reg(r) => Ok(r),
        Operand::Const(v) => {
            writeln!(ctx.file, "li {}, {}", hint, v)?;
            Ok(hint)
        }
        Operand::Stack(_, _) => unreachable!(),
    }
}

/// Move operand to specific Reg
fn emit_operand_to_reg(val: Operand, target: RiscVReg, ctx: &mut Context) -> anyhow::Result<()> {
    match val {
        Operand::Reg(r) => {
            if r != target {
                writeln!(ctx.file, "mv {}, {}", target, r)?;
            }
        }
        Operand::Stack(offset, size) => {
            let s = size_suffix(size);
            writeln!(ctx.file, "l{} {}, {}(sp)", s, target, offset)?;
        }
        Operand::Const(v) => {
            writeln!(ctx.file, "li {}, {}", target, v)?;
        }
    }
    Ok(())
}

/// Store operand to specific stack position.
fn emit_store_operand(
    val: Operand,
    (offset, size): (i32, u32),
    ctx: &mut Context,
) -> anyhow::Result<()> {
    let reg = emit_operand_as_reg(val, T6, ctx)?;
    let s = size_suffix(size);
    writeln!(ctx.file, "s{} {}, {}(sp)", s, reg, offset)?;
    Ok(())
}

fn ir_into_operand(val: ir::Value, ctx: &mut Context) -> Operand {
    use ir::Value::*;

    match val {
        Reg(r) => match ctx.reg_alloc[&r] {
            AllocReg::Reg(reg) => Operand::Reg(reg),
            AllocReg::Stack(offset, size) => Operand::Stack(offset, size),
        },
        v @ (Int(_) | Bool(_)) => {
            let v = extract_const_val(v);
            Operand::Const(v)
        }
    }
}

fn emit_call(
    fn_sym: Symbol,
    args: Vec<ir::Value>,
    ctx: &mut Context,
) -> anyhow::Result<Option<Operand>> {
    let arg_cnt = args.len() as i32;
    // store arguments
    for (i, arg) in args.into_iter().enumerate() {
        let asm_value = ir_into_operand(arg, ctx);
        if i < 8 {
            let target = ARG_REGS[i];
            emit_operand_to_reg(asm_value, target, ctx)?;
        } else {
            let target_offset = 4 * (i as i32 - arg_cnt);
            emit_store_operand(asm_value, (target_offset, 4), ctx)?;
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
    Ok((ret_ty != Ty::Void).then_some(Operand::Reg(A0)))
}

fn emit_quaruple(quaruple: Quaruple, ctx: &mut Context) -> anyhow::Result<()> {
    use ir::{BinaryOp, OpArg, UnaryOp};

    let target = quaruple.result.map(|result| ctx.reg_alloc[&result]);
    let target_reg = target.and_then(AllocReg::into_reg);

    let val = match quaruple.op {
        OpArg::Arg(n) => {
            let val = if n < 8 {
                Operand::Reg(ARG_REGS[n])
            } else {
                let n = n as i32;
                let offset = ctx.frame_size + (n - 8) * 4;
                Operand::Stack(offset, 4)
            };
            Some(val)
        }
        OpArg::Unary { op, arg } => match op {
            UnaryOp::Const => Some(ir_into_operand(arg, ctx)),
        },
        OpArg::Binary { op, arg1, arg2 } => {
            let op1 = ir_into_operand(arg1, ctx);
            let op2 = ir_into_operand(arg2, ctx);
            let op1 = emit_operand_as_reg(op1, T6, ctx)?;
            let op2 = emit_load_operand(op2, T5, ctx)?;
            let res = target_reg.unwrap_or(T6);
            match op {
                BinaryOp::Add => {
                    writeln!(ctx.file, "add {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Sub => {
                    if let Some(val) = op2.into_const() {
                        writeln!(ctx.file, "addi {}, {}, {}", res, op1, -val)?;
                    } else {
                        writeln!(ctx.file, "sub {}, {}, {}", res, op1, op2)?;
                    }
                }
                BinaryOp::Mul => {
                    writeln!(ctx.file, "mul {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Div => {
                    writeln!(ctx.file, "div {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Eq => {
                    writeln!(ctx.file, "sub {}, {}, {}", T6, op1, op2)?;
                    writeln!(ctx.file, "seqz {}, {}", res, T6)?;
                }
                BinaryOp::Ne => {
                    writeln!(ctx.file, "sub {}, {}, {}", T6, op1, op2)?;
                    writeln!(ctx.file, "snez {}, {}", res, T6)?;
                }
                BinaryOp::Lt => {
                    writeln!(ctx.file, "slt {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Le => {
                    let op2 = emit_operand_as_reg(op2, T5, ctx)?;
                    writeln!(ctx.file, "sgt {}, {}, {}", res, op1, op2)?;
                    writeln!(ctx.file, "xori {0}, {0}, 1", res)?;
                }
                BinaryOp::Gt => {
                    let op2 = emit_operand_as_reg(op2, T5, ctx)?;
                    writeln!(ctx.file, "sgt {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Ge => {
                    writeln!(ctx.file, "slt {}, {}, {}", res, op1, op2)?;
                    writeln!(ctx.file, "xori {0}, {0}, 1", res)?;
                }
            };
            Some(Operand::Reg(T6))
        }
        OpArg::Call { fn_val, args } => emit_call(fn_val, args, ctx)?,
        OpArg::LoadArr { .. } => todo!("riscv array"),
        OpArg::StoreArr { .. } => todo!("riscv array"),
    };
    if let (Some(result), Some(val)) = (quaruple.result, val) {
        match ctx.reg_alloc[&result] {
            AllocReg::Reg(reg) => {
                emit_operand_to_reg(val, reg, ctx)?;
            }
            AllocReg::Stack(offset, size) => {
                emit_store_operand(val, (offset, size), ctx)?;
            }
        }
    }

    Ok(())
}

fn emit_branch(branch_op: BranchOp, name: &str, ctx: &mut Context) -> anyhow::Result<()> {
    match branch_op {
        BranchOp::Ret(v) => {
            if let Some(v) = v {
                let val = ir_into_operand(v, ctx);
                emit_operand_to_reg(val, A0, ctx)?;
            }
            writeln!(ctx.file, "j .exit{}", name)?;
        }
        BranchOp::Goto(label) => {
            writeln!(ctx.file, "j .{}", label)?;
        }
        BranchOp::CondGoto(val, label_true, label_false) => {
            let val = ir_into_operand(val, ctx);
            let cond = emit_load_operand(val, T6, ctx)?;
            writeln!(ctx.file, "beqz {}, .{}", cond, label_false)?;
            writeln!(ctx.file, "j .{}", label_true)?;
        }
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
            //dbg!(quaruple.to_string());
            emit_quaruple(quaruple, ctx)?;
        }
        //dbg!(ir_block.exit.to_string());
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

mod reg_alloc;

use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    io::Write,
};

use crate::{
    backend::riscv32::reg_alloc::{
        emit_input_reg_alloc, emit_reg_alloc, emit_reg_alloc_tmp, emit_spill_all,
    },
    context::Context as QContext,
    ir::{self, BranchOp, IrGraph, Quaruple},
    opt,
    sym_table::{SymTable, Symbol},
    ty::Ty,
};

use self::{reg_alloc::LocalRegAllocator, RiscVReg::*};

struct Context<'a> {
    sym_table: SymTable,
    file: &'a mut dyn Write,

    frame_size: i32,
    /// 给Symbol分配的栈上地址
    stack_alloc: HashMap<Symbol, Stack>,
    reg_allocator: LocalRegAllocator,
}

impl<'a> Context<'a> {
    fn new(file: &'a mut dyn Write, ctx: QContext) -> Self {
        Context {
            sym_table: ctx.sym_table,
            file,
            frame_size: 0,
            stack_alloc: HashMap::new(),
            reg_allocator: Default::default(),
        }
    }
}

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum RiscVReg {
    A0, A1, A2, A3, A4, A5, A6, A7,
    T0, T1, T2, T3, T4, T5, T6,
}

const ARG_REGS: &[RiscVReg] = &[A0, A1, A2, A3, A4, A5, A6, A7];
const TMP_REGS: &[RiscVReg] = &[T0, T1, T2, T3, T4, T5, T6];

impl Display for RiscVReg {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let name = format!("{:?}", self);
        write!(fmt, "{}", name.to_ascii_lowercase())
    }
}

#[derive(Debug, Copy, Clone)]
struct Stack {
    offset: i32,
    size: u32,
}

impl Stack {
    fn new(offset: i32, size: u32) -> Self {
        Self { offset, size }
    }
}

#[derive(Debug, Copy, Clone)]
enum Operand {
    Reg(RiscVReg),
    Const(i32),
}

impl Operand {
    fn into_reg(self) -> Option<RiscVReg> {
        match self {
            Operand::Reg(reg) => Some(reg),
            _ => None,
        }
    }

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

/// Convert operand to Reg
fn emit_operand_as_reg(val: Operand, ctx: &mut Context) -> anyhow::Result<RiscVReg> {
    match val {
        Operand::Reg(r) => Ok(r),
        Operand::Const(v) => {
            let t1 = emit_reg_alloc_tmp(ctx)?;
            writeln!(ctx.file, "li {}, {}", t1, v)?;
            Ok(t1)
        }
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
        Operand::Const(v) => {
            writeln!(ctx.file, "li {}, {}", target, v)?;
        }
    }
    Ok(())
}

/// load operand to Reg
fn emit_load_stack(val: Stack, target: RiscVReg, ctx: &mut Context) -> anyhow::Result<RiscVReg> {
    let s = size_suffix(val.size);
    writeln!(ctx.file, "l{} {}, {}(sp)", s, target, val.offset)?;
    Ok(target)
}

/// Store operand to specific stack position.
fn emit_store_operand(val: Operand, stack: Stack, ctx: &mut Context, free: bool) -> anyhow::Result<()> {
    let reg = emit_operand_as_reg(val, ctx)?;
    let s = size_suffix(stack.size);
    writeln!(ctx.file, "s{} {}, {}(sp)", s, reg, stack.offset)?;
    if free {
        ctx.reg_allocator.reg_free(reg);
    }
    Ok(())
}

fn emit_ir_into_operand(val: ir::Value, ctx: &mut Context) -> anyhow::Result<Operand> {
    use ir::Value::*;

    let val = match val {
        Reg(r) => {
            let reg = emit_reg_alloc(r, ctx)?;
            Operand::Reg(reg)
        }
        v @ (Int(_) | Bool(_)) => {
            let v = extract_const_val(v);
            Operand::Const(v)
        }
    };
    Ok(val)
}

fn emit_sub(res: RiscVReg, op1: RiscVReg, op2: Operand, ctx: &mut Context) -> anyhow::Result<()> {
    if let Some(val) = op2.into_const() {
        writeln!(ctx.file, "addi {}, {}, {}", res, op1, -val)?;
    } else {
        writeln!(ctx.file, "sub {}, {}, {}", res, op1, op2)?;
    }
    Ok(())
}

fn emit_call(
    fn_sym: Symbol,
    args: Vec<ir::Value>,
    ctx: &mut Context,
) -> anyhow::Result<Option<RiscVReg>> {
    let arg_cnt = args.len() as i32;
    // store arguments
    for (i, arg) in args.into_iter().enumerate() {
        let asm_value = emit_ir_into_operand(arg, ctx)?;
        if i < 8 {
            let target = ARG_REGS[i];
            emit_operand_to_reg(asm_value, target, ctx)?;
        } else {
            let target_offset = 4 * (i as i32 - arg_cnt);
            emit_store_operand(asm_value, Stack::new(target_offset, 4), ctx, true)?;
        };
        if let Some(reg) = asm_value.into_reg() {
            ctx.reg_allocator.reg_free(reg);
        }
    }
    emit_spill_all(ctx)?;
    if arg_cnt > 8 {
        writeln!(ctx.file, "addi sp, sp, {}", -(arg_cnt - 8) * 4)?;
    }
    let name = ctx.sym_table.name_of(fn_sym).unwrap();
    writeln!(ctx.file, "call {}", name)?;
    if arg_cnt > 8 {
        writeln!(ctx.file, "addi sp, sp, {}", (arg_cnt - 8) * 4)?;
    }
    let ret_ty = ctx.sym_table.ty_of(fn_sym).unwrap().fn_ret_ty();
    Ok((ret_ty != Ty::Void).then_some(A0))
}

fn emit_quaruple(quaruple: Quaruple, ctx: &mut Context) -> anyhow::Result<()> {
    use ir::{BinaryOp, OpArg, UnaryOp};

    let mut used_reg = vec![];

    match quaruple.op {
        OpArg::Arg(n) => {
            let target = if let Some(result) = quaruple.result {
                emit_input_reg_alloc(result, ctx)?
            } else {
                let reg = emit_reg_alloc_tmp(ctx)?;
                ctx.reg_allocator.reg_free(reg);
                reg
            };

            if n < 8 {
                writeln!(ctx.file, "mv {}, {}", target, ARG_REGS[n])?;
            } else {
                let n = n as i32;
                let offset = ctx.frame_size + (n - 8) * 4;
                let op = Stack::new(offset, 4);
                emit_load_stack(op, target, ctx)?;
            };
            used_reg.push(target);
        }
        OpArg::Unary { op, arg } => match op {
            UnaryOp::Const => {
                let target_op = emit_ir_into_operand(arg, ctx)?;
                let reg = emit_operand_as_reg(target_op, ctx)?;
                ctx.reg_allocator.reg_free(reg);
                if let Some(result) = quaruple.result {
                    ctx.reg_allocator.set_reg_as_input(reg, result);
                }
                used_reg.push(reg);
            }
        },
        OpArg::Binary { op, arg1, arg2 } => {
            let op2_should_be_reg = [BinaryOp::Mul, BinaryOp::Div, BinaryOp::Le, BinaryOp::Gt];
            if quaruple.result.is_none() {
                return Ok(());
            }
            let op1 = emit_ir_into_operand(arg1, ctx)?;
            let mut op2 = emit_ir_into_operand(arg2, ctx)?;
            let op1 = emit_operand_as_reg(op1, ctx)?;
            if op2_should_be_reg.contains(&op) {
                op2 = Operand::Reg(emit_operand_as_reg(op2, ctx)?);
            }
            ctx.reg_allocator.reg_free(op1);
            if let Some(reg) = op2.into_reg() {
                ctx.reg_allocator.reg_free(reg);
                used_reg.push(reg);
            }
            let res = emit_input_reg_alloc(quaruple.result.unwrap(), ctx)?;
            match op {
                BinaryOp::Add => {
                    writeln!(ctx.file, "add {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Sub => {
                    emit_sub(res, op1, op2, ctx)?;
                }
                BinaryOp::Mul => {
                    writeln!(ctx.file, "mul {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Div => {
                    writeln!(ctx.file, "div {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Eq => {
                    let t = emit_reg_alloc_tmp(ctx)?;
                    ctx.reg_allocator.reg_free(t);
                    emit_sub(t, op1, op2, ctx)?;
                    writeln!(ctx.file, "seqz {}, {}", res, t)?;
                }
                BinaryOp::Ne => {
                    let t = emit_reg_alloc_tmp(ctx)?;
                    ctx.reg_allocator.reg_free(t);
                    emit_sub(t, op1, op2, ctx)?;
                    writeln!(ctx.file, "snez {}, {}", res, t)?;
                }
                BinaryOp::Lt => {
                    writeln!(ctx.file, "slt {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Le => {
                    writeln!(ctx.file, "sgt {}, {}, {}", res, op1, op2)?;
                    writeln!(ctx.file, "xori {0}, {0}, 1", res)?;
                }
                BinaryOp::Gt => {
                    writeln!(ctx.file, "sgt {}, {}, {}", res, op1, op2)?;
                }
                BinaryOp::Ge => {
                    writeln!(ctx.file, "slt {}, {}, {}", res, op1, op2)?;
                    writeln!(ctx.file, "xori {0}, {0}, 1", res)?;
                }
            };
            used_reg.extend([res, op1]);
        }
        OpArg::Call { fn_val, args } => {
            let a0 = emit_call(fn_val, args, ctx)?;
            if let (Some(result), Some(A0)) = (quaruple.result, a0) {
                let res = emit_input_reg_alloc(result, ctx)?;
                writeln!(ctx.file, "mv {}, {}", res, A0)?;
                used_reg.push(res);
            }
        }
        OpArg::LoadArr { .. } => todo!("riscv array"),
        OpArg::StoreArr { .. } => todo!("riscv array"),
    }

    for reg in used_reg {
        ctx.reg_allocator.finish_read(reg);
    }

    Ok(())
}

fn emit_branch(branch_op: BranchOp, name: &str, ctx: &mut Context) -> anyhow::Result<()> {
    match branch_op {
        BranchOp::Ret(v) => {
            if let Some(v) = v {
                let val = emit_ir_into_operand(v, ctx)?;
                emit_operand_to_reg(val, A0, ctx)?;
            }
            writeln!(ctx.file, "j .exit{}", name)?;
        }
        BranchOp::Goto(label) => {
            emit_spill_all(ctx)?;
            writeln!(ctx.file, "j .{}", label)?;
        }
        BranchOp::CondGoto(val, label_true, label_false) => {
            let val = emit_ir_into_operand(val, ctx)?;
            let cond = emit_operand_as_reg(val, ctx)?;
            ctx.reg_allocator.reg_free(cond);
            emit_spill_all(ctx)?;
            writeln!(ctx.file, "beqz {}, .{}", cond, label_false)?;
            writeln!(ctx.file, "j .{}", label_true)?;
        }
    }
    Ok(())
}

/// 分配寄存器，确定栈帧结构
fn allocate_stack_frame(fn_sym: Symbol, ctx: &mut Context) {
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
        alloc.insert(local, Stack::new(offset, item_size));
        sp = offset + size;
    }
    ctx.stack_alloc = alloc;
    // saved ra at top of the frame
    sp += 4;
    // frame are 16-byte aligned
    ctx.frame_size = align_byte_up(sp, 16);
}

fn emit_function(mut ir_graph: IrGraph, fn_sym: Symbol, ctx: &mut Context) -> anyhow::Result<()> {
    let name = ctx.sym_table.name_of(fn_sym).unwrap().to_owned();
    writeln!(ctx.file, ".globl {0}\n{0}:", name)?;
    allocate_stack_frame(fn_sym, ctx);

    // enter frame
    writeln!(ctx.file, "addi sp, sp, {}", -ctx.frame_size)?;
    writeln!(ctx.file, "sw ra, {}(sp)", ctx.frame_size - 4)?;

    let non_locals = opt::find_block_nonlocal(&ir_graph);

    for &label in &ir_graph.block_order {
        let ir_block = ir_graph.blocks.remove(&label).unwrap();
        let used_list = opt::next_use_pos(&ir_block, &non_locals);
        ctx.reg_allocator = LocalRegAllocator::new(used_list, Vec::from(TMP_REGS));
        writeln!(ctx.file, ".{}:", label)?;
        for quaruple in ir_block.ir_list {
            //dbg!(quaruple.to_string());
            //dbg!(&ctx.reg_allocator);
            emit_quaruple(quaruple, ctx)?;
            ctx.reg_allocator.next_line();
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

use std::fmt::{Display, Formatter};

use itertools::Itertools;

use crate::{
    context::Context,
    ir::{self, Ir, IrVec, Value},
    sym_table::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Int,
    Bool,
    Array(Box<Ty>, i32),
    Void,
    Fn(Vec<Ty>, Box<Ty>),
}

impl Display for Ty {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Int => write!(fmt, "int"),
            Ty::Bool => write!(fmt, "bool"),
            Ty::Array(ty, cnt) => write!(fmt, "{}[{}]", ty, cnt),
            Ty::Void => write!(fmt, "void"),
            Ty::Fn(param, ret) => {
                write!(fmt, "{}({})", ret, param.iter().join(", "))
            }
        }
    }
}

impl Ty {
    fn fun_ret_ty(&self) -> Option<Ty> {
        match self {
            Ty::Fn(_, ret_ty) => Some(*ret_ty.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Op {
    Binary(ir::BinaryOp),
    Unary(ir::UnaryOp),
    Cond,
    Call,
}

impl From<ir::UnaryOp> for Op {
    fn from(op: ir::UnaryOp) -> Self {
        Op::Unary(op)
    }
}

impl From<ir::BinaryOp> for Op {
    fn from(op: ir::BinaryOp) -> Self {
        Op::Binary(op)
    }
}

/// 检查 `args` 是否相容，如果相容，返回结果类型
fn ty_check_op(op: Op, args: &[Value], ctx: &Context) -> Ty {
    use ir::{BinaryOp, BinaryOp::*, UnaryOp::*};
    let args: Vec<_> = args.iter().map(|v| ty_from_value(*v, ctx)).collect();
    match (op, args.as_slice()) {
        // assign operator: t -> t
        (Op::Unary(Const), [ty]) => ty.clone(),
        // arithmetic operator: (int, int) -> int
        (Op::Binary(Add | Sub | Mul | Div), &[Ty::Int, Ty::Int]) => Ty::Int,
        // comparison operator: (int, int) -> bool
        (Op::Binary(Lt | Le | Gt | Ge), &[Ty::Int, Ty::Int]) => Ty::Bool,
        // equality operator: (t, t) -> bool
        (Op::Binary(BinaryOp::Eq | Ne), [ty_l, ty_r]) if ty_l == ty_r => Ty::Bool,
        // if bool
        (Op::Cond, &[Ty::Bool]) => Ty::Bool,
        // fn(args...) -> ret
        (Op::Call, &[Ty::Fn(ref arg_ty, ref ret_ty), ref tys @ ..]) if arg_ty == tys => {
            *ret_ty.clone()
        }
        _ => panic!("{:?} are not compatible with {:?}", args, op),
    }
}

fn ty_from_value(value: Value, ctx: &Context) -> Ty {
    match value {
        Value::Reg(r) => ctx.sym_table.ty_of(r).unwrap(),
        Value::Int(_) => Ty::Int,
        Value::Bool(_) => Ty::Bool,
    }
}

/// 执行类型检查
pub fn ty_check(fn_sym: Symbol, ir_vec: &IrVec, ctx: &mut Context) {
    use ir::{BranchOp, OpArg};

    for ir in &ir_vec.ir_list {
        match ir {
            Ir::Quaruple(quaruple) => {
                // 先检查调用正确
                let result_ty = match quaruple.op {
                    OpArg::Arg(_) => continue, // checked in trans_items
                    OpArg::Unary { op, arg } => ty_check_op(op.into(), &[arg], ctx),
                    OpArg::Binary { op, arg1, arg2 } => ty_check_op(op.into(), &[arg1, arg2], ctx),
                    OpArg::Call { fn_val, ref args } => {
                        ty_check_op(Op::Call, &[&[fn_val], args.as_slice()].concat(), ctx)
                    }
                };
                // 再检查返回值是否跟变量匹配
                if let Some(result) = quaruple.result {
                    ctx.sym_table.ty_assert(result, result_ty);
                }
            }
            Ir::Branch(BranchOp::CondGoto(value, ..)) => {
                ty_check_op(Op::Cond, &[*value], ctx);
            }
            Ir::Branch(BranchOp::Ret(value)) => {
                let ty = ty_from_value(*value, ctx);
                let ret_ty = ctx.sym_table.ty_of(fn_sym).unwrap().fun_ret_ty().unwrap();
                if ty != ret_ty {
                    panic!("expect to return {}, found {}", ret_ty, ty);
                }
            }
            _ => {} // Just pass
        }
    }
}

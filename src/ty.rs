use std::fmt::{Display, Formatter};

use itertools::Itertools;

use crate::{
    context::Context,
    ir::{self, Ir, IrVec, Value},
    log_and_exit,
    sym_table::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyBasic {
    Int,
    Bool,
    Array(Box<TyBasic>, u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Basic(TyBasic),
    Void,
    Fn(Vec<TyBasic>, Box<Ty>),
}

pub enum TyPat {
    The,
    ElemOf(Box<TyPat>, u32),
}

impl From<TyBasic> for Ty {
    fn from(ty: TyBasic) -> Self {
        Ty::Basic(ty)
    }
}

impl TyBasic {
    fn get_elem_ty_rank(&self) -> (TyBasic, usize) {
        match self {
            TyBasic::Int => (TyBasic::Int, 0),
            TyBasic::Bool => (TyBasic::Bool, 0),
            TyBasic::Array(ty, _) => {
                let (elem_ty, n) = ty.get_elem_ty_rank();
                (elem_ty, n + 1)
            }
        }
    }
}

impl Display for TyBasic {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TyBasic::Int => write!(fmt, "int"),
            TyBasic::Bool => write!(fmt, "bool"),
            TyBasic::Array(ty, cnt) => write!(fmt, "{}[{}]", ty, cnt),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Basic(ty) => write!(fmt, "{}", ty),
            Ty::Void => write!(fmt, "void"),
            Ty::Fn(param, ret) => {
                write!(fmt, "{}({})", ret, param.iter().join(", "))
            }
        }
    }
}

impl PartialEq<TyBasic> for Ty {
    fn eq(&self, other: &TyBasic) -> bool {
        match self {
            Ty::Basic(s) => s == other,
            _ => false,
        }
    }
}

impl Ty {
    pub fn fun_ret_ty(&self) -> Option<Ty> {
        match self {
            Ty::Fn(_, ret_ty) => Some(*ret_ty.clone()),
            _ => None,
        }
    }

    pub fn into_ty_basic(self) -> Option<TyBasic> {
        match self {
            Ty::Basic(ty) => Some(ty),
            _ => None,
        }
    }
}

impl TyPat {
    pub fn match_ty(self, ty: TyBasic) -> TyBasic {
        match self {
            TyPat::The => ty,
            TyPat::ElemOf(pat, n) => pat.match_ty(TyBasic::Array(Box::new(ty), n)),
        }
    }
}

#[test]
fn ty_pat() {
    use TyPat::*;

    let pat = The;
    assert_eq!(pat.match_ty(TyBasic::Bool), TyBasic::Bool);

    let pat = ElemOf(Box::new(ElemOf(Box::new(The), 3)), 5);
    assert_eq!(
        pat.match_ty(TyBasic::Int),
        TyBasic::Array(Box::new(TyBasic::Array(Box::new(TyBasic::Int), 5)), 3)
    );
}

#[derive(Debug, Copy, Clone)]
enum Op {
    Binary(ir::BinaryOp),
    Unary(ir::UnaryOp),
    Cond,
    Call,
    Index,
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
        (
            Op::Binary(Add | Sub | Mul | Div),
            &[Ty::Basic(TyBasic::Int), Ty::Basic(TyBasic::Int)],
        ) => TyBasic::Int.into(),
        // comparison operator: (int, int) -> bool
        (Op::Binary(Lt | Le | Gt | Ge), &[Ty::Basic(TyBasic::Int), Ty::Basic(TyBasic::Int)]) => {
            TyBasic::Bool.into()
        }
        // equality operator: (t, t) -> bool
        (Op::Binary(BinaryOp::Eq | Ne), [ty_l, ty_r]) if ty_l == ty_r => TyBasic::Bool.into(),
        // if bool
        (Op::Cond, &[Ty::Basic(TyBasic::Bool)]) => TyBasic::Bool.into(),
        // fn(args...) -> ret
        (Op::Call, &[Ty::Fn(ref arg_ty, ref ret_ty), ref tys @ ..]) if tys == arg_ty => {
            *ret_ty.clone()
        }
        // arr[idx...]
        (Op::Index, &[Ty::Basic(ref arr_ty), ref idx_ty @ ..])
            if idx_ty.iter().all(|ty| ty == &Ty::Basic(TyBasic::Int)) =>
        {
            let (elem_ty, n) = arr_ty.get_elem_ty_rank();
            if idx_ty.len() != n {
                log_and_exit!("wrong array rank");
            }
            elem_ty.into()
        }
        _ => log_and_exit!("{:?} are not compatible with {:?}", args, op),
    }
}

fn ty_from_value(value: Value, ctx: &Context) -> Ty {
    match value {
        Value::Reg(r) => ctx.sym_table.ty_of(r).unwrap(),
        Value::Int(_) => TyBasic::Int.into(),
        Value::Bool(_) => TyBasic::Bool.into(),
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
                        ty_check_op(Op::Call, &[&[fn_val.into()], args.as_slice()].concat(), ctx)
                    }
                    OpArg::LoadArr { arr, ref idx } => {
                        ty_check_op(Op::Index, &[&[arr.into()], idx.as_slice()].concat(), ctx)
                    }
                    OpArg::StoreArr { arr, ref idx, val } => {
                        let ty =
                            ty_check_op(Op::Index, &[&[arr.into()], idx.as_slice()].concat(), ctx);
                        let val_ty = ty_from_value(val, ctx);
                        if ty != val_ty {
                            log_and_exit!("cannot store type {} into value of type {}", ty, val_ty);
                        }
                        Ty::Void
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
            Ir::Branch(BranchOp::Ret(Some(value))) => {
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

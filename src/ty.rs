use crate::{
    ast::Ty,
    context::Context,
    quaruple::{self, Quaruple, Value},
};

#[derive(Debug, Copy, Clone)]
enum Op {
    Binary(quaruple::BinaryOp),
    Unary(quaruple::UnaryOp),
}

impl From<quaruple::UnaryOp> for Op {
    fn from(op: quaruple::UnaryOp) -> Self {
        Op::Unary(op)
    }
}

impl From<quaruple::BinaryOp> for Op {
    fn from(op: quaruple::BinaryOp) -> Self {
        Op::Binary(op)
    }
}

/// 检查 `args` 是否相容，如果相容，返回结果类型
fn ty_check_op(op: Op, args: &[Ty]) -> Ty {
    use quaruple::{BinaryOp, BinaryOp::*, UnaryOp::*};
    match (op, args) {
        // assign operator: t -> t
        (Op::Unary(Assign), &[ty]) => ty,
        // arithmetic operator: (int, int) -> int
        (Op::Binary(Add | Sub | Mul | Div), &[Ty::Int, Ty::Int]) => Ty::Int,
        // comparison operator: (int, int) -> bool
        (Op::Binary(Lt | Le | Gt | Ge), &[Ty::Int, Ty::Int]) => Ty::Bool,
        // equality operator: (t, t) -> bool
        (Op::Binary(BinaryOp::Eq | Ne), &[ty_l, ty_r]) if ty_l == ty_r => Ty::Bool,
        // return int
        (Op::Unary(Ret), &[Ty::Int]) => Ty::Int,
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
pub fn ty_check(quaruples: &[Quaruple], ctx: &mut Context) {
    use quaruple::OpArg;

    for quaruple in quaruples {
        let result_ty = match &quaruple.op {
            &OpArg::Unary { op, arg } => ty_check_op(op.into(), &[ty_from_value(arg, ctx)]),
            &OpArg::Binary { op, arg1, arg2 } => ty_check_op(
                op.into(),
                &[ty_from_value(arg1, ctx), ty_from_value(arg2, ctx)],
            ),
        };
        if let Some(result) = quaruple.result {
            ctx.sym_table.ty_assert(result, result_ty);
        }
    }
}

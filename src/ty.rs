use crate::{
    ast::Ty,
    context::Context,
    quaruple::{self, Quaruple, Value},
};

#[derive(Debug, Copy, Clone)]
enum Op {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl From<quaruple::UnaryOp> for Op {
    fn from(op: quaruple::UnaryOp) -> Self {
        use quaruple::UnaryOp::*;

        match op {
            Assign => Op::Assign,
            Ret => Op::Assign,
        }
    }
}

impl From<quaruple::BinaryOp> for Op {
    fn from(op: quaruple::BinaryOp) -> Self {
        use quaruple::BinaryOp::*;

        match op {
            Add => Op::Add,
            Sub => Op::Sub,
            Mul => Op::Mul,
            Div => Op::Div,
            Eq => Op::Eq,
            Ne => Op::Ne,
            Lt => Op::Lt,
            Le => Op::Le,
            Gt => Op::Gt,
            Ge => Op::Ge,
        }
    }
}

/// 检查 `args` 是否相容，如果相容，返回结果类型
fn ty_check_op(op: Op, args: &[Ty]) -> Ty {
    todo!()
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

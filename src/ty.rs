use crate::{
    ast::Ty,
    context::Context,
    ir::{self, Ir, IrVec, Value},
};

#[derive(Debug, Copy, Clone)]
enum Op {
    Binary(ir::BinaryOp),
    Unary(ir::UnaryOp),
    Cond,
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
        (Op::Unary(Assign), &[ty]) => ty,
        // arithmetic operator: (int, int) -> int
        (Op::Binary(Add | Sub | Mul | Div), &[Ty::Int, Ty::Int]) => Ty::Int,
        // comparison operator: (int, int) -> bool
        (Op::Binary(Lt | Le | Gt | Ge), &[Ty::Int, Ty::Int]) => Ty::Bool,
        // equality operator: (t, t) -> bool
        (Op::Binary(BinaryOp::Eq | Ne), &[ty_l, ty_r]) if ty_l == ty_r => Ty::Bool,
        // if bool
        (Op::Cond, &[Ty::Bool]) => Ty::Bool,
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
pub fn ty_check(ir_vec: &IrVec, ctx: &mut Context) {
    use ir::{BranchOp, OpArg};

    for ir in &ir_vec.ir_list {
        match ir {
            Ir::Quaruple(quaruple) => {
                let result_ty = match quaruple.op {
                    OpArg::Unary { op, arg } => ty_check_op(op.into(), &[arg], ctx),
                    OpArg::Binary { op, arg1, arg2 } => ty_check_op(op.into(), &[arg1, arg2], ctx),
                    OpArg::Call { .. } => todo!("fun type check"),
                };
                if let Some(result) = quaruple.result {
                    ctx.sym_table.ty_assert(result, result_ty);
                }
            }
            Ir::Branch(BranchOp::CondGoto(value, ..)) => {
                ty_check_op(Op::Cond, &[*value], ctx);
            }
            _ => {} // Just pass
        }
    }
}

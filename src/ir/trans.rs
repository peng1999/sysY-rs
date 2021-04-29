use either::{Either, Left, Right};

use super::{BinaryOp, BranchOp, UnaryOp, Value};
use crate::{
    ast::{self, Expr},
    context::Context,
    ir::{IrVec, OpArg},
};

/// Return a `Right(Value)`, or `Left(Expr)` if `expr` is not a atom.
fn atom_to_value(expr: Expr, ctx: &mut Context) -> Either<Expr, Value> {
    match expr {
        Expr::Ident(name) => {
            let ident = ctx.sym_lookup_or_panic(name);
            Right(ident.into())
        }
        Expr::IntLit(v) => Right(Value::Int(v)),
        Expr::BoolLit(v) => Right(Value::Bool(v)),
        _ => Left(expr),
    }
}

fn trans_compond_expr(expr: Expr, ir_vec: &mut IrVec, ctx: &mut Context) -> OpArg {
    match expr {
        Expr::Binary(op, lhs, rhs) => {
            let lval = trans_expr_val(*lhs, ir_vec, ctx);
            let rval = trans_expr_val(*rhs, ir_vec, ctx);
            BinaryOp::from_ast_op(op).with_arg(lval, rval)
        }
        // Neg
        expr => unimplemented!("{:?}", expr),
    }
}

/// Translate a expr when result is unknown or unneeded.
fn trans_expr_place(expr: Expr, ir_vec: &mut IrVec, ctx: &mut Context) -> OpArg {
    match atom_to_value(expr, ctx) {
        Right(val) => UnaryOp::Assign.with_arg(val),
        Left(expr) => trans_compond_expr(expr, ir_vec, ctx),
    }
}

fn trans_expr_val(expr: Expr, ir_vec: &mut IrVec, ctx: &mut Context) -> Value {
    match atom_to_value(expr, ctx) {
        Right(val) => val,
        Left(expr) => {
            let result = ctx.sym_table.gen_const_symbol();
            let ir = trans_compond_expr(expr, ir_vec, ctx).with_result(Some(result));
            ir_vec.push(ir);
            Value::Reg(result)
        }
    }
}

fn trans_if_stmt(
    expr: ast::Expr,
    true_case: ast::Stmt,
    false_case: ast::Stmt,
    ir_vec: &mut IrVec,
    ctx: &mut Context,
) {
    let label_true = ctx.next_label();
    let label_false = ctx.next_label();
    let label_end = ctx.next_label();

    let val = trans_expr_val(expr, ir_vec, ctx);
    ir_vec.push(BranchOp::CondGoto(val, label_true, label_false));
    ir_vec.push(label_true);
    trans_stmt(true_case, ir_vec, ctx);
    ir_vec.push(BranchOp::Goto(label_end));
    ir_vec.push(label_false);
    trans_stmt(false_case, ir_vec, ctx);
    ir_vec.push(label_end);
}

fn trans_while_stmt(
    expr: ast::Expr,
    body: ast::Stmt,
    ir_vec: &mut IrVec,
    ctx: &mut Context,
) {
    let label_start = ctx.next_label();
    let label_body = ctx.next_label();
    let label_end = ctx.next_label();

    ir_vec.push(label_start);
    let val = trans_expr_val(expr, ir_vec, ctx);
    ir_vec.push(BranchOp::CondGoto(val, label_body, label_end));
    ir_vec.push(label_body);
    trans_stmt(body, ir_vec, ctx);
    ir_vec.push(BranchOp::Goto(label_start));
    ir_vec.push(label_end);
}

fn trans_stmt(stmt: ast::Stmt, ir_vec: &mut IrVec, ctx: &mut Context) {
    use ast::Stmt::*;

    match stmt {
        Decl(ty, name, expr) => {
            let arg = trans_expr_place(*expr, ir_vec, ctx);
            let ident = ctx.sym_insert(name).unwrap_or_else(|_| {
                let sym = ctx.interner.resolve(name).unwrap();
                panic!("name redefinition: {}", sym);
            });
            ir_vec.push(arg.with_result(Some(ident)));
            // 类型断言需要在有AST时进行处理
            ctx.sym_table.ty_assert(ident, ty);
        }
        Assign(name, expr) => match *name {
            ast::Expr::Ident(name) => {
                let arg = trans_expr_place(*expr, ir_vec, ctx);
                let ident = ctx.sym_lookup_or_panic(name);
                ir_vec.push(arg.with_result(Some(ident)));
            }
            _ => todo!(),
        },
        Expr(expr) => {
            trans_expr_place(expr, ir_vec, ctx);
        }
        Block(inner) => {
            ctx.sym_begin_scope();
            trans_stmts(inner, ir_vec, ctx);
            ctx.sym_end_scope();
        }
        Return(expr) => {
            let val = trans_expr_val(expr.unwrap(), ir_vec, ctx);
            ir_vec.push(BranchOp::Ret(val));
        }
        Empty => {}
        If(expr, true_case, false_case) => {
            trans_if_stmt(*expr, *true_case, *false_case, ir_vec, ctx);
        }
        While(expr, body) => {
            trans_while_stmt(*expr, *body, ir_vec, ctx);
        }
        _ => unimplemented!(),
    }
}

pub fn trans_stmts(stmts: Vec<ast::Stmt>, ir_vec: &mut IrVec, ctx: &mut Context) {
    for stmt in stmts {
        trans_stmt(stmt, ir_vec, ctx);
    }
}

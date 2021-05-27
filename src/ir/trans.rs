use either::{Either, Left, Right};

use super::{BinaryOp, BranchOp, UnaryOp, Value};
use crate::{
    ast::{Expr, ExprKind, FuncHead, Item, Stmt, Ty},
    context::Context,
    error::LogResult,
    ir::{IrVec, OpArg},
    sym_table::Symbol,
};

/// Return a `Right(Value)`, or `Left(Expr)` if `expr` is not a atom.
fn atom_to_value(expr: Expr, ctx: &mut Context) -> Either<Expr, Value> {
    match *expr {
        ExprKind::Ident(name) => {
            let ident = ctx.sym_lookup_or_panic(name, expr.span());
            Right(ident.into())
        }
        ExprKind::IntLit(v) => Right(Value::Int(v)),
        ExprKind::BoolLit(v) => Right(Value::Bool(v)),
        _ => Left(expr),
    }
}

fn trans_compond_expr(expr: Expr, ir_vec: &mut IrVec, ctx: &mut Context) -> OpArg {
    match expr.into_inner() {
        ExprKind::Binary(op, lhs, rhs) => {
            let lval = trans_expr_val(lhs, ir_vec, ctx);
            let rval = trans_expr_val(rhs, ir_vec, ctx);
            BinaryOp::from_ast_op(op).with_arg(lval, rval)
        }
        ExprKind::Call(fun, args) => {
            let fun_val = trans_expr_val(fun, ir_vec, ctx);
            let args_val = args
                .into_iter()
                .map(|e| trans_expr_val(e, ir_vec, ctx))
                .collect();
            OpArg::call(fun_val, args_val)
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
    expr: Expr,
    true_case: Stmt,
    false_case: Stmt,
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

fn trans_while_stmt(expr: Expr, body: Stmt, ir_vec: &mut IrVec, ctx: &mut Context) {
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

fn trans_stmt(stmt: Stmt, ir_vec: &mut IrVec, ctx: &mut Context) {
    use Stmt::*;

    match stmt {
        Decl(ty, name, expr) => {
            let arg = trans_expr_place(expr, ir_vec, ctx);
            let ident = ctx.sym_insert(name).unwrap_or_log(ctx);
            ir_vec.push(arg.with_result(Some(ident)));
            // 类型断言需要在有AST时进行处理
            ctx.sym_table.ty_assert(ident, ty);
        }
        Assign(name, expr) => match *name {
            ExprKind::Ident(sym) => {
                let arg = trans_expr_place(expr, ir_vec, ctx);
                let ident = ctx.sym_lookup_or_panic(sym, name.span());
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
            trans_if_stmt(expr, *true_case, *false_case, ir_vec, ctx);
        }
        While(expr, body) => {
            trans_while_stmt(expr, *body, ir_vec, ctx);
        }
        _ => unimplemented!(),
    }
}

fn trans_stmts(stmts: Vec<Stmt>, ir_vec: &mut IrVec, ctx: &mut Context) {
    for stmt in stmts {
        trans_stmt(stmt, ir_vec, ctx);
    }
}

fn register_func(func_head: FuncHead, ctx: &mut Context) -> Symbol {
    let name = ctx.interner.resolve(func_head.name).unwrap().to_string();
    let param_ty = func_head.param.into_iter().map(|(ty, _)| ty).collect();
    let fun_ty = Ty::Fun(param_ty, func_head.ret_ty.map(Box::new));

    let fun_sym = ctx.sym_insert_const(func_head.name).unwrap_or_log(ctx);
    ctx.sym_table.ty_assert_with_name(fun_sym, fun_ty, name);

    fun_sym
}

pub fn trans_items(items: Vec<Item>, ctx: &mut Context) -> Vec<(Symbol, IrVec)> {
    let mut fun_ir = Vec::with_capacity(items.len());
    // 进入全局作用域
    ctx.sym_begin_scope();

    for item in items {
        match item {
            Item::FuncDef(func_head, stmts) => {
                let param = func_head.param.clone();
                let fun_sym = register_func(func_head, ctx);

                let mut ir_vec = IrVec::new(ctx.next_label());
                ctx.sym_begin_scope();
                for (ty, name) in param {
                    let sym = ctx.sym_insert(name).unwrap_or_log(ctx);
                    ctx.sym_table.ty_assert(sym, ty);
                }
                trans_stmts(stmts, &mut ir_vec, ctx);
                ctx.sym_end_scope();

                fun_ir.push((fun_sym, ir_vec));
            }
            Item::FuncDecl(func_head) => {
                register_func(func_head, ctx);
            }
        }
    }
    fun_ir
}

use either::{Either, Left, Right};

use crate::{
    ast::{Expr, ExprKind, FnHead, Item, Stmt},
    context::Context,
    error::LogResult,
    ir::{IrVec, OpArg},
    sym_table::Symbol,
    ty::Ty,
};

use super::{BinaryOp, BranchOp, UnaryOp, Value};

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
        ExprKind::Call(fn_sym, args) => {
            let fun_val = trans_expr_val(fn_sym, ir_vec, ctx).into_reg().unwrap();
            let args_val = args
                .into_iter()
                .map(|e| trans_expr_val(e, ir_vec, ctx))
                .collect();
            OpArg::call(fun_val, args_val)
        }
        ExprKind::Index(arr, idx) => {
            let arr_val = trans_expr_val(arr, ir_vec, ctx).into_reg().unwrap();
            let idx_val = idx
                .into_iter()
                .map(|e| trans_expr_val(e, ir_vec, ctx))
                .collect();
            OpArg::load_arr(arr_val, idx_val)
        }
        // Neg
        expr => unimplemented!("{:?}", expr),
    }
}

/// Translate a expr when result is unknown or unneeded.
#[must_use]
fn trans_expr_place(expr: Expr, ir_vec: &mut IrVec, ctx: &mut Context) -> OpArg {
    match atom_to_value(expr, ctx) {
        Right(val) => UnaryOp::Const.with_arg(val),
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
            let ident = if let Some(expr) = expr {
                let arg = trans_expr_place(expr, ir_vec, ctx);
                let ident = ctx.sym_insert(name).unwrap_or_log(ctx);
                ir_vec.push(arg.with_result(Some(ident)));
                ident
            } else {
                ctx.sym_insert(name).unwrap_or_log(ctx)
            };
            // 类型断言需要在有AST时进行处理
            ctx.sym_table.ty_assert(ident, ty);
        }
        Assign(name, expr) => {
            let name_span = name.span();
            match name.into_inner() {
                ExprKind::Ident(sym) => {
                    let arg = trans_expr_place(expr, ir_vec, ctx);
                    let ident = ctx.sym_lookup_or_panic(sym, name_span);
                    ir_vec.push(arg.with_result(Some(ident)));
                }
                ExprKind::Index(arr, idx) => {
                    let val = trans_expr_val(expr, ir_vec, ctx);
                    let ident = trans_expr_val(arr, ir_vec, ctx).into_reg().unwrap();
                    let idx_val = idx
                        .into_iter()
                        .map(|e| trans_expr_val(e, ir_vec, ctx))
                        .collect();
                    ir_vec.push(OpArg::store_arr(ident, idx_val, val).with_result(None));
                }
                _ => todo!(),
            }
        }
        Expr(expr) => {
            let arg = trans_expr_place(expr, ir_vec, ctx);
            ir_vec.push(arg.with_result(None));
        }
        Block(inner) => {
            ctx.sym_begin_scope();
            trans_stmts(inner, ir_vec, ctx);
            ctx.sym_end_scope();
        }
        Return(expr) => {
            let val = trans_expr_val(expr.unwrap(), ir_vec, ctx);
            ir_vec.push(BranchOp::Ret(Some(val)));
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

fn register_fn(fn_head: FnHead, ctx: &mut Context) -> Symbol {
    let name = ctx.interner.resolve(fn_head.name).unwrap().to_string();
    let param_ty = fn_head.param.into_iter().map(|(ty, _)| ty).collect();
    let fn_ty = Ty::Fn(param_ty, Box::new(fn_head.ret_ty));

    let fun_sym = ctx
        .sym_insert_const(fn_head.name)
        .unwrap_or_else(|e| e.get_sym());
    ctx.sym_table.ty_assert_with_name(fun_sym, fn_ty, name);

    fun_sym
}

pub fn trans_items(items: Vec<Item>, ctx: &mut Context) -> Vec<(Symbol, Option<IrVec>)> {
    // 进入全局作用域
    ctx.sym_begin_scope();

    items
        .into_iter()
        .map(|item| match item {
            Item::FnDef(fn_head, stmts) => {
                let param = fn_head.param.clone();
                let fn_sym = register_fn(fn_head, ctx);

                let mut ir_vec = IrVec::new(ctx.next_label());
                ctx.sym_table.set_current_fn(fn_sym);
                ctx.sym_begin_scope();
                // Arg 指令必须在函数的开头
                for (n, (ty, name)) in param.into_iter().enumerate() {
                    let sym = ctx.sym_insert(name).unwrap_or_log(ctx);
                    ctx.sym_table.ty_assert(sym, ty);
                    ir_vec.push(OpArg::Arg(n).with_result(Some(sym)));
                }
                trans_stmts(stmts, &mut ir_vec, ctx);
                let ret_ty = ctx.sym_table.ty_of(fn_sym).unwrap().fn_ret_ty();
                if ret_ty == Ty::Void {
                    ir_vec.push(BranchOp::Ret(None));
                }
                ctx.sym_end_scope();
                ctx.sym_table.clear_current_fn();

                (fn_sym, Some(ir_vec))
            }
            Item::FnDecl(fn_head) => {
                let fn_sym = register_fn(fn_head, ctx);
                (fn_sym, None)
            }
        })
        .collect()
}

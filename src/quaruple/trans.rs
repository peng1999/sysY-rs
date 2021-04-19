use either::{Either, Left, Right};

use super::{BinaryOp, Quaruple, UnaryOp, Value, Ident};
use crate::{
    ast::{self, Expr},
    context::Context,
};

fn atom_to_value(expr: Expr, ctx: &mut Context) -> Either<Value, Expr> {
    match expr {
        Expr::Ident(name) => {
            let ident = ctx.sym_lookup_or_panic(name);
            Left(ident.into())
        }
        Expr::IntLit(v) => Left(Value::Int(v)),
        _ => Right(expr),
    }
}

fn trans_compond_expr(
    expr: Expr,
    result: Option<Ident>,
    quaruples: &mut Vec<Quaruple>,
    ctx: &mut Context,
) {
    match expr {
        Expr::Binary(op, lhs, rhs) => {
            let lval = trans_expr_val(*lhs, quaruples, ctx);
            let rval = trans_expr_val(*rhs, quaruples, ctx);
            quaruples.push(Quaruple {
                result,
                op: BinaryOp::from_ast_op(op).with_arg(lval, rval),
            })
        }
        _ => unimplemented!(),
    }
}

fn trans_expr_place(expr: Expr, quaruples: &mut Vec<Quaruple>, ctx: &mut Context) {
    match atom_to_value(expr, ctx) {
        Left(val) => quaruples.push(Quaruple {
            result: None,
            op: UnaryOp::Assign.with_arg(val),
        }),
        Right(expr) => trans_compond_expr(expr, None, quaruples, ctx),
    }
}

fn trans_expr_val(expr: Expr, quaruples: &mut Vec<Quaruple>, ctx: &mut Context) -> Value {
    match atom_to_value(expr, ctx) {
        Left(val) => val,
        Right(expr) => {
            let result = ctx.id.get_next_id();
            trans_compond_expr(expr, Some(result), quaruples, ctx);
            Value::Reg(result)
        }
    }
}

pub fn trans_stmts(stmts: Vec<ast::Stmt>, quaruples: &mut Vec<Quaruple>, ctx: &mut Context) {
    use ast::Stmt::*;

    for stmt in stmts {
        match stmt {
            Decl(_, name, expr) => {
                trans_expr_place(*expr, quaruples, ctx);
                let ident = ctx.sym_insert(name).unwrap_or_else(|_| {
                    let sym = ctx.interner.resolve(name).unwrap();
                    panic!("name redefinition: {}", sym);
                });
                quaruples.last_mut().unwrap().result = Some(ident);
            }
            Assign(name, expr) => match *name {
                ast::Expr::Ident(name) => {
                    trans_expr_place(*expr, quaruples, ctx);
                    let ident = ctx.sym_lookup_or_panic(name);
                    quaruples.last_mut().unwrap().result = Some(ident);
                }
                _ => todo!(),
            },
            Expr(expr) => {
                trans_expr_place(expr, quaruples, ctx);
            }
            Block(inner) => {
                ctx.sym_begin_scope();
                trans_stmts(inner, quaruples, ctx);
                ctx.sym_end_scope();
            }
            Return(expr) => {
                let val = trans_expr_val(expr.unwrap(), quaruples, ctx);
                quaruples.push(Quaruple {
                    result: None,
                    op: UnaryOp::Ret.with_arg(val),
                });
            }
            _ => unimplemented!(),
        }
    }
}

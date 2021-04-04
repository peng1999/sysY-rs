use crate::{
    ast,
    context::{Context, Ident},
};

#[derive(Debug)]
pub struct Quaruple {
    result: Option<Reg>,
    op: OpArg,
}

#[derive(Debug, Clone, Copy)]
pub struct Reg {
    sym: Ident,
    is_const: bool,
}

#[derive(Debug)]
pub enum Value {
    Reg(Reg),
    Int(i32),
}

#[derive(Debug)]
pub enum OpArg {
    Unary {
        op: UnaryOp,
        arg: Value,
    },
    Binary {
        op: BinaryOp,
        arg1: Value,
        arg2: Value,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Assign,
    Ret,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl Ident {
    fn to_value(&self, is_const: bool) -> Value {
        Value::Reg(Reg {
            sym: *self,
            is_const,
        })
    }
}

impl Reg {
    fn new(sym: Ident, is_const: bool) -> Reg {
        Reg { sym, is_const }
    }
}

impl UnaryOp {
    fn with_arg(self, arg: Value) -> OpArg {
        OpArg::Unary { op: self, arg }
    }
}

impl BinaryOp {
    fn from_ast_op(op: ast::BinOp) -> BinaryOp {
        match op {
            ast::BinOp::Add => BinaryOp::Add,
            ast::BinOp::Sub => BinaryOp::Sub,
            ast::BinOp::Mul => BinaryOp::Mul,
            ast::BinOp::Div => BinaryOp::Div,
            _ => unimplemented!(),
        }
    }
    fn with_arg(self, arg1: Value, arg2: Value) -> OpArg {
        OpArg::Binary {
            op: self,
            arg1,
            arg2,
        }
    }
}

fn trans_expr(
    expr: ast::Expr,
    gen_tmp: bool,
    quaruples: &mut Vec<Quaruple>,
    ctx: &mut Context,
) -> Option<Value> {
    use ast::Expr;

    if let Expr::Ident(name) = expr {
        let ident = ctx.sym_lookup_or_panic(name);
        if !gen_tmp {
            quaruples.push(Quaruple {
                result: None,
                op: UnaryOp::Assign.with_arg(ident.to_value(false)),
            });
            return None;
        } else {
            return Some(ident.to_value(false));
        }
    }

    if let Expr::Lit(val) = expr {
        if !gen_tmp {
            quaruples.push(Quaruple {
                result: None,
                op: UnaryOp::Assign.with_arg(Value::Int(val)),
            });
            return None;
        } else {
            return Some(Value::Int(val));
        }
    }

    let result = gen_tmp
        .then(|| Reg::new(ctx.id.get_next_id(), true));

    match expr {
        Expr::Binary(op, lhs, rhs) => {
            let lval = trans_expr(*lhs, true, quaruples, ctx);
            let rval = trans_expr(*rhs, true, quaruples, ctx);
            quaruples.push(Quaruple {
                result,
                op: BinaryOp::from_ast_op(op).with_arg(lval.unwrap(), rval.unwrap()),
            })
        }
        _ => unimplemented!(),
    }

    result.map(|r| Value::Reg(r))
}

pub fn trans_stmts(stmts: Vec<ast::Stmt>, quaruples: &mut Vec<Quaruple>, ctx: &mut Context) {
    use ast::{Expr, Stmt::*};

    for stmt in stmts {
        match stmt {
            Decl(_, name, expr) => {
                trans_expr(*expr, false, quaruples, ctx);
                let ident = ctx.sym_insert(name).unwrap_or_else(|_| {
                    let sym = ctx.interner.resolve(name).unwrap();
                    panic!("name redefinition: {}", sym);
                });
                quaruples.last_mut().unwrap().result = Some(Reg::new(ident, false));
            }
            Assign(name, expr) => match *name {
                Expr::Ident(name) => {
                    trans_expr(*expr, false, quaruples, ctx);
                    let ident = ctx.sym_lookup_or_panic(name);
                    quaruples.last_mut().unwrap().result = Some(Reg::new(ident, false));
                }
                _ => todo!(),
            },
            Expr(expr) => {
                trans_expr(expr, false, quaruples, ctx);
            }
            Block(inner) => {
                ctx.sym_begin_scope();
                trans_stmts(inner, quaruples, ctx);
                ctx.sym_end_scope();
            }
            _ => unimplemented!(),
        }
    }
}

mod display;
mod trans;

use crate::{ast, context::Ident};

pub use trans::trans_stmts;

#[derive(Debug)]
pub struct Quaruple {
    pub result: Option<Ident>,
    pub op: OpArg,
}

#[derive(Debug)]
pub enum Value {
    Reg(Ident),
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

impl From<Ident> for Value {
    fn from(ident: Ident) -> Value {
        Value::Reg(ident)
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

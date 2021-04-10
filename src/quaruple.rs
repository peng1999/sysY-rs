mod display;
mod trans;

use crate::{ast, context::Ident};

pub use trans::trans_stmts;

#[derive(Debug)]
pub struct Quaruple {
    pub result: Option<Reg>,
    pub op: OpArg,
}

#[derive(Debug, Clone, Copy)]
pub struct Reg {
    pub sym: Ident,
    pub is_const: bool,
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

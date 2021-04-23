mod display;
mod trans;

use crate::{ast, sym_table::Symbol};

pub use trans::trans_stmts;

#[derive(Debug)]
pub struct Quaruple {
    pub result: Option<Symbol>,
    pub op: OpArg,
}

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Reg(Symbol),
    Int(i32),
    Bool(bool),
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
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl From<Symbol> for Value {
    fn from(ident: Symbol) -> Value {
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
            ast::BinOp::Eq => BinaryOp::Eq,
            ast::BinOp::Ne => BinaryOp::Ne,
            ast::BinOp::Lt => BinaryOp::Lt,
            ast::BinOp::Le => BinaryOp::Le,
            ast::BinOp::Gt => BinaryOp::Gt,
            ast::BinOp::Ge => BinaryOp::Ge,
            _ => todo!(),
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

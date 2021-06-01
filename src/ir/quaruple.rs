use super::Value;
use crate::{ast, sym_table::Symbol};

#[derive(Debug)]
pub struct Quaruple {
    pub result: Option<Symbol>,
    pub op: OpArg,
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
    Call {
        fun: Value,
        args: Vec<Value>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Const,
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

impl OpArg {
    pub fn call(fun: Value, args: Vec<Value>) -> OpArg {
        OpArg::Call { fun, args }
    }
    pub fn with_result(self, result: Option<Symbol>) -> Quaruple {
        Quaruple { result, op: self }
    }
}

impl UnaryOp {
    pub fn with_arg(self, arg: Value) -> OpArg {
        OpArg::Unary { op: self, arg }
    }
}

impl BinaryOp {
    pub fn from_ast_op(op: ast::BinOp) -> BinaryOp {
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
    pub fn with_arg(self, arg1: Value, arg2: Value) -> OpArg {
        OpArg::Binary {
            op: self,
            arg1,
            arg2,
        }
    }
}

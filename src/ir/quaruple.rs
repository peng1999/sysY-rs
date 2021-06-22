use super::Value;
use crate::{ast, sym_table::Symbol};

#[derive(Debug)]
pub struct Quaruple {
    pub result: Option<Symbol>,
    pub op: OpArg,
}

#[derive(Debug)]
pub enum OpArg {
    Arg(usize),
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
        fn_val: Symbol,
        args: Vec<Value>,
    },
    LoadArr {
        arr: Symbol,
        idx: Vec<Value>,
    },
    StoreArr {
        arr: Symbol,
        idx: Vec<Value>,
        val: Value,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Const,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
    pub fn call(fn_val: Symbol, args: Vec<Value>) -> OpArg {
        OpArg::Call { fn_val, args }
    }

    pub fn store_arr(arr: Symbol, idx: Vec<Value>, val: Value) -> OpArg {
        OpArg::StoreArr { arr, idx, val }
    }

    pub fn load_arr(arr: Symbol, idx: Vec<Value>) -> OpArg {
        OpArg::LoadArr { arr, idx }
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
            ast::BinOp::And | ast::BinOp::Or => panic!(),
            _ => todo!("rem"),
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

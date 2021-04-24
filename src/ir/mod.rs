mod display;
mod quaruple;
mod trans;

use std::collections::HashMap;

use crate::sym_table::Symbol;

pub use quaruple::{BinaryOp, OpArg, Quaruple, UnaryOp};
pub use trans::trans_stmts;

pub struct IrVec {
    ir_list: Vec<Ir>,
    labels: HashMap<Label, usize>,
}

#[derive(Debug)]
pub enum Ir {
    Quaruple(Quaruple),
    Branch(BranchOp),
}

#[derive(Debug)]
pub enum BranchOp {
    Ret(Value),
    Goto(Label),
    CondGoto(Value, Label, Label),
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Label(i32);

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Reg(Symbol),
    Int(i32),
    Bool(bool),
}

impl From<Quaruple> for Ir {
    fn from(qu: Quaruple) -> Ir {
        Ir::Quaruple(qu)
    }
}

impl From<BranchOp> for Ir {
    fn from(op: BranchOp) -> Ir {
        Ir::Branch(op)
    }
}

impl From<Symbol> for Value {
    fn from(ident: Symbol) -> Value {
        Value::Reg(ident)
    }
}

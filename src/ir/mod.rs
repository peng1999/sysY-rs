mod display;
mod quaruple;
mod trans;

use crate::sym_table::Symbol;

pub use quaruple::{BinaryOp, OpArg, Quaruple, UnaryOp};
pub use trans::trans_stmts;

pub struct IrVec {
    pub ir_list: Vec<Ir>,
}

#[derive(Debug)]
pub enum Ir {
    Quaruple(Quaruple),
    Branch(BranchOp),
    Label(Label),
}

#[derive(Debug)]
pub enum BranchOp {
    Ret(Value),
    Goto(Label),
    CondGoto(Value, Label, Label),
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq)]
pub struct Label(pub i32);

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Reg(Symbol),
    Int(i32),
    Bool(bool),
}

impl From<Label> for Ir {
    fn from(lb: Label) -> Ir {
        Ir::Label(lb)
    }
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

impl IrVec {
    pub fn new() -> IrVec {
        IrVec { ir_list: vec![] }
    }

    fn push(&mut self, ir: impl Into<Ir>) {
        self.ir_list.push(ir.into());
    }
}

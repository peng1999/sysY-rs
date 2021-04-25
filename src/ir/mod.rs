mod display;
mod quaruple;
mod trans;

use std::collections::{HashMap, HashSet};

use crate::sym_table::Symbol;

pub use quaruple::{BinaryOp, OpArg, Quaruple, UnaryOp};
pub use trans::trans_stmts;

pub struct IrVec {
    pub ir_list: Vec<Ir>,
    pub label_list: HashMap<usize, Label>,
    labels_index: HashMap<Label, usize>,
    next_labels: HashSet<Label>,
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

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq)]
pub struct Label(pub i32);

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

impl IrVec {
    pub fn new() -> IrVec {
        IrVec {
            ir_list: vec![],
            label_list: HashMap::new(),
            labels_index: HashMap::new(),
            next_labels: HashSet::new(),
        }
    }

    fn push(&mut self, ir: impl Into<Ir>) {
        // 如果有标签，就插入
        let len = self.ir_list.len();
        if let Some(&label) = self.next_labels.iter().next() {
            self.label_list.insert(len, label);
        }
        for label in self.next_labels.drain() {
            self.labels_index.insert(label, len);
        }
        self.ir_list.push(ir.into());
    }

    fn push_label(&mut self, label: Label) {
        self.next_labels.insert(label);
    }

    pub fn remove_dup_label(&mut self) {
        for ir in &mut self.ir_list {
            let labels = &self.labels_index;
            let label_list = &self.label_list;
            let update_label = |label: &mut Label| *label = label_list[&labels[label]];
            match ir {
                Ir::Branch(BranchOp::Goto(label)) => {
                    update_label(label);
                }
                Ir::Branch(BranchOp::CondGoto(_, true_label, false_label)) => {
                    update_label(true_label);
                    update_label(false_label);
                }
                _ => {}
            }
        }
        self.labels_index.clear();
        for (&index, &label) in &self.label_list {
            self.labels_index.insert(label, index);
        }
    }
}

mod display;
mod quaruple;
mod trans;

use std::{collections::HashMap, convert::TryFrom};

use itertools::Itertools;

use crate::sym_table::Symbol;

pub use quaruple::{BinaryOp, OpArg, Quaruple, UnaryOp};
pub use trans::trans_items;

/// The linear intermediate representation.
///
/// Constraint: `ir_list` has at one label in the beginning
#[derive(Debug)]
pub struct IrVec {
    pub ir_list: Vec<Ir>,
}

#[derive(Debug)]
pub struct IrGraph {
    pub blocks: HashMap<Label, IrBlock>,
    pub block_order: Vec<Label>,
}

#[derive(Debug)]
pub enum Ir {
    Quaruple(Quaruple),
    Branch(BranchOp),
    Label(Label),
}

#[derive(Debug)]
pub struct IrBlock {
    pub ir_list: Vec<Quaruple>,
    pub exit: BranchOp,
}

#[derive(Debug)]
pub enum BranchOp {
    Ret(Option<Value>),
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

impl TryFrom<Ir> for Label {
    type Error = ();

    fn try_from(value: Ir) -> Result<Self, Self::Error> {
        match value {
            Ir::Label(label) => Ok(label),
            _ => Err(()),
        }
    }
}

impl From<Symbol> for Value {
    fn from(ident: Symbol) -> Value {
        Value::Reg(ident)
    }
}

impl Value {
    pub fn unwrap_reg(self) -> Symbol {
        match self {
            Value::Reg(s) => s,
            o => panic!("Value should not contain {:?}", o),
        }
    }
}

impl IrVec {
    pub fn new(entry: Label) -> IrVec {
        IrVec {
            ir_list: vec![Ir::Label(entry)],
        }
    }

    fn push(&mut self, ir: impl Into<Ir>) {
        self.ir_list.push(ir.into());
    }
}

impl IrGraph {
    pub fn from_ir_vec(ir_vec: IrVec) -> IrGraph {
        let mut label_map = HashMap::new();
        let group_by = ir_vec
            .ir_list
            .into_iter()
            .group_by(|ir| matches!(ir, Ir::Label(_)));

        let (labels, ir_list): (Vec<_>, Vec<_>) =
            group_by.into_iter().partition(|&(is_label, _)| is_label);

        // find first label of every block
        let block_order: Vec<Label> = labels
            .into_iter()
            .map(|(_, group)| {
                let mut group = group.map(|label| Label::try_from(label).unwrap());
                let first = group.next().unwrap();
                label_map.insert(first, first);
                for label in group {
                    label_map.insert(label, first);
                }
                first
            })
            .collect();

        let blocks: HashMap<_, _> = ir_list
            .into_iter()
            .map(|(_, groups)| groups)
            .enumerate()
            .map(|(idx, groups)| {
                let (quaruples, branch) = transform_label(groups, &mut label_map);

                let exit = branch.unwrap_or_else(|| {
                    let next_block = block_order
                        .get(idx + 1)
                        .expect("function did not exit properly.");
                    BranchOp::Goto(*next_block)
                });
                (
                    block_order[idx],
                    IrBlock {
                        ir_list: quaruples,
                        exit,
                    },
                )
            })
            .collect();

        IrGraph {
            blocks,
            block_order,
        }
    }
}

/// 将 Ir 按照 `label_map` 重新整理
fn transform_label(
    group: impl Iterator<Item = Ir>,
    label_map: &mut HashMap<Label, Label>,
) -> (Vec<Quaruple>, Option<BranchOp>) {
    let mut quaruples = vec![];

    for ir in group {
        match ir {
            Ir::Quaruple(quaruple) => quaruples.push(quaruple),
            Ir::Branch(op) => {
                let exit = match op {
                    BranchOp::Goto(label) => BranchOp::Goto(label_map[&label]),
                    BranchOp::CondGoto(value, true_label, false_label) => {
                        BranchOp::CondGoto(value, label_map[&true_label], label_map[&false_label])
                    }
                    ret @ BranchOp::Ret(_) => ret,
                };
                return (quaruples, Some(exit));
            }
            _ => unreachable!(),
        }
    }
    (quaruples, None)
}

use std::collections::{hash_map::Entry, HashMap};

use crate::{
    ir::{BinaryOp, BranchOp, IrGraph, Label, OpArg, UnaryOp, Value},
    sym_table::Symbol,
};

fn get_prev_block(ir_graph: &IrGraph) -> HashMap<Label, Vec<Label>> {
    let mut prev_block = HashMap::new();
    for (label, ir_block) in &ir_graph.blocks {
        match ir_block.exit {
            BranchOp::Ret(_) => {}
            BranchOp::Goto(next) => {
                prev_block.entry(next).or_insert_with(Vec::new).push(*label);
            }
            BranchOp::CondGoto(_, label1, label2) => {
                for next in [label1, label2] {
                    prev_block.entry(next).or_insert_with(Vec::new).push(*label);
                }
            }
        }
    }
    prev_block
}

fn eval_op(op: BinaryOp, arg1: ConstVal, arg2: ConstVal) -> ConstVal {
    match (arg1, arg2) {
        (ConstVal::Int(arg1), ConstVal::Int(arg2)) => match op {
            BinaryOp::Add => ConstVal::Int(arg1 + arg2),
            BinaryOp::Sub => ConstVal::Int(arg1 - arg2),
            BinaryOp::Mul => ConstVal::Int(arg1 * arg2),
            BinaryOp::Div => ConstVal::Int(arg1 / arg2),
            BinaryOp::Eq => ConstVal::Bool(arg1 == arg2),
            BinaryOp::Ne => ConstVal::Bool(arg1 != arg2),
            BinaryOp::Lt => ConstVal::Bool(arg1 < arg2),
            BinaryOp::Le => ConstVal::Bool(arg1 <= arg2),
            BinaryOp::Gt => ConstVal::Bool(arg1 > arg2),
            BinaryOp::Ge => ConstVal::Bool(arg1 >= arg2),
        },
        (ConstVal::Bool(arg1), ConstVal::Bool(arg2)) => match op {
            BinaryOp::Eq => ConstVal::Bool(arg1 == arg2),
            BinaryOp::Ne => ConstVal::Bool(arg1 != arg2),
            _ => panic!(),
        },
        _ => ConstVal::NotConst,
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum ConstVal {
    Int(i32),
    Bool(bool),
    NotConst,
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
struct ConstSet {
    val: HashMap<Symbol, ConstVal>,
}

impl ConstVal {
    fn from_val(val: Value, const_set: &ConstSet) -> Option<ConstVal> {
        match val {
            Value::Reg(s) => const_set.val.get(&s).copied(),
            Value::Int(v) => Some(ConstVal::Int(v)),
            Value::Bool(v) => Some(ConstVal::Bool(v)),
        }
    }

    fn merge(self: ConstVal, other: ConstVal) -> ConstVal {
        use ConstVal::*;

        match (self, other) {
            (Int(v1), Int(v2)) if v1 == v2 => Int(v1),
            (Bool(v1), Bool(v2)) if v1 == v2 => Bool(v1),
            _ => NotConst,
        }
    }

    fn transfer_from(op: &OpArg, in_: &ConstSet) -> Option<ConstVal> {
        match op {
            OpArg::Unary { op, arg } => match op {
                UnaryOp::Const => ConstVal::from_val(*arg, in_),
            },
            OpArg::Binary { op, arg1, arg2 } => {
                let val1 = ConstVal::from_val(*arg1, in_);
                let val2 = ConstVal::from_val(*arg2, in_);
                match (val1, val2) {
                    (Some(val1), Some(val2)) => Some(eval_op(*op, val1, val2)),
                    (Some(ConstVal::NotConst), _) | (_, Some(ConstVal::NotConst)) => {
                        Some(ConstVal::NotConst)
                    }
                    _ => None,
                }
            }
            _ => Some(ConstVal::NotConst),
        }
    }
}

impl ConstSet {
    fn merge_with(&mut self, other: &ConstSet) {
        for (&sym, &val) in &other.val {
            self.val
                .entry(sym)
                .and_modify(|a| *a = a.merge(val))
                .or_insert(val);
        }
    }
}

pub fn global_const_propagation(ir_graph: &mut IrGraph) {
    let prev_block = get_prev_block(ir_graph);

    // initialize
    let mut in_const_sets = HashMap::new();
    let mut out_const_sets = HashMap::new();

    while {
        let mut modified = false;

        // merge
        for label in &ir_graph.block_order {
            if let Some(sources) = prev_block.get(label) {
                let mut in_ = ConstSet::default();
                for src in sources.iter().map(|lbl| out_const_sets.get(lbl)).flatten() {
                    in_.merge_with(src);
                }
                in_const_sets.insert(*label, in_);
            }
        }

        // update
        for (label, block) in &ir_graph.blocks {
            let mut in_ = in_const_sets.remove(label).unwrap_or_default();

            for ir in &block.ir_list {
                if let Some(result) = ir.result {
                    if let Some(val) = ConstVal::transfer_from(&ir.op, &in_) {
                        in_.val.insert(result, val);
                    }
                }
            }

            match out_const_sets.entry(label) {
                Entry::Occupied(mut out) => {
                    if out.get() != &in_ {
                        modified = true;
                        out.insert(in_);
                    }
                }
                Entry::Vacant(entry) => {
                    modified = true;
                    entry.insert(in_);
                }
            }
        }

        modified
    } {}

    dbg!(out_const_sets
        .iter()
        .map(|(l, v)| (
            l,
            v.val
                .iter()
                .filter(|(_, &v)| v != ConstVal::NotConst)
                .collect::<Vec<_>>()
        ))
        .collect::<Vec<_>>());
}

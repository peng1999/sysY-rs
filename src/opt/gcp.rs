use std::collections::HashMap;

use crate::{
    ir::{BinaryOp, IrGraph, OpArg, UnaryOp, Value},
    sym_table::Symbol,
};

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

    fn to_val(self) -> Option<Value> {
        match self {
            ConstVal::Int(v) => Some(Value::Int(v)),
            ConstVal::Bool(v) => Some(Value::Bool(v)),
            _ => None,
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
    let prev_block = super::get_prev_block(ir_graph);

    // initialize
    let mut out_const_sets = HashMap::new();

    let merge_with_prev_block = |label, in_: &mut ConstSet, out_const_sets: &HashMap<_, _>| {
        if let Some(sources) = prev_block.get(label) {
            for src in sources.iter().map(|lbl| out_const_sets.get(lbl)).flatten() {
                in_.merge_with(src);
            }
        }
    };

    while {
        let mut modified = false;

        for label in &ir_graph.block_order {
            // merge
            let mut in_ = ConstSet::default();
            merge_with_prev_block(label, &mut in_, &out_const_sets);

            // update
            let block = ir_graph.blocks.get(label).unwrap();
            for ir in &block.ir_list {
                if let Some(result) = ir.result {
                    if let Some(val) = ConstVal::transfer_from(&ir.op, &in_) {
                        in_.val.insert(result, val);
                    }
                }
            }

            modified = modified || super::insert_or_modify(&mut out_const_sets, label, in_);
        }

        modified
    } {}

    // dbg!(out_const_sets
    //     .iter()
    //     .map(|(l, v)| (
    //         l,
    //         v.val
    //             .iter()
    //             .filter(|(_, &v)| v != ConstVal::NotConst)
    //             .collect::<Vec<_>>()
    //     ))
    //     .collect::<Vec<_>>());

    // rewrite
    for label in &ir_graph.block_order {
        let mut in_ = ConstSet::default();
        merge_with_prev_block(label, &mut in_, &out_const_sets);

        let block = ir_graph.blocks.get_mut(label).unwrap();
        for ir in &mut block.ir_list {
            for value in super::collect_ir_values(ir) {
                if let Some(sym) = value.into_reg() {
                    if let Some(val) = in_.val.get(&sym).unwrap().to_val() {
                        *value = val;
                    }
                }
            }
            if let Some(result) = ir.result {
                if let Some(val) = ConstVal::transfer_from(&ir.op, &in_) {
                    in_.val.insert(result, val);
                    if let Some(val) = val.to_val() {
                        ir.op = UnaryOp::Const.with_arg(val);
                        continue;
                    }
                }
            }
        }
    }
}

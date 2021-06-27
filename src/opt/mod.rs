pub mod flow;

use std::collections::{HashMap, HashSet};

use crate::{
    ir::{BranchOp, IrBlock, IrGraph, OpArg, Quaruple, Value},
    sym_table::Symbol,
};

fn collect_ir_values(ir: &mut Quaruple) -> Vec<&mut Value> {
    match &mut ir.op {
        OpArg::Arg(_) => vec![],
        OpArg::Unary { op: _, arg } => vec![arg],
        OpArg::Binary { op: _, arg1, arg2 } => {
            vec![arg1, arg2]
        }
        OpArg::Call { args, .. }
        | OpArg::LoadArr { idx: args, .. }
        | OpArg::StoreArr { idx: args, .. } => args.iter_mut().collect(),
    }
}

fn collect_ir_op(ir: &Quaruple, set: &mut impl Extend<Symbol>) {
    match &ir.op {
        OpArg::Arg(_) => {}
        OpArg::Unary { op: _, arg } => set.extend(arg.into_reg().into_iter()),
        OpArg::Binary { op: _, arg1, arg2 } => {
            set.extend(arg1.into_reg().into_iter());
            set.extend(arg2.into_reg().into_iter());
        }
        OpArg::Call { args, .. }
        | OpArg::LoadArr { idx: args, .. }
        | OpArg::StoreArr { idx: args, .. } => {
            set.extend(args.iter().map(|v| v.into_reg()).flatten())
        }
    }
}

fn sym_in_branch(br: &BranchOp) -> Option<Symbol> {
    match br {
        BranchOp::CondGoto(v, _, _) => v.into_reg(),
        _ => None,
    }
}

pub fn find_block_nonlocal(ir_graph: &IrGraph) -> Vec<Symbol> {
    let mut counter = HashMap::new();

    for block in ir_graph.blocks.values() {
        let mut var_set = HashSet::new();
        for ir in &block.ir_list {
            collect_ir_op(ir, &mut var_set);
            var_set.extend(ir.result.iter());
        }
        if let Some(sym) = sym_in_branch(&block.exit) {
            var_set.insert(sym);
        }

        for sym in var_set {
            *counter.entry(sym).or_insert(0) += 1;
        }
    }

    counter
        .into_iter()
        .filter_map(|(sym, cnt)| (cnt > 1).then_some(sym))
        .collect::<Vec<_>>()
}

pub fn next_use_pos(ir_block: &IrBlock, non_locals: &[Symbol]) -> HashMap<(Symbol, usize), usize> {
    let len = ir_block.ir_list.len();
    let mut last_use = sym_in_branch(&ir_block.exit)
        .into_iter()
        .chain(non_locals.iter().copied())
        .map(|s| (s, len))
        .collect::<HashMap<_, _>>();

    let mut next_use = HashMap::new();
    if let Some(reg) = sym_in_branch(&ir_block.exit) {
        if non_locals.contains(&reg) {
            next_use.insert((reg, len), len);
        }
    }
    for (i, ir) in ir_block.ir_list.iter().enumerate().rev() {
        let mut args = vec![];
        collect_ir_op(ir, &mut args);

        if let OpArg::Call { .. } = ir.op {
            last_use.clear();
        }
        if let Some(ret) = ir.result {
            last_use.remove(&ret);
        }

        for arg in args {
            if let Some(&place) = last_use.get(&arg) {
                next_use.insert((arg, i), place);
            }
            last_use.insert(arg, i);
        }
    }
    next_use
}

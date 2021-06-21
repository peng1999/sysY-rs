use std::collections::{HashMap, HashSet};

use crate::{
    ir::{IrGraph, OpArg, Quaruple},
    sym_table::Symbol,
};

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

pub fn find_block_local(ir_graph: &IrGraph) -> Vec<Symbol> {
    let mut counter = HashMap::new();

    for (_, block) in &ir_graph.blocks {
        let mut var_set = HashSet::new();
        for ir in &block.ir_list {
            collect_ir_op(ir, &mut var_set);
            var_set.extend(ir.result.iter());
        }

        for sym in var_set {
            *counter.entry(sym).or_insert(0) += 1;
        }
    }

    counter
        .into_iter()
        .filter_map(|(sym, cnt)| (cnt == 1).then_some(sym))
        .collect::<Vec<_>>()
}

pub fn next_use_pos(ir_list: &[Quaruple]) -> HashMap<(Symbol, usize), usize> {
    let mut last_use = HashMap::new();
    let mut next_use = HashMap::new();
    for (i, ir) in ir_list.iter().enumerate().rev() {
        let mut args = vec![];
        collect_ir_op(ir, &mut args);

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
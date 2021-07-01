use std::{
    collections::{HashMap, HashSet},
    mem,
};

use itertools::Itertools;

use crate::{
    ir::{IrGraph, Label, OpArg},
    sym_table::Symbol,
};

type VarSet = HashSet<Symbol>;

fn is_pure(op: &OpArg) -> bool {
    match op {
        OpArg::Arg(_) | OpArg::Unary { .. } | OpArg::Binary { .. } => true,
        OpArg::Call { .. } | OpArg::LoadArr { .. } | OpArg::StoreArr { .. } => false,
    }
}

fn merge_with_next_block(
    label: Label,
    in_var_set: &HashMap<Label, VarSet>,
    ir_graph: &IrGraph,
) -> VarSet {
    let mut out = HashSet::new();
    for next in super::collect_branch_next(&ir_graph.blocks[&label].exit) {
        if let Some(src) = in_var_set.get(&next) {
            out.extend(src);
        }
    }
    out
}

pub fn dead_code_elimation(ir_graph: &mut IrGraph) {
    // initialize
    let mut in_var_set = HashMap::new();

    while {
        let mut modified = false;

        for &label in ir_graph.block_order.iter().rev() {
            // merge
            let mut out = merge_with_next_block(label, &in_var_set, ir_graph);

            // update
            let block = ir_graph.blocks.get(&label).unwrap();
            if let Some(sym) = super::sym_in_branch(&block.exit) {
                out.insert(sym);
            }
            for ir in block.ir_list.iter().rev() {
                let mut not_used = false;
                if let Some(sym) = ir.result {
                    not_used = !out.remove(&sym);
                }

                if not_used && is_pure(&ir.op) {
                    continue;
                }

                let mut args = HashSet::new();
                super::collect_ir_op(ir, &mut args);
                for sym in args {
                    out.insert(sym);
                }
            }

            modified = modified || super::insert_or_modify(&mut in_var_set, label, out);
        }

        modified
    } {}

    // rewrite
    for &label in ir_graph.block_order.iter().rev() {
        let mut out = merge_with_next_block(label, &in_var_set, ir_graph);

        let block = ir_graph.blocks.get_mut(&label).unwrap();
        if let Some(sym) = super::sym_in_branch(&block.exit) {
            out.insert(sym);
        }
        let mut mark = vec![false; block.ir_list.len()];
        for (i, ir) in block.ir_list.iter_mut().enumerate().rev() {
            if let Some(sym) = ir.result {
                if !out.remove(&sym) {
                    // 移除失败，变量并未被使用
                    ir.result = None;
                }
            }

            if ir.result == None && is_pure(&ir.op) {
                mark[i] = true;
                continue;
            }

            let mut args = HashSet::new();
            super::collect_ir_op(ir, &mut args);
            for sym in args {
                out.insert(sym);
            }
        }
        let mut tmp_ir = Vec::new();
        mem::swap(&mut tmp_ir, &mut block.ir_list);
        block.ir_list = tmp_ir
            .into_iter()
            .zip_eq(mark)
            .filter_map(|(ir, mark)| (!mark).then_some(ir))
            .collect();
    }
}

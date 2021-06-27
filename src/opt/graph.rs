use std::collections::{HashSet, VecDeque};

use crate::ir::IrGraph;

pub fn remove_unreachable_block(ir_graph: &mut IrGraph) {
    if ir_graph.block_order.is_empty() {
        return;
    }

    let mut reached = HashSet::new();
    let mut pending = VecDeque::new();
    pending.push_back(ir_graph.block_order[0]);

    while let Some(label) = pending.pop_front() {
        if reached.insert(label) {
            for next in super::collect_branch_next(&ir_graph.blocks[&label].exit) {
                pending.push_back(next);
            }
        }
    }

    ir_graph.blocks.retain(|k, _| reached.contains(k));
    ir_graph.block_order.retain(|k| reached.contains(k));
}
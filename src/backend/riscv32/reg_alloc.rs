use std::collections::HashMap;

use crate::sym_table::Symbol;

use super::{emit_load_stack, emit_store_operand, Context, Operand, RiscVReg};

#[derive(Debug, Default)]
pub struct LocalRegAllocator {
    /// 逐出列表：寄存器对应的Symbol
    reg_lookup: HashMap<RiscVReg, Symbol>,
    /// 空闲寄存器
    free_reg: Vec<RiscVReg>,
    /// Symbol下一次使用的行号
    next_use: HashMap<(Symbol, usize), usize>,
    /// Reg下一次使用的行号
    reg_next_use: HashMap<RiscVReg, usize>,
    /// 当前行号
    current_line: usize,
}

impl LocalRegAllocator {
    pub(super) fn new(used_list: HashMap<(Symbol, usize), usize>, regs: Vec<RiscVReg>) -> Self {
        Self {
            free_reg: regs,
            next_use: used_list,
            ..Default::default()
        }
    }

    pub fn next_line(&mut self) {
        self.current_line += 1;
    }

    /// 寄存器所存的Symbol在下一次写之前不会读
    pub(super) fn reg_free(&mut self, reg: RiscVReg) {
        let sym = self.reg_lookup.get(&reg);
        if sym
            .map(|&sym| self.next_use.get(&(sym, self.current_line)).is_none())
            .unwrap_or(true)
        {
            // 可以用了
            self.free_reg.push(reg);
            // 不需要被逐出了
            self.reg_lookup.remove(&reg);
        }
    }

    pub(super) fn finish_read(&mut self, reg: RiscVReg) {
        if let Some(&sym) = self.reg_lookup.get(&reg) {
            if let Some(&line) = self.next_use.get(&(sym, self.current_line)) {
                self.reg_next_use.insert(reg, line);
            } else {
                self.reg_next_use.insert(reg, usize::MAX);
            }
        }
    }

    pub(super) fn set_reg_as_input(&mut self, reg: RiscVReg, sym: Symbol) {
        let free_reg = self.free_reg.pop().unwrap();
        assert_eq!(free_reg, reg);
        if let Some(reg) = self.find_sym(sym) {
            self.reg_free(reg);
        }
        self.reg_lookup.insert(reg, sym);
    }

    /// 查找Symbol所在寄存器
    fn find_sym(&self, sym: Symbol) -> Option<RiscVReg> {
        self.reg_lookup
            .iter()
            .find_map(|(&reg, &s)| (s == sym).then_some(reg))
    }

    /// 逐出寄存器，但不写回
    fn spill_furthest_sym(&mut self) -> (RiscVReg, Symbol) {
        let (&reg, &sym) = self
            .reg_lookup
            .iter()
            .max_by_key(|&(reg, _)| self.reg_next_use[reg])
            .unwrap();
        self.reg_lookup.remove(&reg);
        self.reg_next_use.remove(&reg);
        self.free_reg.push(reg);
        (reg, sym)
    }

    fn spill_all(&mut self) -> Vec<(RiscVReg, Symbol)> {
        self.reg_next_use.clear();
        let keys = self.reg_lookup.drain().collect::<Vec<_>>();
        self.free_reg.extend(keys.iter().map(|(reg, _)| reg));
        keys
    }
}

pub(super) fn emit_spill_all(ctx: &mut Context) -> anyhow::Result<()> {
    let kv = ctx.reg_allocator.spill_all();
    for (reg, sym) in kv {
        emit_store_operand(Operand::Reg(reg), ctx.stack_alloc[&sym], ctx, false)?;
    }
    Ok(())
}

/// 分配寄存器，不读初值
pub(super) fn emit_reg_alloc_tmp(ctx: &mut Context) -> anyhow::Result<RiscVReg> {
    let reg = if let Some(reg) = ctx.reg_allocator.free_reg.pop() {
        reg
    } else {
        let (reg, sym) = ctx.reg_allocator.spill_furthest_sym();
        emit_store_operand(Operand::Reg(reg), ctx.stack_alloc[&sym], ctx, false)?;
        reg
    };
    Ok(reg)
}

/// 分配寄存器，读初值
pub(super) fn emit_reg_alloc(sym: Symbol, ctx: &mut Context) -> anyhow::Result<RiscVReg> {
    let result;
    if let Some(reg) = ctx.reg_allocator.find_sym(sym) {
        result = reg;
    } else {
        result = emit_input_reg_alloc(sym, ctx)?;
        emit_load_stack(ctx.stack_alloc[&sym], result, ctx)?;
    }
    Ok(result)
}

/// 分配寄存器，不读初值
pub(super) fn emit_input_reg_alloc(sym: Symbol, ctx: &mut Context) -> anyhow::Result<RiscVReg> {
    let result = emit_reg_alloc_tmp(ctx)?;
    ctx.reg_allocator.reg_lookup.insert(result, sym);
    Ok(result)
}

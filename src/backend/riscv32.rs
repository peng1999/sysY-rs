use std::{collections::HashMap, io::Write};

use crate::{
    context::Context as QContext,
    ir::IrGraph,
    sym_table::{SymTable, Symbol},
};

struct Context<'a> {
    sym_table: SymTable,

    file: &'a mut dyn Write,
}

impl<'a> Context<'a> {
    fn new(file: &'a mut dyn Write, ctx: QContext) -> Self {
        Context {
            sym_table: ctx.sym_table,
            file,
        }
    }
}

#[rustfmt::skip]
enum RiscVReg {
    A0, A1, A2, A3, A4, A5, A6, A7,
    T0, T1, T2, T3, T4, T5, T6,
}

enum AllocReg {
    Reg(RiscVReg),
    Stack(i32), // offset of sp
}

fn align_byte_up(pos: i32, align: i32) -> i32 {
    if pos % align == 0 {
        pos
    } else {
        pos + (align - pos % align)
    }
}

#[test]
fn align_byte_works() {
    assert_eq!(align_byte_up(8, 4), 8);
    assert_eq!(align_byte_up(9, 4), 12);
    assert_eq!(align_byte_up(10, 4), 12);
}

fn allocate_register(fn_sym: Symbol, ctx: &mut Context) -> (i32, HashMap<Symbol, AllocReg>) {
    let mut sp = 0;
    let mut alloc = HashMap::new();
    for &local in ctx.sym_table.locals_of(fn_sym) {
        let size = ctx
            .sym_table
            .ty_of(local)
            .unwrap()
            .into_ty_basic()
            .unwrap()
            .byte_size() as i32;
        let offset = align_byte_up(sp, size);
        alloc.insert(local, AllocReg::Stack(offset));
        sp = offset + size;
    }
    // saved ra at top of the frame
    sp += 4;
    // frame are 16-byte aligned
    sp = align_byte_up(sp, 16);
    (sp, alloc)
}

fn emit_function(ir_graph: IrGraph, fn_sym: Symbol, ctx: &mut Context) -> anyhow::Result<()> {
    let name = ctx.sym_table.name_of(fn_sym).unwrap();
    writeln!(ctx.file, "{}:", name)?;
    let (framesize, alloc) = allocate_register(fn_sym, ctx);
    // enter frame
    writeln!(ctx.file, "addi sp, sp, {}", -framesize)?;
    writeln!(ctx.file, "sw ra, {}(sp)", framesize - 4)?;

    writeln!(ctx.file, "lw ra, {}(sp)", framesize - 4)?;
    writeln!(ctx.file, "addi sp, sp, {}", framesize)?;
    writeln!(ctx.file, "ret")?;
    Ok(())
}

pub fn emit_asm(
    ir_graph: Vec<(Symbol, Option<IrGraph>)>,
    file: &mut dyn Write,
    qctx: QContext,
) -> anyhow::Result<()> {
    let mut ctx = Context::new(file, qctx);

    for (fn_sym, ir_graph) in ir_graph {
        if let Some(ir_graph) = ir_graph {
            emit_function(ir_graph, fn_sym, &mut ctx)?;
        }
    }
    Ok(())
}

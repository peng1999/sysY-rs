#![feature(map_try_insert)]
#![feature(result_cloned)]
#![feature(label_break_value)]
#![feature(bool_to_option)]
#![allow(clippy::new_without_default)]

#[macro_use]
extern crate lalrpop_util;

use std::{fmt::Display, fs, io::Write, path::PathBuf};

use clap::{AppSettings, Clap};

use crate::{context::Context, error::LogResult, ir::IrGraph, sym_table::Symbol};

lalrpop_mod! {
    #[allow(clippy::all)]
    pub syntax
}
mod ast;
mod backend;
mod context;
mod error;
mod ir;
mod opt;
mod sym_table;
mod ty;

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(long, possible_values = &["ast", "ir", "mir", "llvm", "riscv"])]
    emit: Option<String>,

    /// Output file
    #[clap(short = 'o')]
    output_file: Option<PathBuf>,

    /// Source file
    #[clap(required = true)]
    input_file: PathBuf,

    #[clap(
        short = 'f',
        multiple = false,
        multiple_occurrences = true,
        possible_values = &["gcp", "dce"]
    )]
    optimize_pass: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();
    let source = fs::read_to_string(&opts.input_file)?;
    // is default mode
    let mut output: Box<dyn Write> = opts
        .output_file
        .as_ref()
        .map(fs::File::create)
        .transpose()?
        .map_or_else(
            || Box::new(std::io::stdout()) as Box<dyn Write>,
            |f| Box::new(f) as Box<dyn Write>,
        );

    'exit: {
        let mut ctx = context::Context::new(&source);
        let parser = syntax::ProgParser::new();
        let ast_tree = parser.parse(&mut ctx, &source).unwrap_or_log(&mut ctx);
        if opts.emit.as_deref() == Some("ast") {
            writeln!(output, "{:#?}", ast_tree)?;
            break 'exit;
        }

        let fun_ir = ir::trans_items(ast_tree, &mut ctx);

        if opts.emit.as_deref() == Some("ir") {
            display_fun_ir(&mut output, fun_ir, &mut ctx)?;
            break 'exit;
        }

        fun_ir
            .iter()
            .filter_map(|(fn_sym, ir_vec)| ir_vec.as_ref().map(|v| (fn_sym, v)))
            .for_each(|(fn_sym, ir_vec)| ty::ty_check(*fn_sym, ir_vec, &mut ctx));

        let mut ir_graph = fun_ir
            .into_iter()
            .map(|(fn_sym, ir_vec)| (fn_sym, ir_vec.map(IrGraph::from_ir_vec)))
            .collect::<Vec<_>>();

        // Optimization pass
        for (_, fn_graph) in &mut ir_graph {
            if let Some(fn_graph) = fn_graph {
                for optimize in &opts.optimize_pass {
                    match optimize.as_ref() {
                        "gcp" => opt::gcp::global_const_propagation(fn_graph),
                        "dce" => opt::dce::dead_code_elimination(fn_graph),
                        _ => unreachable!(),
                    }
                }
                opt::graph::remove_unreachable_block(fn_graph);
            }
        }

        if opts.emit.as_deref() == Some("mir") {
            display_fun_ir(&mut output, ir_graph, &mut ctx)?;
            break 'exit;
        }

        if opts.emit.as_deref() == Some("llvm") {
            backend::llvm::emit_llvm_ir(ir_graph, &mut output, ctx)?;
            break 'exit;
        }

        if opts.emit.as_deref() == Some("riscv") {
            backend::riscv32::emit_asm(ir_graph, &mut output, ctx)?;
            break 'exit;
        }

        let mut file = opts.input_file;
        let out_path = opts.output_file.unwrap_or_else(|| {
            file.set_extension("o");
            file
        });

        backend::llvm::emit_obj(ir_graph, &out_path, ctx);
    }
    Ok(())
}

fn display_fun_ir(
    output: &mut Box<dyn Write>,
    fun_ir: impl IntoIterator<Item = (Symbol, Option<impl Display>)>,
    ctx: &mut Context,
) -> std::io::Result<()> {
    for (fn_sym, ir_vec) in fun_ir {
        writeln!(
            output,
            "{}: {}",
            ctx.sym_table.name_of(fn_sym).unwrap(),
            ctx.sym_table.ty_of(fn_sym).unwrap()
        )?;
        if let Some(ir_vec) = ir_vec {
            writeln!(output, "{}", ir_vec)?;
        }
    }
    Ok(())
}

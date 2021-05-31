#![feature(map_try_insert)]
#![feature(result_cloned)]
#![feature(label_break_value)]
#![allow(clippy::new_without_default)]

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod! {
    #[allow(clippy::all)]
    pub syntax
}
mod ast;
mod context;
mod error;
mod ir;
mod llvm;
mod sym_table;
mod ty;

use std::{fmt::Display, fs, io::Write, path::PathBuf};

use clap::{AppSettings, Clap};

use crate::{context::Context, error::LogResult, ir::IrGraph, sym_table::Symbol};

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(long, possible_values = &["ast", "ir", "mir", "llvm"])]
    emit: Option<String>,

    /// Output file
    #[clap(short = 'o')]
    output_file: Option<PathBuf>,

    /// Source file
    #[clap(required = true)]
    input_file: PathBuf,
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
            .filter_map(|(fun, ir_vec)| ir_vec.as_ref().map(|v| (fun, v)))
            .for_each(|(fun, ir_vec)| ty::ty_check(*fun, ir_vec, &mut ctx));

        let ir_graph = fun_ir
            .into_iter()
            .map(|(fun, ir_vec)| (fun, ir_vec.map(IrGraph::from_ir_vec)))
            .collect::<Vec<_>>();

        if opts.emit.as_deref() == Some("mir") {
            display_fun_ir(&mut output, ir_graph, &mut ctx)?;
            break 'exit;
        }

        if opts.emit.as_deref() == Some("llvm") {
            llvm::emit_llvm_ir(ir_graph, &mut output, ctx)?;
            break 'exit;
        }

        let mut file = opts.input_file;
        let out_path = opts.output_file.unwrap_or_else(|| {
            file.set_extension("o");
            file
        });

        llvm::emit_obj(ir_graph, &out_path, ctx);
    }
    Ok(())
}

fn display_fun_ir(
    output: &mut Box<dyn Write>,
    fun_ir: impl IntoIterator<Item = (Symbol, Option<impl Display>)>,
    ctx: &mut Context,
) -> std::io::Result<()> {
    for (fun, ir_vec) in fun_ir {
        writeln!(
            output,
            "{}: {}",
            ctx.sym_table.name_of(fun).unwrap(),
            ctx.sym_table.ty_of(fun).unwrap()
        )?;
        if let Some(ir_vec) = ir_vec {
            writeln!(output, "{}", ir_vec)?;
        }
    }
    Ok(())
}

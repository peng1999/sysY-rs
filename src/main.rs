#![feature(map_try_insert)]
#![feature(result_cloned)]
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

use std::{fs, io::Write, path::PathBuf};

use clap::{AppSettings, Clap};

use crate::{
    error::LogResult,
    ir::{IrGraph, IrVec},
};

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

    #[allow(clippy::never_loop)] // Just want to use the `break`
    loop {
        let mut ctx = context::Context::new(&source);
        let parser = syntax::ProgParser::new();
        let ast_tree = parser.parse(&mut ctx, &source).unwrap_or_log(&mut ctx);
        if opts.emit.as_deref() == Some("ast") {
            writeln!(output, "{:#?}", ast_tree)?;
            break;
        }

        let fun_ir = ir::trans_items(ast_tree, &mut ctx);

        if opts.emit.as_deref() == Some("ir") {
            for (name, ir_vec) in fun_ir {
                writeln!(
                    output,
                    "{}: {}",
                    ctx.sym_table.name_of(name).unwrap(),
                    ctx.sym_table.ty_of(name).unwrap()
                )?;
                writeln!(output, "{}", ir_vec)?;
            }
            break;
        }

        ty::ty_check(todo!("&ir_vec"), &mut ctx);

        let ir_graph = IrGraph::from_ir_vec(todo!("ir_vec"));

        if opts.emit.as_deref() == Some("mir") {
            writeln!(output, "{}", ir_graph)?;
            break;
        }

        if opts.emit.as_deref() == Some("llvm") {
            llvm::emit_llvm_ir(ir_graph, &mut output, ctx)?;
            break;
        }

        let mut file = opts.input_file;
        let out_path = opts.output_file.unwrap_or_else(|| {
            file.set_extension("o");
            file
        });

        llvm::emit_obj(ir_graph, &out_path, ctx);

        break;
    }
    Ok(())
}

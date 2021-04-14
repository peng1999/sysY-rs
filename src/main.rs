#![feature(bool_to_option)]
#![feature(io_read_to_string)]
#![feature(map_try_insert)]
#![allow(clippy::new_without_default)]

#[macro_use]
extern crate lalrpop_util;

mod ast;
lalrpop_mod! {
    #[allow(clippy::all)]
    pub syntax
}
mod context;
mod llvm;
mod quaruple;

use std::{fs, io::Write, path::PathBuf};

use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(long, possible_values = &["ast", "ir", "llvm"])]
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
        let mut ctx = context::Context::new();
        let parser = syntax::ItemParser::new();
        let ast_tree = parser.parse(&mut ctx, &source);
        if opts.emit.as_deref() == Some("ast") {
            writeln!(output, "{:#?}", ast_tree)?;
            break;
        }
        if let Ok(ast::Item::FuncDef(_, _, _, stmts)) = ast_tree {
            let mut quar = vec![];
            ctx.sym_begin_scope();
            quaruple::trans_stmts(stmts, &mut quar, &mut ctx);
            ctx.sym_end_scope();

            if opts.emit.as_deref() == Some("ir") {
                let ir_form = quar
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("\n");
                writeln!(output, "{}", ir_form)?;
                break;
            }

            if opts.emit.as_deref() == Some("llvm") {
                llvm::emit_llvm_ir(quar, &mut output)?;
                break;
            }

            let mut file = opts.input_file;
            let out_path = opts.output_file.unwrap_or_else(|| {
                file.set_extension("o");
                file
            });

            llvm::emit_obj(quar, &out_path);

        } else {
            eprintln!("The program is not a funtion definition!");
        }

        break;
    }
    Ok(())
}

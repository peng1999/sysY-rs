#![feature(io_read_to_string)]
#![feature(map_try_insert)]

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

use std::{fs, path::PathBuf};

use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(long, possible_values = &["ast", "ir", "llvm"])]
    emit: Option<String>,
    #[clap(required = true)]
    input_file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();
    let source = fs::read_to_string(opts.input_file)?;
    loop {
        let mut ctx = context::Context::new();
        let parser = syntax::ItemParser::new();
        let ast_tree = parser.parse(&mut ctx, &source);
        if opts.emit.as_deref() == Some("ast") {
            println!("{:#?}", ast_tree);
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
                println!("{}", ir_form);
                break;
            }

            llvm::run(quar);
        } else {
            println!("The program is not a funtion definition!");
        }

        break;
    }
    Ok(())
}

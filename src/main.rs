#![feature(io_read_to_string)]
#![feature(map_try_insert)]

#[macro_use]
extern crate lalrpop_util;

mod ast;
mod lexer;
lalrpop_mod! {
    #[allow(clippy::all)]
    pub syntax
}
mod context;
mod llvm;
mod quaruple;

use std::io;

fn main() -> anyhow::Result<()> {
    let source = io::read_to_string(&mut io::stdin())?;
    let arg1 = std::env::args().nth(1);
    loop {
        if let Some("lexer") = arg1.as_deref() {
            lexer::run(source);
            break;
        }

        let mut ctx = context::Context::new();
        let parser = syntax::ItemParser::new();
        let ast_tree = parser.parse(&mut ctx, &source);
        if let Some("ast") = arg1.as_deref() {
            println!("{:#?}", ast_tree);
            break;
        }
        if let Ok(ast::Item::FuncDef(_, _, _, stmts)) = ast_tree {
            let mut quar = vec![];
            ctx.sym_begin_scope();
            quaruple::trans_stmts(stmts, &mut quar, &mut ctx);
            ctx.sym_end_scope();

            if let Some("ir") = arg1.as_deref() {
                let ir_form = quar
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("\n");
                println!("{}", ir_form);
                break;
            }

            llvm::run(quar, &mut ctx);
        } else {
            println!("The program is not a funtion definition!");
        }

        break;
    }
    Ok(())
}

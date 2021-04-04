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
    let arg2 = std::env::args().nth(2);
    if let Some("lexer") = arg1.as_deref() {
        lexer::run(source);
    } else if let Some("ast") = arg1.as_deref() {
        let mut ctx = context::Context::new();
        let parser = syntax::ItemParser::new();
        let ast_tree = parser.parse(&mut ctx, &source);
        if let Some("ir") = arg2.as_deref() {
            if let Ok(ast::Item::FuncDef(_, _, _, stmts)) = ast_tree {
                let mut quar = vec![];
                ctx.sym_begin_scope();
                quaruple::trans_stmts(stmts, &mut quar, &mut ctx);
                ctx.sym_end_scope();
                println!("{:#?}", quar);
            }
        } else {
            println!("{:#?}", ast_tree);
        }
    } else {
        llvm::run();
    }
    Ok(())
}

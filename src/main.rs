#![feature(io_read_to_string)]

#[macro_use] extern crate lalrpop_util;

mod ast;
mod lexer;
lalrpop_mod!(pub syntax);
mod llvm;

use std::io;

fn main() -> anyhow::Result<()> {
    let source = io::read_to_string(&mut io::stdin())?;
    let arg1 = std::env::args().nth(1);
    if let Some("lexer") = arg1.as_deref() {
        lexer::run(source);
    } else if let Some("ast") = arg1.as_deref() {
        let parser = syntax::StmtParser::new();
        let ast_tree = parser.parse(&source);
        println!("{:#?}", ast_tree);
    } else {
        llvm::run();
    }
    Ok(())
}

#![feature(io_read_to_string)]

#[macro_use]
extern crate pest_derive;

mod lexer;
mod parser;
mod ast;

use std::io;

fn main() -> anyhow::Result<()> {
    let source = io::read_to_string(&mut io::stdin())?;
    let arg1 = std::env::args().nth(1);
    if let Some("lexer") = arg1.as_deref() {
        lexer::run(source);
    } else if let Some("parser") = arg1.as_deref() {
        parser::run(source);
    } else {
        let ast_tree = parser::parse(source)?;
        println!("{:#?}", ast_tree);
    }
    Ok(())
}

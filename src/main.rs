#![feature(io_read_to_string)]

#[macro_use]
extern crate pest_derive;

mod lexer;
mod parser;

use std::io;

fn main() -> anyhow::Result<()> {
    let source = io::read_to_string(&mut io::stdin())?;
    let arg1 = std::env::args().next();
    if let Some("lexer") = arg1.as_deref() {
        lexer::run(source);
    } else {
        parser::run(source);
    }
    Ok(())
}

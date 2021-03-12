use pest::Parser;

#[derive(Parser)]
#[grammar = "syntax.pest"]
pub struct SysYParser;

pub fn run(source: String) {
    let result = SysYParser::parse(Rule::Prog, &source);
    println!("{:#?}", result);
}

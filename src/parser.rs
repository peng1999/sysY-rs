use pest::{iterators::Pair, Parser};

use crate::ast;

#[derive(Parser)]
#[grammar = "syntax.pest"]
pub struct SysYParser;

pub fn parse(source: String) -> anyhow::Result<ast::Expr> {
    let mut result = SysYParser::parse(Rule::Expr, &source)?;
    let pair = result.next().unwrap();
    Ok(parse_expr(pair))
}

fn parse_expr(pair: Pair<Rule>) -> ast::Expr {
    match pair.as_rule() {
        Rule::AddTerm | Rule::MulTerm => {
            let mut pairs = pair.into_inner();
            let lhs = parse_expr(pairs.next().unwrap());
            let op = parse_bin_op(pairs.next().unwrap());
            let rhs = parse_expr(pairs.next().unwrap());
            let mut expr = ast::Expr::Binary(op, Box::new(lhs), Box::new(rhs));
            while let Some(op) = pairs.next() {
                let rhs = parse_expr(pairs.next().unwrap());
                expr = ast::Expr::Binary(parse_bin_op(op), Box::new(expr), Box::new(rhs));
            }
            expr
        }
        Rule::UnaryTerm => {
            let mut pairs = pair.into_inner();
            let op = parse_un_op(pairs.next().unwrap());
            let hs = parse_expr(pairs.next().unwrap());
            ast::Expr::Unary(op, Box::new(hs))
        }
        Rule::CallTerm => {
            let mut pairs = pair.into_inner();
            let name = parse_expr(pairs.next().unwrap());
            let parms: Vec<_> = pairs.map(parse_expr).collect();
            ast::Expr::Call(Box::new(name), parms)
        }
        Rule::SubTerm => {
            let mut pairs = pair.into_inner();
            let name = parse_expr(pairs.next().unwrap());
            let parms: Vec<_> = pairs.map(parse_expr).collect();
            ast::Expr::Index(Box::new(name), parms)
        }
        Rule::NumLit => ast::Expr::Lit(pair.as_str().parse().unwrap()),
        Rule::Ident => ast::Expr::Ident(pair.as_str().to_owned()),
        _ => panic!(),
    }
}

fn parse_bin_op(pair: Pair<Rule>) -> ast::BinOp {
    use ast::BinOp::*;

    match pair.as_str() {
        "+" => Add,
        "-" => Sub,
        "*" => Mul,
        "/" => Div,
        "%" => Rem,
        _ => unimplemented!(),
    }
}

fn parse_un_op(pair: Pair<Rule>) -> ast::UnOp {
    match pair.as_str() {
        "+" => ast::UnOp::Pos,
        "-" => ast::UnOp::Neg,
        "!" => ast::UnOp::Not,
        _ => panic!(),
    }
}

pub fn run(source: String) {
    let result = SysYParser::parse(Rule::Prog, &source);
    match result {
        Ok(pairs) => {
            let lines: Vec<_> = pairs.map(|pair| format_pair(pair, 0, true)).collect();
            let lines = lines.join("\n");
            println!("{}", lines);
        }
        Err(err) => println!("{}", err),
    }
}

// Copied from https://github.com/pest-parser/site/blob/master/src/main.rs
fn format_pair(pair: Pair<Rule>, indent_level: usize, is_newline: bool) -> String {
    let indent = if is_newline {
        "  ".repeat(indent_level)
    } else {
        "".to_string()
    };

    let children: Vec<_> = pair.clone().into_inner().collect();
    let len = children.len();
    let children: Vec<_> = children
        .into_iter()
        .map(|pair| {
            format_pair(
                pair,
                if len > 1 {
                    indent_level + 1
                } else {
                    indent_level
                },
                len > 1,
            )
        })
        .collect();

    let dash = if is_newline { "- " } else { "" };

    match len {
        0 => format!(
            "{}{}{:?}: {:?}",
            indent,
            dash,
            pair.as_rule(),
            pair.as_str()
        ),
        1 => format!("{}{}{:?} > {}", indent, dash, pair.as_rule(), children[0]),
        _ => format!(
            "{}{}{:?}\n{}",
            indent,
            dash,
            pair.as_rule(),
            children.join("\n")
        ),
    }
}

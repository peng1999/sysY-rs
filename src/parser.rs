use pest::{iterators::Pair, Parser};

#[derive(Parser)]
#[grammar = "syntax.pest"]
pub struct SysYParser;

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

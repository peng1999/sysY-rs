use std::{collections::BTreeMap, process::exit};

use lalrpop_util::{lexer::Token, ParseError};

use crate::context::Context;

pub trait LogError {
    fn log_and_exit(self, ctx: &mut Context) -> !;
}

pub trait LogResult {
    type Unwrapped;
    fn unwrap_or_log(self, ctx: &mut Context) -> Self::Unwrapped;
}

impl<T, E: LogError> LogResult for Result<T, E> {
    type Unwrapped = T;

    fn unwrap_or_log(self, ctx: &mut Context) -> T {
        self.unwrap_or_else(|e| e.log_and_exit(ctx))
    }
}

impl<'input> LogError for ParseError<usize, Token<'input>, &'static str> {
    fn log_and_exit(self, ctx: &mut Context) -> ! {
        use ParseError::*;
        match &self {
            InvalidToken { location }
            | UnrecognizedToken {
                token: (location, ..),
                ..
            }
            | UnrecognizedEOF { location, .. } => {
                let (line, col) = ctx.line_col_lookup.lookup(*location);
                eprint!("{}:{}: ", line, col);
            }
            _ => {}
        }
        match self {
            InvalidToken { location } => eprintln!(
                "error: invalid character '{}'",
                ctx.source[location..].chars().next().unwrap(),
            ),
            UnrecognizedToken {
                token: (_, token, _),
                expected: _,
            } => eprintln!("error: unexpected token '{}'", token),
            UnrecognizedEOF { .. } => eprintln!("error: unexpected EOF"),
            ExtraToken { .. } => unimplemented!("extra token"),
            User { .. } => unimplemented!("user"),
        }
        exit(1);
    }
}

#[derive(Debug)]
pub struct LineColLookup<'a> {
    // map byte position to line number
    line_start: BTreeMap<usize, usize>,
    source: &'a str,
}

impl LineColLookup<'_> {
    pub fn new(source: &str) -> LineColLookup {
        let line_start = source
            .split('\n')
            .map(str::len)
            .scan(0, |acc, n| {
                let start = *acc;
                *acc += n + 1;
                Some(start)
            })
            .enumerate()
            .map(|(i, n)| (n, i + 1))
            .collect();
        LineColLookup { line_start, source }
    }

    /// Returns (line number, column number) of the byte position.
    ///
    /// bytes start from `byte_pos` must be valid code points.
    pub fn lookup(&self, byte_pos: usize) -> (usize, usize) {
        let (&start_pos, &line_num) = self.line_start.range(..=byte_pos).next_back().unwrap();
        let col_num = 1 + self.source[start_pos..byte_pos].chars().count();
        (line_num, col_num)
    }
}

#[test]
fn line_col_lookup_null_str() {
    let source = "";
    let lookup = LineColLookup::new(source);
    assert_eq!(lookup.lookup(0), (1, 1));
}

#[test]
fn line_col_lookup_multiline() {
    let source = "line1\nline2";
    let lookup = LineColLookup::new(source);
    assert_eq!(lookup.lookup(0), (1, 1));
    assert_eq!(lookup.lookup(6), (2, 1));
    assert_eq!(lookup.lookup(10), (2, 5));

    let source = "line1\nline2\n";
    let lookup = LineColLookup::new(source);
    assert_eq!(lookup.lookup(0), (1, 1));
    assert_eq!(lookup.lookup(6), (2, 1));
    assert_eq!(lookup.lookup(10), (2, 5));
}

#[test]
fn line_col_lookup_multibyte() {
    let source = "line1我\nl们ine2";
    let lookup = LineColLookup::new(source);
    assert_eq!(&source[0..1], "l");
    assert_eq!(lookup.lookup(0), (1, 1));
    assert_eq!(&source[9..10], "l");
    assert_eq!(lookup.lookup(9), (2, 1));
    assert_eq!(&source[14..15], "n");
    assert_eq!(lookup.lookup(14), (2, 4));
}

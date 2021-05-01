use std::process::exit;

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
        match self {
            InvalidToken { location } => eprintln!(
                "{}: error: invalid character '{}'",
                location,
                ctx.source[location..].chars().next().unwrap(),
            ),
            UnrecognizedToken {
                token: (start, token, _),
                expected: _,
            } => eprintln!("{}: error: unexpected token '{}'", start, token),
            UnrecognizedEOF { location, .. } => eprintln!("{}: error: unexpected EOF", location),
            ExtraToken { .. } => unimplemented!("extra token"),
            User { .. } => unimplemented!("user"),
        }
        exit(1);
    }
}

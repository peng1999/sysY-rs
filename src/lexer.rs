use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("const")]
    KeywordConst,
    #[token("int")]
    KeywordInt,
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,
    #[token("while")]
    KeywordWhile,
    #[token("break")]
    KeywordBreak,
    #[token("continue")]
    KeywordContinue,
    #[token("return")]
    KeywordReturn,

    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token(">")]
    GT,
    #[token("<")]
    LT,
    #[token("=")]
    Assign,

    #[regex("[a-zA-Z_][0-9a-zA-Z_]*")]
    Ident,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

pub fn run(source: String) {
    let lex = Token::lexer(&source);

    for token in lex {
        println!("{:?}", token);
    }
}

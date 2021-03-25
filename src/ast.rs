use crate::context::Symbol;

#[derive(Debug)]
pub enum Item {
    FuncDef(Symbol, Symbol, Vec<()>, Vec<Stmt>),
}

#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Decl(Symbol, Symbol, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Expr(Expr),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    Return(Option<Expr>),
    Break,
    Empty,
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Vec<Expr>),
    Ident(Symbol),
    Lit(i32),
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

use crate::context::IString;

#[derive(Debug)]
pub enum Item {
    FuncDef(IString, IString, Vec<()>, Vec<Stmt>),
}

#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Decl(Ty, IString, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Expr(Expr),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    Return(Option<Expr>),
    Break,
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty {
    Int,
    Bool,
    Array(i32),
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Vec<Expr>),
    Ident(IString),
    IntLit(i32),
    BoolLit(bool),
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

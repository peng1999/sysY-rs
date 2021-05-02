use std::ops::Deref;

use crate::context::IString;

#[derive(Debug)]
pub struct Spanned<T> {
    start: usize,
    end: usize,
    pub inner: Box<T>,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &*self.inner
    }
}

impl<T> Spanned<T> {
    pub fn new(start: usize, end: usize, inner: T) -> Spanned<T> {
        Spanned { start, end, inner: Box::new(inner) }
    }

    pub fn into_inner(self) -> T {
        *self.inner
    }

    pub fn span(&self) -> (usize, usize) {
        (self.start, self.end)
    }
}

#[derive(Debug)]
pub enum Item {
    FuncDef(IString, IString, Vec<()>, Vec<Stmt>),
}

#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Decl(Ty, IString, Expr),
    Assign(Expr, Expr),
    Expr(Expr),
    If(Expr, Box<Stmt>, Box<Stmt>),
    While(Expr, Box<Stmt>),
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

pub type Expr = Spanned<ExprKind>;

#[derive(Debug)]
pub enum ExprKind {
    Binary(BinOp, Expr, Expr),
    Unary(UnOp, Expr),
    Call(Expr, Vec<Expr>),
    Index(Expr, Vec<Expr>),
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

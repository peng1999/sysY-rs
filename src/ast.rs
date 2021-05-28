use std::ops::Deref;

use crate::ty::Ty;

/// Interned string occurs in source code
pub type IString = string_interner::DefaultSymbol;

#[derive(Debug)]
pub struct Spanned<T> {
    span: (usize, usize),
    inner: Box<T>,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &*self.inner
    }
}

impl<T> Spanned<T> {
    pub fn new(span: (usize, usize), inner: T) -> Spanned<T> {
        Spanned {
            span,
            inner: Box::new(inner),
        }
    }

    pub fn into_inner(self) -> T {
        *self.inner
    }

    pub fn span(&self) -> (usize, usize) {
        self.span
    }
}

#[derive(Debug)]
pub enum Item {
    FuncDef(FuncHead, Vec<Stmt>),
    FuncDecl(FuncHead),
}

#[derive(Debug)]
pub struct FuncHead {
    pub name: IString,
    pub ret_ty: Ty,
    pub param: Vec<(Ty, IString)>,
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

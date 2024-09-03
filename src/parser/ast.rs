use crate::span::Span;

use super::node::NodeID;

#[derive(Debug, Clone, Copy)]
pub struct Program<'cx> {
    pub id: NodeID,
    pub stmts: &'cx [&'cx Stmt<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct Stmt<'cx> {
    pub id: NodeID,
    pub kind: StmtKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum StmtKind<'cx> {
    ExprStmt(&'cx Expr<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'cx> {
    pub id: NodeID,
    pub kind: ExprKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'cx> {
    BinOp(&'cx BinExpr<'cx>),
    BoolLit(&'cx BoolLit),
    NumLit(&'cx NumLit),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub struct BinExpr<'cx> {
    pub id: NodeID,
    pub left: &'cx Expr<'cx>,
    pub op: BinOp,
    pub right: &'cx Expr<'cx>,
    pub span: Span
}

#[derive(Debug, Clone, Copy)]
pub struct Lit<T> {
    pub id: NodeID,
    pub val: T,
    pub span: Span,
}

pub type NumLit = Lit<f64>;
pub type BoolLit = Lit<bool>;

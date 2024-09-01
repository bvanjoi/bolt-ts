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
    BinOp(&'cx BinOpExpr<'cx>),
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
pub struct BinOpExpr<'cx> {
    left: &'cx Expr<'cx>,
    op: BinOp,
    right: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct NumLit {
    pub num: f64,
    pub span: Span,
}

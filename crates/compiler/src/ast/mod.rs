mod node;
mod node_flags;

pub use node::{Node, NodeID};
use rts_span::Span;

use crate::atoms::AtomId;

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
    Var(&'cx VarStmt<'cx>),
    Expr(&'cx Expr<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub enum VarKind {
    Var,
    Let,
    Const,
}

#[derive(Debug, Clone, Copy)]
pub struct VarStmt<'cx> {
    pub id: NodeID,
    pub kind: VarKind,
    pub span: Span,
    pub list: &'cx [&'cx VarDecl<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'cx> {
    pub id: NodeID,
    pub kind: ExprKind<'cx>,
}

impl Expr<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            ExprKind::BinOp(bin) => bin.span,
            ExprKind::BoolLit(lit) => lit.span,
            ExprKind::NumLit(lit) => lit.span,
            ExprKind::NullLit(lit) => lit.span,
            ExprKind::StringLit(lit) => lit.span,
            ExprKind::Ident(ident) => ident.span,
            ExprKind::ArrayLit(lit) => lit.span,
            ExprKind::Omit(expr) => expr.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'cx> {
    BinOp(&'cx BinExpr<'cx>),
    BoolLit(&'cx BoolLit),
    NumLit(&'cx NumLit),
    StringLit(&'cx StringLit),
    NullLit(&'cx NullLit),
    ArrayLit(&'cx ArrayLit<'cx>),
    Ident(&'cx Ident),
    Omit(&'cx OmitExpr)
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayLit<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub elems: &'cx [&'cx Expr<'cx>]
}

#[derive(Debug, Clone, Copy)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Pipe,
}

impl BinOpKind {
    pub fn as_str(self) -> &'static str {
        match self {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Pipe => "|",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinExpr<'cx> {
    pub id: NodeID,
    pub left: &'cx Expr<'cx>,
    pub op: BinOp,
    pub right: &'cx Expr<'cx>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Lit<T> {
    pub id: NodeID,
    pub val: T,
    pub span: Span,
}

pub type NumLit = Lit<f64>;
pub type BoolLit = Lit<bool>;
pub type NullLit = Lit<()>;
pub type StringLit = Lit<AtomId>;

#[derive(Debug, Clone, Copy)]
pub struct VarDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
    pub init: Option<&'cx Expr<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub id: NodeID,
    pub span: Span,
    pub name: AtomId,
}


#[derive(Debug, Clone, Copy)]
pub struct OmitExpr {
    pub id: NodeID,
    pub span: Span,
}
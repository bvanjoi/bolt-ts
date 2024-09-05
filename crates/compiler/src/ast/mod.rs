mod node;

pub use node::{Node, NodeID};
use rts_span::Span;

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

impl Expr<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            ExprKind::BinOp(bin) => bin.span,
            ExprKind::BoolLit(lit) => lit.span,
            ExprKind::NumLit(lit) => lit.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'cx> {
    BinOp(&'cx BinExpr<'cx>),
    BoolLit(&'cx BoolLit),
    NumLit(&'cx NumLit),
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
}

impl BinOpKind {
    pub fn as_str(self) -> &'static str {
        match self {
            BinOpKind::Add => "+",
            BinOpKind::Sub => todo!(),
            BinOpKind::Mul => todo!(),
            BinOpKind::Div => todo!(),
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

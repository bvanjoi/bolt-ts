use super::node::NodeID;

#[derive(Debug, Clone, Copy)]
pub struct Stmt<'p> {
    id: NodeID,
    kind: StmtKind<'p>,
}

#[derive(Debug, Clone, Copy)]
pub enum StmtKind<'p> {
    ExprStmt(&'p Expr<'p>),
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'p> {
    pub id: NodeID,
    kind: ExprKind<'p>
}


#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub struct BinOpExpr<'p> {
    left: &'p Expr<'p>,
    op: BinOp,
    right: &'p Expr<'p>
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'p> {
    BinOp(&'p BinOpExpr<'p>)
}

#[derive(Debug, Clone, Copy)]
pub struct Program<'p> {
    pub id: NodeID,
    pub stmts: &'p [Stmt<'p>],
}

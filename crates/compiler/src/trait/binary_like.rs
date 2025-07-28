use bolt_ts_ast as ast;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryLikeOp {
    // assign op
    Eq,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    ShlEq,
    ShrEq,
    UShrEq,
    BitAndEq,
    BitXorEq,
    BitOrEq,
    // binary op
    Add,
    Sub,
    Mul,
    Div,
    Pipe,
    PipePipe,
    Less,
    LessEq,
    Shl,
    Great,
    GreatEq,
    Shr,
    UShr,
    BitAnd,
    LogicalAnd,
    EqEq,
    EqEqEq,
    Instanceof,
    In,
    Satisfies,
}

pub trait BinaryLike<'cx>: Copy + std::fmt::Debug {
    fn left(&self) -> &'cx ast::Expr<'cx>;
    fn op(&self) -> BinaryLikeOp;
    fn right(&self) -> &'cx ast::Expr<'cx>;
}

impl<'cx> BinaryLike<'cx> for ast::AssignExpr<'cx> {
    fn left(&self) -> &'cx ast::Expr<'cx> {
        self.left
    }

    fn op(&self) -> BinaryLikeOp {
        unsafe { std::mem::transmute(self.op) }
    }

    fn right(&self) -> &'cx ast::Expr<'cx> {
        self.right
    }
}

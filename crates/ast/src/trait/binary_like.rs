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
    fn left(&self) -> &'cx crate::Expr<'cx>;
    fn op(&self) -> BinaryLikeOp;
    fn right(&self) -> &'cx crate::Expr<'cx>;
}

impl<'cx> BinaryLike<'cx> for crate::AssignExpr<'cx> {
    fn left(&self) -> &'cx crate::Expr<'cx> {
        self.left
    }

    fn op(&self) -> BinaryLikeOp {
        unsafe { std::mem::transmute(self.op) }
    }

    fn right(&self) -> &'cx crate::Expr<'cx> {
        self.right
    }
}

use rts_span::Span;

use crate::{ast, ty};

use super::TyChecker;

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
}

pub(super) trait BinaryLikeExpr<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn left(&self) -> &'cx ast::Expr<'cx>;
    fn op(&self) -> BinaryLikeOp;
    fn right(&self) -> &'cx ast::Expr<'cx>;
}

impl<'cx> BinaryLikeExpr<'cx> for ast::AssignExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }

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

impl<'cx> TyChecker<'cx> {
    fn check_assign_op(&mut self, span: Span, left: &'cx ty::Ty<'cx>, right: &'cx ty::Ty<'cx>) {
        self.check_type_assignable_to_and_optionally_elaborate(span, right, left);
    }

    pub(super) fn check_binary_like_expr(
        &mut self,
        expr: &impl BinaryLikeExpr<'cx>,
        left_ty: &'cx ty::Ty<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        match expr.op() {
            BinaryLikeOp::Eq => {
                self.check_assign_op(expr.right().span(), left_ty, right_ty);
                right_ty
            }
            _ => unreachable!(),
        }
    }
}

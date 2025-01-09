use crate::ir::{BinaryLike, BinaryLikeOp};
use crate::{ast, ty};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    fn check_assign_op(
        &mut self,
        assign_ty: &'cx ty::Ty<'cx>,
        value_ty: &'cx ty::Ty<'cx>,
        left: &'cx ast::Expr<'cx>,
        right: &'cx ast::Expr<'cx>,
    ) {
        self.check_type_assignable_to_and_optionally_elaborate(
            value_ty,
            assign_ty,
            Some(right.id()),
        );
    }

    pub(super) fn check_binary_like_expr(
        &mut self,
        expr: &impl BinaryLike<'cx>,
        left_ty: &'cx ty::Ty<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        match expr.op() {
            BinaryLikeOp::Eq => {
                self.check_assign_op(left_ty, right_ty, expr.left(), expr.right());
                right_ty
            }
            _ => unreachable!(),
        }
    }
}

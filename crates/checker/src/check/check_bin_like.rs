use super::TyChecker;
use super::ty;

use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_assign_op(
        &mut self,
        assign_ty: &'cx ty::Ty<'cx>,
        value_ty: &'cx ty::Ty<'cx>,
        left: ast::NodeID,
        right: ast::NodeID,
    ) {
        self.check_type_assignable_to_and_optionally_elaborate(
            value_ty,
            assign_ty,
            Some(left),
            Some(right),
        );
    }

    pub(super) fn check_binary_like_expr_for_equal(
        &mut self,
        left_ty: &'cx ty::Ty<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
        left: ast::NodeID,
        right: ast::NodeID,
    ) -> &'cx ty::Ty<'cx> {
        self.check_assign_op(left_ty, right_ty, left, right);
        right_ty
    }
}

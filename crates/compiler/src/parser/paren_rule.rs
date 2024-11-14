use crate::ast;

use super::utils::is_left_hand_side_expr_kind;

pub(super) trait ParenRuleTrait<'cx> {
    fn paren_left_side_of_access(
        &self,
        expr: &'cx ast::Expr<'cx>,
        optional_chain: bool,
    ) -> &'cx ast::Expr<'cx>;
}

pub(super) struct ParenRule;

impl<'cx> ParenRuleTrait<'cx> for ParenRule {
    fn paren_left_side_of_access(
        &self,
        expr: &'cx ast::Expr<'cx>,
        optional_chain: bool,
    ) -> &'cx ast::Expr<'cx> {
        todo!()
    }
}

pub(super) struct NoParenRule;
impl<'cx> ParenRuleTrait<'cx> for NoParenRule {
    fn paren_left_side_of_access(
        &self,
        expr: &'cx ast::Expr<'cx>,
        _: bool,
    ) -> &'cx ast::Expr<'cx> {
        assert!(is_left_hand_side_expr_kind(expr));
        expr
    }
}

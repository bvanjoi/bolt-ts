use super::super::Expr;

pub trait ParenRuleTrait<'cx> {
    fn paren_left_side_of_access(
        &self,
        expr: &'cx Expr<'cx>,
        optional_chain: bool,
    ) -> &'cx Expr<'cx>;
}

pub struct ParenRule;

impl<'cx> ParenRuleTrait<'cx> for ParenRule {
    fn paren_left_side_of_access(
        &self,
        _expr: &'cx Expr<'cx>,
        _optional_chain: bool,
    ) -> &'cx Expr<'cx> {
        todo!()
    }
}

pub struct NoParenRule;
impl<'cx> ParenRuleTrait<'cx> for NoParenRule {
    fn paren_left_side_of_access(&self, expr: &'cx Expr<'cx>, _: bool) -> &'cx Expr<'cx> {
        debug_assert!(
            expr.is_left_hand_side_expr_kind(),
            "Expected left-hand side expression, but got {expr:#?}"
        );
        expr
    }
}

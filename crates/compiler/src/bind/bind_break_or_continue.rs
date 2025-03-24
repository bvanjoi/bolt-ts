use super::BinderState;

pub(super) trait BreakOrContinue {}
impl<'cx> BreakOrContinue for bolt_ts_ast::BreakStmt<'cx> {}
impl<'cx> BreakOrContinue for bolt_ts_ast::ContinueStmt<'cx> {}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_break_or_continue_stmt(&mut self, node: &impl BreakOrContinue) {}
}

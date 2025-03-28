use super::BinderState;

pub(super) trait BreakOrContinue {}
impl BreakOrContinue for bolt_ts_ast::BreakStmt<'_> {}
impl BreakOrContinue for bolt_ts_ast::ContinueStmt<'_> {}

impl BinderState<'_, '_, '_> {
    pub(super) fn bind_break_or_continue_stmt(&mut self, node: &impl BreakOrContinue) {}
}

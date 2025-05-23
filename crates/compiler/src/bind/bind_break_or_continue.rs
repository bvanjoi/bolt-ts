use super::BinderState;

pub(super) trait BreakOrContinue<'cx> {
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident>;
}
impl<'cx> BreakOrContinue<'cx> for bolt_ts_ast::BreakStmt<'cx> {
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident> {
        self.label
    }
}
impl<'cx> BreakOrContinue<'cx> for bolt_ts_ast::ContinueStmt<'cx> {
    fn label(&self) -> Option<&'cx bolt_ts_ast::Ident> {
        self.label
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_break_or_continue_stmt(&mut self, node: &impl BreakOrContinue<'cx>) {
        if let Some(label) = node.label() {
            self.bind(label.id);
        }
    }
}

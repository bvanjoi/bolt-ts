use super::Resolver;
use crate::ir;

impl<'cx, 'r> Resolver<'cx, 'r> {
    pub(super) fn resolve_call_like_expr(&mut self, expr: &impl ir::CallLike<'cx>) {
        self.resolve_expr(expr.callee());
        for arg in expr.args() {
            self.resolve_expr(arg);
        }
    }
}

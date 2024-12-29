use super::Resolver;
use crate::ir;

impl<'cx> Resolver<'cx, '_> {
    pub(super) fn resolve_call_like_expr(&mut self, expr: &impl ir::CallLike<'cx>) {
        self.resolve_expr(expr.callee());
        for arg in expr.args() {
            self.resolve_expr(arg);
        }
    }
}

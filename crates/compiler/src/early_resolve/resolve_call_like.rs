use super::Resolver;
use crate::ir;

impl<'cx> Resolver<'cx, '_, '_> {
    pub(super) fn resolve_call_like_expr(&mut self, expr: &impl ir::CallLike<'cx>) {
        self.resolve_expr(expr.callee());
        if let Some(ty_args) = expr.ty_args() {
            for ty_arg in ty_args.list {
                self.resolve_ty(ty_arg);
            }
        }
        for arg in expr.args() {
            self.resolve_expr(arg);
        }
    }
}

use super::BinderState;
use crate::ir::CallLike;

impl<'cx> BinderState<'cx> {
    pub(super) fn bind_call_like(&mut self, expr: &'cx impl CallLike<'cx>) {
        self.bind_expr(expr.callee());
        if let Some(ty_args) = expr.ty_args() {
            for ty_arg in ty_args.list {
                self.bind_ty(ty_arg);
            }
        }
        for arg in expr.args() {
            self.bind_expr(arg);
        }
    }
}

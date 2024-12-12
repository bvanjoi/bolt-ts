use super::BinderState;
use crate::ir::CallLike;

impl<'cx> BinderState<'cx> {
    pub(super) fn bind_call_like(&mut self, call: &'cx impl CallLike<'cx>) {
        self.bind_expr(call.callee());
        for arg in call.args() {
            self.bind_expr(arg);
        }
    }
}

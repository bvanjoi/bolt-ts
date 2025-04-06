use super::BinderState;

pub(super) trait RetOrThrow<'cx> {
    fn expr(&self) -> Option<&'cx bolt_ts_ast::Expr<'cx>>;
    fn is_ret(&self) -> bool;
}

impl<'cx> RetOrThrow<'cx> for bolt_ts_ast::RetStmt<'cx> {
    fn expr(&self) -> Option<&'cx bolt_ts_ast::Expr<'cx>> {
        self.expr
    }
    fn is_ret(&self) -> bool {
        true
    }
}

impl<'cx> RetOrThrow<'cx> for bolt_ts_ast::ThrowStmt<'cx> {
    fn expr(&self) -> Option<&'cx bolt_ts_ast::Expr<'cx>> {
        Some(self.expr)
    }
    fn is_ret(&self) -> bool {
        false
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_ret_or_throw(&mut self, node: &impl RetOrThrow<'cx>) {
        let saved_in_return_position = self.in_return_position;
        self.in_return_position = true;
        if let Some(expr) = node.expr() {
            self.bind(expr.id());
        }
        self.in_return_position = saved_in_return_position;
        if node.is_ret() {
            self.has_explicit_ret = true;
            if let Some(current_return_target) = self.current_return_target {
                // TODO:
                // self.flow_nodes
                //     .add_antecedent(current_return_target, self.current_flow.unwrap());
            }
        }
        self.current_flow = Some(self.unreachable_flow_node);
        self.has_flow_effects = true;
    }
}

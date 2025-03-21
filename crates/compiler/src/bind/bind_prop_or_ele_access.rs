use super::BinderState;

trait PropOrEleAccessExpr<'cx> {
    fn is_optional_chain(&self) -> bool;
    fn bind_expr(&self, b: &mut BinderState<'cx, '_, '_>);
    fn bind_name(&self, b: &mut BinderState<'cx, '_, '_>);
}

impl<'cx> PropOrEleAccessExpr<'cx> for bolt_ts_ast::PropAccessExpr<'cx> {
    fn is_optional_chain(&self) -> bool {
        false
    }
    fn bind_expr(&self, b: &mut BinderState<'cx, '_, '_>) {
        b.bind(self.expr.id());
    }
    fn bind_name(&self, b: &mut BinderState<'cx, '_, '_>) {
        b.bind(self.name.id);
    }
}
impl<'cx> PropOrEleAccessExpr<'cx> for bolt_ts_ast::EleAccessExpr<'cx> {
    fn is_optional_chain(&self) -> bool {
        false
    }
    fn bind_expr(&self, b: &mut BinderState<'cx, '_, '_>) {
        b.bind(self.expr.id());
    }
    fn bind_name(&self, b: &mut BinderState<'cx, '_, '_>) {
        b.bind(self.arg.id());
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_access_expr_flow(&mut self, node: &impl PropOrEleAccessExpr<'cx>) {
        if node.is_optional_chain() {
            todo!()
        } else {
            node.bind_expr(self);
            node.bind_name(self);
        }
    }
}

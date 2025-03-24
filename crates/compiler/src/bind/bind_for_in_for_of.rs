use super::BinderState;

pub(super) trait ForInForOf<'cx> {
    fn expr(&self) -> &'cx bolt_ts_ast::Expr<'cx>;
    fn init(&self) -> bolt_ts_ast::ForInitKind<'cx>;
    fn stmt(&self) -> &'cx bolt_ts_ast::Stmt<'cx>;
}

impl<'cx> ForInForOf<'cx> for bolt_ts_ast::ForInStmt<'cx> {
    fn expr(&self) -> &'cx bolt_ts_ast::Expr<'cx> {
        &self.expr
    }
    fn init(&self) -> bolt_ts_ast::ForInitKind<'cx> {
        self.init
    }
    fn stmt(&self) -> &'cx bolt_ts_ast::Stmt<'cx> {
        &self.body
    }
}

impl<'cx> ForInForOf<'cx> for bolt_ts_ast::ForOfStmt<'cx> {
    fn expr(&self) -> &'cx bolt_ts_ast::Expr<'cx> {
        &self.expr
    }
    fn init(&self) -> bolt_ts_ast::ForInitKind<'cx> {
        self.init
    }
    fn stmt(&self) -> &'cx bolt_ts_ast::Stmt<'cx> {
        &self.body
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_for_in_or_for_of_stmt(&mut self, node: &impl ForInForOf<'cx>) {
        self.bind(node.expr().id());
        use bolt_ts_ast::ForInitKind::*;
        match node.init() {
            Var(list) => {
                for item in list {
                    self.bind(item.id);
                }
            }
            Expr(expr) => self.bind(expr.id()),
        }
        self.bind_iterative_stmt(node.stmt());
    }
}

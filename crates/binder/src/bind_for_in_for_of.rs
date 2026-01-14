use super::BinderState;

pub(super) trait ForInForOf<'cx> {
    fn expr(&self) -> &'cx bolt_ts_ast::Expr<'cx>;
    fn init(&self) -> bolt_ts_ast::ForInitKind<'cx>;
    fn stmt(&self) -> &'cx bolt_ts_ast::Stmt<'cx>;
}

impl<'cx> ForInForOf<'cx> for bolt_ts_ast::ForInStmt<'cx> {
    fn expr(&self) -> &'cx bolt_ts_ast::Expr<'cx> {
        self.expr
    }
    fn init(&self) -> bolt_ts_ast::ForInitKind<'cx> {
        self.init
    }
    fn stmt(&self) -> &'cx bolt_ts_ast::Stmt<'cx> {
        self.body
    }
}

impl<'cx> ForInForOf<'cx> for bolt_ts_ast::ForOfStmt<'cx> {
    fn expr(&self) -> &'cx bolt_ts_ast::Expr<'cx> {
        self.expr
    }
    fn init(&self) -> bolt_ts_ast::ForInitKind<'cx> {
        self.init
    }
    fn stmt(&self) -> &'cx bolt_ts_ast::Stmt<'cx> {
        self.body
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn bind_for_in_or_for_of_stmt(&mut self, node: &impl ForInForOf<'cx>) {
        let pre_loop_label = {
            let label = self.flow_nodes.create_loop_label();
            self.set_continue_target(label)
        };
        let post_loop_label = self.flow_nodes.create_branch_label();
        self.bind(node.expr().id());
        self.flow_nodes
            .add_antecedent(pre_loop_label, self.current_flow.unwrap());
        self.current_flow = Some(pre_loop_label);
        self.flow_nodes
            .add_antecedent(post_loop_label, self.current_flow.unwrap());
        use bolt_ts_ast::ForInitKind::*;
        match node.init() {
            Var(list) => {
                for item in list {
                    self.bind(item.id);
                }
            }
            Expr(expr) => {
                self.bind(expr.id());
                // TODO: bind_assignment_target_flow
            }
        }
        self.bind_iterative_stmt(node.stmt(), post_loop_label, pre_loop_label);
        self.flow_nodes
            .add_antecedent(pre_loop_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(post_loop_label));
    }
}

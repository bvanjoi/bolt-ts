use bolt_ts_ast as ast;

use super::NodeFlags;
use super::TyChecker;

impl TyChecker<'_> {
    pub(super) fn check_node_deferred(&mut self, node: ast::NodeID) {
        let root = node.into_root();
        let flags = self.get_node_links(root).flags();
        if !flags.intersects(NodeFlags::TYPE_CHECKED) {
            self.deferred_nodes[node.module().as_usize()].insert(node);
        }
    }

    pub fn check_deferred_nodes(&mut self, module_id: bolt_ts_span::ModuleID) {
        let mut deferred_nodes = std::mem::take(&mut self.deferred_nodes[module_id.as_usize()]);
        while let Some(node) = deferred_nodes.pop() {
            self.check_deferred_node(node);
        }
    }

    fn check_deferred_node(&mut self, node: ast::NodeID) {
        let n = self.p.node(node);
        use bolt_ts_ast::Node::*;
        match n {
            FnExpr(expr) => self.check_fn_like_expr_deferred(expr),
            ArrowFnExpr(expr) => self.check_fn_like_expr_deferred(expr),
            ObjectMethodMember(expr) => {
                let body = ast::ArrowFnExprBody::Block(expr.body);
                self.check_fn_like_expr_or_object_method_member_deferred(expr.id, body)
            }
            _ => unreachable!("{n:#?}"),
        }
    }
}

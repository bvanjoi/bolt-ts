use bolt_ts_ast as ast;

use crate::check::errors;
use crate::check::relation::RelationKind;
use crate::check::symbol_info::SymbolInfo;

use super::NodeCheckFlags;
use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_node_deferred(&mut self, node: ast::NodeID) {
        let root = node.into_root();
        let flags = self.get_node_links(root).flags();
        if !flags.intersects(NodeCheckFlags::TYPE_CHECKED) {
            self.deferred_nodes[node.module().as_usize()].insert(node);
        }
    }

    pub fn check_deferred_nodes(&mut self, module_id: bolt_ts_span::ModuleID) {
        let mut deferred_nodes = std::mem::take(&mut self.deferred_nodes[module_id.as_usize()]);
        while let Some(node) = deferred_nodes.pop() {
            self.check_deferred_node(node);
        }
    }

    fn check_assertion_deferred(
        &mut self,
        node_id: ast::NodeID,
        span: bolt_ts_span::Span,
        assert_ty: &'cx ast::Ty<'cx>,
    ) {
        let Some(expr_ty) = self.get_node_links(node_id).get_assertion_expression_ty() else {
            unreachable!()
        };
        let expr_ty = self.get_base_ty_of_literal_ty(expr_ty);
        let expr_ty = self.get_regular_ty_of_object_literal(expr_ty);
        let target_ty = self.get_ty_from_type_node(assert_ty);
        if !self.is_error(target_ty)
            && let widened_ty = self.get_widened_ty(expr_ty)
            && !self.is_type_related_to(target_ty, widened_ty, RelationKind::Comparable)
        {
            // TODO: report error in `check_type_comparable_to`
            if !self.check_type_comparable_to(expr_ty, target_ty, Some(node_id)) {
                let error = errors::ConversionOfType0ToType1MayBeAMistakeBecauseNeitherTypeSufficientlyOverlapsWithTheOtherIfThisWasIntentionalConvertTheExpressionToUnknownFirst {
                    span: span,
                    source_ty: widened_ty.to_string(self),
                    target_ty: target_ty.to_string(self),
                };
                self.push_error(Box::new(error));
            }
        }
    }

    fn check_deferred_node(&mut self, node: ast::NodeID) {
        let saved_current_node = self.current_node;
        self.current_node = Some(node);
        self.instantiation_count = 0;

        use bolt_ts_ast::Node::*;
        match self.p.node(node) {
            FnExpr(expr) => self.check_fn_like_expr_deferred(expr),
            ArrowFnExpr(expr) => self.check_fn_like_expr_deferred(expr),
            ObjectMethodMember(expr) => {
                let body = ast::ArrowFnExprBody::Block(expr.body);
                self.check_fn_like_expr_or_object_method_member_deferred(expr, body)
            }
            TyAssertionExpr(n) => {
                self.check_assertion_deferred(n.id, n.span, n.ty);
            }
            AsExpr(n) => {
                self.check_assertion_deferred(n.id, n.span, n.ty);
            }
            _ => unreachable!("{:#?}", self.p.node(node)),
        }

        self.current_node = saved_current_node;
    }
}

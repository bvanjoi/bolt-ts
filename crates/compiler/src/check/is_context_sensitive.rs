use crate::keyword;
use bolt_ts_ast as ast;

use super::TyChecker;

impl TyChecker<'_> {
    fn has_context_sensitive_params(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        if node.ty_params().is_none() {
            if let Some(params) = node.params() {
                if params.iter().any(|p| p.ty.is_none()) {
                    return true;
                }
            }

            if !node.is_arrow_fn_expr() {
                let param = node.params().and_then(|params| params.first());
                if param.map_or(true, |param| match param.name {
                    bolt_ts_ast::Binding::Ident(ident) => ident.name == keyword::KW_THIS,
                    bolt_ts_ast::Binding::ObjectPat(_) => false,
                    bolt_ts_ast::Binding::ArrayPat(_) => todo!(),
                }) {
                    return true;
                }
            }
        }
        false
    }

    fn has_context_sensitive_return_expr(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        if node.ty_params().is_some() {
            false
        } else if let Some(ast::ArrowFnExprBody::Expr(e)) = node.fn_body() {
            self.is_context_sensitive(e.id())
        } else {
            false
        }
    }

    fn is_context_sensitive_fn_like(&self, id: ast::NodeID) -> bool {
        self.has_context_sensitive_params(id) || self.has_context_sensitive_return_expr(id)
    }

    pub(super) fn is_context_sensitive(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        assert!(!node.is_class_method_ele(), "{:#?}", node);
        if node.is_fn_expr()
            || node.is_arrow_fn_expr()
            || node.is_method_signature()
            || node.is_fn_decl()
        {
            self.is_context_sensitive_fn_like(id)
        } else if let Some(o) = node.as_object_lit() {
            o.members.iter().any(|m| self.is_context_sensitive(m.id()))
        } else if let Some(a) = node.as_array_lit() {
            a.elems.iter().any(|m| self.is_context_sensitive(m.id()))
        } else if let Some(c) = node.as_cond_expr() {
            self.is_context_sensitive(c.when_true.id())
                || self.is_context_sensitive(c.when_false.id())
        } else if let Some(b) = node.as_bin_expr() {
            matches!(b.op.kind, ast::BinOpKind::PipePipe) || self.is_context_sensitive(b.right.id())
        } else if let Some(p) = node.as_paren_expr() {
            self.is_context_sensitive(p.expr.id())
        } else {
            false
        }
    }
}

use bolt_ts_ast as ast;
use bolt_ts_ast::keyword;
use bolt_ts_ast_visitor::Visitor;

use super::TyChecker;

impl TyChecker<'_> {
    pub(super) fn has_context_sensitive_params(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        if node.ty_params().is_none() {
            if let Some(params) = node.params()
                && params.iter().any(|p| p.ty.is_none())
            {
                return true;
            }

            if !node.is_arrow_fn_expr() {
                let param = node.params().and_then(|params| params.first());
                if param.is_none_or(|param| match param.name.kind {
                    bolt_ts_ast::BindingKind::Ident(ident) => ident.name == keyword::KW_THIS,
                    bolt_ts_ast::BindingKind::ObjectPat(_) => false,
                    bolt_ts_ast::BindingKind::ArrayPat(_) => todo!(),
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
        } else if let Some(fn_body) = node.fn_body() {
            match fn_body {
                ast::ArrowFnExprBody::Expr(n) => self.is_context_sensitive(n.id()),
                ast::ArrowFnExprBody::Block(n) => {
                    let mut visitor = FindContextSensitiveInArrowFnBlock {
                        checker: self,
                        found: false,
                    };
                    visitor.visit_block_stmt(n);
                    visitor.found
                }
            }
        } else {
            false
        }
    }

    fn is_context_sensitive_fn_like(&self, id: ast::NodeID) -> bool {
        // TODO: has_context_sensitive_yield_expression
        self.has_context_sensitive_params(id) || self.has_context_sensitive_return_expr(id)
    }

    pub(super) fn is_context_sensitive(&self, id: ast::NodeID) -> bool {
        let node = self.p.node(id);
        assert!(!node.is_class_method_elem(), "{node:#?}");
        use ast::Node::*;
        match node {
            FnExpr(_) | ArrowFnExpr(_) | MethodSignature(_) | FnDecl(_) | ObjectMethodMember(_) => {
                self.is_context_sensitive_fn_like(id)
            }
            ObjectLit(n) => n.members.iter().any(|m| self.is_context_sensitive(m.id())),
            ArrayLit(n) => n.elems.iter().any(|m| self.is_context_sensitive(m.id())),
            CondExpr(n) => {
                self.is_context_sensitive(n.when_true.id())
                    || self.is_context_sensitive(n.when_false.id())
            }
            BinExpr(n) => {
                matches!(n.op.kind, ast::BinOpKind::LogicalOr)
                    || self.is_context_sensitive(n.right.id())
            }
            ObjectPropAssignment(n) => self.is_context_sensitive(n.init.id()),
            ParenExpr(n) => self.is_context_sensitive(n.expr.id()),
            _ => false,
        }
    }
}

struct FindContextSensitiveInArrowFnBlock<'a, 'cx> {
    checker: &'a TyChecker<'cx>,
    found: bool,
}

impl<'a, 'cx> Visitor<'cx> for FindContextSensitiveInArrowFnBlock<'a, 'cx> {
    fn visit_ret_stmt(&mut self, node: &'cx bolt_ts_ast::RetStmt<'cx>) {
        if let Some(n) = node.expr {
            if self.checker.is_context_sensitive(n.id()) {
                self.found = true;
            }
        }
    }
}

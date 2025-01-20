use bolt_ts_span::Span;

use crate::bind::{SymbolFnKind, SymbolID};
use crate::{ast, keyword};

use super::errors;
use super::TyChecker;

impl TyChecker<'_> {
    pub(super) fn check_fn_like_symbol(&mut self, symbol: SymbolID) {
        let f = &self.binder.symbol(symbol).expect_fn();
        assert!(!f.decls.is_empty());
        assert_ne!(f.kind, SymbolFnKind::FnExpr);

        let mut last_seen_non_ambient_decl = None;
        for decl in &f.decls {
            let node = self.p.node(*decl);
            let is_ambient_context = node.node_flags().intersects(ast::NodeFlags::AMBIENT);
            let is_ambient_context_or_interface = self.p.parent(*decl).is_some_and(|parent| {
                let p = self.p.node(parent);
                p.is_interface_decl() || p.is_object_lit_ty()
            }) || is_ambient_context;

            if !is_ambient_context_or_interface {
                last_seen_non_ambient_decl = Some(*decl);
            }
        }

        if let Some(last_seen_non_ambient_decl) = last_seen_non_ambient_decl {
            let n = self.p.node(last_seen_non_ambient_decl);
            if n.fn_body().is_none()
                && !n.has_syntactic_modifier(ast::ModifierKind::Abstract.into())
            {
                if f.kind == SymbolFnKind::Ctor {
                    let node = self.p.node(f.decls[0]).expect_class_ctor();
                    let lo = node.span.lo;
                    let hi = lo + keyword::KW_CONSTRUCTOR_STR.len() as u32;
                    let span = Span::new(lo, hi, node.span.module);
                    let error = errors::ConstructorImplementationIsMissing { span };
                    self.push_error(Box::new(error));
                } else {
                    let span = self.p.node(f.decls[0]).ident_name().unwrap().span;
                    let error = errors::FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration { span };
                    self.push_error(Box::new(error));
                }
            }
        }
    }
}

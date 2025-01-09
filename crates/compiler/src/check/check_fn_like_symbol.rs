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

        let last_seen_non_ambient_decl = f.decls.iter().any(|decl| {
            let node = self.p.node(*decl);
            match node {
                ast::Node::FnDecl(n) => {
                    n.body.is_some()
                        || n.modifiers
                            .map_or(false, |m| m.flags.contains(ast::ModifierKind::Declare))
                }
                ast::Node::ClassCtor(n) => n.body.is_some(),
                ast::Node::ClassMethodEle(n) => {
                    if !n.flags.intersects(ast::NodeFlags::AMBIENT) {
                        n.body.is_some()
                    } else {
                        true
                    }
                }
                _ => unreachable!("{:#?}", node),
            }
        });

        if !last_seen_non_ambient_decl {
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

use crate::bind::{SymbolFnKind, SymbolID};
use crate::{ast, errors};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_symbol(&mut self, symbol: SymbolID) {
        let f = &self.binder.symbol(symbol).expect_fn();
        assert!(!f.decls.is_empty());
        assert_ne!(f.kind, SymbolFnKind::FnExpr);

        let last_seen_non_ambient_decl = f.decls.iter().any(|decl| {
            let node = self.p.node(*decl);
            match node {
                ast::Node::FnDecl(n) => {
                    n.body.is_some()
                        || n.modifiers.map_or(false, |m| {
                            // TODO: use bit flags
                            m.list.iter().any(|m| m.kind == ast::ModifierKind::Declare)
                        })
                }
                ast::Node::ClassCtor(n) => n.body.is_some(),
                ast::Node::ClassMethodEle(n) => n.body.is_some(),
                _ => unreachable!("{:#?}", node),
            }
        });

        if !last_seen_non_ambient_decl {
            let span = self.p.node(f.decls[0]).span();
            if f.kind == SymbolFnKind::Ctor {
                let error = errors::ConstructorImplementationIsMissing { span };
                self.push_error(span.module, Box::new(error));
            } else {
                let error = errors::FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration { span };
                self.push_error(span.module, Box::new(error));
            }
        }
    }
}

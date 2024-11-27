use bolt_ts_span::ModuleID;

use crate::{
    ast,
    bind::{SymbolFnKind, SymbolID, SymbolKind},
    errors,
};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_symbol(&mut self, module: ModuleID, symbol: SymbolID) {
        let SymbolKind::Function { decls, kind } =
            &self.binder.get(module).symbols.get(symbol).kind
        else {
            unreachable!()
        };
        assert!(!decls.is_empty());

        let last_seen_non_ambient_decl = decls.iter().any(|decl| {
            let node = self.p.get(module).nodes().get(*decl);
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
            let span = self.p.get(module).nodes().get(decls[0]).span();
            if *kind == SymbolFnKind::Ctor {
                let error = errors::ConstructorImplementationIsMissing { span };
                self.push_error(span.module, Box::new(error));
            } else if *kind == SymbolFnKind::Fn {
                let error = errors::FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration { span };
                self.push_error(span.module, Box::new(error));
            }
        }
    }
}

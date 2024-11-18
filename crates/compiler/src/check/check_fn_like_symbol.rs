use crate::{
    ast,
    bind::{SymbolFnKind, SymbolID, SymbolKind},
    errors,
};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_symbol(&mut self, symbol: SymbolID) {
        let SymbolKind::Function { decls, kind } = &self.symbols.get(symbol).kind else {
            unreachable!()
        };
        assert!(!decls.is_empty());

        let last_seen_non_ambient_decl = decls.iter().any(|decl| {
            let node = self.nodes.get(*decl);
            match node {
                ast::Node::FnDecl(n) => n.body.is_some(),
                ast::Node::ClassCtor(n) => n.body.is_some(),
                _ => unreachable!("{:#?}", node),
            }
        });

        if !last_seen_non_ambient_decl {
            if *kind == SymbolFnKind::Ctor {
                let span = self.nodes.get(decls[0]).span();
                let error = errors::ConstructorImplementationIsMissing { span };
                self.push_error(span.module, Box::new(error));
            }
        }
    }
}

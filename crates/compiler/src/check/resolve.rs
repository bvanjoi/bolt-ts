use super::TyChecker;
use crate::ast;
use crate::bind::{Symbol, SymbolID};

#[derive(Debug, Clone, Copy)]
pub enum ExpectedArgsCount {
    Count(usize),
    Range { lo: usize, hi: usize },
}

impl std::fmt::Display for ExpectedArgsCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedArgsCount::Count(c) => write!(f, "{c}"),
            ExpectedArgsCount::Range { lo, hi } => write!(f, "{lo}-{hi}"),
        }
    }
}

impl<'cx> TyChecker<'cx> {
    #[inline]
    pub(super) fn resolve_symbol_by_ident(&self, ident: &'cx ast::Ident) -> SymbolID {
        self.binder.final_res(ident.id)
    }

    #[inline]
    pub(super) fn get_symbol_of_decl(&self, id: ast::NodeID) -> SymbolID {
        #[cfg(debug_assertions)]
        fn is_decl(p: &super::Parser, node: ast::NodeID) -> bool {
            if p.node(node).is_decl() {
                true
            } else if p.node(node).is_ident() {
                p.node(p.parent(node).unwrap()).is_var_decl()
            } else {
                false
            }
        }
        debug_assert!(
            is_decl(self.p, id),
            "expected a decl node, but got {:#?}",
            self.p.node(id)
        );
        self.binder.final_res(id)
    }

    #[inline]
    pub(super) fn get_symbol_from_expr(&self, id: ast::NodeID) -> Option<SymbolID> {
        let node = self.p.node(id);
        if Symbol::can_have_symbol(node) {
            Some(self.binder.final_res(id))
        } else if let Some(ident) = node.as_ident() {
            Some(self.resolve_symbol_by_ident(ident))
        } else if node.as_prop_access_expr().is_some() {
            todo!()
        } else {
            None
        }
    }
}

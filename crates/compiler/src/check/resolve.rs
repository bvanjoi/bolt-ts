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
        assert!(
            self.p.node(id).is_decl(),
            "expected a decl node, but got {:?}",
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
        } else if let Some(_) = node.as_prop_access_expr() {
            todo!()
        } else {
            None
        }
    }
}

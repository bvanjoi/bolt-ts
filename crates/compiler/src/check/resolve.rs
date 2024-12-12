use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;

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
    pub(super) fn resolve_symbol_by_ident(&mut self, ident: &'cx ast::Ident) -> SymbolID {
        self.binder.final_res(ident.id)
    }
}

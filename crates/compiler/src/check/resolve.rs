use super::TyChecker;
use crate::ast;
use crate::bind::{Symbol, SymbolID, SymbolName};

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
        if let Some(id) = self.final_res.get(&ident.id) {
            return *id;
        }
        let res = resolve_symbol_by_ident(self, ident);
        self.final_res.insert(ident.id, res);
        res
    }
}

fn resolve_symbol_by_ident(checker: &TyChecker, ident: &ast::Ident) -> SymbolID {
    assert!(!checker.final_res.contains_key(&ident.id));
    let name = ident.name;
    let Some(mut scope_id) = checker.node_id_to_scope_id.get(&ident.id).copied() else {
        return Symbol::ERR;
    };
    let res = loop {
        if let Some(id) = checker
            .res
            .get(&(scope_id, SymbolName::Normal(name)))
            .copied()
        {
            break id;
        }
        if let Some(parent) = checker.scope_id_parent_map[&scope_id] {
            scope_id = parent;
        } else {
            break Symbol::ERR;
        }
    };
    res
}

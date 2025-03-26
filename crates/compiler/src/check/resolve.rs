use super::symbol_info::SymbolInfo;
use super::{TyChecker, errors};
use crate::bind::{Symbol, SymbolFlags, SymbolName};
use crate::bind::{SymbolID, SymbolTable};

use bolt_ts_ast as ast;
use bolt_ts_atom::AtomId;
use bolt_ts_utils::fx_hashset_with_capacity;

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
        let symbol = self.final_res(ident.id);
        symbol
    }

    #[inline]
    pub(super) fn final_res(&self, id: ast::NodeID) -> SymbolID {
        self.binder
            .get(id.module())
            .final_res
            .get(&id)
            .copied()
            .unwrap_or_else(|| {
                let n = self.p.node(id);
                let Some(node) = n.as_ident() else {
                    unreachable!("final_res not found for {:#?}", n);
                };
                let name = self.atoms.get(node.name);
                let span = self.p.node(id).span();
                panic!("The resolution of `{name}({span})` is not found.");
            })
    }

    pub(super) fn resolve_entity_name(
        &mut self,
        name: &'cx ast::EntityName<'cx>,
        meaning: SymbolFlags,
        dont_resolve_alias: bool,
    ) -> SymbolID {
        use bolt_ts_ast::EntityNameKind::*;
        let symbol;
        match name.kind {
            Ident(n) => {
                let id = self.resolve_symbol_by_ident(n);
                symbol = self.get_merged_symbol(id);
                if symbol == Symbol::ERR || dont_resolve_alias {
                    return symbol;
                }
            }
            Qualified(n) => {
                let ns = self.resolve_entity_name(n.left, SymbolFlags::NAMESPACE, false);
                if ns == Symbol::ERR {
                    return Symbol::ERR;
                }
                let exports = self.get_exports_of_symbol(ns);
                symbol = self
                    .get_symbol(exports, SymbolName::Normal(n.right.name), meaning)
                    .unwrap_or(Symbol::ERR);
            }
        }
        let flags = self.symbol(symbol).flags();
        if flags.intersects(meaning) {
            symbol
        } else if flags.intersects(SymbolFlags::ALIAS) {
            self.resolve_alias(symbol)
        } else {
            Symbol::ERR
        }
    }

    fn get_symbol(
        &mut self,
        symbols: &'cx SymbolTable,
        name: SymbolName,
        meaning: SymbolFlags,
    ) -> Option<SymbolID> {
        if !meaning.is_empty() {
            // TODO: get_merged_symbol;
            if let Some(symbol) = symbols.0.get(&name) {
                let flags = self.binder.symbol(*symbol).flags;
                if flags.intersects(meaning) {
                    return Some(*symbol);
                } else if flags.intersects(SymbolFlags::ALIAS) {
                    let target_flags = self.get_symbol_flags(*symbol, false);
                    if target_flags.intersects(meaning) {
                        return Some(*symbol);
                    }
                }
            }
        }
        None
    }

    fn get_symbol_flags(
        &mut self,
        mut symbol: SymbolID,
        exclude_ty_only_meaning: bool,
    ) -> SymbolFlags {
        let mut seen_symbols = fx_hashset_with_capacity(32);
        let mut symbol_flags = self.symbol(symbol).flags();
        let mut flags = symbol_flags;
        while symbol_flags.intersects(SymbolFlags::ALIAS) {
            let target = self.resolve_alias(symbol);
            let target = self.get_export_symbol_of_value_symbol_if_exported(target);
            if target == Symbol::ERR {
                return SymbolFlags::all();
            } else if target == symbol || seen_symbols.contains(&target) {
                break;
            }
            let t = self.symbol(target);
            let t_flags = t.flags();
            if t_flags.intersects(SymbolFlags::ALIAS) {
                seen_symbols.insert(target);
            }

            flags |= t_flags;
            symbol = target;
            symbol_flags = t_flags;
        }
        flags
    }

    fn get_export_symbol_of_value_symbol_if_exported(&mut self, symbol: SymbolID) -> SymbolID {
        // TODO: get merged symbol
        let s = self.binder.symbol(symbol);
        if s.flags.intersects(SymbolFlags::VALUE) {
            s.export_symbol.unwrap_or(symbol)
        } else {
            symbol
        }
    }

    fn check_alias_symbol(&mut self, node: ast::NodeID) {
        let symbol = self.get_symbol_of_decl(node);
        self.resolve_alias(symbol);
    }

    pub(super) fn check_import_binding(&mut self, node: ast::NodeID) {
        self.check_alias_symbol(node);
    }
}

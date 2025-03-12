use bolt_ts_ast::{self as ast, NodeID};
use bolt_ts_span::ModuleID;

use crate::bind::{Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::ty;

use super::TyChecker;

#[derive(Debug, Clone, Copy)]
pub(super) struct TransientSymbol<'cx> {
    pub(super) name: SymbolName,
    pub(super) flags: SymbolFlags,
    pub(super) links: crate::check::SymbolLinks<'cx>,
    // TODO: flatten
    pub(super) origin: Option<SymbolID>,
}

pub(super) fn create_transient_symbol<'cx>(
    symbols: &mut Vec<TransientSymbol<'cx>>,
    symbol: TransientSymbol<'cx>,
) -> SymbolID {
    let len = symbols.len();
    symbols.push(symbol);
    let s = SymbolID::new(ModuleID::TRANSIENT, len as u32);
    s
}

#[derive(Debug, Clone, Copy)]
pub(super) enum CheckSymbol<'cx, 'checker> {
    Transient(&'checker TransientSymbol<'cx>),
    Normal(&'checker Symbol),
}

impl CheckSymbol<'_, '_> {
    pub(crate) fn flags(&self) -> SymbolFlags {
        match self {
            CheckSymbol::Transient(symbol) => symbol.flags,
            CheckSymbol::Normal(symbol) => symbol.flags,
        }
    }
    pub(crate) fn name(&self) -> SymbolName {
        match self {
            CheckSymbol::Transient(symbol) => symbol.name,
            CheckSymbol::Normal(symbol) => symbol.name,
        }
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn create_transient_symbol(
        &mut self,
        name: SymbolName,
        symbol_flags: SymbolFlags,
        origin: Option<SymbolID>,
        links: crate::check::SymbolLinks<'cx>,
    ) -> SymbolID {
        let symbol_flags = symbol_flags | SymbolFlags::TRANSIENT;
        let symbol = TransientSymbol {
            name,
            flags: symbol_flags,
            links,
            origin,
        };
        create_transient_symbol(&mut self.transient_symbols, symbol)
    }

    pub(super) fn create_transient_symbol_with_ty(
        &mut self,
        source: SymbolID,
        ty: &'cx crate::ty::Ty<'cx>,
    ) -> SymbolID {
        let s = self.symbol(source);
        let symbol_flags = s.flags();
        let name = s.name();
        let check_flags = self.get_check_flags(source) & crate::ty::CheckFlags::READONLY;
        let links = crate::check::SymbolLinks::default()
            .with_check_flags(check_flags)
            .with_ty(ty)
            .with_target(source);
        self.create_transient_symbol(name, symbol_flags, Some(source), links)
    }

    pub(super) fn get_transient(&self, symbol: SymbolID) -> Option<&TransientSymbol<'cx>> {
        if symbol.module() == ModuleID::TRANSIENT {
            Some(&self.transient_symbols[symbol.index_as_usize()])
        } else {
            None
        }
    }

    pub(super) fn get_mut_transient(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&mut TransientSymbol<'cx>> {
        if symbol.module() == ModuleID::TRANSIENT {
            Some(&mut self.transient_symbols[symbol.index_as_usize()])
        } else {
            None
        }
    }

    pub(super) fn get_check_flags(&self, symbol: SymbolID) -> crate::ty::CheckFlags {
        if let Some(t) = self.get_transient(symbol) {
            t.links.get_check_flags().unwrap_or_default()
        } else {
            bitflags::Flags::empty()
        }
    }

    pub(super) fn symbol(&self, symbol: SymbolID) -> CheckSymbol<'cx, '_> {
        if symbol.module() == ModuleID::TRANSIENT {
            let symbol = self.get_transient(symbol).unwrap();
            CheckSymbol::Transient(symbol)
        } else {
            let symbol = self.binder.symbol(symbol);
            CheckSymbol::Normal(symbol)
        }
    }

    pub(crate) fn symbol_opt_decl(&self, symbol: SymbolID) -> Option<ast::NodeID> {
        if symbol.module() == ModuleID::TRANSIENT {
            let symbol = self.get_transient(symbol).unwrap();
            symbol.origin.and_then(|s| self.symbol_opt_decl(s))
        } else {
            symbol.opt_decl(self.binder)
        }
    }

    pub(crate) fn get_symbol_decl(&self, symbol: SymbolID) -> Option<NodeID> {
        if let Some(s) = self.get_transient(symbol) {
            if let Some(origin) = s.origin {
                self.get_symbol_decl(origin)
            } else {
                None
            }
        } else {
            symbol.opt_decl(self.binder)
        }
    }

    pub(crate) fn is_readonly_symbol(&self, symbol: SymbolID) -> bool {
        if self
            .get_check_flags(symbol)
            .intersects(ty::CheckFlags::READONLY)
        {
            return true;
        }
        let symbol_flags = self.symbol(symbol).flags();
        symbol_flags.intersects(SymbolFlags::PROPERTY)
            && self
                .decl_modifier_flags_from_symbol(symbol)
                .intersects(ast::ModifierKind::Readonly)
            || symbol_flags.intersects(SymbolFlags::ENUM_MEMBER)
    }

    pub(crate) fn get_late_flag(&self, symbol: SymbolID) -> ty::CheckFlags {
        self.get_check_flags(symbol)
            .intersection(ty::CheckFlags::LATE)
    }
}

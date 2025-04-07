use bolt_ts_ast::{self as ast, NodeID};
use bolt_ts_span::ModuleID;

use crate::bind::{Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::ty;

use super::TyChecker;
use super::symbol_info::SymbolInfo;

#[derive(Debug, Clone, Copy)]
pub(crate) struct TransientSymbol<'cx> {
    pub(super) name: SymbolName,
    pub(super) flags: SymbolFlags,
    pub(super) links: crate::check::SymbolLinks<'cx>,
    pub(super) declarations: Option<&'cx [ast::NodeID]>,
    pub(super) value_declaration: Option<ast::NodeID>,
    // TODO: flatten
    pub(super) origin: Option<SymbolID>,
}

#[derive(Debug, Default)]
pub(crate) struct TransientSymbols<'cx>(Vec<TransientSymbol<'cx>>);

impl<'cx> TransientSymbols<'cx> {
    pub(crate) fn new(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }

    pub(super) fn create_transient_symbol(&mut self, symbol: TransientSymbol<'cx>) -> SymbolID {
        let len = self.0.len();
        self.0.push(symbol);
        SymbolID::new(ModuleID::TRANSIENT, len as u32)
    }

    pub(super) fn get(&self, symbol: SymbolID) -> Option<&TransientSymbol<'cx>> {
        if symbol.module() == ModuleID::TRANSIENT {
            let idx = symbol.index_as_usize();
            debug_assert!(idx < self.0.len());
            Some(unsafe { self.0.get_unchecked(idx) })
        } else {
            None
        }
    }

    pub(super) fn get_mut(&mut self, symbol: SymbolID) -> Option<&mut TransientSymbol<'cx>> {
        if symbol.module() == ModuleID::TRANSIENT {
            let idx = symbol.index_as_usize();
            debug_assert!(idx < self.0.len());
            Some(unsafe { self.0.get_unchecked_mut(idx) })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) enum CheckSymbol<'cx, 'checker> {
    Transient(&'checker TransientSymbol<'cx>),
    Normal(&'checker Symbol),
}

#[derive(Debug, Clone, Copy)]
pub enum BorrowedDeclarations<'cx, 'checker> {
    FromTransient(Option<&'cx [ast::NodeID]>),
    FromNormal(&'checker [ast::NodeID]),
}

impl<'cx, 'checker> CheckSymbol<'cx, 'checker> {
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
    pub(crate) fn declarations(&self) -> BorrowedDeclarations<'cx, 'checker> {
        match self {
            CheckSymbol::Transient(s) => BorrowedDeclarations::FromTransient(s.declarations),
            CheckSymbol::Normal(s) => BorrowedDeclarations::FromNormal(&s.decls),
        }
    }
    pub(crate) fn value_declaration(&self) -> Option<ast::NodeID> {
        match self {
            CheckSymbol::Transient(s) => s.value_declaration,
            CheckSymbol::Normal(s) => s.value_decl,
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
        declarations: Option<&'cx [ast::NodeID]>,
        value_declaration: Option<ast::NodeID>,
    ) -> SymbolID {
        let symbol_flags = symbol_flags | SymbolFlags::TRANSIENT;
        let symbol = TransientSymbol {
            name,
            flags: symbol_flags,
            links,
            origin,
            declarations,
            value_declaration,
        };
        self.transient_symbols.create_transient_symbol(symbol)
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
        let value_declaration = s.value_declaration();
        
        self.create_transient_symbol(
            name,
            symbol_flags,
            Some(source),
            links,
            None,
            value_declaration,
        )
    }

    pub(super) fn get_transient(&self, symbol: SymbolID) -> Option<&TransientSymbol<'cx>> {
        self.transient_symbols.get(symbol)
    }

    pub(super) fn get_check_flags(&self, symbol: SymbolID) -> crate::ty::CheckFlags {
        if let Some(t) = self.get_transient(symbol) {
            t.links.get_check_flags().unwrap_or_default()
        } else {
            bitflags::Flags::empty()
        }
    }

    pub(crate) fn get_symbol_decl(&self, symbol: SymbolID) -> Option<NodeID> {
        if symbol.module() == ModuleID::TRANSIENT {
            let symbol = self.get_transient(symbol).unwrap();
            symbol.declarations.and_then(|d| d.first().copied())
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

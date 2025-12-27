use bolt_ts_ast::{self as ast, NodeID};
use bolt_ts_span::ModuleID;

use crate::ty;
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID, SymbolName, Symbols};

use super::TyChecker;
use super::symbol_info::SymbolInfo;

pub(super) fn create_transient_symbol(symbols: &mut Symbols, symbol: Symbol) -> SymbolID {
    debug_assert!(symbol.flags.intersects(SymbolFlags::TRANSIENT));
    debug_assert_eq!(symbols.module(), ModuleID::TRANSIENT);
    let id = symbols.insert(symbol);
    debug_assert_eq!(id.module(), ModuleID::TRANSIENT);
    id
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn create_transient_symbol(
        &mut self,
        name: SymbolName,
        flags: SymbolFlags,
        links: crate::check::SymbolLinks<'cx>,
        decls: Option<thin_vec::ThinVec<ast::NodeID>>,
        value_declaration: Option<ast::NodeID>,
    ) -> SymbolID {
        debug_assert!(flags.contains(SymbolFlags::TRANSIENT));
        let symbol = Symbol {
            name,
            flags,
            decls,
            value_decl: value_declaration,
            members: None,
            exports: None,
            parent: None,
            merged_id: None,
            export_symbol: None,
            const_enum_only_module: None,
            is_replaceable_by_method: None,
        };
        let symbols = self.get_mut_transient_symbols();
        let id = create_transient_symbol(symbols, symbol);
        debug_assert_eq!(id.index_as_usize(), self.transient_symbol_links.len());
        self.transient_symbol_links.push(links);
        id
    }

    pub(super) fn create_transient_symbol_with_ty(
        &mut self,
        source: SymbolID,
        ty: &'cx crate::ty::Ty<'cx>,
    ) -> SymbolID {
        let s = self.symbol(source);
        let check_flags = self.get_check_flags(source) & crate::ty::CheckFlags::READONLY;
        let links = crate::check::SymbolLinks::default()
            .with_check_flags(check_flags)
            .with_ty(ty)
            .with_target(source);
        self.create_transient_symbol(
            s.name,
            s.flags | SymbolFlags::TRANSIENT,
            links,
            None,
            s.value_decl,
        )
    }

    pub(super) fn get_transient(&self, symbol: SymbolID) -> Option<&Symbol> {
        if symbol.module() != ModuleID::TRANSIENT {
            return None;
        }
        Some(self.get_transient_symbols().get(symbol))
    }

    pub(super) fn get_check_flags(&self, symbol: SymbolID) -> crate::ty::CheckFlags {
        if self.get_transient(symbol).is_some() {
            self.transient_symbol_links[symbol.index_as_usize()]
                .get_check_flags()
                .unwrap_or_default()
        } else {
            bitflags::Flags::empty()
        }
    }

    pub(crate) fn get_symbol_decl(&self, symbol: SymbolID) -> Option<NodeID> {
        if symbol.module() == ModuleID::TRANSIENT {
            let symbol = self.get_transient(symbol).unwrap();
            symbol
                .decls
                .as_ref()
                .and_then(|decls| decls.first().copied())
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
        let symbol_flags = self.symbol(symbol).flags;
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

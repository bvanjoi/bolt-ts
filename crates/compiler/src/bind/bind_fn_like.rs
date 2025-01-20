use crate::{ast, ir};

use super::symbol::{FnSymbol, SymbolKind};
use super::{BinderState, Symbol, SymbolFlags};
use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

impl<'cx> BinderState<'cx> {
    pub(super) fn create_fn_decl_like_symbol(
        &mut self,
        container: ast::NodeID,
        decl: &impl ir::FnDeclLike<'cx>,
        ele_name: super::SymbolName,
        ele_fn_kind: super::SymbolFnKind,
        is_export: bool,
    ) -> super::SymbolID {
        let container = self.final_res[&container];

        fn members<'a>(
            container: &'a mut Symbol<'_>,
            is_export: bool,
        ) -> &'a mut FxHashMap<super::SymbolName, super::SymbolID> {
            if let Some(i) = &mut container.kind.1 {
                return &mut i.members;
            }
            let s = &mut container.kind.0;
            if let SymbolKind::Class(c) = s {
                if is_export {
                    &mut c.exports
                } else {
                    &mut c.members
                }
            } else if let SymbolKind::BlockContainer(c) = s {
                &mut c.locals
            } else if let SymbolKind::Object(obj) = s {
                &mut obj.members
            } else if let SymbolKind::TyLit(obj) = s {
                &mut obj.members
            } else {
                unreachable!("{:#?}", s)
            }
        }

        if let Some(s) = members(self.symbols.get_mut(container), is_export)
            .get(&ele_name)
            .copied()
        {
            let symbol = self.symbols.get_mut(s);
            match &mut symbol.kind.0 {
                SymbolKind::Fn(FnSymbol { decls, kind }) => {
                    assert!(*kind == ele_fn_kind);
                    assert!(!decls.is_empty());
                    decls.push(decl.id());
                }
                _ => unreachable!("symbol: {:#?}", symbol),
            }
            self.create_final_res(decl.id(), s);
            s
        } else {
            let symbol = self.declare_symbol(
                ele_name,
                SymbolFlags::FUNCTION,
                SymbolKind::Fn(FnSymbol {
                    kind: ele_fn_kind,
                    decls: thin_vec![decl.id()],
                }),
                SymbolFlags::empty(),
            );
            self.create_final_res(decl.id(), symbol);
            let prev = members(self.symbols.get_mut(container), is_export).insert(ele_name, symbol);
            assert!(prev.is_none());
            symbol
        }
    }
}

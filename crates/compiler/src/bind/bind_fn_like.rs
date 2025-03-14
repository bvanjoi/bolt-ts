use crate::ir;

use super::symbol::{FnSymbol, SymbolKind};
use super::{BinderState, SymbolFlags};
use bolt_ts_ast as ast;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn members(
        &mut self,
        container: ast::NodeID,
        is_export: bool,
    ) -> &mut FxHashMap<super::SymbolName, super::SymbolID> {
        self.opt_members(container, is_export).unwrap()
    }

    pub(super) fn opt_members(
        &mut self,
        container: ast::NodeID,
        is_export: bool,
    ) -> Option<&mut FxHashMap<super::SymbolName, super::SymbolID>> {
        if self.p.node(container).has_locals() {
            // TODO: remove this condition
            if !self.p.node(container).is_block_stmt() {
                return Some(
                    self.locals
                        .entry(container)
                        .or_insert_with(|| fx_hashmap_with_capacity(64)),
                );
            }
        }
        let container = self.final_res.get(&container).copied()?;

        let container = self.symbols.get_mut(container);
        if let Some(i) = &mut container.kind.1 {
            return Some(if is_export {
                &mut i.exports.0
            } else {
                &mut i.members.0
            });
        }
        let s = &mut container.kind.0;
        if let SymbolKind::BlockContainer(c) = s {
            Some(if is_export {
                &mut c.exports
            } else {
                &mut c.locals
            })
        } else if let SymbolKind::Object(obj) = s {
            Some(&mut obj.members)
        } else if let SymbolKind::TyLit(obj) = s {
            Some(&mut obj.members)
        } else {
            None
        }
    }

    pub(super) fn create_fn_decl_like_symbol(
        &mut self,
        container: ast::NodeID,
        decl: &impl ir::FnDeclLike<'cx>,
        ele_name: super::SymbolName,
        ele_fn_kind: super::SymbolFnKind,
        is_export: bool,
    ) -> super::SymbolID {
        if let Some(s) = self.members(container, is_export).get(&ele_name).copied() {
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
            let prev = self.members(container, is_export).insert(ele_name, symbol);
            assert!(prev.is_none());
            symbol
        }
    }
}

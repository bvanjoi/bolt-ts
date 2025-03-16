use crate::ir;

use super::symbol::SymbolKind;
use super::{BinderState, SymbolFlags};
use bolt_ts_ast as ast;
use rustc_hash::FxHashMap;

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
        } else {
            None
        }
    }

    pub(super) fn create_fn_decl_like_symbol(
        &mut self,
        container: ast::NodeID,
        decl: &impl ir::FnDeclLike<'cx>,
        ele_name: super::SymbolName,
        includes: SymbolFlags,
        excludes: SymbolFlags,
        is_export: bool,
    ) -> super::SymbolID {
        let loc = self.temp_local(container, is_export);
        let symbol = self.declare_symbol_and_add_to_symbol_table(
            container,
            ele_name,
            decl.id(),
            loc,
            includes,
            excludes,
        );
        let key = (self.scope_id, ele_name);
        self.res.insert(key, symbol);
        self.create_final_res(decl.id(), symbol);
        symbol
    }
}

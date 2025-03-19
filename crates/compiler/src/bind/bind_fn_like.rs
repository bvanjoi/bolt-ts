use crate::ir;

use super::{BinderState, SymbolFlags};
use bolt_ts_ast as ast;
use rustc_hash::FxHashMap;

impl<'cx> BinderState<'cx, '_, '_> {
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

use super::symbol::SymbolFlags;
use super::{BinderState, SymbolID, SymbolName};
use crate::ir;

use bolt_ts_ast as ast;

impl<'cx> BinderState<'cx, '_, '_> {
    fn create_class_symbol(&mut self, c: &impl ir::ClassLike<'cx>, is_expr: bool) -> SymbolID {
        let name = c
            .name()
            .map_or(SymbolName::ClassExpr, |name| SymbolName::Normal(name.name));
        let id = c.id();
        let symbol = if !is_expr {
            self.bind_block_scoped_decl(id, name, SymbolFlags::CLASS, SymbolFlags::CLASS_EXCLUDES)
        } else {
            self.bind_anonymous_decl(id, SymbolFlags::CLASS, name)
            // TODO: if node.name then xxxx
        };
        self.create_final_res(id, symbol);
        // TODO: prototype field
        symbol
    }

    pub(super) fn bind_prop_or_method_or_access(
        &mut self,
        decl_id: ast::NodeID,
        name: SymbolName,
        includes: SymbolFlags,
        excludes: SymbolFlags,
    ) -> SymbolID {
        // TODO: has dynamic name
        self.declare_symbol_and_add_to_symbol_table(name, decl_id, None, includes, excludes)
    }

    pub(super) fn bind_class_like_decl(
        &mut self,
        class: &'cx impl ir::ClassLike<'cx>,
        is_expr: bool,
    ) {
        self.create_class_symbol(class, is_expr);
    }
}

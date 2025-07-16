use super::symbol::SymbolFlags;
use super::{BinderState, SymbolID, SymbolName, prop_name};
use crate::r#trait;

use bolt_ts_ast as ast;

impl<'cx> BinderState<'cx, '_, '_> {
    fn create_class_symbol(&mut self, c: &impl r#trait::ClassLike<'cx>, is_expr: bool) -> SymbolID {
        let name = c
            .name()
            .map_or(SymbolName::ClassExpr, |name| SymbolName::Atom(name.name));
        let id = c.id();
        let symbol = if !is_expr {
            self.bind_block_scoped_decl(id, name, SymbolFlags::CLASS, SymbolFlags::CLASS_EXCLUDES)
        } else {
            self.bind_anonymous_decl(id, SymbolFlags::CLASS, name)
        };
        self.create_final_res(id, symbol);
        // TODO: prototype field
        symbol
    }

    pub(super) fn bind_prop_or_method_or_access(
        &mut self,
        decl_id: ast::NodeID,
        name: &'cx ast::PropName<'cx>,
        includes: SymbolFlags,
        excludes: SymbolFlags,
    ) -> SymbolID {
        if self.node_query().has_dynamic_name(decl_id) {
            self.bind_anonymous_decl(decl_id, includes, SymbolName::Computed)
        } else {
            let name = prop_name(name);
            self.declare_symbol_and_add_to_symbol_table(name, decl_id, includes, excludes)
        }
    }

    pub(super) fn bind_class_like_decl(
        &mut self,
        class: &'cx impl r#trait::ClassLike<'cx>,
        is_expr: bool,
    ) {
        self.create_class_symbol(class, is_expr);
    }
}

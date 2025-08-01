use super::TyChecker;
use super::symbol_info::SymbolInfo;
use crate::bind::{Symbol, SymbolID, SymbolName};

use bolt_ts_ast as ast;

impl TyChecker<'_> {
    pub fn get_symbol_of_node(&self, id: ast::NodeID) -> Option<SymbolID> {
        Symbol::can_have_symbol(self.p.node(id)).then(|| self.get_symbol_of_decl(id))
    }
    #[inline]
    pub(super) fn get_symbol_of_decl(&self, id: ast::NodeID) -> SymbolID {
        debug_assert!(
            self.p.node(id).is_decl(),
            "expected a decl node, but got {:#?}",
            self.p.node(id)
        );
        let id = self.final_res(id);
        // TODO: get_late_bound_symbol
        self.get_merged_symbol(id)
    }

    #[inline]
    pub(super) fn get_symbol_for_expr(&mut self, id: ast::NodeID) -> Option<SymbolID> {
        let node = self.p.node(id);
        if Symbol::can_have_symbol(node) {
            Some(self.final_res(id))
        } else if let Some(ident) = node.as_ident() {
            Some(self.resolve_symbol_by_ident(ident))
        } else if let Some(p) = node.as_prop_access_expr() {
            let lhs_ty = self.get_ty_of_expr(p.expr);
            // TODO: is_private
            self.get_prop_of_ty(lhs_ty, SymbolName::Atom(p.name.name))
        } else {
            None
        }
    }

    fn get_symbol_of_name_or_prop_access_expr(&self, id: ast::NodeID) -> Option<SymbolID> {
        // TODO:
        None
    }

    pub(super) fn get_symbol_at_loc(&self, id: ast::NodeID) -> Option<SymbolID> {
        use bolt_ts_ast::Node::*;
        if self
            .node_query(id.module())
            .is_decl_name_or_import_prop_name(id)
        {
            let p = self.parent(id).unwrap();
            let parent_symbol = self.get_symbol_of_decl(p);
            return if self.p.is_import_or_export_spec(p) {
                todo!()
            } else {
                Some(parent_symbol)
            };
        }
        match self.p.node(id) {
            Ident(_) if self.node_query(id.module()).is_this_in_type_query(id) => {
                self.get_symbol_of_name_or_prop_access_expr(id)
            }
            _ => None,
        }
    }
}

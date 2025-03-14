use super::TyChecker;
use crate::bind::{Symbol, SymbolID, SymbolName};
use bolt_ts_ast as ast;

impl TyChecker<'_> {
    #[inline]
    pub(super) fn get_symbol_of_decl(&self, id: ast::NodeID) -> SymbolID {
        fn is_decl(p: &super::Parser, node: ast::NodeID) -> bool {
            if p.node(node).is_decl() {
                true
            } else if p.node(node).is_ident() {
                let n = p.node(p.parent(node).unwrap());
                n.is_var_decl() || n.is_param_decl()
            } else {
                false
            }
        }
        debug_assert!(
            is_decl(self.p, id),
            "expected a decl node, but got {:#?}",
            self.p.node(id)
        );
        self.final_res(id)
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
            self.get_prop_of_ty(lhs_ty, SymbolName::Ele(p.name.name))
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
        if self.p.is_decl_name_or_import_prop_name(id) {
            let p = self.p.parent(id).unwrap();
            let parent_symbol = self.get_symbol_of_decl(p);
            return if self.p.is_import_or_export_spec(p) {
                todo!()
            } else {
                Some(parent_symbol)
            };
        }
        match self.p.node(id) {
            Ident(_) if self.p.is_this_in_type_query(id) => {
                self.get_symbol_of_name_or_prop_access_expr(id)
            }
            _ => None,
        }
    }
}

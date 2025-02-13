use super::TyChecker;
use crate::ast;
use crate::bind::{Symbol, SymbolID};

impl<'cx> TyChecker<'cx> {
    #[inline]
    pub(super) fn get_symbol_of_decl(&self, id: ast::NodeID) -> SymbolID {
        fn is_decl(p: &super::Parser, node: ast::NodeID) -> bool {
            if p.node(node).is_decl() {
                true
            } else if p.node(node).is_ident() {
                p.node(p.parent(node).unwrap()).is_var_decl()
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
    pub(super) fn get_symbol_from_expr(&self, id: ast::NodeID) -> Option<SymbolID> {
        let node = self.p.node(id);
        if Symbol::can_have_symbol(node) {
            Some(self.final_res(id))
        } else if let Some(ident) = node.as_ident() {
            Some(self.resolve_symbol_by_ident(ident))
        } else if node.as_prop_access_expr().is_some() {
            todo!()
        } else {
            None
        }
    }

    fn get_symbol_of_name_or_prop_access_expr(&self, id: ast::NodeID) -> Option<SymbolID> {
        // TODO:
        None
    }

    pub(super) fn get_symbol_at_loc(&self, id: ast::NodeID) -> Option<SymbolID> {
        use ast::Node::*;
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

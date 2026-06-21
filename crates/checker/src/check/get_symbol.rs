use super::CheckMode;
use super::TyChecker;

use bolt_ts_ast::{self as ast};
use bolt_ts_binder::{Symbol, SymbolID, SymbolName};

impl TyChecker<'_> {
    pub fn get_symbol_of_node(&self, id: ast::NodeID) -> Option<SymbolID> {
        let n = self.p.node(id);
        if Symbol::can_have_symbol(n) {
            Some(self.final_res(id))
        } else {
            None
        }
    }

    pub(crate) fn get_local_symbol_of_decl(&mut self, id: ast::NodeID) -> Option<SymbolID> {
        debug_assert!(self.p.node(id).is_declaration());
        let module = id.module();
        debug_assert!(module != bolt_ts_span::ModuleID::TRANSIENT);
        debug_assert!(module.as_usize() < self.binder.bind_results.len());
        let result = unsafe { self.binder.bind_results.get_unchecked(module.as_usize()) };
        result.local_symbols.get(&id.index_as_u32()).copied()
    }

    #[inline]
    pub(crate) fn get_symbol_of_declaration(&mut self, id: ast::NodeID) -> SymbolID {
        debug_assert!(self.p.node(id).is_declaration());
        debug_assert!(id.module() != bolt_ts_span::ModuleID::TRANSIENT);
        let id = self.final_res(id);
        debug_assert!(id.module() != bolt_ts_span::ModuleID::TRANSIENT);
        let id = self.get_late_bound_symbol(id);
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
            self.get_prop_of_ty::<false, false>(lhs_ty, SymbolName::Atom(p.name.name))
        } else {
            None
        }
    }

    fn get_symbol_of_name_or_prop_access_expr(&mut self, mut id: ast::NodeID) -> Option<SymbolID> {
        // TODO:

        while self
            .node_query(id.module())
            .is_right_side_of_qualified_name_or_prop_access(id)
        {
            id = self.parent(id).unwrap();
        }

        let node = self.p.node(id);
        match node {
            ast::Node::Ident(_) => Some(self.final_res(id)),
            ast::Node::PrivateIdent(_) => todo!(),
            ast::Node::PropAccessExpr(n) => {
                if let Some(s) = self.get_node_links(id).get_resolved_symbol() {
                    return Some(s);
                };

                self.check_property_access_expression(n, Some(CheckMode::empty()));
                if self.get_node_links(id).get_resolved_symbol().is_none() {
                    let expr = self.check_expression_cached(n.expr, None);
                    let literal_ty = self.get_string_literal_type_from_string(n.name.name);
                    let resolved_symbol = self.get_applicable_index_symbol(expr, literal_ty);
                    if let Some(resolved_symbol) = resolved_symbol {
                        self.get_mut_node_links(id)
                            .set_resolved_symbol(resolved_symbol);
                        return Some(resolved_symbol);
                    }

                    return resolved_symbol;
                }

                self.get_node_links(id).get_resolved_symbol()
            }
            ast::Node::QualifiedName(n) => {
                if let Some(s) = self.get_node_links(id).get_resolved_symbol() {
                    return Some(s);
                };
                self.check_qualified_name(n, Some(CheckMode::empty()));
                // TODO: is_doc
                self.get_node_links(id).get_resolved_symbol()
            }
            _ => None,
        }
    }

    pub fn get_symbol_at_location(&mut self, id: ast::NodeID) -> Option<SymbolID> {
        use bolt_ts_ast::Node::*;
        let nq = self.node_query(id.module());
        if nq.is_decl_name_or_import_prop_name(id) {
            let p = self.parent(id).unwrap();
            let parent_symbol = self.get_symbol_of_declaration(p);
            return if self.p.is_import_or_export_spec(p) {
                todo!()
            } else {
                Some(parent_symbol)
            };
        }
        // TODO: is_literal_computed_property_declaration_name
        let n = self.p.node(id);
        if let Ident(n) = n {
            // TODO: other
        }
        match n {
            Ident(_) | PrivateIdent(_) | PropAccessExpr(_) | QualifiedName(_)
                if !nq.is_this_in_type_query(id) =>
            {
                self.get_symbol_of_name_or_prop_access_expr(id)
            }
            _ => None,
        }
    }
}

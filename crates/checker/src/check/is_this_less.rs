use bolt_ts_ast::{self as ast};
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID};

use super::TyChecker;
use super::symbol_info::SymbolInfo;

impl<'cx> TyChecker<'cx> {
    fn is_this_less_ty_param(&self, ty_param: &ast::TyParam<'cx>) -> bool {
        let constraint = self.get_effective_constraint_of_ty_param(ty_param);
        constraint.is_none_or(|c| c.is_this_less())
    }

    fn is_this_less_fn_like_decl(&self, node: ast::NodeID) -> bool {
        let n = self.p.node(node);
        if n.is_class_ctor() || n.is_ctor_sig_decl() {
            true
        } else if let Some(ret_ty) = self.get_effective_ret_type_node(node) {
            ret_ty.is_this_less()
                && n.params()
                    .unwrap_or_default()
                    .iter()
                    .all(|param| ast::Node::ParamDecl(param).is_this_less_var_like_decl())
                && self
                    .get_effective_ty_param_decls(node)
                    .iter()
                    .all(|ty_param| self.is_this_less_ty_param(ty_param))
        } else {
            false
        }
    }

    pub(super) fn is_this_less(&self, symbol: SymbolID) -> bool {
        let Some(decls) = &self.symbol(symbol).decls else {
            return false;
        };
        if decls.len() != 1 {
            return false;
        }
        let decl = decls[0];
        let decl = self.p.node(decl);
        use ast::Node::*;
        match decl {
            PropSignature(_) | ObjectPropAssignment(_) | ClassPropElem(_) => {
                decl.is_this_less_var_like_decl()
            }
            MethodSignature(_)
            | ObjectMethodMember(_)
            | ClassMethodElem(_)
            | CtorSigDecl(_)
            | ClassCtor(_)
            | GetterDecl(_)
            | SetterDecl(_) => self.is_this_less_fn_like_decl(decls[0]),
            _ => false,
        }
    }

    pub(super) fn is_this_less_interface(&mut self, symbol: SymbolID) -> bool {
        let s = self.symbol(symbol);
        let Some(decls) = s.decls.clone() else {
            return false;
        };
        for decl in decls {
            if !self.p.node(decl).is_interface_decl() {
                continue;
            } else if self
                .p
                .node_flags(decl)
                .intersects(ast::NodeFlags::CONTAINS_THIS)
            {
                return false;
            }
            let Some(base_ty_nodes) = self.get_interface_base_ty_nodes(decl) else {
                continue;
            };
            for node in base_ty_nodes {
                let base_symbol =
                    self.resolve_entity_name::<true, false>(node.name, SymbolFlags::TYPE);
                if base_symbol == Symbol::ERR
                    || !self
                        .symbol(base_symbol)
                        .flags
                        .intersects(SymbolFlags::INTERFACE)
                    || Self::this_ty(self.get_declared_ty_of_class_or_interface(base_symbol))
                        .is_some()
                {
                    return false;
                }
            }
        }
        true
    }
}

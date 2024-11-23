use rustc_hash::FxHashMap;

use super::ty;
use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;
use crate::bind::SymbolKind;
use crate::bind::SymbolName;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_declared_ty_of_symbol(&mut self, id: SymbolID) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ty) = self.get_symbol_links(id).get_declared_ty() {
            return Some(ty);
        }
        use crate::bind::SymbolKind::*;
        match &self.symbols.get(id).kind {
            Class { .. } | Interface { .. } => {
                let ty = self.get_declared_ty_of_class_or_interface(id);
                let ty = self.crate_interface_ty(ty);
                self.get_mut_symbol_links(id).set_declared_ty(ty);
                Some(ty)
            }
            _ => None,
        }
    }

    fn get_interface_base_ty_nodes(&self, decl: ast::NodeID) -> Option<&'cx [&'cx ast::Ty<'cx>]> {
        use ast::Node::*;
        let InterfaceDecl(interface) = self.nodes.get(decl) else {
            unreachable!()
        };
        interface.extends.map(|extends| extends.tys)
    }

    fn resolve_base_tys_of_interface(&mut self, decl: ast::NodeID) -> &'cx [&'cx ty::Ty<'cx>] {
        use ast::Node::*;
        let InterfaceDecl(_) = self.nodes.get(decl) else {
            unreachable!()
        };
        let Some(ty_nodes) = self.get_interface_base_ty_nodes(decl) else {
            return &[];
        };
        let mut tys = thin_vec::ThinVec::with_capacity(ty_nodes.len());
        for node in ty_nodes {
            let base_ty = self.get_ty_from_type_node(node);
            tys.push(base_ty);
        }
        self.alloc(tys)
    }

    fn get_base_tys(&mut self, id: SymbolID) -> &'cx [&'cx ty::Ty<'cx>] {
        use crate::bind::SymbolKind::*;
        match self.symbols.get(id).kind {
            Class { .. } => self.alloc([self.undefined_ty()]),
            Interface { decl, .. } => self.resolve_base_tys_of_interface(decl),
            _ => unreachable!(),
        }
    }

    fn get_declared_ty_of_class_or_interface(&mut self, id: SymbolID) -> ty::InterfaceTy<'cx> {
        let _outer_ty_params = self.get_outer_ty_params_of_class_or_interface(id);
        let base_tys = self.get_base_tys(id);
        use crate::bind::SymbolKind::*;
        let declared_props = match &self.symbols.get(id).kind {
            Class { members, .. } | Interface { members, .. } => {
                let ids = members
                    .values()
                    .filter_map(|m| {
                        if matches!(self.symbols.get(*m).kind, Property { .. }) {
                            Some(*m)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                self.alloc(ids)
            }
            _ => unreachable!(),
        };
        let index_infos = match &self.symbols.get(id).kind {
            Class { members, .. } | Interface { members, .. } => members.get(&SymbolName::Index),
            _ => unreachable!(),
        }
        .map(|index| {
            let SymbolKind::Index { decl } = &self.symbols.get(*index).kind else {
                unreachable!()
            };
            let ast::Node::IndexSigDecl(decl) = self.nodes.get(*decl) else {
                unreachable!()
            };
            let val_ty = self.get_ty_from_type_node(decl.ty);
            decl.params.iter().map(|param| {
                let Some(ty) = param.ty else { unreachable!() };
                let key_ty = self.get_ty_from_type_node(ty);
                self.alloc(ty::IndexInfo { key_ty, val_ty })
            })
        })
        .map(|info| info.collect::<Vec<_>>())
        .unwrap_or_default();
        ty::InterfaceTy {
            symbol: id,
            declared_props,
            base_tys,
            index_infos: self.alloc(index_infos),
        }
    }

    fn get_outer_ty_params_of_class_or_interface(
        &self,
        id: SymbolID,
    ) -> Option<ast::TyParams<'cx>> {
        use crate::bind::SymbolKind::*;
        match self.symbols.get(id).kind {
            Class { decl, .. } => self.get_outer_ty_params(decl),
            Interface { decl, .. } => self.get_outer_ty_params(decl),
            _ => unreachable!(),
        }
    }

    fn get_outer_ty_params(&self, id: ast::NodeID) -> Option<ast::TyParams<'cx>> {
        let mut id = id;
        loop {
            if let Some(next) = self.node_parent_map.parent(id) {
                id = next;
            } else {
                return None;
            }

            // use ast::Node::*;
            // match self.nodes.get(id) {
            //     // ClassDecl(class) => return class.ty_params,
            //     // ClassExpr(class) => return class.ty_params,
            //     // _ => {}
            // }
        }
    }
}

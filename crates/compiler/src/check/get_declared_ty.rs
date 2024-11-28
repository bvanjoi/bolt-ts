use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;

use super::ty;
use super::TyChecker;
use crate::ast;
use crate::bind::ClassSymbol;
use crate::bind::{SymbolID, SymbolKind, SymbolName};
use crate::errors;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_declared_ty_of_symbol(
        &mut self,
        module: ModuleID,
        id: SymbolID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ty) = self.get_symbol_links(module, id).get_declared_ty() {
            return Some(ty);
        }
        use crate::bind::SymbolKind::*;
        match &self.binder.get(module).symbols.get(id).kind {
            Class { .. } | Interface { .. } => {
                let ty = self.get_declared_ty_of_class_or_interface(module, id);
                // TODO: remove this
                if let Some(ty) = self.get_symbol_links(module, id).get_declared_ty() {
                    return Some(ty);
                }
                self.get_mut_symbol_links(module, id).set_declared_ty(ty);
                Some(ty)
            }
            _ => None,
        }
    }

    fn get_interface_base_ty_nodes(&self, decl: ast::NodeID) -> Option<&'cx [&'cx ast::Ty<'cx>]> {
        use ast::Node::*;
        let InterfaceDecl(interface) = self.p.get(decl.module()).nodes().get(decl) else {
            unreachable!()
        };
        interface.extends.map(|extends| extends.tys)
    }

    fn resolve_base_tys_of_interface(&mut self, decl: ast::NodeID) -> &'cx [&'cx ty::Ty<'cx>] {
        assert!(matches!(
            self.p.get(decl.module()).nodes().get(decl),
            ast::Node::InterfaceDecl(_)
        ));
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

    fn get_class_base_expr(&self, decl: ast::NodeID) -> Option<&'cx ast::Expr<'cx>> {
        use ast::Node::*;
        let extends = match self.p.get(decl.module()).nodes().get(decl) {
            ClassDecl(c) => c.extends,
            ClassExpr(c) => c.extends,
            _ => unreachable!(),
        };
        extends.map(|extends| extends.expr)
    }

    fn resolve_base_tys_of_class(
        &mut self,
        symbol: SymbolID,
        decl: ast::NodeID,
    ) -> (Option<&'cx ty::Ty<'cx>>, &'cx [&'cx ty::Ty<'cx>]) {
        assert!(self.p.get(decl.module()).nodes().get(decl).is_class_like());
        let base_ctor = self.get_base_constructor_type_of_class(decl.module(), symbol);
        let Some(extends) = self.get_class_base_expr(decl) else {
            return (Some(base_ctor), &[]);
        };
        if base_ctor.kind.as_class().is_some() {
            let ty = self.get_type_from_ty_reference(&extends);
            (Some(base_ctor), self.alloc([ty]))
        } else {
            (Some(base_ctor), self.alloc([base_ctor]))
        }
    }

    fn get_base_tys(
        &mut self,
        module: ModuleID,
        id: SymbolID,
    ) -> (Option<&'cx ty::Ty<'cx>>, &'cx [&'cx ty::Ty<'cx>]) {
        use crate::bind::SymbolKind::*;
        match &self.binder.get(module).symbols.get(id).kind {
            Class(symbol) => {
                let decl = symbol.decl;
                self.resolve_base_tys_of_class(id, decl)
            }
            Interface { decl, .. } => (None, self.resolve_base_tys_of_interface(*decl)),
            _ => unreachable!(),
        }
    }

    fn get_effective_base_type_node(&self, id: ast::NodeID) -> Option<&'cx ast::Expr<'cx>> {
        let extends = match self.p.get(id.module()).nodes().get(id) {
            ast::Node::ClassDecl(c) => c.extends,
            ast::Node::ClassExpr(c) => c.extends,
            _ => None,
        };
        extends.map(|extends| extends.expr)
    }

    fn push_ty_resolution(&mut self, module: ModuleID, id: SymbolID) -> bool {
        if let Some(start) = self.find_resolution_cycle_start_index(module, id) {
            for index in start..self.resolution_res.len() {
                self.resolution_res[index] = false;
            }
            return false;
        } else {
            self.resolution_tys.push((module, id));
            self.resolution_res.push(true);
            return true;
        }
    }

    fn pop_ty_resolution(&mut self) -> bool {
        self.resolution_tys.pop().unwrap();
        self.resolution_res.pop().unwrap()
    }

    fn find_resolution_cycle_start_index(
        &self,
        module: ModuleID,
        symbol: SymbolID,
    ) -> Option<usize> {
        self.resolution_tys
            .iter()
            .rev()
            .position(|ty| ty.0 == module && ty.1 == symbol)
            .map(|rev_index| self.resolution_tys.len() - 1 - rev_index)
    }

    fn get_base_constructor_type_of_class(
        &mut self,
        module: ModuleID,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        let decl = match &self.binder.get(module).symbols.get(symbol).kind {
            crate::bind::SymbolKind::Class(symbol) => symbol.decl,
            _ => unreachable!(),
        };
        let Some(extends) = self.get_effective_base_type_node(decl) else {
            return self.undefined_ty();
        };
        if !self.push_ty_resolution(module, symbol) {
            return self.error_ty();
        }
        let base_ty = self.check_expr(extends);
        if !self.pop_ty_resolution() {
            let decl = self.p.get(decl.module()).nodes().get(decl);
            let name = if let ast::Node::ClassDecl(c) = decl {
                self.atoms.get(c.name.name).to_string()
            } else {
                unreachable!()
            };
            let error = errors::DeclIsReferencedDirectlyOrIndirectlyInItsOwnBaseExpression {
                span: decl.span(),
                decl: name,
            };
            self.push_error(decl.span().module, Box::new(error));
            return self.error_ty();
        }
        base_ty
    }

    pub(super) fn get_props_from_members(
        &self,
        module: ModuleID,
        members: &FxHashMap<SymbolName, SymbolID>,
    ) -> &'cx [SymbolID] {
        let props = members
            .values()
            .filter_map(|m| {
                self.binder
                    .get(module)
                    .symbols
                    .get(*m)
                    .name
                    .as_atom()
                    .map(|_| *m)
            })
            .collect::<Vec<_>>();
        self.alloc(props)
    }

    fn get_declared_ty_of_class_or_interface(
        &mut self,
        module: ModuleID,
        id: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        let _outer_ty_params = self.get_outer_ty_params_of_class_or_interface(module, id);
        let (base_ctor_ty, base_tys) = self.get_base_tys(module, id);
        use crate::bind::SymbolKind::*;
        let members = match &self.binder.get(module).symbols.get(id).kind {
            Class(ClassSymbol { members, .. }) | Interface { members, .. } => {
                self.alloc(members.clone())
            }
            _ => unreachable!(),
        };
        let declared_props = self.get_props_from_members(module, &members);
        let index_infos = members
            .get(&SymbolName::Index)
            .copied()
            .map(|index| {
                let SymbolKind::Index { decl } = &self.binder.get(module).symbols.get(index).kind
                else {
                    unreachable!()
                };
                let ast::Node::IndexSigDecl(decl) = self.p.get(decl.module()).nodes().get(*decl)
                else {
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
        self.crate_interface_ty(ty::InterfaceTy {
            module,
            symbol: id,
            members,
            declared_props,
            base_tys,
            index_infos: self.alloc(index_infos),
            base_ctor_ty,
        })
    }

    fn get_outer_ty_params_of_class_or_interface(
        &self,
        module: ModuleID,
        id: SymbolID,
    ) -> Option<ast::TyParams<'cx>> {
        use crate::bind::SymbolKind::*;
        match &self.binder.get(module).symbols.get(id).kind {
            Class(symbol) => self.get_outer_ty_params(symbol.decl),
            Interface { decl, .. } => self.get_outer_ty_params(*decl),
            _ => unreachable!(),
        }
    }

    fn get_outer_ty_params(&self, id: ast::NodeID) -> Option<ast::TyParams<'cx>> {
        let mut id = id;
        loop {
            if let Some(next) = self.p.get(id.module()).parent_map().parent(id) {
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

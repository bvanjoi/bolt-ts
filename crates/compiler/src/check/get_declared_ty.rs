use rustc_hash::FxHashMap;

use super::errors;
use super::ty;
use super::utils::append_if_unique;
use super::TyChecker;
use crate::ast;
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::ty::{Sig, SigFlags, Sigs};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_declared_ty_of_symbol(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        self.try_get_declared_ty_of_symbol(symbol)
            .unwrap_or_else(|| self.error_ty())
    }

    pub(super) fn try_get_declared_ty_of_symbol(
        &mut self,
        id: SymbolID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ty) = self.get_symbol_links(id).get_declared_ty() {
            return Some(ty);
        }
        let symbol = self.binder.symbol(id);
        if (SymbolFlags::CLASS | SymbolFlags::INTERFACE).intersects(symbol.flags) {
            Some(self.get_declared_ty_of_class_or_interface(id))
        } else if symbol.flags == SymbolFlags::TYPE_ALIAS {
            let ty = self.get_declared_ty_of_type_alias(id);
            self.get_mut_symbol_links(id).set_declared_ty(ty);
            if let Some(ty_params) =
                self.get_local_ty_params_of_class_or_interface_or_type_alias(id)
            {
                self.get_mut_symbol_links(id).set_ty_params(ty_params);
            }
            Some(ty)
        } else if symbol.flags == SymbolFlags::TYPE_PARAMETER {
            let ty = self.get_declared_ty_of_ty_param(id);
            Some(ty)
        } else {
            None
        }
    }

    pub(super) fn get_declared_ty_of_ty_param(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_declared_ty() {
            return ty;
        }
        let ty_param_id = self.binder.symbol(symbol).expect_ty_param().decl;
        let container = self.p.node(self.p.parent(ty_param_id).unwrap());
        let ty_params = container.ty_params();
        let offset = ty_params
            .unwrap()
            .iter()
            .position(|p| p.id == ty_param_id)
            .unwrap();
        let parm_ty = self.alloc(ty::ParamTy { symbol, offset });
        let ty = self.new_ty(ty::TyKind::Param(parm_ty));
        self.get_mut_symbol_links(symbol).set_declared_ty(ty);
        ty
    }

    fn get_declared_ty_of_type_alias(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if !self.push_ty_resolution(symbol) {
            return self.error_ty();
        }
        let alias = self.binder.symbol(symbol).expect_ty_alias();
        let decl = self.p.node(alias.decl).expect_type_decl();
        let ty = self.get_ty_from_type_node(decl.ty);
        ty
    }

    fn get_interface_base_ty_nodes(&self, decl: ast::NodeID) -> Option<&'cx [&'cx ast::Ty<'cx>]> {
        use ast::Node::*;
        let InterfaceDecl(interface) = self.p.node(decl) else {
            unreachable!()
        };
        interface.extends.map(|extends| extends.tys)
    }

    fn resolve_base_tys_of_interface(&mut self, decl: ast::NodeID) -> &'cx [&'cx ty::Ty<'cx>] {
        assert!(matches!(self.p.node(decl), ast::Node::InterfaceDecl(_)));
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
        let extends = match self.p.node(decl) {
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
        assert!(self.p.node(decl).is_class_like());
        let base_ctor = self.get_base_constructor_type_of_class(symbol);
        let Some(extends) = self.get_class_base_expr(decl) else {
            return (Some(base_ctor), &[]);
        };
        if base_ctor.kind.is_object_class() {
            let ty = self.get_ty_from_ty_reference(extends);
            (Some(base_ctor), self.alloc([ty]))
        } else {
            (Some(base_ctor), self.alloc([base_ctor]))
        }
    }

    fn get_base_tys(
        &mut self,
        id: SymbolID,
    ) -> (Option<&'cx ty::Ty<'cx>>, &'cx [&'cx ty::Ty<'cx>]) {
        let symbol = self.binder.symbol(id);
        if symbol.flags.intersects(SymbolFlags::CLASS) {
            let c = symbol.expect_class();
            self.resolve_base_tys_of_class(id, c.decl)
        } else if symbol.flags.intersects(SymbolFlags::INTERFACE) {
            let i = symbol.expect_interface();
            (None, self.resolve_base_tys_of_interface(i.decl))
        } else {
            unreachable!()
        }
    }

    pub(super) fn append_ty_params(
        &mut self,
        res: &mut Vec<&'cx ty::Ty<'cx>>,
        params: ast::TyParams<'cx>,
    ) {
        for param in params {
            let symbol = self.get_symbol_of_decl(param.id);
            let value = self.get_declared_ty_of_symbol(symbol);
            assert!(value.kind.is_param());
            append_if_unique(res, value);
        }
    }

    fn push_ty_resolution(&mut self, symbol: SymbolID) -> bool {
        if let Some(start) = self.find_resolution_cycle_start_index(symbol) {
            for index in start..self.resolution_res.len() {
                self.resolution_res[index] = false;
            }
            return false;
        } else {
            self.resolution_tys.push(symbol);
            self.resolution_res.push(true);
            return true;
        }
    }

    fn pop_ty_resolution(&mut self) -> bool {
        self.resolution_tys.pop().unwrap();
        self.resolution_res.pop().unwrap()
    }

    fn find_resolution_cycle_start_index(&self, symbol: SymbolID) -> Option<usize> {
        self.resolution_tys
            .iter()
            .rev()
            .position(|ty| symbol.eq(ty))
            .map(|rev_index| self.resolution_tys.len() - 1 - rev_index)
    }

    fn get_base_constructor_type_of_class(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        let decl = self.binder.symbol(symbol).expect_class().decl;
        let Some(extends) = self.get_effective_base_type_node(decl) else {
            return self.undefined_ty();
        };
        if !self.push_ty_resolution(symbol) {
            let decl = self.p.node(decl);
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
        let base_ty = self.check_expr(extends);
        if !self.pop_ty_resolution() {
            return self.error_ty();
        }
        base_ty
    }

    pub(super) fn get_props_from_members(
        &self,
        members: &FxHashMap<SymbolName, SymbolID>,
    ) -> &'cx [SymbolID] {
        let props = members
            .values()
            .filter_map(|m| self.binder.symbol(*m).name.as_atom().map(|_| *m))
            .collect::<Vec<_>>();
        self.alloc(props)
    }

    fn get_declared_ty_of_class_or_interface(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            return ty;
        }

        let outer_ty_params = self.get_outer_ty_params_of_class_or_interface(symbol);
        let (base_ctor_ty, base_tys) = self.get_base_tys(symbol);
        let s = self.binder.symbol(symbol);
        let is_class = s.flags.intersects(SymbolFlags::CLASS);
        let class_node_id = is_class.then(|| s.expect_class().decl);
        let members = if is_class {
            let c = s.expect_class();
            self.alloc(c.members.clone())
        } else if s.flags.intersects(SymbolFlags::INTERFACE) {
            let i = s.expect_interface();
            self.alloc(i.members.clone())
        } else {
            unreachable!()
        };
        let declared_props = self.get_props_from_members(&members);
        let declared_index_infos = members
            .get(&SymbolName::Index)
            .copied()
            .map(|symbol| {
                let decl = self.binder.symbol(symbol).expect_index().decl;
                let decl = self.p.node(decl).expect_index_sig_decl();
                let val_ty = self.get_ty_from_type_node(decl.ty);
                decl.params
                    .iter()
                    .map(|param| {
                        let Some(ty) = param.ty else { unreachable!() };
                        let key_ty = self.get_ty_from_type_node(ty);
                        self.alloc(ty::IndexInfo {
                            key_ty,
                            val_ty,
                            symbol,
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let declared_ctor_sigs = members
            .get(&SymbolName::Constructor)
            .copied()
            .map(|s| self.get_sigs_of_symbol(s))
            .unwrap_or_default();
        let declared_ctor_sigs = if is_class && declared_ctor_sigs.is_empty() {
            // TODO: base
            let mut flags = SigFlags::empty();
            let node_id = class_node_id.unwrap();
            if let Some(c) = self.p.node(node_id).as_class_decl() {
                if let Some(mods) = c.modifiers {
                    if mods.flags.contains(ast::ModifierKind::Abstract) {
                        flags.insert(SigFlags::HAS_ABSTRACT);
                    }
                }
            }
            let sig = self.alloc(Sig {
                flags,
                ty_params: None,
                params: &[],
                min_args_count: 0,
                ret: None,
                node_id,
            });
            let sigs: Sigs<'cx> = self.alloc([sig]);
            sigs
        } else {
            declared_ctor_sigs
        };

        let declared_call_sigs = members
            .get(&SymbolName::Call)
            .copied()
            .map(|s| self.get_sigs_of_symbol(s))
            .unwrap_or_default();

        // TODO: remove this
        if let Some(ty) = self.get_symbol_links(symbol).get_ty() {
            return ty;
        }

        let ty = self.crate_interface_ty(ty::InterfaceTy {
            symbol,
            members,
            declared_props,
            base_tys,
            declared_index_infos: self.alloc(declared_index_infos),
            declared_ctor_sigs,
            declared_call_sigs,
            base_ctor_ty,
        });
        self.get_mut_symbol_links(symbol).set_ty(ty);
        ty
    }

    fn get_outer_ty_params_of_class_or_interface(&mut self, id: SymbolID) -> Option<ty::Tys<'cx>> {
        let s = self.binder.symbol(id);
        let decl = if s.flags.intersects(SymbolFlags::CLASS) {
            s.expect_class().decl
        } else if s.flags.intersects(SymbolFlags::INTERFACE) {
            s.expect_interface().decl
        } else {
            unreachable!()
        };
        let ty_params = self.get_outer_ty_params(decl);
        if let Some(ty_params) = ty_params {
            Some(self.alloc(ty_params))
        } else {
            None
        }
    }

    pub(super) fn get_outer_ty_params(&mut self, id: ast::NodeID) -> Option<Vec<&'cx ty::Ty<'cx>>> {
        let mut id = id;
        loop {
            if let Some(next) = self.p.parent(id) {
                id = next;
            } else {
                return None;
            }
            let node = self.p.node(id);
            use ast::Node::*;
            match node {
                TypeDecl(_) => {
                    let mut outer_ty_params = self.get_outer_ty_params(id).unwrap_or_default();
                    self.append_ty_params(
                        &mut outer_ty_params,
                        self.get_effective_ty_param_decls(id),
                    );
                    if outer_ty_params.is_empty() {
                        return None;
                    } else {
                        return Some(outer_ty_params);
                    }
                }
                _ => (),
            };
        }
    }
}

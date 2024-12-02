use bolt_ts_span::ModuleID;
use thin_vec::thin_vec;

use super::ty::{self, Ty, TyKind};
use super::{F64Represent, TyChecker};
use crate::ast;
use crate::atoms::AtomId;
use crate::bind::{SymbolID, SymbolKind, SymbolName};
use crate::keyword;
use crate::ty::AccessFlags;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_type_of_symbol(&mut self, module: ModuleID, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
            return ty;
        }
        use crate::bind::SymbolKind::*;
        let ty = match &self.binder.get(module).symbols.get(id).kind {
            Err => return self.error_ty(),
            FunctionScopedVar => return self.undefined_ty(),
            BlockScopedVar => return self.undefined_ty(),
            Class { .. } => {
                let ty = self.get_type_of_class_decl(module, id);
                // TODO: delete
                if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
                    return ty;
                }
                self.get_mut_symbol_links(module, id).set_ty(ty);
                // ---
                ty
            }
            Function { .. } | FnExpr { .. } => self.get_type_of_func_decl(module, id),
            Property { .. } => self.get_type_of_prop(module, id),
            Object { .. } => {
                let ty = self.get_type_of_object(module, id);
                // TODO: delete
                if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
                    return ty;
                }
                self.get_mut_symbol_links(module, id).set_ty(ty);
                // ---
                ty
            }
            BlockContainer { .. } => return self.undefined_ty(),
            Interface { .. } => return self.undefined_ty(),
            Index { .. } => return self.undefined_ty(),
            TypeAlias { .. } => return self.undefined_ty(),
            TyParam { .. } => return self.undefined_ty(),
        };
        ty
    }

    fn get_type_of_object(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        self.undefined_ty()
    }

    fn get_base_type_variable_of_class(
        &mut self,
        module: ModuleID,
        symbol: SymbolID,
    ) -> &'cx Ty<'cx> {
        let class_ty = self.get_declared_ty_of_symbol(module, symbol);
        let Some(i) = class_ty.kind.as_object_interface() else {
            unreachable!()
        };

        i.base_ctor_ty.unwrap()
    }

    fn get_type_of_prop(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let decl = match self.binder.get(module).symbols.get(symbol).kind {
            crate::bind::SymbolKind::Property { decl, .. } => decl,
            _ => unreachable!(),
        };
        let ty = match self.p.get(decl.module()).nodes().get(decl) {
            ast::Node::ClassPropEle(prop) => prop
                .ty
                .map(|ty| self.get_ty_from_type_node(ty))
                .unwrap_or(self.undefined_ty()),
            ast::Node::PropSignature(prop) => prop
                .ty
                .map(|ty| self.get_ty_from_type_node(ty))
                .unwrap_or(self.undefined_ty()),
            _ => unreachable!(),
        };
        ty
    }

    fn get_type_of_class_decl(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let base = self.get_base_type_variable_of_class(module, symbol);
        self.create_class_ty(ty::ClassTy { module, symbol })
    }

    fn get_type_of_func_decl(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let params = self.get_sig_of_symbol(module, symbol);
        self.create_fn_ty(ty::FnTy {
            params,
            ret: self.undefined_ty(),
            module,
            symbol,
        })
    }

    fn get_sig_of_symbol(&mut self, module: ModuleID, id: SymbolID) -> &'cx [&'cx Ty<'cx>] {
        use crate::bind::SymbolKind::*;
        let decls = match &self.binder.get(module).symbols.get(id).kind {
            Err => todo!(),
            Function { decls, .. } => decls.clone(),
            FnExpr { decl } => thin_vec![*decl],
            FunctionScopedVar => todo!(),
            BlockScopedVar => todo!(),
            Object { .. } => todo!(),
            Property { .. } => todo!(),
            Class { .. } => todo!(),
            BlockContainer { .. } => todo!(),
            Interface { .. } => todo!(),
            Index { .. } => todo!(),
            TypeAlias { .. } => todo!(),
            TyParam { .. } => todo!(),
        };

        for decl in decls {
            let params = match self.p.get(decl.module()).nodes().get(decl) {
                ast::Node::FnDecl(f) => f.params,
                ast::Node::ArrowFnExpr(f) => f.params,
                ast::Node::FnExpr(f) => f.params,
                ast::Node::ClassMethodEle(m) => m.params,
                _ => unreachable!("{:#?}", self.p.get(decl.module()).nodes().get(decl)),
            };
            let params = params
                .iter()
                .map(|param| {
                    param
                        .ty
                        .map(|ty| self.get_ty_from_type_node(ty))
                        .unwrap_or(self.any_ty())
                })
                .collect::<Vec<_>>();
            return self.alloc(params);
        }
        todo!()
    }

    pub(super) fn get_ty_from_type_node(&mut self, ty: &'cx ast::Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO: cache
        use ast::TyKind::*;
        match ty.kind {
            Refer(refer) => {
                if refer.name.name == keyword::IDENT_BOOLEAN {
                    assert!(refer.args.is_none());
                    self.boolean_ty()
                } else if refer.name.name == keyword::IDENT_NUMBER {
                    assert!(refer.args.is_none());
                    self.number_ty()
                } else if refer.name.name == keyword::IDENT_STRING {
                    assert!(refer.args.is_none());
                    self.string_ty()
                } else if refer.name.name == keyword::IDENT_ANY {
                    assert!(refer.args.is_none());
                    self.any_ty()
                } else if refer.name.name == keyword::IDENT_VOID {
                    assert!(refer.args.is_none());
                    self.void_ty()
                } else {
                    self.get_ty_from_ty_reference(refer)
                }
            }
            Array(array) => self.get_ty_from_array_node(array),
            Fn(_) => self.undefined_ty(),
            Lit(lit) => {
                if !self
                    .binder
                    .get(lit.id.module())
                    .final_res
                    .contains_key(&lit.id)
                {
                    unreachable!()
                }

                let module = lit.id.module();
                let symbol = self.binder.get(module).final_res[&lit.id];
                let SymbolKind::Object(object) = &self.binder.get(module).symbols.get(symbol).kind
                else {
                    unreachable!()
                };
                let members = self.alloc(object.members.clone());
                let declared_props = self.get_props_from_members(module, members);
                self.create_object_lit_ty(ty::ObjectLitTy {
                    members,
                    declared_props,
                    module: lit.id.module(),
                    symbol,
                })
            }
            ExprWithArg(expr) => self.get_ty_from_ty_reference(expr),
            NumLit(num) => self.get_number_literal_type(num.val),
            StringLit(s) => self.get_string_literal_type(s.val),
            Tuple(_) => self.undefined_ty(),
            Rest(_) => self.undefined_ty(),
            IndexedAccess(node) => self.get_ty_from_indexed_access_node(node),
            Cond(node) => self.get_ty_from_cond_node(node),
        }
    }

    fn get_indexed_access_ty(
        &mut self,
        object_ty: &'cx Ty<'cx>,
        index_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        if object_ty.kind.is_generic_object_type() {
            let ty = self.alloc(ty::IndexedAccessTy {
                object_ty,
                index_ty,
                access_flags: AccessFlags::PERSISTENT,
            });
            self.new_ty(ty::TyKind::IndexedAccess(ty))
        } else {
            self.undefined_ty()
        }
    }

    fn get_ty_from_indexed_access_node(
        &mut self,
        node: &'cx ast::IndexedAccessTy<'cx>,
    ) -> &'cx Ty<'cx> {
        let object_ty = self.get_ty_from_type_node(node.ty);
        let index_ty = self.get_ty_from_type_node(node.index_ty);
        self.get_indexed_access_ty(object_ty, index_ty)
    }

    fn get_alias_symbol_for_ty_node(&self, node: ast::NodeID) -> Option<SymbolID> {
        let parent_map = self.p.get(node.module()).parent_map();
        let nodes = self.p.get(node.module()).nodes();
        let mut host = parent_map.parent(node);
        while let Some(node_id) = host {
            let node = nodes.get(node);
            if node.is_paren_type_node() {
                host = parent_map.parent(node_id);
            } else {
                break;
            }
        }
        host.and_then(|node_id| nodes.get(node_id).is_type_decl().then(|| node_id))
            .and_then(|node_id| {
                let binder = self.binder.get(node_id.module());
                let symbol = binder.final_res.get(&node_id).copied().unwrap();
                assert!(binder.symbols.get(symbol).kind.as_type_alias().is_some());
                Some(symbol)
            })
    }

    fn get_local_ty_params_of_class_or_interface_or_type_alias(
        &mut self,
        module: ModuleID,
        symbol: SymbolID,
    ) -> Option<&'cx [&'cx ty::ParamTy]>  {
        use SymbolKind::*;
        match &self.binder.get(module).symbols.get(symbol).kind {
            TypeAlias(alias) => {
                self.get_effective_ty_params_decls(&[alias.decl])
            }
            _ => None
        }
    }

    fn get_ty_from_cond_node(&mut self, node: &'cx ast::CondTy<'cx>) -> &'cx Ty<'cx> {
        let check_ty = self.get_ty_from_type_node(node.check_ty);
        let alias_symbol = self.get_alias_symbol_for_ty_node(node.id);
        let alias_ty_args = alias_symbol.and_then(|symbol| {
            self.get_local_ty_params_of_class_or_interface_or_type_alias(node.id.module(), symbol)
        });
        // let all_outer_ty_params = self.getout
        // dbg!(alias_ty_args);
        check_ty
    }

    fn get_ty_from_array_node(&mut self, node: &'cx ast::ArrayTy<'cx>) -> &'cx Ty<'cx> {
        let ele_ty = self.get_ty_from_type_node(node.ele);
        self.create_array_ty(ty::ArrayTy { ty: ele_ty })
    }

    pub(super) fn get_number_literal_type(&mut self, val: f64) -> &'cx Ty<'cx> {
        let key = F64Represent::new(val);
        if let Some(id) = self.num_lit_tys.get(&key) {
            self.tys[id]
        } else {
            let kind = TyKind::NumberLit(self.alloc(ty::NumberLitTy { val }));
            let ty = self.new_ty(kind);
            self.num_lit_tys.insert(key, ty.id);
            ty
        }
    }

    pub(super) fn get_string_literal_type(&mut self, val: AtomId) -> &'cx Ty<'cx> {
        if let Some(id) = self.string_lit_tys.get(&val) {
            self.tys[id]
        } else {
            let kind = TyKind::StringLit(self.alloc(ty::StringLitTy { val }));
            let ty = self.new_ty(kind);
            self.string_lit_tys.insert(val, ty.id);
            ty
        }
    }

    pub(super) fn get_global_type(&mut self, name: SymbolName) -> &'cx Ty<'cx> {
        let Some((m, s)) = self.global_symbols.get(name) else {
            unreachable!()
        };
        self.get_declared_ty_of_symbol(m, s)
    }
}

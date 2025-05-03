use rustc_hash::FxHashMap;

use super::TyChecker;
use super::cycle_check::Cycle;
use super::cycle_check::ResolutionKey;
use super::errors;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::utils::append_if_unique;
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::check::InstantiationTyMap;
use crate::check::TyCacheTrait;
use crate::check::links::TyLinks;
use crate::ty::ObjectFlags;
use crate::ty::TypeFlags;
use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_declared_ty_of_symbol(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        let ty = self
            .try_get_declared_ty_of_symbol(symbol)
            .unwrap_or(self.error_ty);
        ty
    }

    pub(super) fn try_get_declared_ty_of_symbol(
        &mut self,
        id: SymbolID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let flags = self.binder.symbol(id).flags;
        if flags.intersects(SymbolFlags::CLASS_OR_INTERFACE) {
            let ty = self.get_declared_ty_of_class_or_interface(id);
            Some(ty)
        } else if flags.intersects(SymbolFlags::TYPE_ALIAS) {
            let ty = self.get_declared_ty_of_type_alias(id);
            Some(ty)
        } else if flags.intersects(SymbolFlags::TYPE_PARAMETER) {
            let ty = self.get_declared_ty_of_ty_param(id);
            Some(ty)
        } else if flags.intersects(SymbolFlags::ALIAS) {
            let ty = self.get_declared_ty_of_alias(id);
            Some(ty)
        } else {
            None
        }
    }

    fn get_declared_ty_of_alias(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_declared_ty() {
            return ty;
        }
        let s = self.resolve_alias(symbol);
        let ty = self.get_declared_ty_of_symbol(s);
        self.get_mut_symbol_links(symbol).set_declared_ty(ty);
        ty
    }

    pub(super) fn get_declared_ty_of_ty_param(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_declared_ty() {
            return ty;
        }
        let ty_param_id = self.binder.symbol(symbol).opt_decl().unwrap();
        let container = self.p.node(self.p.parent(ty_param_id).unwrap());
        let ty_params = container.ty_params();
        let offset =
            ty_params.and_then(|ty_params| ty_params.iter().position(|p| p.id == ty_param_id));
        let ty = self.create_param_ty(symbol, offset, false);
        self.get_mut_symbol_links(symbol).set_declared_ty(ty);
        ty
    }

    fn get_declared_ty_of_type_alias(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_declared_ty() {
            return ty;
        }
        if !self.push_ty_resolution(ResolutionKey::DeclaredType(symbol)) {
            return self.error_ty;
        }
        let decl = self.binder.symbol(symbol).decls.as_ref().unwrap()[0];
        let decl = self.p.node(decl).expect_type_alias_decl();
        let mut ty = self.get_ty_from_type_node(decl.ty);
        if !self.pop_ty_resolution().has_cycle() {
            if let Some(ty_params) =
                self.get_local_ty_params_of_class_or_interface_or_type_alias(symbol)
            {
                self.get_mut_symbol_links(symbol).set_ty_params(ty_params);
            }
        } else {
            ty = self.error_ty;
        }
        self.get_mut_symbol_links(symbol).set_declared_ty(ty);
        ty
    }

    pub(super) fn append_ty_params(
        &mut self,
        res: &mut Vec<&'cx ty::Ty<'cx>>,
        ty_params: ast::TyParams<'cx>,
    ) {
        for ty_param in ty_params {
            let symbol = self.get_symbol_of_decl(ty_param.id);
            let value = self.get_declared_ty_of_ty_param(symbol);
            assert!(value.kind.is_param(), "value: {:#?}", value);
            append_if_unique(res, value);
        }
    }

    pub(super) fn check_entity_name(
        &mut self,
        name: &'cx ast::EntityName<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        use bolt_ts_ast::EntityNameKind;
        match name.kind {
            EntityNameKind::Ident(ident) => self.check_ident(ident),
            EntityNameKind::Qualified(name) => self.check_qualified_name(name),
        }
    }

    fn check_qualified_name(&mut self, node: &'cx ast::QualifiedName<'cx>) -> &'cx ty::Ty<'cx> {
        let left = self.check_entity_name(node.left);
        let apparent_ty = self.get_apparent_ty(left);
        self._get_prop_of_ty(node.id, apparent_ty, left, node.right)
    }

    pub(super) fn get_type_of_param(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        let decl = self.symbol(symbol).value_decl;
        let ty = self.get_type_of_symbol(symbol);
        self.add_optionality(
            ty,
            false,
            decl.is_some_and(|decl| {
                let n = self.p.node(decl);
                n.initializer().is_some() || n.is_optional_decl()
            }),
        )
    }

    fn is_mixin_constructor_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        let sigs = self.get_signatures_of_type(ty, ty::SigKind::Constructor);
        if sigs.len() == 1 {
            let sig = sigs[0];
            if sig.ty_params.is_none() && sig.params.len() == 1 && sig.has_rest_param() {
                let param_ty = self.get_type_of_param(sig.params[0]);
                return self.is_type_any(Some(param_ty))
                    || self
                        .get_element_ty_of_array_ty(param_ty)
                        .is_some_and(|t| t == self.any_ty);
            }
        }
        false
    }

    fn is_constructor_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if !self
            .get_signatures_of_type(ty, ty::SigKind::Constructor)
            .is_empty()
        {
            true
        } else if ty.flags.intersects(TypeFlags::TYPE_VARIABLE) {
            self.get_base_constraint_of_ty(ty)
                .is_some_and(|c| self.is_mixin_constructor_ty(c))
        } else {
            false
        }
    }

    pub(super) fn get_base_constructor_type_of_class(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(resolved_base_ctor_ty) = self.get_ty_links(ty.id).get_resolved_base_ctor_ty() {
            return resolved_base_ctor_ty;
        }
        let symbol = ty.symbol().unwrap();
        let decl = self.binder.symbol(symbol).decls.as_ref().unwrap()[0];
        let Some(extends) = self.get_effective_base_type_node(decl) else {
            return self.undefined_ty;
        };
        if !self.push_ty_resolution(ResolutionKey::ResolvedBaseConstructorType(ty.id)) {
            return self.error_ty;
        }
        let base_ctor_ty = self.check_expr(extends.expr_with_ty_args.expr);
        if let Cycle::Some(_) = self.pop_ty_resolution() {
            let decl = self.p.node(decl);
            let name = if let ast::Node::ClassDecl(c) = decl {
                self.atoms.get(c.name.unwrap().name).to_string()
            } else {
                unreachable!()
            };
            let error = errors::DeclIsReferencedDirectlyOrIndirectlyInItsOwnBaseExpression {
                span: decl.span(),
                decl: name,
            };
            self.push_error(Box::new(error));
            let error_ty = self.error_ty;
            self.get_mut_ty_links(ty.id)
                .set_resolved_base_ctor_ty(error_ty);
            return self.error_ty;
        }
        if !base_ctor_ty.flags.intersects(TypeFlags::ANY)
            && base_ctor_ty != self.null_widening_ty
            && !self.is_constructor_ty(base_ctor_ty)
        {
            let error = errors::TypeXIsNotAConstructorFunctionType {
                span: extends.expr_with_ty_args.expr.span(),
                ty: base_ctor_ty.to_string(self),
            };
            self.push_error(Box::new(error));
            let e = self.error_ty;
            self.get_mut_ty_links(ty.id).set_resolved_base_ctor_ty(e);
            e
        } else {
            self.get_mut_ty_links(ty.id)
                .set_resolved_base_ctor_ty(base_ctor_ty);
            base_ctor_ty
        }
    }

    pub(super) fn get_props_from_members(
        &mut self,
        members: &FxHashMap<SymbolName, SymbolID>,
    ) -> &'cx [SymbolID] {
        let props = members
            .values()
            .filter(|&&m| self.is_named_member(m))
            .copied()
            .collect::<Vec<_>>();
        self.alloc(props)
    }

    fn is_named_member(&mut self, member: SymbolID) -> bool {
        let name = self.symbol(member).name;
        (name.as_atom().is_some() || name.is_numeric()) && self.symbol_is_value(member, false)
    }

    pub(super) fn get_declared_ty_of_class_or_interface(
        &mut self,
        symbol: SymbolID,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_declared_ty() {
            return ty;
        }

        let resolving = self.error_ty;
        self.get_mut_symbol_links(symbol).set_declared_ty(resolving);

        let outer_ty_params = self.get_outer_ty_params_of_class_or_interface(symbol);
        let local_ty_params = self.get_local_ty_params_of_class_or_interface_or_type_alias(symbol);
        let kind = if self
            .binder
            .symbol(symbol)
            .flags
            .intersects(SymbolFlags::CLASS)
        {
            SymbolFlags::CLASS
        } else {
            SymbolFlags::INTERFACE
        };

        let mut is_this_less_interface = true;
        let ty = if outer_ty_params.is_some()
            || local_ty_params.is_some()
            || kind == SymbolFlags::CLASS
            || {
                is_this_less_interface = self.is_this_less_interface(symbol);
                !is_this_less_interface
            } {
            let ty_params = self.concatenate(outer_ty_params, local_ty_params);
            assert!(kind == SymbolFlags::CLASS || !is_this_less_interface || !ty_params.is_empty());
            let this_ty = self.create_param_ty(symbol, None, true);
            let target = self.create_interface_ty(
                symbol,
                Some(ty_params),
                outer_ty_params,
                local_ty_params,
                Some(this_ty),
            );
            let ty = self.alloc(ty::ReferenceTy {
                target,
                mapper: None,
                node: None,
            });
            let ty = self.create_object_ty(ty::ObjectTyKind::Reference(ty), ObjectFlags::REFERENCE);
            assert!(!self.ty_links.contains_key(&ty.id));
            self.ty_links
                .insert(ty.id, TyLinks::default().with_resolved_ty_args(ty_params));
            let id = InstantiationTyMap::create_id(target.id, ty_params);
            self.instantiation_ty_map.insert(id, ty);
            ty
        } else {
            self.create_interface_ty(symbol, None, None, None, None)
        };

        self.get_mut_symbol_links(symbol).override_declared_ty(ty);
        ty
    }

    fn get_outer_ty_params_of_class_or_interface(&mut self, id: SymbolID) -> Option<ty::Tys<'cx>> {
        let s = self.binder.symbol(id);
        let decl = if s
            .flags
            .intersects(SymbolFlags::CLASS.union(SymbolFlags::FUNCTION))
        {
            s.value_decl.unwrap()
        } else {
            s.decls
                .as_ref()
                .and_then(|decls| {
                    decls
                        .iter()
                        .find(|decl| {
                            let n = self.p.node(**decl);
                            if n.is_interface_decl() {
                                true
                            } else if let Some(v) = n.as_var_decl() {
                                v.init
                                    .is_some_and(|init| matches!(init.kind, ast::ExprKind::Fn(_)))
                            } else {
                                false
                            }
                        })
                        .copied()
                })
                .unwrap()
        };
        let ty_params = self.get_outer_ty_params(decl, false);
        if let Some(ty_params) = ty_params {
            Some(self.alloc(ty_params))
        } else {
            None
        }
    }

    pub(super) fn get_outer_ty_params(
        &mut self,
        mut id: ast::NodeID,
        include_this: bool,
    ) -> Option<Vec<&'cx ty::Ty<'cx>>> {
        loop {
            if let Some(next) = self.p.parent(id) {
                id = next;
            } else {
                return None;
            }
            let node = self.p.node(id);
            use bolt_ts_ast::Node::*;
            match node {
                ClassDecl(_) | ClassExpr(_) | InterfaceDecl(_) | CallSigDecl(_)
                | MethodSignature(_) | FnTy(_) | CtorSigDecl(_) | FnDecl(_)
                | ClassMethodElem(_) | ArrowFnExpr(_) | TypeAliasDecl(_) | MappedTy(_)
                | CondTy(_) => {
                    let outer_ty_params = self.get_outer_ty_params(id, include_this);
                    if (node.is_fn_expr()
                        || node.is_arrow_fn_expr()
                        || self.p.is_object_lit_method(id))
                        && self.is_context_sensitive(id)
                    {
                        let symbol = self.get_symbol_of_decl(id);
                        let ty = self.get_type_of_symbol(symbol);
                        let sigs = self.get_signatures_of_type(ty, ty::SigKind::Call);
                        if let Some(sigs) = sigs.first() {
                            if let Some(ty_params) = sigs.ty_params {
                                return if let Some(mut outer_ty_params) = outer_ty_params {
                                    outer_ty_params.extend(ty_params);
                                    Some(outer_ty_params)
                                } else {
                                    Some(ty_params.to_vec())
                                };
                            }
                        }
                    }

                    if let Some(mapped) = node.as_mapped_ty() {
                        let symbol = self.get_symbol_of_decl(mapped.ty_param.id);
                        let ty = self.get_declared_ty_of_ty_param(symbol);
                        return if let Some(mut outer_ty_params) = outer_ty_params {
                            outer_ty_params.push(ty);
                            Some(outer_ty_params)
                        } else {
                            Some(vec![ty])
                        };
                    } else if let Some(cond) = node.as_cond_ty() {
                        let mut outer_ty_params = outer_ty_params.unwrap_or_default();
                        if let Some(infer_ty_params) = self.get_infer_ty_params(cond) {
                            outer_ty_params.extend(infer_ty_params);
                        };
                        return Some(outer_ty_params);
                        // return if let Some(infer_ty_params) = self.get_infer_ty_params(cond) {
                        //     if infer_ty_params.is_empty() {
                        //         outer_ty_params
                        //     } else if let Some(mut outer_ty_params) = outer_ty_params {
                        //         outer_ty_params.extend(infer_ty_params);
                        //         Some(outer_ty_params)
                        //     } else {
                        //         Some(infer_ty_params.to_vec())
                        //     }
                        // } else {
                        //     outer_ty_params
                        // };
                    }

                    let mut outer_ty_params = outer_ty_params.unwrap_or_default();
                    self.append_ty_params(
                        &mut outer_ty_params,
                        self.get_effective_ty_param_decls(id),
                    );
                    if include_this
                        && (node.is_class_decl()
                            || node.is_class_expr()
                            || node.is_interface_decl())
                    {
                        let symbol = self.get_symbol_of_decl(id);
                        let declared_ty = self.get_declared_ty_of_symbol(symbol);
                        if let Some(this_ty) = Self::this_ty(declared_ty) {
                            outer_ty_params.push(this_ty);
                        }
                    }
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

    pub(super) fn get_declared_ty_of_enum_member(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_declared_ty() {
            return ty;
        }
        // let parent = self.symbol(symbol)
        // let decl = self.binder.symbol(symbol).expect_ns().decls[0];
        // let decl = self.p.node(decl).expect_enum_member_decl();
        // let ty = self.get_ty_from_type_node(decl.ty);
        // TODO:
        let ty = self.any_ty;
        if let Some(old) = self.get_symbol_links(symbol).get_declared_ty() {
            old
        } else {
            self.get_mut_symbol_links(symbol).set_declared_ty(ty);
            ty
        }
    }
}

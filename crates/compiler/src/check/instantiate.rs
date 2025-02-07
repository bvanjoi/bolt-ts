use crate::bind::SymbolID;
use crate::ty::{ObjectFlags, TypeFlags};
use crate::{ast, ty};

use super::create_ty::IntersectionFlags;
use super::{InstantiationTyMap, TyChecker};

impl<'cx> TyChecker<'cx> {
    pub fn instantiate_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(mapper) = mapper {
            self.instantiate_ty_with_alias(ty, mapper, None, None)
        } else {
            ty
        }
    }

    pub fn instantiate_list<T: PartialEq + Copy>(
        &mut self,
        list: &'cx [T],
        mapper: &'cx dyn ty::TyMap<'cx>,
        f: impl Fn(&mut Self, T, &'cx dyn ty::TyMap<'cx>) -> T,
    ) -> &'cx [T] {
        let len = list.len();
        for i in 0..len {
            let item = list[i];
            let mapped = f(self, item, mapper);
            if item != mapped {
                let mut result = Vec::with_capacity(list.len());
                result.extend(list[0..i].iter());
                result.push(mapped);
                for j in i + 1..len {
                    result.push(f(self, list[j], mapper));
                }
                assert_eq!(result.len(), list.len());
                let result = self.alloc(result);
                return result;
            }
        }
        list
    }

    pub(super) fn instantiate_tys(
        &mut self,
        tys: ty::Tys<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> ty::Tys<'cx> {
        self.instantiate_list(tys, mapper, |this, item, mapper| {
            this.instantiate_ty(item, Some(mapper))
        })
    }

    pub(super) fn could_contain_ty_var(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        let object_flags = ty.get_object_flags();
        if object_flags.intersects(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED) {
            return object_flags.intersects(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES);
        }
        if ty.kind.is_instantiable_non_primitive() {
            true
        } else if let Some(object) = ty.kind.as_object() {
            // TODO: !isNonGenericTopLevelType(type)
            if object.kind.is_reference() {
                true
            } else {
                object.kind.is_anonymous()
            }
        } else if let Some(unions) = ty.kind.as_union() {
            unions.tys.iter().any(|t| self.could_contain_ty_var(t))
        } else if let Some(i) = ty.kind.as_intersection() {
            i.tys.iter().any(|t| self.could_contain_ty_var(t))
        } else {
            false
        }
    }

    pub(super) fn instantiate_ty_with_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if !self.could_contain_ty_var(ty) {
            return ty;
        }
        self.instantiate(ty, mapper, alias_symbol, alias_ty_args)
    }

    fn instantiate(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        use ty::TyKind::*;
        match ty.kind {
            Param(_) => self.get_mapped_ty(mapper, ty),
            IndexedAccess(indexed) => {
                let object_ty = self.instantiate_ty_with_alias(
                    indexed.object_ty,
                    mapper,
                    alias_symbol,
                    alias_ty_args,
                );
                let index_ty = self.instantiate_ty_with_alias(
                    indexed.index_ty,
                    mapper,
                    alias_symbol,
                    alias_ty_args,
                );
                self.get_indexed_access_ty(object_ty, index_ty, Some(indexed.access_flags), None)
            }
            Cond(cond) => {
                let mapper = self.combine_ty_mappers(cond.mapper, mapper);
                self.get_cond_ty_instantiation(ty, mapper, alias_symbol, alias_ty_args)
            }
            Object(_) => self.get_object_ty_instantiation(ty, mapper),
            Union(u) => self.instantiate_union_or_intersection(
                ty,
                u.tys,
                true,
                mapper,
                alias_symbol,
                alias_ty_args,
            ),
            Intersection(i) => self.instantiate_union_or_intersection(
                ty,
                i.tys,
                false,
                mapper,
                alias_symbol,
                alias_ty_args,
            ),
            _ => {
                todo!("{:?}", ty.kind);
            }
        }
    }

    fn instantiate_union_or_intersection(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        tys: ty::Tys<'cx>,
        is_union: bool,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let new_tys = self.instantiate_tys(tys, mapper);
        if new_tys == tys {
            return ty;
        }
        if is_union {
            self.get_union_ty(new_tys, ty::UnionReduction::Lit)
        } else {
            self.get_intersection_ty(
                new_tys,
                IntersectionFlags::None,
                alias_symbol,
                alias_ty_args,
            )
        }
    }

    fn instantiate_anonymous_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let target = ty.kind.expect_object_anonymous();
        self.create_instantiating_anonymous_ty(target.symbol, ty, mapper, object_flags)
    }

    fn get_object_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(refer) = ty.kind.as_object_reference() {
            if refer.node.is_none() {
                let Some(resolved_ty_args) = self.ty_links[&ty.id].get_resolved_ty_args() else {
                    return ty;
                };
                let new_ty_args = self.instantiate_tys(resolved_ty_args, mapper);
                return if resolved_ty_args == new_ty_args {
                    ty
                } else {
                    self.create_normalized_ty_reference(refer.target, new_ty_args)
                };
            }
        }

        let decl = if let Some(refer) = ty.kind.as_object_reference() {
            refer.node.unwrap()
        } else if let Some(s) = ty.symbol() {
            if let Some(decl) = self.binder.symbol(s).opt_decl() {
                decl
            } else {
                // TODO: delete return
                return ty;
            }
        } else {
            // TODO: delete return
            return ty;
        };

        let object_flags = ty.get_object_flags();
        let target = if let Some(_) = ty.kind.as_object_reference() {
            // TODO: resolved type
            ty
        } else if object_flags.intersects(ObjectFlags::INSTANTIATED) {
            if let Some(r) = ty.kind.as_object_reference() {
                r.target
            } else if let Some(a) = ty.kind.as_object_anonymous() {
                a.target.unwrap()
            } else {
                unreachable!()
            }
        } else {
            ty
        };

        let ty_mapper = if let Some(refer) = ty.kind.as_object_reference() {
            refer.mapper
        } else if let Some(a) = ty.kind.as_object_anonymous() {
            a.mapper
        } else {
            None
        };

        // TODO: single_signature_type
        let ty_params = if let Some(ty_params) = self.get_node_links(decl).get_outer_ty_params() {
            ty_params
        } else {
            let outer_params = if let Some(outer_params) = self.get_outer_ty_params(decl, true) {
                self.alloc(outer_params)
            } else {
                self.empty_array()
            };
            self.get_mut_node_links(decl)
                .set_outer_ty_params(outer_params);
            outer_params
        };
        if !ty_params.is_empty() {
            // TODO: alias_symbol
            let target_ty_params_id = InstantiationTyMap::create_id(target.id, ty_params);
            if !self.instantiation_ty_map.contain(target_ty_params_id) {
                self.instantiation_ty_map.insert(target_ty_params_id, ty);
            }
            let combined_mapper = self.combine_ty_mappers(ty_mapper, mapper);
            let ty_args = ty_params
                .iter()
                .map(|t| self.get_mapped_ty(combined_mapper, t))
                .collect::<Vec<_>>();
            let id = InstantiationTyMap::create_id(target.id, &ty_args);
            if let Some(instantiated) = self.instantiation_ty_map.get(id) {
                return instantiated;
            }
            let mut object_flags = ty.get_object_flags()
                    & !(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED
                        | ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES)
                    | ObjectFlags::INSTANTIATED /* TODO: propagating for alias_ty_args */;
            if ty.flags.intersects(TypeFlags::OBJECT_FLAGS_TYPE)
                && !object_flags.intersects(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED)
            {
                let result_could_contain_ty_vars = ty_args
                    .iter()
                    .any(|ty_arg| self.could_contain_ty_var(ty_arg));
                if object_flags.intersects(
                    ObjectFlags::MAPPED | ObjectFlags::ANONYMOUS | ObjectFlags::REFERENCE,
                ) {
                    object_flags |= ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED
                        | if result_could_contain_ty_vars {
                            ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES
                        } else {
                            ObjectFlags::empty()
                        };
                } else {
                    object_flags |= if !result_could_contain_ty_vars {
                        ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED
                    } else {
                        ObjectFlags::empty()
                    }
                }
            }
            let new_mapper = self.create_ty_mapper(ty_params, self.alloc(ty_args));
            let ty = if target.kind.is_object_reference() {
                let ty = ty.kind.expect_object_reference();
                self.create_deferred_ty_reference(ty.target, ty.node.unwrap(), Some(new_mapper))
            } else {
                self.instantiate_anonymous_ty(target, new_mapper, object_flags)
            };
            self.instantiation_ty_map.insert(id, ty);
            ty
        } else {
            ty
        }
    }

    fn get_cond_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let cond_ty = ty.kind.expect_cond_ty();
        let Some(outer_ty_params) = cond_ty.root.outer_ty_params else {
            return ty;
        };
        let ty_args = outer_ty_params
            .iter()
            .map(|t| self.get_mapped_ty(mapper, t))
            .collect::<Vec<_>>();
        let key = InstantiationTyMap::create_id(ty.id, &ty_args);
        if let Some(instantiated) = self.instantiation_ty_map.get(key) {
            return instantiated;
        }
        let ty_args = self.alloc(ty_args);
        let new_mapper = self.create_ty_mapper(outer_ty_params, ty_args);
        let ty = self.get_cond_ty(cond_ty.root, Some(new_mapper), alias_symbol, alias_ty_args);
        self.instantiation_ty_map.insert(key, ty);
        ty
    }

    pub(super) fn get_permissive_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty
            .flags
            .intersects(TypeFlags::PRIMITIVE | TypeFlags::ANY_OR_UNKNOWN | TypeFlags::NEVER)
        {
            ty
        } else if let Some(ty) = self.get_ty_links(ty.id).get_permissive_instantiation() {
            ty
        } else {
            let instantiated = self.instantiate_ty(ty, Some(self.permissive_mapper));
            self.get_mut_ty_links(ty.id)
                .set_permissive_instantiation(instantiated);
            instantiated
        }
    }

    pub(super) fn get_restrictive_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty
            .flags
            .intersects(TypeFlags::PRIMITIVE | TypeFlags::ANY_OR_UNKNOWN | TypeFlags::NEVER)
        {
            ty
        } else if let Some(ty) = self.get_ty_links(ty.id).get_restrictive_instantiation() {
            ty
        } else {
            let restrictive_instantiation = self.instantiate_ty(ty, Some(self.restrictive_mapper));
            self.get_mut_ty_links(ty.id)
                .set_restrictive_instantiation(restrictive_instantiation);
            if let Some(t) = self
                .get_ty_links(restrictive_instantiation.id)
                .get_restrictive_instantiation()
            {
                assert!(t == restrictive_instantiation);
                return t;
            } else {
                self.get_mut_ty_links(restrictive_instantiation.id)
                    .set_restrictive_instantiation(restrictive_instantiation);
            }
            restrictive_instantiation
        }
    }

    pub(super) fn get_sig_instantiation(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        ty_args: Option<ty::Tys<'cx>>,
        is_js: bool,
        inferred_ty_params: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Sig<'cx> {
        let ty_args = self.fill_missing_ty_args(
            ty_args,
            sig.ty_params,
            self.get_min_ty_arg_count(sig.ty_params),
        );
        let sig = self.get_sig_instantiation_without_filling_ty_args(sig, ty_args);
        if let Some(inferred_ty_params) = inferred_ty_params {
            todo!()
        }
        sig
    }

    pub(super) fn create_sig_instantiation(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Sig<'cx> {
        let ty_params = self.get_ty_params_for_mapper(sig);
        let mapper = self.create_ty_mapper_with_optional_target(ty_params, ty_args);
        self.instantiate_sig(sig, mapper, true)
    }

    fn get_sig_instantiation_without_filling_ty_args(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Sig<'cx> {
        // TODO: cache
        self.create_sig_instantiation(sig, ty_args)
    }

    pub(super) fn get_type_alias_instantiation(
        &mut self,
        symbol: SymbolID,
        ty_args: ty::Tys<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.get_declared_ty_of_symbol(symbol);
        let Some(ty_params) = self.get_symbol_links(symbol).get_ty_params() else {
            unreachable!()
        };
        let min_params_count = self.get_min_ty_arg_count(Some(ty_params));
        // TODO: cache
        let ty_args = self.fill_missing_ty_args(Some(ty_args), Some(ty_params), min_params_count);
        let mapper = self.create_ty_mapper_with_optional_target(ty_params, ty_args);
        self.instantiate_ty_with_alias(ty, mapper, alias_symbol, alias_ty_args)
    }

    pub(super) fn get_min_ty_arg_count(&self, ty_params: Option<ty::Tys<'cx>>) -> usize {
        let Some(ty_params) = ty_params else {
            return 0;
        };
        let mut min = 0;
        for (i, param) in ty_params.iter().enumerate() {
            let param = param.kind.expect_param();
            if !self.has_ty_param_default(param) {
                min = i + 1;
            }
        }
        min
    }

    pub(super) fn fill_missing_ty_args(
        &mut self,
        ty_args: Option<ty::Tys<'cx>>,
        ty_params: Option<ty::Tys<'cx>>,
        min_ty_argument_count: usize,
    ) -> Option<ty::Tys<'cx>> {
        let Some(ty_params) = ty_params else {
            return Some(&[]);
        };
        if ty_params.is_empty() {
            return Some(&[]);
        }
        let args_len = ty_args.map_or(0, |args| args.len());
        let params_len = ty_params.len();
        if args_len >= min_ty_argument_count && args_len <= params_len {
            let mut result = Vec::with_capacity(params_len);
            if let Some(ty_args) = ty_args {
                for arg in ty_args {
                    result.push(*arg);
                }
            }

            let base_default_ty = self.any_ty;
            for param in ty_params.iter().skip(args_len) {
                if let Some(default_ty) = self.get_default_ty_from_ty_param(param) {
                    result.push(default_ty);
                } else {
                    result.push(base_default_ty);
                }
            }
            Some(self.alloc(result))
        } else {
            ty_args
        }
    }

    pub(super) fn get_default_ty_from_ty_param(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let param = ty.kind.expect_param();
        // TODO: cache `self.get_default_of_param(param_ty, id)`
        let default = self.ty_param_node(param).default?;
        Some(self.get_ty_from_type_node(default))
    }

    fn has_ty_param_default(&self, ty_param: &'cx ty::ParamTy) -> bool {
        let param = self.ty_param_node(ty_param);
        param.default.is_some()
    }

    fn ty_param_node(&self, ty_param: &'cx ty::ParamTy) -> &'cx ast::TyParam<'cx> {
        let symbol = self.binder.symbol(ty_param.symbol);
        let symbol = symbol.expect_ty_param();
        let node = self.p.node(symbol.decl);
        let Some(param) = node.as_ty_param() else {
            unreachable!()
        };
        param
    }
}

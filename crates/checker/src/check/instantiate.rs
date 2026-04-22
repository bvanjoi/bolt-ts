use std::borrow::Cow;

use bolt_ts_ast as ast;
use bolt_ts_ast::MappedTyModifiers;
use bolt_ts_ast::keyword;
use bolt_ts_binder::{SymbolFlags, SymbolID};
use bolt_ts_ty::TypeFacts;
use thin_vec::thin_vec;

use super::create_ty::IntersectionFlags;
use super::instantiation_ty_map::TyCacheTrait;
use super::instantiation_ty_map::{ConditionalTyInstantiationTyMap, TyAliasInstantiationMap};
use super::ty::{self, ObjectMappedTyLinks};
use super::ty::{ObjectFlags, TyMapper, TypeFlags};
use super::utils::{capitalize, uncapitalize};
use super::{InstantiationTyMap, StringMappingTyMap, TyChecker};
use super::{TyInstantiationMap, errors};

impl<'cx> TyChecker<'cx> {
    pub(super) fn instantiate_instantiable_tys(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(TypeFlags::INSTANTIABLE) {
            return self.instantiate_ty_worker(ty, mapper);
        }
        match ty.kind {
            ty::TyKind::Union(u) => {
                let tys = u
                    .tys
                    .iter()
                    .map(|t| self.instantiate_instantiable_tys(t, mapper))
                    .collect::<Vec<_>>();
                self.get_union_ty::<false>(&tys, ty::UnionReduction::None, None, None, None)
            }
            ty::TyKind::Intersection(i) => {
                let tys = i
                    .tys
                    .iter()
                    .map(|t| self.instantiate_instantiable_tys(t, mapper))
                    .collect::<Vec<_>>();
                self.get_intersection_ty(&tys, IntersectionFlags::None, None, None)
            }
            _ => ty,
        }
    }

    pub fn instantiate_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(mapper) = mapper {
            self.instantiate_ty_worker(ty, mapper)
        } else {
            ty
        }
    }

    #[inline]
    pub fn instantiate_ty_worker(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.instantiate_ty_with_alias(ty, mapper, None, None)
    }

    pub(super) fn instantiate_list<T: PartialEq + Copy>(
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
                for t in list.iter().take(len).skip(i + 1) {
                    result.push(f(self, *t, mapper));
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
            this.instantiate_ty_worker(item, mapper)
        })
    }

    fn is_non_generic_top_level_type(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        if let Some(alias_symbol) = ty.alias_symbol()
            && ty.alias_ty_arguments().is_none()
        {
            let s = self.symbol(alias_symbol);
            s.get_declaration_of_kind(|n| self.p.node(n).is_type_alias_decl())
                .is_some_and(|decl| {
                    let Some(p) = self.parent(decl) else {
                        return false;
                    };
                    self.node_query(decl.module())
                        .find_ancestor(p, |n| {
                            if n.is_program() {
                                Some(true)
                            } else if n.is_module_decl() {
                                None
                            } else {
                                Some(false)
                            }
                        })
                        .is_some()
                })
        } else {
            false
        }
    }

    pub(super) fn could_contain_ty_var(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        let object_flags = ty.get_object_flags();
        if object_flags.intersects(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED) {
            return object_flags.intersects(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES);
        }
        if let Some(res) = self.common_ty_links_arena[ty.links].get_contain_ty_variables() {
            return res;
        }
        let res = if ty.flags.intersects(TypeFlags::INSTANTIABLE) {
            true
        } else if let Some(object) = ty.kind.as_object() {
            !self.is_non_generic_top_level_type(ty) && {
                if object.flags.contains(ObjectFlags::REFERENCE)
                    && (object.kind.as_reference().is_some_and(|r| r.node.is_some())
                        || self
                            .get_ty_arguments(ty)
                            .iter()
                            .any(|t| self.could_contain_ty_var(t)))
                {
                    true
                } else if let Some(a) = object.kind.as_anonymous()
                    && let Some(symbol) = a.symbol
                    && let s = self.symbol(symbol)
                    && s.flags.intersects(
                        SymbolFlags::FUNCTION
                            .union(SymbolFlags::METHOD)
                            .union(SymbolFlags::CLASS)
                            .union(SymbolFlags::TYPE_LITERAL)
                            .union(SymbolFlags::OBJECT_LITERAL),
                    )
                    && s.decls.is_some()
                {
                    true
                } else {
                    object_flags.intersects(
                        ObjectFlags::MAPPED
                            .union(ObjectFlags::REVERSE_MAPPED)
                            .union(ObjectFlags::OBJECT_REST_TYPE)
                            .union(ObjectFlags::INSTANTIATION_EXPRESSION_TYPE),
                    )
                }
            }
        } else if !ty.flags.contains(TypeFlags::ENUM_LITERAL)
            && let Some(tys) = ty.kind.tys_of_union_or_intersection()
            && !self.is_non_generic_top_level_type(ty)
        {
            tys.iter().any(|t| self.could_contain_ty_var(t))
        } else {
            false
        };
        self.common_ty_links_arena[ty.links].set_contain_ty_variables(res);
        res
    }

    fn instantiate_ty_with_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if !self.could_contain_ty_var(ty) {
            return ty;
        }

        if self.instantiation_count >= 5_000_000 || self.instantiation_depth == 1000 {
            let current_node = self.current_node.unwrap();
            let error = errors::TypeInstantiationIsExcessivelyDeepAndPossiblyInfinite {
                span: self.p.node(current_node).span(),
            };
            self.push_error(Box::new(error));
            return self.error_ty;
        }

        let id = TyInstantiationMap::create_ty_key(&(ty.id, alias_symbol, alias_ty_arguments));
        let cached_index = self.find_active_mapper(mapper);
        if let Some(index) = cached_index {
            if let Some(cached) = self.activity_ty_mapper_caches[index].get(&id) {
                return cached;
            }
        } else {
            self.push_active_mapper(mapper);
        }

        self.instantiation_count += 1;
        self.instantiation_depth += 1;
        let ret = self.instantiate(ty, mapper, alias_symbol, alias_ty_arguments);
        self.instantiation_depth -= 1;

        if let Some(index) = cached_index {
            let prev = self.activity_ty_mapper_caches[index].insert(id, ret);
            debug_assert!(prev.is_none())
        } else {
            self.pop_active_mapper();
        }

        ret
    }

    fn find_active_mapper(&mut self, mapper: &'cx dyn ty::TyMap<'cx>) -> Option<usize> {
        for (i, activity) in self.activity_ty_mapper.iter().rev().enumerate() {
            if std::ptr::eq(mapper, *activity) {
                let idx = self.activity_ty_mapper.len() - 1 - i;
                return Some(idx);
            }
        }
        None
    }

    fn push_active_mapper(&mut self, mapper: &'cx dyn ty::TyMap<'cx>) {
        self.activity_ty_mapper.push(mapper);
        self.activity_ty_mapper_caches.push(Default::default());
    }

    fn pop_active_mapper(&mut self) {
        self.activity_ty_mapper.pop().unwrap();
        self.activity_ty_mapper_caches.pop().unwrap();
    }

    fn instantiate(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        use ty::TyKind::*;
        match ty.kind {
            Param(_) => self.get_mapped_ty(mapper, ty),
            Object(_) => {
                self.get_object_ty_instantiation(ty, mapper, alias_symbol, alias_ty_arguments)
            }
            Union(_) => self.instantiate_union_or_intersection::<true>(
                ty,
                mapper,
                alias_symbol,
                alias_ty_arguments,
            ),
            Intersection(_) => self.instantiate_union_or_intersection::<false>(
                ty,
                mapper,
                alias_symbol,
                alias_ty_arguments,
            ),
            Index(index) => {
                let t = self.instantiate_ty_worker(index.ty, mapper);
                self.get_index_ty(t, ty::IndexFlags::empty())
            }
            TemplateLit(lit) => {
                let tys = self.instantiate_tys(lit.tys, mapper);
                self.get_template_lit_ty(lit.texts, tys)
            }
            StringMapping(s) => {
                let ty = self.instantiate_ty_worker(s.ty, mapper);
                self.get_string_mapping_ty(s.symbol, ty)
            }
            IndexedAccess(indexed) => {
                let new_alias_symbol = alias_symbol.or_else(|| ty.alias_symbol());
                let new_alias_ty_arguments = if alias_symbol.is_some() {
                    alias_ty_arguments
                } else {
                    ty.alias_ty_arguments()
                        .map(|tys| self.instantiate_tys(tys, mapper))
                };
                let object_ty =
                    self.instantiate_ty_with_alias(indexed.object_ty, mapper, None, None);
                let index_ty = self.instantiate_ty_with_alias(indexed.index_ty, mapper, None, None);
                self.get_indexed_access_ty(
                    object_ty,
                    index_ty,
                    Some(indexed.access_flags),
                    None,
                    new_alias_symbol,
                    new_alias_ty_arguments,
                )
            }
            Cond(cond) => {
                let mapper = self.combine_ty_mappers(cond.mapper, mapper);
                self.get_cond_ty_instantiation::<false>(
                    ty,
                    mapper,
                    alias_symbol,
                    alias_ty_arguments,
                )
            }
            Substitution(sub) => {
                let new_base_ty = self.instantiate_ty_worker(sub.base_ty, mapper);
                if ty.is_no_infer_ty() {
                    return self.get_no_infer_ty(new_base_ty);
                }
                let new_constraint = self.instantiate_ty_worker(sub.constraint, mapper);
                if new_base_ty.flags.intersects(TypeFlags::TYPE_VARIABLE)
                    && self.is_generic_ty(new_constraint)
                {
                    self.get_substitution_ty(new_base_ty, new_constraint)
                } else if new_constraint.flags.intersects(TypeFlags::ANY_OR_UNKNOWN) || {
                    let source = self.get_restrictive_instantiation(new_base_ty);
                    let target = self.get_restrictive_instantiation(new_constraint);
                    self.is_type_assignable_to(source, target)
                } {
                    new_base_ty
                } else if new_base_ty.flags.intersects(TypeFlags::TYPE_VARIABLE) {
                    self.get_substitution_ty(new_base_ty, new_constraint)
                } else {
                    self.get_intersection_ty(
                        &[new_base_ty, new_constraint],
                        IntersectionFlags::None,
                        None,
                        None,
                    )
                }
            }
            _ => {
                todo!("{:?}", ty.kind);
            }
        }
    }

    fn instantiate_union_or_intersection<const IS_UNION: bool>(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let origin = ty.kind.as_union().and_then(|u| u.origin);
        let tys = if let Some(origin) = origin
            && let Some(tys) = origin.kind.tys_of_union_or_intersection()
        {
            tys
        } else {
            ty.kind.tys_of_union_or_intersection().unwrap()
        };
        let new_tys = self.instantiate_tys(tys, mapper);
        if std::ptr::eq(new_tys, tys) && alias_symbol == ty.alias_symbol() {
            return ty;
        }
        let new_alias_symbol = alias_symbol.or_else(|| ty.alias_symbol());
        let new_alias_ty_arguments = if alias_symbol.is_some() {
            alias_ty_arguments
        } else {
            ty.alias_ty_arguments()
                .map(|tys| self.instantiate_tys(tys, mapper))
        };
        if !IS_UNION || origin.is_some_and(|origin| origin.flags.contains(TypeFlags::INTERSECTION))
        {
            self.get_intersection_ty(
                new_tys,
                IntersectionFlags::None,
                new_alias_symbol,
                new_alias_ty_arguments,
            )
        } else {
            self.get_union_ty::<false>(
                new_tys,
                ty::UnionReduction::Lit,
                new_alias_symbol,
                new_alias_ty_arguments,
                None,
            )
        }
    }

    fn instantiate_anonymous_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        object_flags: ObjectFlags,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let target = ty.kind.expect_object_anonymous();
        self.create_instantiating_anonymous_ty(
            target.symbol.unwrap(),
            ty,
            mapper,
            object_flags,
            target.node,
            alias_symbol,
            alias_ty_arguments,
        )
    }

    pub(crate) fn get_homomorphic_ty_var(
        &mut self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let constraint_ty = self.get_constraint_ty_from_mapped_ty(ty);
        constraint_ty.kind.as_index_ty().and_then(|index_ty| {
            let ty_var = self.get_actual_ty_variable(index_ty.ty);
            if ty_var.flags.contains(TypeFlags::TYPE_PARAMETER) {
                Some(ty_var)
            } else {
                None
            }
        })
    }

    fn instantiate_mapped_tuple_ty(
        &mut self,
        tuple_ty: &'cx ty::Ty<'cx>,
        mapped_ty: &'cx ty::Ty<'cx>,
        ty_var: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(t) = tuple_ty.as_tuple() else {
            unreachable!()
        };
        let element_flags = t.element_flags;
        let fixed_length = t.fixed_length;
        let fixed_mapper = if fixed_length > 0 {
            self.prepend_ty_mapping(ty_var, tuple_ty, Some(mapper))
        } else {
            mapper
        };
        let new_elements_tys = self
            .get_element_tys(tuple_ty)
            .iter()
            .enumerate()
            .map(|(i, ty)| {
                let flags = element_flags[i];
                if i < fixed_length {
                    let val = self.atoms.atom(&i.to_string());
                    let key = self.get_string_literal_type::<false>(val, None);
                    self.instantiate_mapped_ty_template(
                        mapped_ty,
                        key,
                        flags.contains(ty::ElementFlags::OPTIONAL),
                        fixed_mapper,
                    )
                } else if flags.contains(ty::ElementFlags::VARIADIC) {
                    let mapper = self.prepend_ty_mapping(ty_var, ty, Some(mapper));
                    self.instantiate_ty_worker(mapped_ty, mapper)
                } else {
                    let target = self.create_array_ty(ty, false);
                    let mapper = self.prepend_ty_mapping(ty_var, target, Some(mapper));
                    let t = self.instantiate_ty_worker(mapped_ty, mapper);
                    self.get_element_ty_of_array_ty(t)
                        .unwrap_or(self.unknown_ty)
                }
            })
            .collect::<Vec<_>>();
        let mapped_ty_decl = mapped_ty.kind.expect_object_mapped().decl;
        let modifiers = mapped_ty_decl.get_modifiers();
        let new_element_flags = if modifiers.contains(MappedTyModifiers::INCLUDE_OPTIONAL) {
            self.alloc(
                element_flags
                    .iter()
                    .map(|f| {
                        if f.contains(ty::ElementFlags::REQUIRED) {
                            ty::ElementFlags::OPTIONAL
                        } else {
                            *f
                        }
                    })
                    .collect::<Vec<_>>(),
            )
        } else if modifiers.contains(MappedTyModifiers::EXCLUDE_OPTIONAL) {
            self.alloc(
                element_flags
                    .iter()
                    .map(|f| {
                        if f.contains(ty::ElementFlags::OPTIONAL) {
                            ty::ElementFlags::REQUIRED
                        } else {
                            *f
                        }
                    })
                    .collect::<Vec<_>>(),
            )
        } else {
            element_flags
        };

        let new_readonly = ast::MappedTy::get_modified_readonly_state(t.readonly, modifiers);
        if new_elements_tys.contains(&self.error_ty) {
            self.error_ty
        } else {
            self.create_tuple_ty(
                self.alloc(new_elements_tys),
                Some(new_element_flags),
                new_readonly,
            )
        }
    }

    fn instantiate_mapped_array_ty(
        &mut self,
        array_ty: &'cx ty::Ty<'cx>,
        mapped_ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let m = mapped_ty.kind.expect_object_mapped();
        let element_ty =
            self.instantiate_mapped_ty_template(mapped_ty, self.number_ty, true, mapper);
        if self.is_error(element_ty) {
            self.error_ty
        } else {
            let is_readonly = array_ty.is_readonly_array(self);
            let modifiers = m.decl.get_modifiers();
            let readonly = ast::MappedTy::get_modified_readonly_state(is_readonly, modifiers);
            self.create_array_ty(element_ty, readonly)
        }
    }

    fn instantiate_mapped_ty_template(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        key: &'cx ty::Ty<'cx>,
        is_optional: bool,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let m = ty.kind.expect_object_mapped();
        let source = self.get_ty_param_from_mapped_ty(m);
        let template_mapper = self.append_ty_mapping(Some(mapper), source, key);
        let prop_ty = {
            let template_ty = self.get_template_ty_from_mapped_ty(
                m.target.map_or(m, |t| t.kind.expect_object_mapped()),
            );
            self.instantiate_ty_worker(template_ty, template_mapper)
        };
        let modifiers = m.decl.get_modifiers();
        let strict_null_checks = self.config.compiler_options().strict_null_checks();
        if strict_null_checks
            && modifiers.contains(MappedTyModifiers::INCLUDE_OPTIONAL)
            && !prop_ty.maybe_type_of_kind(TypeFlags::UNDEFINED.union(TypeFlags::VOID))
        {
            self.get_optional_ty::<true>(prop_ty)
        } else if strict_null_checks
            && modifiers.contains(MappedTyModifiers::EXCLUDE_OPTIONAL)
            && is_optional
        {
            self.get_ty_with_facts(prop_ty, TypeFacts::NE_UNDEFINED)
        } else {
            prop_ty
        }
    }

    fn instantiate_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        object_flags: ObjectFlags,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        fn instantiate_constituent<'cx>(
            c: &mut TyChecker<'cx>,
            ty: &'cx ty::Ty<'cx>,
            ty_var: &'cx ty::Ty<'cx>,
            mapped_ty_var: &'cx ty::Ty<'cx>,
            mapper: &'cx dyn ty::TyMap<'cx>,
            object_flags: ObjectFlags,
        ) -> &'cx ty::Ty<'cx> {
            debug_assert!(ty.kind.as_object_mapped().is_some());
            if mapped_ty_var.flags.intersects(
                TypeFlags::ANY_OR_UNKNOWN
                    .union(TypeFlags::INSTANTIABLE_NON_PRIMITIVE)
                    .union(TypeFlags::OBJECT)
                    .union(TypeFlags::INTERSECTION),
            ) && mapped_ty_var != c.wildcard_ty
                && !c.is_error(mapped_ty_var)
            {
                let mapped_ty = ty.kind.expect_object_mapped();
                if mapped_ty.decl.name_ty.is_none() {
                    if mapped_ty_var.kind.is_array(c)
                        || (mapped_ty_var.flags.intersects(TypeFlags::ANY)
                            && c.find_resolution_cycle_start_index(
                                super::ResolutionKey::ImmediateBaseConstraint(ty_var.id),
                            )
                            .is_none()
                            && c.get_constraint_of_ty_param(ty_var)
                                .is_some_and(|constraint| {
                                    c.every_type(constraint, |this, ty| this.is_array_or_tuple(ty))
                                }))
                    {
                        let mapper = c.prepend_ty_mapping(ty_var, mapped_ty_var, Some(mapper));
                        return c.instantiate_mapped_array_ty(mapped_ty_var, ty, mapper);
                    } else if mapped_ty_var.is_tuple() {
                        return c.instantiate_mapped_tuple_ty(mapped_ty_var, ty, ty_var, mapper);
                    } else if c.is_array_or_tuple_or_intersection(mapped_ty_var) {
                        let tys = mapped_ty_var.kind.expect_intersection().tys;
                        let tys = tys
                            .iter()
                            .map(|mapped_ty_var| {
                                instantiate_constituent(
                                    c,
                                    ty,
                                    ty_var,
                                    mapped_ty_var,
                                    mapper,
                                    object_flags,
                                )
                            })
                            .collect::<Vec<_>>();
                        return c.get_intersection_ty(&tys, IntersectionFlags::None, None, None);
                    }
                }
                let mapper = c.prepend_ty_mapping(ty_var, mapped_ty_var, Some(mapper));
                c.instantiate_anonymous_for_mapped_ty(ty, mapper, object_flags, None, None)
            } else {
                mapped_ty_var
            }
        }

        let m = ty.kind.expect_object_mapped();
        if let Some(ty_var) = self.get_homomorphic_ty_var(m) {
            let mapped_ty_var = self.instantiate_ty_worker(ty_var, mapper);
            if ty_var != mapped_ty_var {
                let mapped_ty_var = self.get_reduced_ty(mapped_ty_var);
                return self
                    .map_ty_with_alias(
                        mapped_ty_var,
                        |this, mapped_ty_var| {
                            Some(instantiate_constituent(
                                this,
                                ty,
                                ty_var,
                                mapped_ty_var,
                                mapper,
                                object_flags,
                            ))
                        },
                        alias_symbol,
                        alias_ty_arguments,
                    )
                    .unwrap();
            }
        }

        let constraint_ty = self.get_constraint_ty_from_mapped_ty(m);
        if self.instantiate_ty_worker(constraint_ty, mapper) == self.wildcard_ty {
            self.wildcard_ty
        } else {
            self.instantiate_anonymous_for_mapped_ty(
                ty,
                mapper,
                object_flags,
                alias_symbol,
                alias_ty_arguments,
            )
        }
    }

    fn instantiate_anonymous_for_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        object_flags: ObjectFlags,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let map = ty.kind.expect_object_mapped();
        let orig_ty_param = self.get_ty_param_from_mapped_ty(map);
        let fresh_ty_param = self.clone_param_ty(orig_ty_param);
        let mapper = {
            let m = self.alloc(TyMapper::make_unary(orig_ty_param, fresh_ty_param));
            self.combine_ty_mappers(Some(m), mapper)
        };
        let prev = self.ty_links.insert(
            fresh_ty_param.id,
            super::TyLinks::default().with_param_ty_mapper(mapper),
        );
        debug_assert!(prev.is_none());
        let new_alias_symbol = alias_symbol.or_else(|| map.alias_symbol);
        let new_alias_ty_arguments = if alias_symbol.is_some() {
            alias_ty_arguments
        } else {
            map.alias_ty_arguments
                .map(|alias_ty_args| self.instantiate_tys(alias_ty_args, mapper))
        };
        let object_flags = if let Some(args) = new_alias_ty_arguments {
            object_flags | ty::Ty::get_propagating_flags_of_tys(args, None)
        } else {
            object_flags
        };

        let links = ObjectMappedTyLinks::default().with_ty_param(fresh_ty_param);
        let links = self.object_mapped_ty_links_arena.alloc(links);
        let ty = self.alloc(ty::MappedTy {
            symbol: map.symbol,
            decl: map.decl,
            alias_symbol: new_alias_symbol,
            alias_ty_arguments: new_alias_ty_arguments,
            target: Some(ty),
            mapper: Some(mapper),
            links,
        });

        self.create_object_ty(
            ty::ObjectTyKind::Mapped(ty),
            object_flags | ObjectFlags::MAPPED,
        )
    }

    fn instantiate_reverse_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let r = ty.kind.expect_object_reverse_mapped();
        let inner_mapped_ty = self.instantiate_ty_worker(r.mapped_ty, mapper);
        if !inner_mapped_ty.kind.is_object_mapped() {
            return ty;
        };
        let inner_index_ty = self.instantiate_ty_worker(r.constraint_ty, mapper);
        if !inner_index_ty.kind.is_index_ty() {
            return ty;
        };
        let s = self.instantiate_ty_worker(r.source, mapper);
        self.infer_ty_for_homomorphic_map_ty(s, inner_mapped_ty, inner_index_ty)
            .unwrap_or(ty)
    }

    fn get_object_ty_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let object_flags = ty.get_object_flags();
        if !object_flags.intersects(
            ObjectFlags::REFERENCE
                .union(ObjectFlags::ANONYMOUS)
                .union(ObjectFlags::MAPPED),
        ) {
            return ty;
        }
        if let Some(refer) = ty.kind.as_object_reference() {
            if refer.node.is_none() {
                let resolved_ty_args = self.ty_links[&ty.id].expect_resolved_ty_args();
                let new_ty_args = self.instantiate_tys(resolved_ty_args, mapper);
                return if resolved_ty_args == new_ty_args {
                    ty
                } else {
                    self.create_normalized_ty_reference(refer.target, new_ty_args)
                };
            }
        } else if ty.kind.is_object_reverse_mapped() {
            return self.instantiate_reverse_mapped_ty(ty, mapper);
        }

        let decl = if let Some(refer) = ty.kind.as_object_reference() {
            refer.node.unwrap()
        } else if object_flags.contains(ObjectFlags::INSTANTIATION_EXPRESSION_TYPE) {
            let t = ty.kind.expect_object_anonymous();
            t.node.unwrap()
        } else if let Some(s) = ty.symbol() {
            if let Some(decl) = self.symbol(s).opt_decl() {
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
        let target = if ty.kind.as_object_reference().is_some() {
            self.get_node_links(decl).expect_resolved_ty()
        } else if object_flags.intersects(ObjectFlags::INSTANTIATED) {
            if let Some(r) = ty.kind.as_object_reference() {
                r.target
            } else if let Some(a) = ty.kind.as_object_anonymous() {
                a.target.unwrap()
            } else if let Some(mapped) = ty.kind.as_object_mapped() {
                mapped.target.unwrap()
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
        } else if let Some(t) = ty.kind.as_object_mapped() {
            t.mapper
        } else {
            None
        };

        // TODO: single_signature_type
        let ty_params = if let Some(ty_params) = self.get_node_links(decl).get_outer_ty_params() {
            ty_params
        } else {
            let ty_params = if let Some(outer_params) = self.get_outer_ty_params::<true>(decl) {
                debug_assert!(!outer_params.is_empty());
                Cow::Owned(outer_params)
            } else {
                Cow::Borrowed(self.empty_array())
            };
            const REFERENCE_FLAGS: ObjectFlags =
                ObjectFlags::REFERENCE.union(ObjectFlags::INSTANTIATION_EXPRESSION_TYPE);
            let all_decls = if ty.get_object_flags().intersects(REFERENCE_FLAGS) {
                Some(thin_vec::thin_vec![decl])
            } else {
                let s = self.symbol(ty.symbol().unwrap());
                debug_assert!(s.decls.is_some());
                s.decls.clone()
            };
            let ty_params = if (ty.get_object_flags().intersects(REFERENCE_FLAGS)
                || self
                    .symbol(target.symbol().unwrap())
                    .flags
                    .intersects(SymbolFlags::METHOD.union(SymbolFlags::TYPE_LITERAL)))
                && target.alias_ty_arguments().is_none()
            {
                Cow::Owned(
                    ty_params
                        .iter()
                        .filter(|tp| {
                            all_decls.as_ref().is_some_and(|all_decls| {
                                all_decls
                                    .iter()
                                    .any(|&d| self.is_ty_param_possibly_referenced(tp, d))
                            })
                        })
                        .copied()
                        .collect::<Vec<_>>(),
                )
            } else {
                ty_params
            };
            let ty_params = match ty_params {
                Cow::Borrowed(n) => n,
                Cow::Owned(n) => self.alloc(n),
            };
            self.get_mut_node_links(decl).set_outer_ty_params(ty_params);
            ty_params
        };

        if !ty_params.is_empty() {
            let new_alias_symbol = alias_symbol.or_else(|| ty.alias_symbol());
            let new_alias_ty_arguments = if alias_symbol.is_some() {
                alias_ty_arguments
            } else {
                ty.alias_ty_arguments()
                    .map(|tys| self.instantiate_tys(tys, mapper))
            };
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
                    & ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED
                        .union(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES).complement()
                    | ObjectFlags::INSTANTIATED /* TODO: propagating for alias_ty_args */;
            if ty.flags.intersects(TypeFlags::OBJECT_FLAGS_TYPE)
                && !object_flags.contains(ObjectFlags::COULD_CONTAIN_TYPE_VARIABLES_COMPUTED)
            {
                let result_could_contain_ty_vars = ty_args
                    .iter()
                    .any(|ty_arg| self.could_contain_ty_var(ty_arg));
                if object_flags.intersects(
                    ObjectFlags::MAPPED
                        .union(ObjectFlags::ANONYMOUS)
                        .union(ObjectFlags::REFERENCE),
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
                self.create_deferred_ty_reference(
                    ty.target,
                    ty.node.unwrap(),
                    Some(new_mapper),
                    new_alias_symbol,
                    new_alias_ty_arguments,
                )
            } else if target.kind.is_object_mapped() {
                self.instantiate_mapped_ty(
                    target,
                    new_mapper,
                    object_flags,
                    new_alias_symbol,
                    new_alias_ty_arguments,
                )
            } else {
                self.instantiate_anonymous_ty(
                    target,
                    new_mapper,
                    object_flags,
                    new_alias_symbol,
                    new_alias_ty_arguments,
                )
            };
            if !self.instantiation_ty_map.contain(id) {
                self.instantiation_ty_map.insert(id, ty);
            } else {
                // cycle
                self.instantiation_ty_map.r#override(id, ty);
            }
            ty
        } else {
            ty
        }
    }

    pub(super) fn get_cond_ty_instantiation<const FORCE_CONSTRAINT: bool>(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let cond_ty = ty.kind.expect_cond_ty();
        let Some(outer_ty_params) = cond_ty.root.outer_ty_params else {
            return ty;
        };
        let ty_args = outer_ty_params
            .iter()
            .map(|t| self.get_mapped_ty(mapper, t))
            .collect::<Vec<_>>();
        let key = ConditionalTyInstantiationTyMap::create_id(
            cond_ty.root.node.id,
            &ty_args,
            alias_symbol,
            alias_ty_arguments,
        );
        if let Some(instantiated) = self.instantiation_ty_map.get(key) {
            return instantiated;
        }
        let ty_args = self.alloc(ty_args);
        let new_mapper = self.create_ty_mapper(outer_ty_params, ty_args);
        let check_ty = cond_ty.root.check_ty;
        let distribution_ty = cond_ty.root.is_distributive.then(|| {
            let ty = self.get_mapped_ty(new_mapper, check_ty);
            self.get_reduced_ty(ty)
        });
        let ty = if let Some(distribution_ty) = distribution_ty.filter(|distribution_ty| {
            !check_ty.eq(distribution_ty)
                && distribution_ty
                    .flags
                    .intersects(TypeFlags::UNION.union(TypeFlags::NEVER))
        }) {
            self.map_ty_with_alias(
                distribution_ty,
                |this, t| {
                    let mapper = this.prepend_ty_mapping(check_ty, t, Some(new_mapper));
                    Some(this.get_cond_ty::<FORCE_CONSTRAINT>(
                        cond_ty.root,
                        Some(mapper),
                        alias_symbol,
                        alias_ty_arguments,
                    ))
                },
                alias_symbol,
                alias_ty_arguments,
            )
            .unwrap()
        } else {
            self.get_cond_ty::<FORCE_CONSTRAINT>(
                cond_ty.root,
                Some(new_mapper),
                alias_symbol,
                alias_ty_arguments,
            )
        };
        if !self.instantiation_ty_map.contain(key) {
            self.instantiation_ty_map.insert(key, ty);
        } else {
            // TODO: cycle
            self.instantiation_ty_map.r#override(key, ty);
        }
        ty
    }

    pub(super) fn get_permissive_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(
            TypeFlags::PRIMITIVE
                .union(TypeFlags::ANY_OR_UNKNOWN)
                .union(TypeFlags::NEVER),
        ) {
            ty
        } else if let Some(ty) = self.common_ty_links_arena[ty.links].get_permissive_instantiation()
        {
            ty
        } else {
            let instantiated = self.instantiate_ty(ty, Some(self.permissive_mapper));
            self.common_ty_links_arena[ty.links].set_permissive_instantiation(instantiated);
            instantiated
        }
    }

    pub(super) fn get_restrictive_instantiation(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(
            TypeFlags::PRIMITIVE
                .union(TypeFlags::ANY_OR_UNKNOWN)
                .union(TypeFlags::NEVER),
        ) {
            ty
        } else if let Some(t) = self.common_ty_links_arena[ty.links].get_restrictive_instantiation()
        {
            t
        } else {
            let restrictive_instantiation = self.instantiate_ty(ty, Some(self.restrictive_mapper));
            if ty == restrictive_instantiation {
                self.common_ty_links_arena[ty.links]
                    .set_restrictive_instantiation(restrictive_instantiation);
            } else if let Some(cached) =
                self.common_ty_links_arena[ty.links].get_restrictive_instantiation()
            {
                assert_eq!(cached, restrictive_instantiation);
            } else {
                self.common_ty_links_arena[ty.links]
                    .set_restrictive_instantiation(restrictive_instantiation);
            }

            if let Some(t) = self.common_ty_links_arena[restrictive_instantiation.links]
                .get_restrictive_instantiation()
            {
                assert!(t == restrictive_instantiation);
                return t;
            } else {
                self.common_ty_links_arena[restrictive_instantiation.links]
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
        let sig_ty_params = self.get_sig_links(sig.id).get_ty_params();
        let ty_args = self.fill_missing_ty_args(
            ty_args,
            sig_ty_params,
            self.get_min_ty_arg_count(sig_ty_params),
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

    pub(super) fn get_string_mapping_ty(
        &mut self,
        symbol: SymbolID,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty
            .flags
            .intersects(TypeFlags::UNION.union(TypeFlags::NEVER))
        {
            self.map_ty(
                ty,
                |this, t| Some(this.get_string_mapping_ty(symbol, t)),
                false,
            )
            .unwrap()
        } else if let Some(s) = ty.kind.as_string_lit() {
            let atom = self.apply_string_mapping(symbol, s.val);
            self.get_string_literal_type_from_string(atom)
        } else if let Some(t) = ty.kind.as_template_lit_ty() {
            let symbol_name = self.symbol(symbol).name.expect_atom();
            match symbol_name {
                keyword::INTRINSIC_TYPE_UPPERCASE => {
                    let texts = t
                        .texts
                        .iter()
                        .map(|t| {
                            let text = self.atoms.get(*t);
                            let text = text.to_uppercase();

                            self.atoms.atom(&text)
                        })
                        .collect::<Vec<_>>();
                    let tys = t
                        .tys
                        .iter()
                        .map(|t| self.get_string_mapping_ty(symbol, t))
                        .collect::<Vec<_>>();
                    self.get_template_lit_ty(&texts, &tys)
                }
                keyword::INTRINSIC_TYPE_LOWERCASE => {
                    let texts = t
                        .texts
                        .iter()
                        .map(|t| {
                            let text = self.atoms.get(*t);
                            let text = text.to_lowercase();

                            self.atoms.atom(&text)
                        })
                        .collect::<Vec<_>>();
                    let tys = t
                        .tys
                        .iter()
                        .map(|t| self.get_string_mapping_ty(symbol, t))
                        .collect::<Vec<_>>();
                    self.get_template_lit_ty(&texts, &tys)
                }
                keyword::INTRINSIC_TYPE_CAPITALIZE => {
                    assert!(!t.texts.is_empty());
                    if t.texts[0] == keyword::IDENT_EMPTY {
                        let t0 = self.get_string_mapping_ty(symbol, t.tys[0]);
                        let mut tys = Vec::with_capacity(t.tys.len());
                        tys.push(t0);
                        tys.extend_from_slice(&t.tys[1..]);
                        self.get_template_lit_ty(t.texts, &tys)
                    } else {
                        let t0 = capitalize(self.atoms.get(t.texts[0]));
                        let atom = self.atoms.atom(&t0);
                        let mut new_texts = Vec::with_capacity(t.texts.len());
                        new_texts.push(atom);
                        new_texts.extend_from_slice(&t.texts[1..]);
                        self.get_template_lit_ty(t.texts, t.tys)
                    }
                }
                keyword::INTRINSIC_TYPE_UNCAPITALIZE => {
                    assert!(!t.texts.is_empty());
                    if t.texts[0] == keyword::IDENT_EMPTY {
                        let t0 = self.get_string_mapping_ty(symbol, t.tys[0]);
                        let mut tys = Vec::with_capacity(t.tys.len());
                        tys.push(t0);
                        tys.extend_from_slice(&t.tys[1..]);
                        self.get_template_lit_ty(t.texts, &tys)
                    } else {
                        let t0 = uncapitalize(self.atoms.get(t.texts[0]));
                        let atom = self.atoms.atom(&t0);
                        let mut new_texts = Vec::with_capacity(t.texts.len());
                        new_texts.push(atom);
                        new_texts.extend_from_slice(&t.texts[1..]);
                        self.get_template_lit_ty(t.texts, t.tys)
                    }
                }
                _ => self.get_template_lit_ty(t.texts, t.tys),
            }
        } else if ty.flags.intersects(TypeFlags::STRING_MAPPING) && Some(symbol) == ty.symbol() {
            ty
        } else if ty.flags.intersects(
            TypeFlags::ANY
                .union(TypeFlags::STRING)
                .union(TypeFlags::STRING_MAPPING),
        ) || self.is_generic_index_ty(ty)
        {
            self.get_string_mapping_ty_for_generic_ty(symbol, ty)
        } else if ty.is_pattern_lit_placeholder_ty() {
            let ty = self.get_template_lit_ty(&[keyword::IDENT_EMPTY, keyword::IDENT_EMPTY], &[ty]);
            self.get_string_mapping_ty_for_generic_ty(symbol, ty)
        } else {
            ty
        }
    }

    fn get_string_mapping_ty_for_generic_ty(
        &mut self,
        symbol: SymbolID,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let cache_key = StringMappingTyMap::create_ty_key(&(symbol, ty));
        if let Some(cache) = self.string_mapping_tys.get(cache_key) {
            return cache;
        }
        let ty = self.create_string_mapping_ty(symbol, ty);
        self.string_mapping_tys.insert(cache_key, ty);
        ty
    }

    pub(super) fn apply_string_mapping(
        &mut self,
        symbol: SymbolID,
        atom: bolt_ts_atom::Atom,
    ) -> bolt_ts_atom::Atom {
        let str = self.atoms.get(atom);
        let ty = self.symbol(symbol).name.expect_atom();
        let str = match ty {
            keyword::INTRINSIC_TYPE_UPPERCASE => str.to_uppercase(),
            keyword::INTRINSIC_TYPE_LOWERCASE => str.to_lowercase(),
            keyword::INTRINSIC_TYPE_CAPITALIZE => capitalize(str),
            keyword::INTRINSIC_TYPE_UNCAPITALIZE => uncapitalize(str),
            _ => unreachable!(),
        };
        self.atoms.atom(&str)
    }

    pub(super) fn get_type_alias_instantiation(
        &mut self,
        symbol: SymbolID,
        ty_args: ty::Tys<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.get_declared_ty_of_symbol(symbol);
        if ty == self.intrinsic_marker_ty {
            let name = self.symbol(symbol).name.expect_atom();
            if ty_args.len() == 1 {
                if name == keyword::INTRINSIC_TYPE_NOINFER {
                    return self.get_no_infer_ty(ty_args[0]);
                } else if name == keyword::INTRINSIC_TYPE_UPPERCASE
                    || name == keyword::INTRINSIC_TYPE_LOWERCASE
                    || name == keyword::INTRINSIC_TYPE_CAPITALIZE
                    || name == keyword::INTRINSIC_TYPE_UNCAPITALIZE
                {
                    return self.get_string_mapping_ty(symbol, ty_args[0]);
                };
            }
        }
        let id =
            TyAliasInstantiationMap::create_ty_key(&(symbol, ty_args, alias_symbol, alias_ty_args));
        if let Some(cache) = self.ty_alias_instantiation_map.get(id) {
            return cache;
        }
        let ty_params = self.get_symbol_links(symbol).expect_ty_params();
        let min_params_count = self.get_min_ty_arg_count_of_ty_params(ty_params);
        let ty_args = self.fill_missing_ty_args(Some(ty_args), Some(ty_params), min_params_count);
        let mapper = self.create_ty_mapper_with_optional_target(ty_params, ty_args);
        let ty = self.instantiate_ty_with_alias(ty, mapper, alias_symbol, alias_ty_args);
        self.ty_alias_instantiation_map.insert(id, ty);
        ty
    }

    pub(super) fn get_min_ty_arg_count(&self, ty_params: Option<ty::Tys<'cx>>) -> usize {
        let Some(ty_params) = ty_params else {
            return 0;
        };
        self.get_min_ty_arg_count_of_ty_params(ty_params)
    }

    pub(super) fn get_min_ty_arg_count_of_ty_params(&self, ty_params: ty::Tys<'cx>) -> usize {
        let mut min = 0;
        for (i, ty_param) in ty_params.iter().enumerate() {
            let ty_param = ty_param.kind.expect_param();
            if !self.has_ty_param_default(ty_param) {
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
            return Some(self.empty_array());
        };
        if ty_params.is_empty() {
            return Some(self.empty_array());
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

            for _ in args_len..params_len {
                result.push(self.error_ty);
            }

            let len = result.len();
            assert!(len == params_len);
            let result = self.arena.alloc(result);

            let base_default_ty = self.any_ty;
            for i in args_len..params_len {
                let dest = std::ptr::addr_of_mut!(result[i]);
                let param = ty_params[i];
                let ty = if let Some(default_ty) = self.get_default_ty_from_ty_param(param) {
                    let targets = unsafe { std::slice::from_raw_parts(result.as_ptr(), len) };
                    let mapper = self.create_ty_mapper(ty_params, targets);
                    self.instantiate_ty_worker(default_ty, mapper)
                } else {
                    base_default_ty
                };
                unsafe { std::ptr::write(dest, ty) };
            }
            Some(result)
        } else {
            ty_args
        }
    }

    pub(super) fn get_default_ty_from_ty_param(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(ty.kind.is_param());
        let default_ty = self.get_resolved_ty_param_default(ty);
        if default_ty != self.no_constraint_ty() && default_ty != self.circular_constraint_ty() {
            Some(default_ty)
        } else {
            None
        }
    }

    fn get_resolved_ty_param_default(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let param = ty.kind.expect_param();
        if let Some(default_ty) = self.get_ty_links(ty.id).get_default() {
            return if default_ty == self.resolving_default_type() {
                let circular_constraint_ty = self.circular_constraint_ty();
                self.get_mut_ty_links(ty.id)
                    .override_default(circular_constraint_ty);
                circular_constraint_ty
            } else {
                default_ty
            };
        }
        if let Some(target) = param.target {
            let target_default = self.get_resolved_ty_param_default(target);
            let mapper = self.ty_links[&ty.id].get_param_ty_mapper().unwrap();
            let default_ty = self.instantiate_ty_worker(target_default, mapper);
            self.get_mut_ty_links(ty.id).set_default(default_ty);
            default_ty
        } else {
            let resolving_default_type = self.resolving_default_type();
            self.get_mut_ty_links(ty.id)
                .set_default(resolving_default_type);
            let default_decl = self.ty_param_nodes(param).and_then(|decls| {
                decls
                    .iter()
                    .find_map(|decl| self.p.node(*decl).as_ty_param().and_then(|n| n.default))
            });
            let default_ty = if let Some(default_decl) = default_decl {
                self.get_ty_from_type_node(default_decl)
            } else {
                self.no_constraint_ty()
            };
            if self.ty_links[&ty.id]
                .get_default()
                .is_some_and(|t| t == resolving_default_type)
            {
                self.get_mut_ty_links(ty.id).override_default(default_ty);
            };
            default_ty
        }
    }

    pub(super) fn ty_param_nodes(
        &self,
        ty_param: &'cx ty::ParamTy<'cx>,
    ) -> Option<&[bolt_ts_ast::NodeID]> {
        let symbol = self.binder.symbol(ty_param.symbol?);
        symbol.decls.as_ref().map(|decls| decls.as_slice())
    }
}

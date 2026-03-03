use std::hash::Hasher;
use std::ops::Not;

use bolt_ts_ast as ast;
use bolt_ts_ast::keyword;
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID, SymbolName};
use bolt_ts_utils::{FxIndexMap, fx_indexmap_with_capacity, no_hashset_with_capacity};

use super::SymbolLinks;
use super::UnionOfUnionTysKey;
use super::get_iteration_tys::AsyncIterationTysResolver;
use super::get_iteration_tys::IterationTysResolver;
use super::get_iteration_tys::SyncIterationTysResolver;
use super::instantiation_ty_map::SubstitutionKey;
use super::instantiation_ty_map::{TyCacheTrait, create_iteration_tys_key};
use super::links::TyLinks;
use super::relation::RelationKind;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::ty::{CheckFlags, ElementFlags, IndexFlags};
use super::ty::{ObjectFlags, TyID, TypeFlags, UnionReduction};
use super::utils::insert_ty;
use super::{InstantiationTyMap, IntersectionMap, UnionMap};
use super::{TyChecker, errors};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntersectionFlags {
    None,
    NoSuperTypeReduction,
    NoConstraintReduction,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_ty(&mut self, kind: ty::TyKind<'cx>, flags: TypeFlags) -> &'cx ty::Ty<'cx> {
        Self::make_ty(
            kind,
            flags,
            &mut self.tys,
            &mut self.common_ty_links_arena,
            self.arena,
        )
    }

    pub(super) fn make_ty(
        kind: ty::TyKind<'cx>,
        flags: TypeFlags,
        tys: &mut Vec<&'cx ty::Ty<'cx>>,
        common_ty_links_arena: &mut ty::CommonTyLinksArena<'cx>,
        arena: &'cx bolt_ts_arena::bumpalo::Bump,
    ) -> &'cx ty::Ty<'cx> {
        let id = TyID::new(tys.len() as u32);
        let links = common_ty_links_arena.alloc(ty::CommonTyLinks::default());
        let ty = arena.alloc(ty::Ty::new(id, kind, flags, links));
        tys.push(ty);
        ty
    }

    pub(super) fn create_object_ty(
        &mut self,
        ty: ty::ObjectTyKind<'cx>,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let kind = ty::TyKind::Object(self.alloc(ty::ObjectTy {
            kind: ty,
            flags: object_flags,
        }));
        self.new_ty(kind, TypeFlags::OBJECT)
    }

    pub(super) fn create_tuple_ty(
        &mut self,
        element_types: ty::Tys<'cx>,
        element_flags: Option<&'cx [ElementFlags]>,
        readonly: bool,
    ) -> &'cx ty::Ty<'cx> {
        let element_flags = element_flags.unwrap_or_else(|| {
            let element_flags = element_types
                .iter()
                .map(|_| ElementFlags::REQUIRED)
                .collect::<Vec<_>>();
            self.alloc(element_flags)
        });
        let tuple_target = self.get_tuple_target_ty(element_flags, readonly);
        if tuple_target == self.empty_generic_ty() {
            self.empty_object_ty()
        } else if element_types.is_empty() {
            tuple_target
        } else {
            self.create_normalized_ty_reference(tuple_target, element_types)
        }
    }

    pub(super) fn create_normalized_ty_reference(
        &mut self,
        target: &'cx ty::Ty<'cx>,
        resolved_ty_args: ty::Tys<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if target.kind.is_object_tuple() {
            self.create_normalized_tuple_ty(target, resolved_ty_args)
        } else {
            self.create_reference_ty(target, Some(resolved_ty_args), ObjectFlags::empty())
        }
    }

    /// Used for these case which cannot compute immediately,
    /// such as the type of `A` in `type A = A[]`;
    pub(super) fn create_deferred_ty_reference(
        &mut self,
        target: &'cx ty::Ty<'cx>,
        node: ast::NodeID,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
        mut alias_symbol: Option<SymbolID>,
        mut alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if alias_symbol.is_none() {
            debug_assert!(alias_ty_arguments.is_none());
            alias_symbol = self.get_alias_symbol_for_ty_node(node);
            let local_alias_ty_arguments = self.get_ty_args_for_alias_symbol(alias_symbol);
            alias_ty_arguments = if let Some(mapper) = mapper
                && let Some(local_alias_ty_arguments) = local_alias_ty_arguments
            {
                Some(self.instantiate_tys(local_alias_ty_arguments, mapper))
            } else {
                local_alias_ty_arguments
            }
        }
        let promise_or_awaitable_links = self
            .promise_or_awaitable_links_arena
            .alloc(Default::default());
        let ty = ty::ReferenceTy {
            target,
            node: Some(node),
            mapper,
            alias_symbol,
            alias_ty_arguments,
            promise_or_awaitable_links,
        };

        self.create_object_ty(
            ty::ObjectTyKind::Reference(self.alloc(ty)),
            ObjectFlags::REFERENCE,
        )
    }

    pub(super) fn create_normalized_tuple_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        element_types: ty::Tys<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let target = ty.kind.expect_object_tuple();
        if !target.combined_flags.intersects(ElementFlags::NON_REQUIRED) {
            return self.create_reference_ty(ty, Some(element_types), ObjectFlags::empty());
        } else if target.combined_flags.intersects(ElementFlags::VARIABLE)
            && element_types.iter().enumerate().any(|(i, t)| {
                target.element_flags[i].contains(ElementFlags::VARIADIC)
                    && t.flags.intersects(TypeFlags::NEVER.union(TypeFlags::UNION))
            })
        {
            // TODO:
        }

        let mut expanded_tys = Vec::with_capacity(element_types.len());
        let mut expanded_flags = Vec::with_capacity(element_types.len());
        let mut last_required_index = usize::MAX;
        let mut first_rest_index = usize::MAX;
        let mut last_optional_or_rest_index = usize::MAX;

        let mut add_ele = |this: &mut Self,
                           ty: &'cx ty::Ty<'cx>,
                           flags: ElementFlags,
                           expanded_tys: &mut Vec<&ty::Ty<'cx>>| {
            if flags.contains(ElementFlags::REQUIRED) {
                last_required_index = expanded_tys.len();
            }
            if flags.contains(ElementFlags::REST) && first_rest_index == usize::MAX {
                first_rest_index = expanded_tys.len()
            }
            if flags.intersects(ElementFlags::OPTIONAL.union(ElementFlags::REST)) {
                last_optional_or_rest_index = expanded_tys.len();
            }
            if flags.contains(ElementFlags::OPTIONAL) {
                let ty = this.add_optionality::<true>(ty, true);
                expanded_tys.push(ty);
            } else {
                expanded_tys.push(ty);
            }
            expanded_flags.push(flags);
        };

        for (i, ty) in element_types.iter().enumerate() {
            let flag = target.element_flags[i];
            if flag.intersects(ElementFlags::VARIADIC) {
                if ty.flags.intersects(TypeFlags::ANY) {
                    add_ele(self, ty, ElementFlags::REST, &mut expanded_tys);
                } else if ty.kind.is_instantiable_non_primitive() || self.is_generic_mapped_ty(ty) {
                    add_ele(self, ty, ElementFlags::VARIADIC, &mut expanded_tys);
                } else if let Some(tuple) = ty.as_tuple() {
                    let elements = self.get_element_tys(ty);
                    if elements.len() + expanded_tys.len() >= 20_000 {
                        if let Some(current_node) = self.current_node {
                            let error = errors::TypeProducesATupleTypeThatIsTooLargeToRepresent {
                                span: self.p.node(current_node).span(),
                            };
                            self.push_error(Box::new(error));
                        } else {
                            todo!()
                        }

                        return self.error_ty;
                    }
                    for (n, t) in elements.iter().enumerate() {
                        add_ele(self, t, tuple.element_flags[n], &mut expanded_tys);
                    }
                } else {
                    let t = if self.is_array_like_ty(ty) {
                        self.get_index_ty_of_ty(ty, self.number_ty)
                            .unwrap_or(self.error_ty)
                    } else {
                        self.error_ty
                    };
                    add_ele(self, t, ElementFlags::REST, &mut expanded_tys);
                }
            } else {
                add_ele(self, ty, flag, &mut expanded_tys);
            }
        }

        // for i in 0..last_required_index {
        //     if expanded_flags[i].intersects(ElementFlags::OPTIONAL) {
        //         expanded_flags[i] = ElementFlags::REQUIRED
        //     }
        // }

        // while first_rest_index < last_optional_or_rest_index {
        //    expanded_tys[first_rest_index] =
        // }

        let expanded_flags = self.alloc(expanded_flags);
        let tuple_target = self.get_tuple_target_ty(expanded_flags, target.readonly);

        if expanded_flags.is_empty() {
            tuple_target
        } else {
            self.create_reference_ty(
                tuple_target,
                Some(self.alloc(expanded_tys)),
                ObjectFlags::empty(),
            )
        }
    }

    pub(super) fn create_reference_ty(
        &mut self,
        target: &'cx ty::Ty<'cx>,
        resolved_ty_args: Option<ty::Tys<'cx>>,
        flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let promise_or_awaitable_links = self
            .promise_or_awaitable_links_arena
            .alloc(Default::default());
        let ty = ty::ReferenceTy {
            target,
            mapper: None,
            node: None,
            alias_symbol: None,
            alias_ty_arguments: None,
            promise_or_awaitable_links,
        };
        if !flags.is_empty() {
            // TODO: delete branch
            let object_flags = flags
                | ObjectFlags::REFERENCE
                | ty::Ty::get_propagating_flags_of_tys(resolved_ty_args.unwrap_or_default(), None);
            let ty =
                self.create_object_ty(ty::ObjectTyKind::Reference(self.alloc(ty)), object_flags);
            assert!(!self.ty_links.contains_key(&ty.id));
            if let Some(resolved_ty_args) = resolved_ty_args {
                self.ty_links.insert(
                    ty.id,
                    TyLinks::default().with_resolved_ty_args(resolved_ty_args),
                );
            }
            return ty;
        }

        let id = InstantiationTyMap::create_id(ty.target.id, resolved_ty_args.unwrap_or_default());
        if let Some(res) = self.instantiation_ty_map.get(id) {
            res
        } else {
            let object_flags = flags
                | ObjectFlags::REFERENCE
                | ty::Ty::get_propagating_flags_of_tys(resolved_ty_args.unwrap_or_default(), None);
            let ty =
                self.create_object_ty(ty::ObjectTyKind::Reference(self.alloc(ty)), object_flags);
            assert!(!self.ty_links.contains_key(&ty.id));
            if let Some(resolved_ty_args) = resolved_ty_args {
                let prev = self.ty_links.insert(
                    ty.id,
                    TyLinks::default().with_resolved_ty_args(resolved_ty_args),
                );
                debug_assert!(prev.is_none());
            }
            self.instantiation_ty_map.insert(id, ty);
            ty
        }
    }

    pub(super) fn create_interface_ty(
        &mut self,
        symbol: SymbolID,
        ty_params: Option<ty::Tys<'cx>>,
        outer_ty_params: Option<ty::Tys<'cx>>,
        local_ty_params: Option<ty::Tys<'cx>>,
        this_ty: Option<&'cx ty::Ty<'cx>>,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let links = self.interface_ty_links_arena.alloc(Default::default());
        let ty = ty::InterfaceTy {
            symbol,
            ty_params,
            outer_ty_params,
            local_ty_params,
            this_ty,
            links,
        };
        self.create_object_ty(ty::ObjectTyKind::Interface(self.alloc(ty)), object_flags)
    }

    pub(super) fn create_anonymous_ty(
        &mut self,
        symbol: Option<SymbolID>,
        object_flags: ObjectFlags,
        node: Option<ast::NodeID>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let links = self.fresh_ty_links_arena.alloc(Default::default());
        debug_assert!(
            node.is_none() || object_flags.contains(ty::ObjectFlags::INSTANTIATION_EXPRESSION_TYPE)
        );
        let ty = self.alloc(ty::AnonymousTy {
            symbol,
            target: None,
            mapper: None,
            fresh_ty_links: links,
            node,
            alias_symbol,
            alias_ty_arguments,
        });

        self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        )
    }

    pub(super) fn create_anonymous_ty_with_resolved(
        &mut self,
        symbol: Option<SymbolID>,
        object_flags: ObjectFlags,
        members: &'cx FxIndexMap<SymbolName, SymbolID>,
        call_sigs: ty::Sigs<'cx>,
        ctor_sigs: ty::Sigs<'cx>,
        index_infos: ty::IndexInfos<'cx>,
        node: Option<ast::NodeID>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.create_anonymous_ty(symbol, object_flags, node, None, None);
        let props = self.get_props_from_members(members);
        let prev = self.ty_links.insert(
            ty.id,
            TyLinks::default().with_structured_members(self.alloc(ty::StructuredMembers {
                members,
                props,
                call_sigs,
                ctor_sigs,
                index_infos,
            })),
        );
        assert!(prev.is_none());
        ty
    }

    pub(super) fn create_instantiating_anonymous_ty(
        &mut self,
        symbol: SymbolID,
        target: &'cx ty::Ty<'cx>,
        mapper: &'cx dyn ty::TyMap<'cx>,
        object_flags: ObjectFlags,
        node: Option<ast::NodeID>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        assert!(target.kind.is_object_anonymous());
        debug_assert!(
            node.is_none() || object_flags.contains(ty::ObjectFlags::INSTANTIATION_EXPRESSION_TYPE)
        );
        let links = self.fresh_ty_links_arena.alloc(Default::default());
        let ty = self.alloc(ty::AnonymousTy {
            symbol: Some(symbol),
            target: Some(target),
            mapper: Some(mapper),
            fresh_ty_links: links,
            node,
            alias_symbol,
            alias_ty_arguments,
        });

        self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        )
    }

    pub(super) fn create_single_sig_ty(&mut self, ty: ty::SingleSigTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(
            ty::ObjectTyKind::SingleSigTy(self.alloc(ty)),
            ObjectFlags::empty(),
        )
    }

    pub(super) fn clone_param_ty(&mut self, old: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let old_param = old.kind.expect_param();
        let param_ty = self.alloc(ty::ParamTy {
            target: Some(old),
            ..*old_param
        });
        self.new_ty(ty::TyKind::Param(param_ty), old.flags)
    }

    pub(super) fn create_param_ty(
        &mut self,
        symbol: SymbolID,
        offset: Option<usize>,
        is_this_ty: bool,
    ) -> &'cx ty::Ty<'cx> {
        let ty = ty::ParamTy {
            symbol,
            offset,
            target: None,
            is_this_ty,
        };
        let parm_ty = self.alloc(ty);
        self.new_ty(ty::TyKind::Param(parm_ty), TypeFlags::TYPE_PARAMETER)
    }

    pub(super) fn create_computed_enum_ty(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        let r_links = self.fresh_ty_links_arena.alloc(Default::default());
        let regular_ty = self.alloc(ty::EnumTy {
            symbol,
            fresh_ty_links: r_links,
        });
        let r = self.new_ty(ty::TyKind::Enum(regular_ty), TypeFlags::ENUM);

        let f_links = self.fresh_ty_links_arena.alloc(Default::default());
        let fresh_ty = self.alloc(ty::EnumTy {
            symbol,
            fresh_ty_links: f_links,
        });
        let f = self.new_ty(ty::TyKind::Enum(fresh_ty), TypeFlags::ENUM);

        self.fresh_ty_links_arena[r_links].set_regular_ty(r);
        self.fresh_ty_links_arena[r_links].set_fresh_ty(f);
        self.fresh_ty_links_arena[f_links].set_regular_ty(r);
        self.fresh_ty_links_arena[f_links].set_fresh_ty(f);

        r
    }

    fn add_ty_to_union(
        &self,
        set: &mut nohash_hasher::IntSet<TyID>,
        mut includes: TypeFlags,
        ty: &'cx ty::Ty<'cx>,
    ) -> TypeFlags {
        let flags = ty.flags;
        if !flags.contains(TypeFlags::NEVER) {
            includes |= flags & TypeFlags::INCLUDES_MASK;
            if flags.intersects(TypeFlags::INSTANTIABLE) {
                includes |= TypeFlags::INCLUDES_INSTANTIABLE;
            }
            let object_flags = ty.get_object_flags();
            if flags.contains(TypeFlags::INTERSECTION)
                && object_flags.contains(ObjectFlags::IS_CONSTRAINED_TYPE_VARIABLE)
            {
                includes |= TypeFlags::INCLUDES_CONSTRAINED_TYPE_VARIABLE;
            }
            if ty == self.wildcard_ty {
                includes |= TypeFlags::INCLUDES_WILDCARD;
            }
            if self.is_error(ty) {
                includes |= TypeFlags::INCLUDES_ERROR;
            }

            if !self.config.strict_null_checks() && flags.intersects(TypeFlags::NULLABLE) {
                if !object_flags.contains(ObjectFlags::CONTAINS_WIDENING_TYPE) {
                    includes |= TypeFlags::INCLUDES_NON_WIDENING_TYPE;
                }
            } else {
                set.insert(ty.id);
            }
        }
        includes
    }

    fn add_tys_to_union(
        &self,
        set: &mut nohash_hasher::IntSet<TyID>,
        mut includes: TypeFlags,
        tys: &[&'cx ty::Ty<'cx>],
    ) -> TypeFlags {
        let mut last_ty = None;
        for ty in tys {
            let is_last = last_ty.is_some_and(|last_ty| last_ty == ty);
            if !is_last {
                includes = if let Some(u) = ty.kind.as_union() {
                    self.add_tys_to_union(set, includes, u.tys)
                } else {
                    self.add_ty_to_union(set, includes, ty)
                };
                last_ty = Some(ty)
            }
        }
        includes
    }

    pub(super) fn create_array_ty(
        &mut self,
        element_ty: &'cx ty::Ty<'cx>,
        readonly: bool,
    ) -> &'cx ty::Ty<'cx> {
        let target = if readonly {
            self.global_readonly_array_ty()
        } else {
            self.global_array_ty()
        };
        let resolved_ty_args = self.alloc([element_ty]);
        self.create_reference_ty(target, Some(resolved_ty_args), ObjectFlags::empty())
    }

    fn remove_redundant_lit_tys(
        &mut self,
        mut tys: Vec<&'cx ty::Ty<'cx>>,
        includes: TypeFlags,
        reduce_void_undefined: bool,
    ) -> Vec<&'cx ty::Ty<'cx>> {
        let mut i = tys.len();
        while i > 0 {
            i -= 1;
            let t = tys[i];
            let flags = t.flags;
            let remove = (flags.intersects(
                TypeFlags::STRING_LITERAL
                    .union(TypeFlags::TEMPLATE_LITERAL)
                    .union(TypeFlags::STRING_MAPPING),
            ) && includes.contains(TypeFlags::STRING))
                || (flags.intersects(TypeFlags::NUMBER_LITERAL)
                    && includes.contains(TypeFlags::NUMBER))
                || (flags.intersects(TypeFlags::BIG_INT_LITERAL)
                    && includes.contains(TypeFlags::BIG_INT))
                || (flags.intersects(TypeFlags::UNIQUE_ES_SYMBOL)
                    && includes.contains(TypeFlags::ES_SYMBOL))
                || (reduce_void_undefined
                    && flags.contains(TypeFlags::UNDEFINED)
                    && includes.contains(TypeFlags::VOID))
                || self.is_fresh_literal_ty(t) && tys.contains(&self.get_regular_ty(t).unwrap());

            if remove {
                tys.remove(i);
            }
        }

        tys
    }

    fn get_union_ty_worker<const IS_ENUM: bool>(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        reduction: UnionReduction,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
        origin: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let mut set = no_hashset_with_capacity(tys.len());
        let includes = self.add_tys_to_union(&mut set, TypeFlags::empty(), tys);

        let mut set: Vec<_> = set.into_iter().map(|id| self.tys[id.as_usize()]).collect();
        set.sort_unstable_by_key(|ty| ty.id.as_u32());

        if reduction != UnionReduction::None {
            if includes.intersects(TypeFlags::ANY_OR_UNKNOWN) {
                return if includes.intersects(TypeFlags::ANY) {
                    if includes.intersects(TypeFlags::INCLUDES_WILDCARD) {
                        self.wildcard_ty
                    } else if includes.intersects(TypeFlags::INCLUDES_ERROR) {
                        self.error_ty
                    } else {
                        self.any_ty
                    }
                } else {
                    self.unknown_ty
                };
            }
            if includes.contains(TypeFlags::UNDEFINED)
                && set.len() >= 2
                && set[0] == self.undefined_ty
                && set[1] == self.missing_ty
            {
                set.remove(1);
            }
            if includes.intersects(
                TypeFlags::ENUM
                    .union(TypeFlags::LITERAL)
                    .union(TypeFlags::UNIQUE_ES_SYMBOL)
                    .union(TypeFlags::TEMPLATE_LITERAL)
                    .union(TypeFlags::STRING_MAPPING),
            ) || includes.contains(TypeFlags::VOID) && includes.contains(TypeFlags::UNDEFINED)
            {
                set = self.remove_redundant_lit_tys(
                    set,
                    includes,
                    reduction == UnionReduction::Subtype,
                );
            }

            if includes.contains(TypeFlags::STRING_LITERAL)
                && includes.intersects(TypeFlags::TEMPLATE_LITERAL.union(TypeFlags::STRING_MAPPING))
            {
                // TODO:
            }

            if reduction == UnionReduction::Subtype {
                set = self.remove_subtypes(set);
            }

            if set.is_empty() {
                return if includes.contains(TypeFlags::NULL) {
                    if includes.intersects(TypeFlags::INCLUDES_NON_WIDENING_TYPE) {
                        self.null_ty
                    } else {
                        self.null_widening_ty
                    }
                } else if includes.contains(TypeFlags::UNDEFINED) {
                    if includes.intersects(TypeFlags::INCLUDES_NON_WIDENING_TYPE) {
                        self.undefined_ty
                    } else {
                        self.undefined_widening_ty
                    }
                } else {
                    self.never_ty
                };
            }
        }

        let pre_computed_object_flags = if includes.intersects(TypeFlags::NOT_PRIMITIVE_UNION) {
            ObjectFlags::empty()
        } else {
            ObjectFlags::PRIMITIVE_UNION
        } | if includes.contains(TypeFlags::INTERSECTION) {
            ObjectFlags::CONTAINS_INTERSECTIONS
        } else {
            ObjectFlags::empty()
        };
        self.get_union_ty_from_sorted_list::<IS_ENUM>(
            set,
            pre_computed_object_flags,
            alias_symbol,
            alias_ty_arguments,
            origin,
        )
    }

    pub(super) fn get_union_ty<const IS_ENUM: bool>(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        reduction: UnionReduction,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
        origin: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if tys.is_empty() {
            self.never_ty
        } else if tys.len() == 1 {
            tys[0]
        } else if tys.len() == 2
            && origin.is_none()
            && (tys[0].kind.is_union() || tys[1].kind.is_union())
        {
            let key = UnionOfUnionTysKey::new(reduction, tys, alias_symbol, alias_ty_arguments);
            if let Some(ty) = self.union_of_union_tys.get(&key) {
                return ty;
            };
            let ty = self.get_union_ty_worker::<IS_ENUM>(
                tys,
                reduction,
                alias_symbol,
                alias_ty_arguments,
                origin,
            );
            let prev = self.union_of_union_tys.insert(key, ty);
            debug_assert!(prev.is_none());
            ty
        } else {
            self.get_union_ty_worker::<IS_ENUM>(
                tys,
                reduction,
                alias_symbol,
                alias_ty_arguments,
                origin,
            )
        }
    }

    pub(super) fn get_union_ty_from_sorted_list<const IS_ENUM: bool>(
        &mut self,
        tys: Vec<&'cx ty::Ty<'cx>>,
        pre_computed_object_flags: ObjectFlags,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
        origin: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        debug_assert!(tys.is_sorted_by_key(|probe| probe.id.as_u32()));
        if tys.is_empty() {
            return self.never_ty;
        } else if tys.len() == 1 {
            return tys[0];
        }
        let id = UnionMap::create_ty_key(&tys);
        if let Some(result) = self.union_tys.get(id) {
            return result;
        }
        let object_flags =
            pre_computed_object_flags | ty::Ty::get_propagating_flags_of_tys(&tys, None);
        let mut flags = TypeFlags::UNION;
        if tys.len() == 2
            && tys[0].flags.intersects(TypeFlags::BOOLEAN_LITERAL)
            && tys[1].flags.intersects(TypeFlags::BOOLEAN_LITERAL)
        {
            flags |= TypeFlags::BOOLEAN;
        } else if IS_ENUM {
            flags |= TypeFlags::ENUM_LITERAL;
        }
        let fresh_ty_links = self.fresh_ty_links_arena.alloc(Default::default());
        let union_ty_links = self.union_ty_links_arena.alloc(Default::default());
        let promise_or_awaitable_links = self
            .promise_or_awaitable_links_arena
            .alloc(Default::default());
        debug_assert!(tys.len() >= 2);
        let union = self.alloc(ty::UnionTy {
            tys: self.alloc(tys),
            object_flags,
            fresh_ty_links,
            union_ty_links,
            alias_symbol,
            alias_ty_arguments,
            promise_or_awaitable_links,
            origin,
        });
        let ty = self.new_ty(ty::TyKind::Union(union), flags);
        self.union_tys.insert(id, ty);
        ty
    }

    fn remove_subtypes(&mut self, mut tys: Vec<&'cx ty::Ty<'cx>>) -> Vec<&'cx ty::Ty<'cx>> {
        let len = tys.len();
        if len < 2 {
            return tys;
        }
        let mut i = len;
        while i > 0 {
            i -= 1;
            let source = tys[i];
            if source.kind.is_structured_or_instantiable() {
                for target in tys.iter() {
                    if !source.eq(target)
                        && self.is_type_related_to(source, target, RelationKind::StrictSubtype)
                    {
                        tys.remove(i);
                        break;
                    }
                }
            }
        }
        tys
    }

    pub(super) fn create_index_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        index_flags: IndexFlags,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.alloc(ty::IndexTy { ty, index_flags });
        self.new_ty(ty::TyKind::Index(ty), TypeFlags::INDEX)
    }

    pub(super) fn get_tuple_target_ty(
        &mut self,
        element_flags: &'cx [ElementFlags],
        readonly: bool,
    ) -> &'cx ty::Ty<'cx> {
        if element_flags.len() == 1 && element_flags[0].contains(ElementFlags::REST) {
            return if readonly {
                self.global_readonly_array_ty()
            } else {
                self.global_array_ty()
            };
        }
        let mut hasher = rustc_hash::FxHasher::default();
        element_flags.iter().for_each(|flag| {
            let u = if flag.intersects(ElementFlags::REQUIRED) {
                b'#'
            } else if flag.intersects(ElementFlags::OPTIONAL) {
                b'?'
            } else if flag.intersects(ElementFlags::REST) {
                b'.'
            } else {
                b'*'
            };
            hasher.write_u8(u);
        });
        if readonly {
            hasher.write_u8(b'R');
        }
        let key = hasher.finish();
        if let Some(ty) = self.tuple_tys.get(&key) {
            return ty;
        }

        fn create_tuple_target_type<'cx>(
            this: &mut TyChecker<'cx>,
            element_flags: &'cx [ElementFlags],
            readonly: bool,
        ) -> &'cx ty::Ty<'cx> {
            let arity = element_flags.len();
            let min_length = element_flags
                .iter()
                .filter(|flag| {
                    flag.intersects(ElementFlags::REQUIRED.union(ElementFlags::VARIADIC))
                })
                .count();
            let mut ty_params: Option<ty::Tys<'cx>> = None;
            let mut props = Vec::with_capacity(arity + 1);
            let mut combined_flags = ElementFlags::empty();
            let check_flags = if readonly {
                CheckFlags::READONLY
            } else {
                CheckFlags::empty()
            };
            if arity > 0 {
                let mut _ty_params = Vec::with_capacity(arity);
                debug_assert_eq!(arity, element_flags.len());
                for (i, flag) in element_flags.iter().enumerate() {
                    let ty_param = this.create_param_ty(Symbol::ERR, Some(i), false);
                    _ty_params.push(ty_param);
                    combined_flags |= *flag;
                    if !combined_flags.intersects(ElementFlags::VARIABLE) {
                        let name = SymbolName::EleNum(i.into());
                        let symbol_flags = SymbolFlags::PROPERTY.union(SymbolFlags::TRANSIENT)
                            | if flag.intersects(ElementFlags::OPTIONAL) {
                                SymbolFlags::OPTIONAL
                            } else {
                                SymbolFlags::empty()
                            };
                        let property = this.create_transient_symbol(
                            name,
                            symbol_flags,
                            SymbolLinks::default()
                                .with_ty(ty_param)
                                .with_check_flags(check_flags),
                            None,
                            None,
                        );
                        props.push(property);
                    }
                }
                ty_params = Some(this.alloc(_ty_params));
            }

            let fixed_length = props.len();
            let length_symbol_name = SymbolName::Atom(keyword::IDENT_LENGTH);
            let ty = if combined_flags.intersects(ElementFlags::VARIABLE) {
                this.number_ty
            } else {
                let tys = (min_length..=arity)
                    .map(|i| this.get_number_literal_type_from_number(i as f64))
                    .collect::<Vec<_>>();
                this.get_union_ty::<false>(&tys, UnionReduction::Lit, None, None, None)
            };
            let length_symbol = this.create_transient_symbol(
                length_symbol_name,
                SymbolFlags::PROPERTY.union(SymbolFlags::TRANSIENT),
                SymbolLinks::default()
                    .with_ty(ty)
                    .with_check_flags(check_flags),
                None,
                None,
            );
            props.push(length_symbol);

            const OBJECT_FLAGS: ObjectFlags = ObjectFlags::TUPLE.union(ObjectFlags::REFERENCE);
            let this_ty = this.create_param_ty(Symbol::ERR, None, true);
            let ty = this.create_interface_ty(
                Symbol::ERR,
                ty_params,
                None,
                ty_params,
                Some(this_ty),
                ObjectFlags::empty(),
            );
            let declared_members = this.alloc(ty::DeclaredMembers {
                props: this.alloc(props),
                call_sigs: this.empty_array(),
                ctor_sigs: this.empty_array(),
                index_infos: this.empty_array(),
            });
            this.interface_ty_links_arena[ty.kind.expect_object_interface().links]
                .set_declared_members(declared_members);
            let resolved_ty_args = ty_params.unwrap_or_default();
            assert!(resolved_ty_args.len() == element_flags.len());
            let tuple = ty::TupleTy {
                ty,
                resolved_ty_args,
                min_length,
                fixed_length,
                element_flags,
                combined_flags,
                readonly,
            };
            let ty =
                this.create_object_ty(ty::ObjectTyKind::Tuple(this.alloc(tuple)), OBJECT_FLAGS);
            this.instantiation_ty_map
                .insert(InstantiationTyMap::create_id(ty.id, resolved_ty_args), ty);
            ty
        }

        let ty = create_tuple_target_type(self, element_flags, readonly);
        let prev = self.tuple_tys.insert(key, ty);
        assert!(prev.is_none());
        ty
    }

    fn add_ty_to_intersection(
        &mut self,
        set: &mut indexmap::IndexSet<TyID>,
        mut includes: TypeFlags,
        mut ty: &'cx ty::Ty<'cx>,
    ) -> TypeFlags {
        if let Some(i) = ty.kind.as_intersection() {
            return self.add_tys_to_intersection(set, includes, i.tys);
        }
        let flags = ty.flags;

        if self.is_empty_anonymous_object_ty(ty) {
            if !includes.intersects(TypeFlags::INCLUDES_EMPTY_OBJECT) {
                includes |= TypeFlags::INCLUDES_EMPTY_OBJECT;
                set.insert(ty.id);
            }
        } else {
            if flags.intersects(TypeFlags::ANY_OR_UNKNOWN) {
                if ty == self.wildcard_ty {
                    includes |= TypeFlags::INCLUDES_WILDCARD;
                }
            } else if self.config.strict_null_checks() || !flags.intersects(TypeFlags::NULLABLE) {
                if ty == self.missing_ty {
                    includes |= TypeFlags::INCLUDES_MISSING_TYPE;
                    ty = self.undefined_ty;
                }
                if !set.contains(&ty.id) {
                    if ty.flags.intersects(TypeFlags::UNIT) && includes.intersects(TypeFlags::UNIT)
                    {
                        includes |= TypeFlags::NON_PRIMITIVE;
                    }
                    set.insert(ty.id);
                }
            }
            includes |= flags & TypeFlags::INCLUDES_MASK;
        }

        includes
    }

    fn add_tys_to_intersection(
        &mut self,
        set: &mut indexmap::IndexSet<TyID>,
        mut includes: TypeFlags,
        tys: &[&'cx ty::Ty<'cx>],
    ) -> TypeFlags {
        for ty in tys {
            let ty = self.get_regular_ty_of_literal_ty(ty);
            includes = self.add_ty_to_intersection(set, includes, ty);
        }
        includes
    }

    fn create_intersection_ty(
        &mut self,
        set: ty::Tys<'cx>,
        flags: ObjectFlags,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let object_flags =
            flags | ty::Ty::get_propagating_flags_of_tys(set, Some(TypeFlags::NULLABLE));
        let ty = self.alloc(ty::IntersectionTy {
            tys: set,
            object_flags,
            alias_symbol,
            alias_ty_arguments,
        });
        self.new_ty(ty::TyKind::Intersection(ty), TypeFlags::INTERSECTION)
    }

    fn intersect_unions_of_primitive_tys(&mut self, tys: &mut Vec<&'cx ty::Ty<'cx>>) -> bool {
        let Some(index) = tys.iter().position(|ty| {
            ty.get_object_flags()
                .intersects(ObjectFlags::PRIMITIVE_UNION)
        }) else {
            return false;
        };
        let mut union_tys = Vec::with_capacity(tys.len());
        let mut i = index + 1;
        while i < tys.len() {
            let t = tys[i];
            if t.get_object_flags()
                .intersects(ObjectFlags::PRIMITIVE_UNION)
            {
                if union_tys.is_empty() {
                    union_tys.push(tys[index]);
                }
                union_tys.push(t);
                tys.remove(i);
            } else {
                i += 1;
            }
        }

        if union_tys.is_empty() {
            return false;
        }

        let mut checked = Vec::with_capacity(tys.len());
        let mut result = Vec::with_capacity(tys.len());
        for ty in &union_tys {
            let u = ty.kind.expect_union();
            for t in u.tys {
                if insert_ty(&mut checked, t) && self.each_union_contains(&union_tys, t) {
                    // TODO: missing_ty and undefined_ty
                    if *t == self.undefined_ty && !result.is_empty() && result[0] == self.missing_ty
                    {
                        continue;
                    } else if *t == self.missing_ty
                        && !result.is_empty()
                        && result[0] == self.undefined_ty
                    {
                        result[0] = self.undefined_ty;
                        continue;
                    }
                    insert_ty(&mut result, t);
                }
            }
        }

        tys[index] = self.get_union_ty_from_sorted_list::<false>(
            result,
            ObjectFlags::PRIMITIVE_UNION,
            None,
            None,
            None,
        );
        true
    }

    fn remove_redundant_super_tys(&mut self, tys: &mut Vec<&'cx ty::Ty<'cx>>, includes: TypeFlags) {
        let mut i = tys.len();
        while i > 0 {
            i -= 1;
            let t = tys[i];
            let remove = t.flags.contains(TypeFlags::STRING)
                && includes.intersects(
                    TypeFlags::STRING_LITERAL
                        .union(TypeFlags::TEMPLATE_LITERAL)
                        .union(TypeFlags::STRING_MAPPING),
                )
                || t.flags.contains(TypeFlags::NUMBER)
                    && includes.contains(TypeFlags::NUMBER_LITERAL)
                || t.flags.contains(TypeFlags::BIG_INT)
                    && includes.contains(TypeFlags::BIG_INT_LITERAL)
                || t.flags.contains(TypeFlags::ES_SYMBOL)
                    && includes.contains(TypeFlags::UNIQUE_ES_SYMBOL)
                || t.flags.contains(TypeFlags::VOID) && includes.contains(TypeFlags::UNDEFINED)
                || self.is_empty_anonymous_object_ty(t)
                    && includes.intersects(TypeFlags::DEFINITELY_NON_NULLABLE);
            if remove {
                tys.remove(i);
            }
        }
    }

    fn extract_redundant_template_lits(&mut self, tys: &mut Vec<&'cx ty::Ty<'cx>>) -> bool {
        let mut i = tys.len();
        let lits = tys
            .iter()
            .filter(|ty| ty.flags.intersects(TypeFlags::STRING_LITERAL))
            .copied()
            .collect::<Vec<_>>();
        while i > 0 {
            i -= 1;
            let t = tys[i];
            if !t
                .flags
                .intersects(TypeFlags::TEMPLATE_LITERAL.union(TypeFlags::STRING_MAPPING))
            {
                continue;
            }
            for t2 in &lits {
                if self.is_ty_sub_type_of(t2, t) {
                    tys.remove(i);
                    break;
                } else if t.is_pattern_lit_ty() {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn get_intersection_ty(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        flags: IntersectionFlags,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let mut set = indexmap::IndexSet::with_capacity(tys.len());
        let includes = self.add_tys_to_intersection(&mut set, TypeFlags::empty(), tys);
        let mut ty_set = set
            .into_iter()
            .map(|id| self.tys[id.as_usize()])
            .collect::<Vec<_>>();

        if includes.contains(TypeFlags::NEVER) {
            return self.never_ty;
        }

        let strict_null_checks = self.config.strict_null_checks();

        if (strict_null_checks
            && includes.intersects(TypeFlags::NULLABLE)
            && includes.intersects(
                TypeFlags::OBJECT
                    .union(TypeFlags::NON_PRIMITIVE)
                    .union(TypeFlags::INCLUDES_EMPTY_OBJECT),
            )
            || (includes.contains(TypeFlags::NON_PRIMITIVE)
                && includes.intersects(
                    TypeFlags::DISJOINT_DOMAINS.intersection(TypeFlags::NON_PRIMITIVE.not()),
                )))
            || includes.intersects(TypeFlags::STRING_LIKE)
                && includes.intersects(
                    TypeFlags::DISJOINT_DOMAINS.intersection(TypeFlags::STRING_LIKE.not()),
                )
            || includes.intersects(TypeFlags::NUMBER_LIKE)
                && includes.intersects(
                    TypeFlags::DISJOINT_DOMAINS.intersection(TypeFlags::NUMBER_LIKE.not()),
                )
            || includes.intersects(TypeFlags::BIG_INT_LIKE)
                && includes.intersects(
                    TypeFlags::DISJOINT_DOMAINS.intersection(TypeFlags::BIG_INT_LIKE.not()),
                )
            || includes.intersects(TypeFlags::ES_SYMBOL_LIKE)
                && includes.intersects(
                    TypeFlags::DISJOINT_DOMAINS.intersection(TypeFlags::ES_SYMBOL_LIKE.not()),
                )
            || includes.intersects(TypeFlags::VOID_LIKE)
                && includes.intersects(
                    TypeFlags::DISJOINT_DOMAINS.intersection(TypeFlags::VOID_LIKE.not()),
                )
            || includes.intersects(TypeFlags::TEMPLATE_LITERAL.union(TypeFlags::STRING_MAPPING))
                && includes.contains(TypeFlags::STRING_LITERAL)
                && self.extract_redundant_template_lits(&mut ty_set)
        {
            return self.never_ty;
        }

        if includes.contains(TypeFlags::ANY) {
            return if includes.intersects(TypeFlags::INCLUDES_WILDCARD) {
                self.wildcard_ty
            } else if includes.intersects(TypeFlags::INCLUDES_ERROR) {
                self.error_ty
            } else {
                self.any_ty
            };
        }

        if !strict_null_checks && includes.intersects(TypeFlags::NULLABLE) {
            if includes.intersects(TypeFlags::INCLUDES_EMPTY_OBJECT) {
                return self.never_ty;
            } else if includes.contains(TypeFlags::UNDEFINED) {
                return self.undefined_ty;
            } else {
                return self.null_ty;
            }
        }
        if (includes.contains(TypeFlags::STRING)
            && includes.intersects(
                TypeFlags::STRING_LITERAL
                    .union(TypeFlags::TEMPLATE_LITERAL)
                    .union(TypeFlags::STRING_MAPPING),
            )
            || includes.contains(TypeFlags::NUMBER) && includes.contains(TypeFlags::NUMBER_LITERAL)
            || includes.contains(TypeFlags::BIG_INT)
                && includes.contains(TypeFlags::BIG_INT_LITERAL)
            || includes.contains(TypeFlags::ES_SYMBOL)
                && includes.contains(TypeFlags::UNIQUE_ES_SYMBOL)
            || includes.contains(TypeFlags::VOID) && includes.contains(TypeFlags::UNDEFINED)
            || includes.intersects(TypeFlags::INCLUDES_EMPTY_OBJECT)
                && includes.intersects(TypeFlags::DEFINITELY_NON_NULLABLE))
            && flags != IntersectionFlags::NoSuperTypeReduction
        {
            self.remove_redundant_super_tys(&mut ty_set, includes);
        }

        if includes.intersects(TypeFlags::INCLUDES_MISSING_TYPE) {
            let idx = ty_set
                .iter()
                .position(|ty| *ty == self.undefined_ty)
                .unwrap();
            ty_set[idx] = self.missing_ty;
        }

        if ty_set.is_empty() {
            return self.unknown_ty;
        } else if ty_set.len() == 1 {
            return ty_set[0];
        }

        let mut object_flags = ObjectFlags::empty();

        if ty_set.len() == 2 && flags != IntersectionFlags::NoConstraintReduction {
            let ty_var_index = if ty_set[0].flags.intersects(TypeFlags::TYPE_VARIABLE) {
                0
            } else {
                1
            };
            let primitive_ty = ty_set[1 - ty_var_index];
            let ty_var = ty_set[ty_var_index];
            if ty_var.flags.intersects(TypeFlags::TYPE_VARIABLE)
                && (primitive_ty
                    .flags
                    .intersects(TypeFlags::PRIMITIVE.union(TypeFlags::NON_PRIMITIVE))
                    && !primitive_ty.is_generic_string_like()
                    || includes.intersects(TypeFlags::INCLUDES_EMPTY_OBJECT))
                && let Some(constraint) = self.get_base_constraint_of_ty(ty_var)
                && self.every_type(constraint, |this, t| {
                    t.flags
                        .intersects(TypeFlags::PRIMITIVE.union(TypeFlags::NON_PRIMITIVE))
                        || this.is_empty_anonymous_object_ty(t)
                })
            {
                if self.is_ty_strict_sub_type_of(constraint, primitive_ty) {
                    return ty_var;
                }
                if !constraint.kind.as_union().is_some_and(|u| {
                    u.tys
                        .iter()
                        .all(|c| self.is_ty_strict_sub_type_of(c, primitive_ty))
                }) && !self.is_ty_strict_sub_type_of(primitive_ty, constraint)
                {
                    return self.never_ty;
                }

                object_flags = ObjectFlags::IS_CONSTRAINED_TYPE_VARIABLE;
            }
        }

        let key = (ty_set, flags == IntersectionFlags::NoConstraintReduction);
        let id = IntersectionMap::create_ty_key(&key);
        if let Some(ty) = self.intersection_tys.get(id) {
            return ty;
        }
        let mut ty_set = key.0;

        let ty = if includes.contains(TypeFlags::UNION) {
            if self.intersect_unions_of_primitive_tys(&mut ty_set) {
                self.get_intersection_ty(&ty_set, IntersectionFlags::None, None, None)
            } else if ty_set.iter().all(|ty| {
                ty.kind
                    .as_union()
                    .is_some_and(|u| u.tys[0].flags.contains(TypeFlags::UNDEFINED))
            }) {
                let contained_undefined_ty = if ty_set.iter().any(|ty| self.contains_missing_ty(ty))
                {
                    self.missing_ty
                } else {
                    self.undefined_ty
                };
                self.remove_from_each(&mut ty_set, TypeFlags::UNDEFINED);
                let i = self.get_intersection_ty(&ty_set, flags, None, None);
                self.get_union_ty::<false>(
                    &[i, contained_undefined_ty],
                    UnionReduction::Lit,
                    alias_symbol,
                    alias_ty_arguments,
                    None,
                )
            } else if ty_set.iter().all(|ty| {
                ty.kind.as_union().is_some_and(|u| {
                    u.tys[0].flags.contains(TypeFlags::NULL)
                        || u.tys[1].flags.contains(TypeFlags::NULL)
                })
            }) {
                todo!()
            } else if ty_set.len() >= 3 && tys.len() > 2 {
                let middle = ty_set.len() / 2;
                let l = self.get_intersection_ty(&ty_set[..middle], flags, None, None);
                let r = self.get_intersection_ty(&ty_set[middle..], flags, None, None);
                self.get_intersection_ty(&[l, r], flags, alias_symbol, alias_ty_arguments)
            } else {
                if !Self::check_cross_product_union(&ty_set) {
                    return self.error_ty;
                }
                let constituents = self.get_cross_product_intersections(&ty_set, flags);
                let origin = if constituents
                    .iter()
                    .any(|ty| ty.flags.contains(TypeFlags::INTERSECTION))
                    && self.get_constituent_count_of_tys(&constituents)
                        > self.get_constituent_count_of_tys(&ty_set)
                {
                    let tys = self.alloc(ty_set);
                    let ty =
                        self.create_origin_union_or_intersection_ty(TypeFlags::INTERSECTION, tys);
                    Some(ty)
                } else {
                    None
                };
                self.get_union_ty::<false>(
                    &constituents,
                    ty::UnionReduction::Lit,
                    alias_symbol,
                    alias_ty_arguments,
                    origin,
                )
            }
        } else {
            let tys = self.alloc(ty_set);
            self.create_intersection_ty(tys, object_flags, alias_symbol, alias_ty_arguments)
        };
        if let Some(old) = self.intersection_tys.get(id) {
            // cycle
            assert_eq!(old, ty);
            old
        } else {
            self.intersection_tys.insert(id, ty);
            ty
        }
    }

    pub(super) fn create_origin_union_or_intersection_ty(
        &mut self,
        flags: TypeFlags,
        tys: ty::Tys<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if flags == TypeFlags::INTERSECTION {
            let ty = self.alloc(ty::IntersectionTy {
                tys,
                object_flags: ObjectFlags::empty(),
                alias_symbol: None,
                alias_ty_arguments: None,
            });
            self.new_ty(ty::TyKind::Intersection(ty), flags)
        } else {
            debug_assert!(flags == TypeFlags::UNION);
            let fresh_ty_links = self.fresh_ty_links_arena.alloc(Default::default());
            let union_ty_links = self.union_ty_links_arena.alloc(Default::default());
            let promise_or_awaitable_links = self
                .promise_or_awaitable_links_arena
                .alloc(Default::default());
            debug_assert!(tys.len() >= 2);
            let union = self.alloc(ty::UnionTy {
                tys,
                object_flags: ObjectFlags::empty(),
                origin: None,
                fresh_ty_links,
                union_ty_links,
                promise_or_awaitable_links,
                alias_symbol: None,
                alias_ty_arguments: None,
            });
            self.new_ty(ty::TyKind::Union(union), flags)
        }
    }

    fn get_constituent_count(&self, ty: &'cx ty::Ty<'cx>) -> usize {
        if !ty.flags.intersects(TypeFlags::UNION_OR_INTERSECTION) || ty.alias_symbol().is_some() {
            1
        } else if let Some(u) = ty.kind.as_union()
            && let Some(origin) = u.origin
        {
            self.get_constituent_count(origin)
        } else {
            self.get_constituent_count_of_tys(ty.kind.tys_of_union_or_intersection().unwrap())
        }
    }

    fn get_constituent_count_of_tys(&self, tys: &[&'cx ty::Ty<'cx>]) -> usize {
        tys.iter()
            .fold(0, |prev, t| prev + self.get_constituent_count(t))
    }

    fn get_cross_product_union_size(tys: &[&'cx ty::Ty<'cx>]) -> usize {
        tys.iter().fold(1, |prev, t| {
            if let Some(u) = t.kind.as_union() {
                prev * u.tys.len()
            } else if t.flags.intersects(TypeFlags::NEVER) {
                0
            } else {
                prev
            }
        })
    }

    fn check_cross_product_union(tys: &[&'cx ty::Ty<'cx>]) -> bool {
        let size = Self::get_cross_product_union_size(tys);
        if size >= 100_000 {
            // TODO: error
            false
        } else {
            true
        }
    }

    fn get_cross_product_intersections(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        flags: IntersectionFlags,
    ) -> Vec<&'cx ty::Ty<'cx>> {
        let count = Self::get_cross_product_union_size(tys);
        let mut intersections = Vec::with_capacity(tys.len());
        for i in 0..count {
            let mut constituents = tys.to_vec();
            let mut n = i;
            for j in (0..tys.len()).rev() {
                if let Some(u) = tys[j].kind.as_union() {
                    let source_tys = u.tys;
                    debug_assert!(source_tys.is_sorted_by_key(|t| t.id.as_u32()));
                    let len = source_tys.len();
                    constituents[j] = source_tys[n % len];
                    n /= len;
                }
            }

            let t = self.get_intersection_ty(&constituents, flags, None, None);
            if !t.flags.contains(TypeFlags::NEVER) {
                intersections.push(t);
            }
        }

        intersections
    }

    pub fn get_substitution_ty(
        &mut self,
        base_ty: &'cx ty::Ty<'cx>,
        constraint: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if constraint.flags.intersects(TypeFlags::ANY_OR_UNKNOWN)
            || base_ty == constraint
            || base_ty.flags.contains(TypeFlags::ANY)
        {
            base_ty
        } else {
            self.get_or_create_substitution_ty(base_ty, constraint)
        }
    }

    fn get_or_create_substitution_ty(
        &mut self,
        base_ty: &'cx ty::Ty<'cx>,
        constraint: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let key = SubstitutionKey::new(base_ty, constraint);
        if let Some(cached) = self.substitution_tys.get(&key) {
            return cached;
        }
        let ty = self.alloc(ty::SubstitutionTy {
            object_flags: ObjectFlags::empty(),
            base_ty,
            constraint,
        });
        let ty = self.new_ty(ty::TyKind::Substitution(ty), TypeFlags::SUBSTITUTION);
        let prev = self.substitution_tys.insert(key, ty);
        debug_assert!(prev.is_none());
        ty
    }

    pub(super) fn get_substitution_intersection(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let sub = ty.kind.expect_substitution_ty();
        if ty.is_no_infer_ty() {
            sub.base_ty
        } else {
            self.get_intersection_ty(
                &[sub.constraint, sub.base_ty],
                IntersectionFlags::None,
                None,
                None,
            )
        }
    }

    pub(super) fn get_no_infer_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.is_no_infer_target_ty(ty) {
            self.get_or_create_substitution_ty(ty, self.unknown_ty)
        } else {
            ty
        }
    }

    pub(super) fn is_no_infer_target_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if let Some(tys) = ty.kind.tys_of_union_or_intersection() {
            tys.iter().any(|ty| self.is_no_infer_target_ty(ty))
        } else if let Some(sub) = ty.kind.as_substitution_ty() {
            !ty.is_no_infer_ty() && self.is_no_infer_target_ty(sub.base_ty)
        } else if ty.kind.is_object() {
            !self.is_empty_anonymous_object_ty(ty)
        } else if ty
            .flags
            .intersects(TypeFlags::INSTANTIABLE.intersection(TypeFlags::SUBSTITUTION.not()))
        {
            !ty.is_pattern_lit_ty()
        } else {
            false
        }
    }

    pub(super) fn create_string_mapping_ty(
        &mut self,
        symbol: SymbolID,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.alloc(ty::StringMappingTy { symbol, ty });
        self.new_ty(ty::TyKind::StringMapping(ty), TypeFlags::STRING_MAPPING)
    }

    pub(crate) fn slice_tuple_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        index: usize,
        end_skip_count: usize,
    ) -> &'cx ty::Ty<'cx> {
        let tuple = ty.as_tuple().unwrap();
        let end_index = TyChecker::get_ty_reference_arity(ty) - end_skip_count;
        if index > tuple.fixed_length {
            self.get_rest_ty_of_tuple_ty(ty)
                .unwrap_or_else(|| self.create_tuple_ty(self.empty_array(), None, false))
        } else {
            let tys = &self.get_ty_arguments(ty)[index..end_index];
            let element_flags = &tuple.element_flags[index..end_index];
            self.create_tuple_ty(tys, Some(element_flags), false)
        }
    }

    pub(super) fn get_template_lit_ty(
        &mut self,
        texts: &[bolt_ts_atom::Atom],
        tys: &[&'cx ty::Ty<'cx>],
    ) -> &'cx ty::Ty<'cx> {
        debug_assert_eq!(texts.len(), tys.len() + 1);

        fn add_spans<'cx>(
            this: &mut TyChecker<'cx>,
            texts: &[bolt_ts_atom::Atom],
            tys: &[&'cx ty::Ty<'cx>],
            new_texts: &mut Vec<bolt_ts_atom::Atom>,
            new_tys: &mut Vec<&'cx ty::Ty<'cx>>,
            text: &mut String,
        ) -> bool {
            for i in 0..tys.len() {
                let t = tys[i];
                if t.flags.intersects(
                    TypeFlags::LITERAL
                        .union(TypeFlags::NULL)
                        .union(TypeFlags::UNDEFINED),
                ) {
                    match t.kind {
                        ty::TyKind::StringLit(t) => {
                            text.push_str(this.atoms.get(t.val));
                        }
                        ty::TyKind::NumberLit(t) => {
                            text.push_str(&t.val.val().to_string());
                        }
                        ty::TyKind::BigIntLit(t) => {
                            if t.neg {
                                text.push('-');
                            }
                            let s = this.atoms.get(t.val);
                            text.push_str(s);
                        }
                        ty::TyKind::Intrinsic(i)
                            if t.flags.intersects(
                                TypeFlags::BOOLEAN_LITERAL.union(TypeFlags::NULLABLE),
                            ) =>
                        {
                            text.push_str(this.atoms.get(i.name));
                        }

                        _ => unreachable!(),
                    }
                    text.push_str(this.atoms.get(texts[i + 1]));
                } else if let Some(template) = t.kind.as_template_lit_ty() {
                    text.push_str(this.atoms.get(template.texts[0]));
                    if !add_spans(this, template.texts, template.tys, new_texts, new_tys, text) {
                        return false;
                    }
                    text.push_str(this.atoms.get(texts[i + 1]));
                } else if this.is_generic_index_ty(t) || t.is_pattern_lit_placeholder_ty() {
                    new_tys.push(t);
                    let atom = this.atoms.atom(text);
                    new_texts.push(atom);
                    *text = this.atoms.get(texts[i + 1]).to_string();
                } else {
                    return false;
                }
            }
            true
        }

        if let Some(union_index) = tys
            .iter()
            .position(|t| t.flags.intersects(TypeFlags::NEVER.union(TypeFlags::UNION)))
        {
            return if Self::check_cross_product_union(&tys) {
                self.map_ty(
                    tys[union_index],
                    |this, t| {
                        let mut tys = tys.to_vec();
                        tys[union_index] = t;
                        Some(this.get_template_lit_ty(texts, &tys))
                    },
                    false,
                )
                .unwrap()
            } else {
                self.error_ty
            };
        };
        if tys.contains(&self.wildcard_ty) {
            return self.wildcard_ty;
        }
        let mut new_tys = Vec::with_capacity(tys.len());
        let mut new_texts = Vec::with_capacity(texts.len());
        let mut text = self.atoms.get(texts[0]).to_string();

        if !add_spans(self, texts, tys, &mut new_texts, &mut new_tys, &mut text) {
            return self.string_ty;
        }

        let text = self.atoms.atom(&text);
        if new_tys.is_empty() {
            return self.get_string_literal_type_from_string(text);
        };
        new_texts.push(text);
        if new_texts.iter().all(|t| *t == keyword::IDENT_EMPTY) {
            if new_tys
                .iter()
                .all(|t| t.flags.intersects(TypeFlags::STRING))
            {
                return self.string_ty;
            } else if new_tys.len() == 1 && new_tys[0].is_pattern_lit_ty() {
                return new_tys[0];
            }
        }
        // TODO: cache;
        let new_texts = self.alloc(new_texts);
        let new_tys = self.alloc(new_tys);
        self.create_template_lit_ty(new_texts, new_tys)
    }

    fn create_template_lit_ty(
        &mut self,
        texts: &'cx [bolt_ts_atom::Atom],
        tys: ty::Tys<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let t = self.alloc(ty::TemplateLitTy { texts, tys });
        self.new_ty(ty::TyKind::TemplateLit(t), TypeFlags::TEMPLATE_LITERAL)
    }

    pub(super) fn get_spread_ty(
        &mut self,
        mut left: &'cx ty::Ty<'cx>,
        mut right: &'cx ty::Ty<'cx>,
        symbol: Option<SymbolID>,
        object_flags: ObjectFlags,
        is_readonly: bool,
    ) -> &'cx ty::Ty<'cx> {
        if left.flags.intersects(TypeFlags::ANY) || right.flags.intersects(TypeFlags::ANY) {
            return self.any_ty;
        } else if left.flags.intersects(TypeFlags::UNKNOWN)
            || right.flags.intersects(TypeFlags::UNKNOWN)
        {
            return self.unknown_ty;
        } else if left.flags.intersects(TypeFlags::NEVER) {
            return right;
        } else if right.flags.intersects(TypeFlags::NEVER) {
            return left;
        }

        left = self.try_merge_union_of_object_ty_and_empty_object(left, is_readonly);
        if left.flags.intersects(TypeFlags::UNION) {
            return if Self::check_cross_product_union(&[left, right]) {
                self.map_ty(
                    left,
                    |this, ty| {
                        Some(this.get_spread_ty(ty, right, symbol, object_flags, is_readonly))
                    },
                    false,
                )
                .unwrap()
            } else {
                self.error_ty
            };
        }

        right = self.try_merge_union_of_object_ty_and_empty_object(right, is_readonly);
        if right.flags.intersects(TypeFlags::UNION) {
            return if Self::check_cross_product_union(&[left, right]) {
                self.map_ty(
                    right,
                    |this, ty| {
                        Some(this.get_spread_ty(left, ty, symbol, object_flags, is_readonly))
                    },
                    false,
                )
                .unwrap()
            } else {
                self.error_ty
            };
        }

        if right.flags.intersects(
            TypeFlags::BOOLEAN_LIKE
                .union(TypeFlags::NUMBER_LIKE)
                .union(TypeFlags::BIG_INT_LIKE)
                .union(TypeFlags::STRING_LIKE)
                .union(TypeFlags::ENUM_LIKE)
                .union(TypeFlags::NON_PRIMITIVE)
                .union(TypeFlags::INDEX),
        ) {
            return left;
        }

        if self.is_generic_object_ty(left) || self.is_generic_object_ty(right) {
            if self.is_empty_object_ty(left) {
                return right;
            } else if let Some(i) = left.kind.as_intersection() {
                let last_ty = i.tys[i.tys.len() - 1];
                if self.is_non_generic_object_ty(last_ty) && self.is_non_generic_object_ty(right) {
                    let a = &i.tys[0..i.tys.len() - 1];
                    let b = self.get_spread_ty(last_ty, right, symbol, object_flags, is_readonly);
                    let tys = {
                        let mut a = a.to_vec();
                        a.push(b);
                        self.alloc(a)
                    };
                    return self.get_intersection_ty(tys, IntersectionFlags::None, None, None);
                }
            }
            return self.get_intersection_ty(&[left, right], IntersectionFlags::None, None, None);
        }

        let left_props = self.get_props_of_ty(left);
        let right_props = self.get_props_of_ty(right);
        let mut members = fx_indexmap_with_capacity(left_props.len() + right_props.len());
        let index_infos = if left == self.empty_object_ty() {
            self.get_index_infos_of_ty(right)
        } else {
            self.get_union_index_infos(&[left, right])
        };

        for right_prop in right_props {
            let name = self.symbol(*right_prop).name;
            // TODO: skip private and project
            let symbol = self.get_spread_symbol(*right_prop, is_readonly);
            members.insert(name, symbol);
        }

        for left_prop in left_props {
            let left_prop_symbol = self.symbol(*left_prop);
            let left_prop_symbol_name = left_prop_symbol.name;
            let left_prop_symbol_flags = left_prop_symbol.flags;
            use indexmap::map::Entry;
            match members.entry(left_prop_symbol_name) {
                Entry::Occupied(mut occ) => {
                    let right_prop = *occ.get();
                    let s = self.symbol(right_prop);
                    if s.flags.intersects(SymbolFlags::OPTIONAL) {
                        let flags = SymbolFlags::PROPERTY.union(SymbolFlags::TRANSIENT)
                            | left_prop_symbol_flags.intersection(SymbolFlags::OPTIONAL);
                        let name = s.name;
                        let links_ty = {
                            let left_ty = self.get_type_of_symbol(*left_prop);
                            let right_ty = self.get_type_of_symbol(right_prop);
                            let left_ty_without_undefined =
                                self.remove_missing_or_undefined_ty(left_ty);
                            let right_ty_without_undefined =
                                self.remove_missing_or_undefined_ty(right_ty);
                            if left_ty_without_undefined == right_ty_without_undefined {
                                left_ty
                            } else {
                                self.get_union_ty::<false>(
                                    &[left_ty, right_ty_without_undefined],
                                    ty::UnionReduction::Subtype,
                                    None,
                                    None,
                                    None,
                                )
                            }
                        };
                        let mut links = SymbolLinks::default().with_ty(links_ty);
                        if let Some(name_ty) = self.get_symbol_links(*left_prop).get_name_ty() {
                            links = links.with_name_ty(name_ty);
                        }
                        // TODO: left_spread, right_spread, declarations
                        let result = self.create_transient_symbol(name, flags, links, None, None);
                        occ.insert(result);
                    }
                }
                Entry::Vacant(vac) => {
                    let symbol = self.get_spread_symbol(*left_prop, is_readonly);
                    vac.insert(symbol);
                }
            }
        }

        let index_infos = self
            .same_map_index_infos(Some(index_infos), |this, index_info, _| {
                if index_info.is_readonly != is_readonly {
                    this.alloc(ty::IndexInfo {
                        is_readonly,
                        ..*index_info
                    })
                } else {
                    index_info
                }
            })
            .unwrap();

        self.create_anonymous_ty_with_resolved(
            symbol,
            ObjectFlags::OBJECT_LITERAL
                | ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL
                | ObjectFlags::CONTAINS_SPREAD
                | object_flags,
            self.alloc(members),
            self.empty_array(),
            self.empty_array(),
            index_infos,
            None,
        )
    }

    pub fn create_iteration_tys(
        &mut self,
        yield_ty: &'cx ty::Ty<'cx>,
        return_ty: &'cx ty::Ty<'cx>,
        next_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::IterationTys<'cx> {
        const NEED_CACHED_TYPE_FLAGS: TypeFlags = TypeFlags::ANY
            .union(TypeFlags::NEVER)
            .union(TypeFlags::UNKNOWN)
            .union(TypeFlags::VOID)
            .union(TypeFlags::UNDEFINED);
        if yield_ty.flags.intersects(TypeFlags::INTRINSIC)
            && return_ty.flags.intersects(NEED_CACHED_TYPE_FLAGS)
            && next_ty.flags.intersects(NEED_CACHED_TYPE_FLAGS)
        {
            let id = create_iteration_tys_key(yield_ty, return_ty, next_ty);
            if let Some(tys) = self.iteration_tys_map.get(&id) {
                return tys;
            }
            let tys = self.alloc(ty::IterationTys {
                yield_ty,
                return_ty,
                next_ty,
            });
            let prev = self.iteration_tys_map.insert(id, tys);
            assert!(prev.is_none());
            tys
        } else {
            self.alloc(ty::IterationTys {
                yield_ty,
                return_ty,
                next_ty,
            })
        }
    }

    fn is_partially_inferable_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        !ty.get_object_flags()
            .intersects(ObjectFlags::NON_INFERRABLE_TYPE)
            || ty.is_object_literal()
                && self.get_props_of_ty(ty).iter().any(|&prop| {
                    let t = self.get_type_of_symbol(prop);
                    self.is_partially_inferable_ty(t)
                })
            || ty.as_tuple().is_some_and(|_| {
                self.get_element_tys(ty)
                    .iter()
                    .any(|t| self.is_partially_inferable_ty(t))
            })
    }

    pub fn create_reverse_mapped_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        constraint: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(target.kind.is_object_mapped());
        debug_assert!(constraint.kind.is_index_ty());
        if !(self.get_index_info_of_ty(source, self.string_ty).is_some()
            || !self.get_props_of_ty(source).is_empty() && self.is_partially_inferable_ty(source))
        {
            None
        } else if source.kind.is_array(self) {
            let first_ty_arg = self.get_ty_arguments(source)[0];
            let element_ty = self.infer_reverse_mapped_ty(first_ty_arg, target, constraint)?;
            let readonly = source.is_readonly_array(self);
            Some(self.create_array_ty(element_ty, readonly))
        } else if let Some(source_tuple) = source.as_tuple() {
            let mut element_tys = vec![];
            for element_ty in self.get_element_tys(source) {
                let Some(t) = self.infer_reverse_mapped_ty(element_ty, target, constraint) else {
                    return None;
                };
                element_tys.push(t);
            }
            let target_mapped_ty = target.kind.as_object_mapped().unwrap();
            let element_flags = if target_mapped_ty
                .decl
                .get_modifiers()
                .contains(ast::MappedTyModifiers::INCLUDE_OPTIONAL)
            {
                let flags = source_tuple
                    .element_flags
                    .iter()
                    .map(|flag| {
                        if flag.contains(ty::ElementFlags::OPTIONAL) {
                            ty::ElementFlags::REQUIRED
                        } else {
                            *flag
                        }
                    })
                    .collect::<Vec<_>>();
                self.alloc(flags)
            } else {
                source_tuple.element_flags
            };
            let element_types = self.alloc(element_tys);
            Some(self.create_tuple_ty(element_types, Some(element_flags), source_tuple.readonly))
        } else {
            let reversed = self.alloc(ty::ReverseMappedTy {
                source,
                mapped_ty: target,
                constraint_ty: constraint,
            });
            Some(self.create_object_ty(
                ty::ObjectTyKind::ReversedMapped(reversed),
                ObjectFlags::REVERSE_MAPPED.union(ObjectFlags::ANONYMOUS),
            ))
        }
    }

    pub(super) fn create_generator_ty(
        &mut self,
        yield_ty: &'cx ty::Ty<'cx>,
        return_ty: &'cx ty::Ty<'cx>,
        next_ty: &'cx ty::Ty<'cx>,
        is_async_generator: bool,
    ) -> &'cx ty::Ty<'cx> {
        if is_async_generator {
            self.create_generator_ty_worker(yield_ty, return_ty, next_ty, AsyncIterationTysResolver)
        } else {
            self.create_generator_ty_worker(yield_ty, return_ty, next_ty, SyncIterationTysResolver)
        }
    }

    fn create_generator_ty_worker(
        &mut self,
        mut yield_ty: &'cx ty::Ty<'cx>,
        mut return_ty: &'cx ty::Ty<'cx>,
        next_ty: &'cx ty::Ty<'cx>,
        resolver: impl IterationTysResolver<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let global_generator_ty = resolver.get_global_generator_ty::<false>(self);
        yield_ty = resolver
            .resolve_iteration_ty(self, yield_ty, None)
            .unwrap_or(self.unknown_ty);
        return_ty = resolver
            .resolve_iteration_ty(self, return_ty, None)
            .unwrap_or(self.unknown_ty);
        if global_generator_ty == self.empty_generic_ty() {
            let global_iterable_iterator_ty =
                resolver.get_global_iterable_iterator_ty::<false>(self);
            if global_iterable_iterator_ty != self.empty_generic_ty() {
                let ty_arguments = self.alloc([yield_ty, return_ty, next_ty]);
                self.create_reference_ty(
                    global_iterable_iterator_ty,
                    Some(ty_arguments),
                    ObjectFlags::empty(),
                )
            } else {
                // TODO: report error
                self.empty_object_ty()
            }
        } else {
            let ty_arguments = self.alloc([yield_ty, return_ty, next_ty]);
            self.create_reference_ty(
                global_generator_ty,
                Some(ty_arguments),
                ObjectFlags::empty(),
            )
        }
    }

    fn create_ty_from_generic_global_ty(
        &mut self,
        generic_global_ty: &'cx ty::Ty<'cx>,
        ty_arguments: ty::Tys<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if generic_global_ty != self.empty_object_ty() {
            self.create_reference_ty(generic_global_ty, Some(ty_arguments), ObjectFlags::empty())
        } else {
            self.empty_object_ty()
        }
    }
}

use bolt_ts_ast as ast;
use bolt_ts_utils::{fx_hashmap_with_capacity, no_hashset_with_capacity};
use rustc_hash::FxHashMap;
use std::hash::Hasher;
use std::ops::Not;

use crate::bind::{Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::check::SymbolLinks;
use crate::check::instantiation_ty_map::{TyCacheTrait, create_iteration_tys_key};
use crate::check::links::TyLinks;
use crate::keyword;
use crate::ty;
use crate::ty::{
    CheckFlags, ElementFlags, IndexFlags, ObjectFlags, TyID, TypeFlags, UnionReduction,
};

use super::relation::RelationKind;
use super::symbol_info::SymbolInfo;
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

    pub(super) fn create_deferred_ty_reference(
        &mut self,
        target: &'cx ty::Ty<'cx>,
        node: ast::NodeID,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: mapper
        let ty = ty::ReferenceTy {
            target,
            node: Some(node),
            mapper,
        };

        (self.create_object_ty(
            ty::ObjectTyKind::Reference(self.alloc(ty)),
            ObjectFlags::REFERENCE,
        )) as _
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
                target.element_flags[i].intersects(ElementFlags::VARIADIC)
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

        let mut add_ele =
            |ty: &'cx ty::Ty<'cx>, flags: ElementFlags, expanded_tys: &mut Vec<&ty::Ty<'cx>>| {
                if flags.intersects(ElementFlags::REQUIRED) {
                    last_required_index = expanded_tys.len();
                }
                if flags.intersects(ElementFlags::REST) && first_rest_index == usize::MAX {
                    first_rest_index = expanded_tys.len()
                }
                if flags.intersects(ElementFlags::OPTIONAL | ElementFlags::REST) {
                    last_optional_or_rest_index = expanded_tys.len();
                }
                if flags.intersects(ElementFlags::OPTIONAL) {
                    // TODO: self.add_optionality
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
                    add_ele(ty, ElementFlags::REST, &mut expanded_tys);
                } else if ty.kind.is_instantiable_non_primitive() || self.is_generic_mapped_ty(ty) {
                    add_ele(ty, ElementFlags::VARIADIC, &mut expanded_tys);
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
                        add_ele(t, tuple.element_flags[n], &mut expanded_tys);
                    }
                } else {
                    let t = if self.is_array_like_ty(ty) {
                        self.get_index_ty_of_ty(ty, self.number_ty)
                            .unwrap_or(self.error_ty)
                    } else {
                        self.error_ty
                    };
                    add_ele(t, ElementFlags::REST, &mut expanded_tys);
                }
            } else {
                add_ele(ty, flag, &mut expanded_tys);
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
        let ty = ty::ReferenceTy {
            target,
            mapper: None,
            node: None,
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
                self.ty_links.insert(
                    ty.id,
                    TyLinks::default().with_resolved_ty_args(resolved_ty_args),
                );
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
        self.create_object_ty(
            ty::ObjectTyKind::Interface(self.alloc(ty)),
            ObjectFlags::empty(),
        )
    }

    pub(super) fn create_anonymous_ty(
        &mut self,
        symbol: Option<SymbolID>,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let links = self.fresh_ty_links_arena.alloc(Default::default());
        let ty = self.alloc(ty::AnonymousTy {
            symbol,
            target: None,
            mapper: None,
            fresh_ty_links: links,
        });

        (self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        )) as _
    }

    pub(super) fn create_anonymous_ty_with_resolved(
        &mut self,
        symbol: Option<SymbolID>,
        object_flags: ObjectFlags,
        members: &'cx FxHashMap<SymbolName, SymbolID>,
        call_sigs: ty::Sigs<'cx>,
        ctor_sigs: ty::Sigs<'cx>,
        index_infos: ty::IndexInfos<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.create_anonymous_ty(symbol, object_flags);
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
    ) -> &'cx ty::Ty<'cx> {
        assert!(target.kind.is_object_anonymous());
        let links = self.fresh_ty_links_arena.alloc(Default::default());
        let ty = self.alloc(ty::AnonymousTy {
            symbol: Some(symbol),
            target: Some(target),
            mapper: Some(mapper),
            fresh_ty_links: links,
        });

        (self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        )) as _
    }

    pub(super) fn create_single_sig_ty(&mut self, ty: ty::SingleSigTy<'cx>) -> &'cx ty::Ty<'cx> {
        (self.create_object_ty(
            ty::ObjectTyKind::SingleSigTy(self.alloc(ty)),
            ObjectFlags::empty(),
        )) as _
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

    fn add_ty_to_union(
        &self,
        set: &mut nohash_hasher::IntSet<TyID>,
        mut includes: TypeFlags,
        ty: &'cx ty::Ty<'cx>,
    ) -> TypeFlags {
        let flags = ty.flags;
        if !flags.intersects(TypeFlags::NEVER) {
            includes |= flags & TypeFlags::INCLUDES_MASK;
            if flags.intersects(TypeFlags::INSTANTIABLE) {
                includes |= TypeFlags::INCLUDES_INSTANTIABLE;
            }
            if ty == self.wildcard_ty {
                includes |= TypeFlags::INCLUDES_WILDCARD;
            }

            if !self.config.strict_null_checks() && flags.intersects(TypeFlags::NULLABLE) {
                if !ty
                    .get_object_flags()
                    .intersects(ObjectFlags::CONTAINS_WIDENING_TYPE)
                {
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
        self.create_reference_ty(
            target,
            Some(self.alloc(vec![element_ty])),
            ObjectFlags::empty(),
        )
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
            ) && includes.intersects(TypeFlags::STRING))
                || (flags.intersects(TypeFlags::NUMBER_LITERAL)
                    && includes.intersects(TypeFlags::NUMBER))
                || (flags.intersects(TypeFlags::BIG_INT_LITERAL)
                    && includes.intersects(TypeFlags::BIG_INT))
                || (flags.intersects(TypeFlags::UNIQUE_ES_SYMBOL)
                    && includes.intersects(TypeFlags::ES_SYMBOL))
                || (reduce_void_undefined
                    && flags.intersects(TypeFlags::UNDEFINED)
                    && includes.intersects(TypeFlags::VOID))
                || self.is_fresh_literal_ty(t) && tys.contains(&self.get_regular_ty(t).unwrap());

            if remove {
                tys.remove(i);
            }
        }

        tys
    }

    pub(super) fn get_union_ty(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        reduction: UnionReduction,
    ) -> &'cx ty::Ty<'cx> {
        if tys.is_empty() {
            return self.never_ty;
        } else if tys.len() == 1 {
            return tys[0];
        }

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
            if includes.intersects(TypeFlags::UNDEFINED)
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
            ) || includes.intersects(TypeFlags::VOID)
                && includes.intersects(TypeFlags::UNDEFINED)
            {
                set = self.remove_redundant_lit_tys(
                    set,
                    includes,
                    reduction == UnionReduction::Subtype,
                );
            }

            if includes.intersects(TypeFlags::STRING_LITERAL)
                && includes.intersects(TypeFlags::TEMPLATE_LITERAL | TypeFlags::STRING_MAPPING)
            {
                // TODO:
            }

            if reduction == UnionReduction::Subtype {
                set = self.remove_subtypes(set);
            }

            if set.is_empty() {
                return if includes.intersects(TypeFlags::NULL) {
                    if includes.intersects(TypeFlags::INCLUDES_NON_WIDENING_TYPE) {
                        self.null_ty
                    } else {
                        // TODO: null_widening_ty
                        self.null_ty
                    }
                } else if includes.intersects(TypeFlags::UNDEFINED) {
                    if includes.intersects(TypeFlags::INCLUDES_NON_WIDENING_TYPE) {
                        self.undefined_ty
                    } else {
                        // TODO: undefined_widening_ty
                        self.undefined_ty
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
        } | if includes.intersects(TypeFlags::INTERSECTION) {
            ObjectFlags::CONTAINS_INTERSECTIONS
        } else {
            ObjectFlags::empty()
        };
        self.get_union_ty_from_sorted_list(set, pre_computed_object_flags)
    }

    pub(super) fn get_union_ty_from_sorted_list(
        &mut self,
        tys: Vec<&'cx ty::Ty<'cx>>,
        pre_computed_object_flags: ObjectFlags,
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
        }
        let fresh_ty_links = self.fresh_ty_links_arena.alloc(Default::default());
        let union = self.alloc(ty::UnionTy {
            tys: self.alloc(tys),
            object_flags,
            fresh_ty_links,
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
        if element_flags.len() == 1 && element_flags[0].intersects(ElementFlags::REST) {
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
                    .map(|i| this.get_number_literal_type(i as f64))
                    .collect::<Vec<_>>();
                this.get_union_ty(&tys, UnionReduction::Lit)
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
            let ty =
                this.create_interface_ty(Symbol::ERR, ty_params, None, ty_params, Some(this_ty));
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
        ty: &'cx ty::Ty<'cx>,
    ) -> TypeFlags {
        let flags = ty.flags;
        if flags.intersects(TypeFlags::INTERSECTION) {
            let ty = ty.kind.expect_intersection();
            return self.add_tys_to_intersection(set, includes, ty.tys);
        }

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
            } else if (self.config.strict_null_checks() || !flags.intersects(TypeFlags::NULLABLE))
                && !set.contains(&ty.id)
            {
                if ty.flags.intersects(TypeFlags::UNIT) && includes.intersects(TypeFlags::UNIT) {
                    includes |= TypeFlags::NON_PRIMITIVE;
                }
                set.insert(ty.id);
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
            includes = self.add_ty_to_intersection(set, includes, ty)
        }
        includes
    }

    fn create_intersection_ty(
        &mut self,
        set: ty::Tys<'cx>,
        flags: ObjectFlags,
        alias_symbol: Option<SymbolID>,
        alias_symbol_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let object_flags =
            flags | ty::Ty::get_propagating_flags_of_tys(set, Some(TypeFlags::NULLABLE));
        let ty = self.alloc(ty::IntersectionTy {
            tys: set,
            object_flags,
            alias_symbol,
            alias_ty_arguments: alias_symbol_ty_args,
        });
        self.new_ty(ty::TyKind::Intersection(ty), TypeFlags::INTERSECTION)
    }

    fn intersect_unions_of_prim_tys(&mut self, tys: &mut Vec<&'cx ty::Ty<'cx>>) -> bool {
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

        tys[index] = self.get_union_ty_from_sorted_list(result, ObjectFlags::PRIMITIVE_UNION);
        true
    }

    fn remove_redundant_super_tys(&mut self, tys: &mut Vec<&'cx ty::Ty<'cx>>, includes: TypeFlags) {
        let mut i = tys.len();
        while i > 0 {
            i -= 1;
            let t = tys[i];
            let remove = t.flags.intersects(TypeFlags::STRING)
                && includes.intersects(
                    TypeFlags::STRING_LITERAL
                        | TypeFlags::TEMPLATE_LITERAL
                        | TypeFlags::STRING_MAPPING,
                )
                || t.flags.intersects(TypeFlags::NUMBER)
                    && includes.intersects(TypeFlags::NUMBER_LITERAL)
                || t.flags.intersects(TypeFlags::BIG_INT)
                    && includes.intersects(TypeFlags::BIG_INT_LITERAL)
                || t.flags.intersects(TypeFlags::ES_SYMBOL)
                    && includes.intersects(TypeFlags::UNIQUE_ES_SYMBOL)
                || t.flags.intersects(TypeFlags::VOID) && includes.intersects(TypeFlags::UNDEFINED)
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
        alias_symbol_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let mut set = indexmap::IndexSet::with_capacity(tys.len());
        let includes = self.add_tys_to_intersection(&mut set, TypeFlags::empty(), tys);
        let mut ty_set = set
            .into_iter()
            .map(|id| self.tys[id.as_usize()])
            .collect::<Vec<_>>();

        if includes.intersects(TypeFlags::NEVER) {
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
            || (includes.intersects(TypeFlags::NON_PRIMITIVE)
                && includes.intersects(
                    TypeFlags::DISJOINT_DOMAINS.intersection(TypeFlags::DISJOINT_DOMAINS.not()),
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
                && includes.intersects(TypeFlags::STRING_LITERAL)
                && self.extract_redundant_template_lits(&mut ty_set)
        {
            return self.never_ty;
        }

        if includes.intersects(TypeFlags::ANY) {
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
            } else if includes.intersects(TypeFlags::UNDEFINED) {
                return self.undefined_ty;
            } else {
                return self.null_ty;
            }
        }
        if (includes.intersects(TypeFlags::STRING)
            && includes.intersects(
                TypeFlags::STRING_LITERAL
                    .union(TypeFlags::TEMPLATE_LITERAL)
                    .union(TypeFlags::STRING_MAPPING),
            )
            || includes.intersects(TypeFlags::NUMBER) // TODO: use `contains`
                && includes.intersects(TypeFlags::NUMBER_LITERAL)
            || includes.intersects(TypeFlags::BIG_INT)
                && includes.intersects(TypeFlags::BIG_INT_LITERAL)
            || includes.intersects(TypeFlags::ES_SYMBOL)
                && includes.intersects(TypeFlags::UNIQUE_ES_SYMBOL)
            || includes.intersects(TypeFlags::VOID) && includes.intersects(TypeFlags::UNDEFINED)
            || includes.intersects(TypeFlags::INCLUDES_EMPTY_OBJECT)
                && includes.intersects(TypeFlags::DEFINITELY_NON_NULLABLE))
            && flags != IntersectionFlags::NoSuperTypeReduction
        {
            self.remove_redundant_super_tys(&mut ty_set, includes);
        }

        if includes.intersects(TypeFlags::INCLUDES_MISSING_TYPE) {
            todo!()
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

        let ty = if includes.intersects(TypeFlags::UNION) {
            if self.intersect_unions_of_prim_tys(&mut ty_set) {
                self.get_intersection_ty(&ty_set, IntersectionFlags::None, None, None)
            } else if ty_set.iter().all(|ty| {
                ty.kind
                    .as_union()
                    .is_some_and(|u| u.tys[0].flags.intersects(TypeFlags::UNDEFINED))
            }) {
                todo!()
            } else if ty_set.iter().all(|ty| {
                ty.kind.as_union().is_some_and(|u| {
                    u.tys[0].flags.intersects(TypeFlags::NULL)
                        || u.tys[1].flags.intersects(TypeFlags::NULL)
                })
            }) {
                todo!()
            } else if ty_set.len() >= 3 && tys.len() > 2 {
                let middle = ty_set.len() / 2;
                let l = self.get_intersection_ty(&ty_set[..middle], flags, None, None);
                let r = self.get_intersection_ty(&ty_set[middle..], flags, None, None);
                self.get_intersection_ty(&[l, r], flags, alias_symbol, alias_symbol_ty_args)
            } else {
                if !Self::check_cross_product_union(&ty_set) {
                    return self.error_ty;
                }
                let constituents = self.get_cross_product_intersections(&ty_set, flags);
                // TODO: origin
                self.get_union_ty(&constituents, ty::UnionReduction::Lit)
            }
        } else {
            let tys = self.alloc(ty_set);
            self.create_intersection_ty(tys, object_flags, alias_symbol, alias_symbol_ty_args)
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
            if !t.flags.intersects(TypeFlags::NEVER) {
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
            || base_ty.flags.intersects(TypeFlags::ANY)
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
        // TODO: cache
        let ty = self.alloc(ty::SubstitutionTy {
            object_flags: ObjectFlags::empty(),
            base_ty,
            constraint,
        });
        self.new_ty(ty::TyKind::Substitution(ty), TypeFlags::SUBSTITUTION)
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
            .intersects(TypeFlags::INSTANTIABLE & !TypeFlags::SUBSTITUTION)
        {
            // TODO: !is_pattern_literal_ty
            false
        } else {
            false
        }
    }

    pub(super) fn create_mapper_ty(
        &mut self,
        symbol: SymbolID,
        decl: &'cx ast::MappedTy<'cx>,
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let links = self.object_mapped_ty_links_arena.alloc(Default::default());
        let ty = self.alloc(ty::MappedTy {
            symbol,
            decl,
            alias_symbol,
            alias_ty_arguments,
            target: None,
            mapper: None,
            links,
        });
        self.create_object_ty(ty::ObjectTyKind::Mapped(ty), ObjectFlags::MAPPED)
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
        texts: &[bolt_ts_atom::AtomId],
        tys: &[&'cx ty::Ty<'cx>],
    ) -> &'cx ty::Ty<'cx> {
        assert_eq!(texts.len(), tys.len() + 1);
        fn add_spans<'cx>(
            this: &mut TyChecker<'cx>,
            texts: &[bolt_ts_atom::AtomId],
            tys: &[&'cx ty::Ty<'cx>],
            new_texts: &mut Vec<bolt_ts_atom::AtomId>,
            new_tys: &mut Vec<&'cx ty::Ty<'cx>>,
            text: &mut String,
        ) -> bool {
            for i in 0..tys.len() {
                let t = tys[i];
                if t.flags
                    .intersects(TypeFlags::LITERAL | TypeFlags::NULL | TypeFlags::UNDEFINED)
                {
                    if let Some(s) = t.kind.as_string_lit() {
                        text.push_str(this.atoms.get(s.val));
                    } else if let Some(n) = t.kind.as_number_lit() {
                        text.push_str(&n.val.to_string());
                    } else if let Some(n) = t.kind.as_bigint_lit() {
                        if n.neg {
                            text.push('-');
                        }
                        let s = this.atoms.get(n.val);
                        text.push_str(s);
                    } else if t
                        .flags
                        .intersects(TypeFlags::BOOLEAN_LITERAL | TypeFlags::NULLABLE)
                    {
                        // TODO:
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
                    let t = std::mem::take(text);
                    let atom = this.atoms.insert_by_str(std::borrow::Cow::Owned(t));
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
            .position(|t| t.flags.intersects(TypeFlags::NEVER | TypeFlags::UNION))
        {
            // TODO: check_cross_product_union
            return self
                .map_ty(
                    tys[union_index],
                    |this, t| {
                        let mut tys = tys.to_vec();
                        tys[union_index] = t;
                        Some(this.get_template_lit_ty(texts, &tys))
                    },
                    false,
                )
                .unwrap();
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

        let text = self.atoms.insert_by_str(std::borrow::Cow::Owned(text));
        if new_tys.is_empty() {
            return self.get_string_literal_type(text);
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
        texts: &'cx [bolt_ts_atom::AtomId],
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
                return self.get_intersection_ty(
                    &[left, right],
                    IntersectionFlags::None,
                    None,
                    None,
                );
            }
        }

        let left_props = self.get_props_of_ty(left);
        let right_props = self.get_props_of_ty(right);
        let mut members = fx_hashmap_with_capacity(left_props.len() + right_props.len());
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
            use std::collections::hash_map::Entry;
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
                                self.get_union_ty(
                                    &[left_ty, right_ty_without_undefined],
                                    ty::UnionReduction::Subtype,
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

        (self.create_anonymous_ty_with_resolved(
            symbol,
            ObjectFlags::OBJECT_LITERAL
                | ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL
                | ObjectFlags::CONTAINS_SPREAD
                | object_flags,
            self.alloc(members),
            self.empty_array(),
            self.empty_array(),
            index_infos,
        )) as _
    }

    pub fn create_iteration_tys(
        &mut self,
        yield_ty: &'cx ty::Ty<'cx>,
        return_ty: &'cx ty::Ty<'cx>,
        next_ty: &'cx ty::Ty<'cx>,
    ) -> ty::IterationTys<'cx> {
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
                return *tys;
            }
            let tys = ty::IterationTys {
                yield_ty,
                return_ty,
                next_ty,
            };
            let prev = self.iteration_tys_map.insert(id, tys);
            assert!(prev.is_none());
            tys
        } else {
            ty::IterationTys {
                yield_ty,
                return_ty,
                next_ty,
            }
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
        constraint_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        assert!(target.kind.is_object_mapped());
        assert!(constraint_ty.kind.is_index_ty());
        if !(self.get_index_info_of_ty(source, self.string_ty).is_some()
            || !self.get_props_of_ty(source).is_empty() && self.is_partially_inferable_ty(source))
        {
            return None;
        };
        if source.kind.is_array(self) {
            todo!()
            // let first_ty_arg = self.get_ty_arguments(source).first();
        } else if source.is_tuple() {
            todo!()
        }
        let reversed = self.alloc(ty::ReverseMappedTy {
            source,
            mapped_ty: target,
            constraint_ty,
        });
        Some(self.create_object_ty(
            ty::ObjectTyKind::ReversedMapped(reversed),
            ObjectFlags::REVERSE_MAPPED.union(ObjectFlags::ANONYMOUS),
        ))
    }
}

use bolt_ts_utils::fx_hashset_with_capacity;
use rustc_hash::{FxHashMap, FxHashSet};
use std::hash::Hasher;

use crate::bind::{Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::check::links::TyLinks;
use crate::check::SymbolLinks;
use crate::keyword;
use crate::ty::{
    self, CheckFlags, ElementFlags, IndexFlags, ObjectFlags, TyID, TypeFlags, UnionReduction,
};

use super::InstantiationTyMap;
use super::{relation::RelationKind, TyChecker};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntersectionFlags {
    None,
    NoSuperTypeReduction,
    NoConstraintReduction,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_ty(&mut self, kind: ty::TyKind<'cx>, flags: TypeFlags) -> &'cx ty::Ty<'cx> {
        Self::make_ty(kind, flags, &mut self.tys, self.arena)
    }

    pub(super) fn make_ty(
        kind: ty::TyKind<'cx>,
        flags: TypeFlags,
        tys: &mut Vec<&'cx ty::Ty<'cx>>,
        arena: &'cx bumpalo::Bump,
    ) -> &'cx ty::Ty<'cx> {
        let id = TyID::new(tys.len() as u32);
        let ty = arena.alloc(ty::Ty::new(id, kind, flags));
        tys.push(ty);
        ty
    }

    fn create_object_ty(
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
            self.create_reference_ty(target, resolved_ty_args, ObjectFlags::empty())
        }
    }

    pub(super) fn create_normalized_tuple_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        element_types: ty::Tys<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let target = ty.kind.expect_object_tuple();
        if !target.combined_flags.intersects(ElementFlags::NON_REQUIRED) {
            return self.create_reference_ty(ty, element_types, ObjectFlags::empty());
        } else if target.combined_flags.intersects(ElementFlags::VARIABLE) {
            // TODO:
        }

        let mut expanded_tys = Vec::with_capacity(element_types.len());
        let mut expanded_flags = Vec::with_capacity(element_types.len());
        let mut last_required_index = usize::MAX;
        let mut first_rest_index = usize::MAX;
        let mut last_optional_or_rest_index = usize::MAX;

        let mut add_ele = |ty: &'cx ty::Ty<'cx>, flags: ElementFlags| {
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
                todo!()
            } else {
                expanded_tys.push(ty);
            }
            expanded_flags.push(flags);
        };

        for (i, ele) in element_types.iter().enumerate() {
            let flag = target.element_flags[i];
            if flag.intersects(ElementFlags::VARIADIC) {
                if ele.flags.intersects(TypeFlags::ANY) {
                    todo!()
                } else if ele.kind.is_instantiable_non_primitive() {
                    add_ele(ele, ElementFlags::VARIADIC);
                } else if ele.kind.is_tuple() {
                    let tuple = ele
                        .kind
                        .expect_object_reference()
                        .target
                        .kind
                        .expect_object_tuple();
                    for i in 0..tuple.resolved_ty_args.len() {
                        add_ele(tuple.resolved_ty_args[i], tuple.element_flags[i]);
                    }
                } else if let Some(tuple) = ele.kind.as_object_tuple() {
                    for i in 0..tuple.resolved_ty_args.len() {
                        add_ele(tuple.resolved_ty_args[i], tuple.element_flags[i]);
                    }
                } else {
                    todo!()
                }
            } else {
                add_ele(ele, flag)
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
            self.create_reference_ty(tuple_target, self.alloc(expanded_tys), ObjectFlags::empty())
        }
    }

    pub(super) fn create_reference_ty(
        &mut self,
        target: &'cx ty::Ty<'cx>,
        resolved_ty_args: ty::Tys<'cx>,
        flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let ty = ty::ReferenceTy {
            target,
            resolved_ty_args,
        };
        if !flags.is_empty() {
            // TODO: delete branch
            let object_flags = flags
                | ObjectFlags::REFERENCE
                | ty::Ty::get_propagating_flags_of_tys(ty.resolved_ty_args, None);
            let ty =
                self.create_object_ty(ty::ObjectTyKind::Reference(self.alloc(ty)), object_flags);
            return ty;
        }

        let id = InstantiationTyMap::create_id(ty.target.id, ty.resolved_ty_args);
        if let Some(res) = self.instantiation_ty_map.get(id) {
            res
        } else {
            let object_flags = flags
                | ObjectFlags::REFERENCE
                | ty::Ty::get_propagating_flags_of_tys(ty.resolved_ty_args, None);
            let ty =
                self.create_object_ty(ty::ObjectTyKind::Reference(self.alloc(ty)), object_flags);
            self.instantiation_ty_map.insert(id, ty);
            ty
        }
    }

    pub(super) fn crate_interface_ty(&mut self, ty: ty::InterfaceTy<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_object_ty(
            ty::ObjectTyKind::Interface(self.alloc(ty)),
            ObjectFlags::empty(),
        )
    }

    pub(super) fn create_anonymous_ty(
        &mut self,
        symbol: SymbolID,
        object_flags: ObjectFlags,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.alloc(ty::AnonymousTy {
            symbol,
            target: None,
            mapper: None,
        });
        let ty = self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        );
        ty
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
        let symbol = symbol.unwrap_or(Symbol::ERR);
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
                base_tys: &[],
                base_ctor_ty: None,
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
        let ty = self.alloc(ty::AnonymousTy {
            symbol,
            target: Some(target),
            mapper: Some(mapper),
        });
        let ty = self.create_object_ty(
            ty::ObjectTyKind::Anonymous(ty),
            object_flags | ObjectFlags::ANONYMOUS,
        );
        ty
    }

    pub(super) fn create_single_sig_ty(&mut self, ty: ty::SingleSigTy<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = self.create_object_ty(
            ty::ObjectTyKind::SingleSigTy(self.alloc(ty)),
            ObjectFlags::empty(),
        );
        ty
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
        offset: usize,
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
        set: &mut FxHashSet<TyID>,
        mut includes: TypeFlags,
        ty: &'cx ty::Ty<'cx>,
    ) -> TypeFlags {
        let flags = ty.flags;
        if !flags.intersects(TypeFlags::NEVER) {
            includes |= flags & TypeFlags::INCLUDES_MASK;
            if flags.intersects(TypeFlags::INSTANTIABLE) {
                includes |= TypeFlags::INSTANTIABLE;
            }
            if ty == self.wildcard_ty {
                includes |= TypeFlags::INCLUDES_WILDCARD;
            }

            if !*self.config.strict_null_checks() && flags.intersects(TypeFlags::NULLABLE) {
            } else {
                set.insert(ty.id);
            }
        }
        includes
    }

    fn add_tys_to_union(
        &self,
        set: &mut FxHashSet<TyID>,
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

    pub(super) fn create_array_ty(&mut self, element_ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.create_reference_ty(
            self.global_array_ty(),
            self.alloc(vec![element_ty]),
            ObjectFlags::empty(),
        )
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

        let mut set = fx_hashset_with_capacity(tys.len());
        let includes = self.add_tys_to_union(&mut set, TypeFlags::empty(), &tys);

        let mut set = {
            let mut set = set
                .into_iter()
                .map(|ty| self.tys[ty.as_usize()])
                .collect::<Vec<_>>();
            set.sort_by(|a, b| a.id.as_u32().cmp(&b.id.as_u32()));
            set
        };

        if reduction != UnionReduction::None {
            if includes.intersects(TypeFlags::ANY_OR_UNKNOWN) {
                return if includes.intersects(TypeFlags::ANY) {
                    self.any_ty
                } else if includes.intersects(TypeFlags::INCLUDES_WILDCARD) {
                    self.wildcard_ty
                } else if includes.intersects(TypeFlags::INCLUDES_ERROR) {
                    self.error_ty
                } else {
                    self.unknown_ty
                };
            }
            if reduction == UnionReduction::Subtype {
                set = self.remove_subtypes(set);
            }

            if set.is_empty() {
                return self.never_ty;
            }
        }

        if set.is_empty() {
            return self.never_ty;
        } else if set.len() == 1 {
            return set[0];
        }
        set.dedup();

        let object_flags = ty::Ty::get_propagating_flags_of_tys(&set, None);
        let union = self.alloc(ty::UnionTy {
            tys: self.alloc(set),
            object_flags,
        });

        self.new_ty(ty::TyKind::Union(union), TypeFlags::UNION)
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
                .filter(|flag| flag.intersects(ElementFlags::REQUIRED | ElementFlags::VARIADIC))
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
                for i in 0..arity {
                    let ty_param = this.create_param_ty(Symbol::ERR, i, false);
                    _ty_params.push(ty_param);
                    let flag = element_flags[i];
                    combined_flags |= flag;
                    if !combined_flags.intersects(ElementFlags::VARIABLE) {
                        let name = SymbolName::EleNum(i.into());
                        let symbol_flags = SymbolFlags::PROPERTY
                            | if flag.intersects(ElementFlags::OPTIONAL) {
                                SymbolFlags::OPTIONAL
                            } else {
                                SymbolFlags::empty()
                            };

                        let property = this.create_transient_symbol(
                            name,
                            symbol_flags,
                            None,
                            SymbolLinks::default()
                                .with_ty(ty_param)
                                .with_check_flags(check_flags),
                        );
                        props.push(property);
                    }
                }
                ty_params = Some(this.alloc(_ty_params));
            }

            let fixed_length = props.len();
            let length_symbol_name = SymbolName::Ele(keyword::IDENT_LENGTH);
            let ty = this.get_number_literal_type(fixed_length as f64);
            // let ty = if combined_flags.intersects(ElementFlags::VARIABLE) {
            //     this.number_ty()
            // } else {
            //     let tys = (min_length..arity)
            //         .into_iter()
            //         .map(|i| this.get_number_literal_type(i as f64))
            //         .collect::<Vec<_>>();
            //     this.create_union_type(tys, UnionReduction::Lit)
            // };
            let length_symbol = this.create_transient_symbol(
                length_symbol_name,
                SymbolFlags::PROPERTY,
                None,
                SymbolLinks::default()
                    .with_ty(ty)
                    .with_check_flags(check_flags),
            );
            props.push(length_symbol);

            let object_flags = ObjectFlags::TUPLE | ObjectFlags::REFERENCE;
            let this_ty = this.create_param_ty(Symbol::ERR, usize::MAX, true);
            let declared_members = this.alloc(ty::DeclaredMembers {
                props: this.alloc(props),
                call_sigs: &[],
                ctor_sigs: &[],
                index_infos: &[],
            });
            let ty = this.crate_interface_ty(ty::InterfaceTy {
                symbol: Symbol::ERR,
                ty_params,
                outer_ty_params: None,
                local_ty_params: ty_params,
                this_ty: Some(this_ty),
                declared_members,
            });
            let tuple = ty::TupleTy {
                ty,
                resolved_ty_args: ty_params.unwrap_or_default(),
                min_length,
                fixed_length,
                element_flags,
                combined_flags,
                readonly,
            };
            let ty =
                this.create_object_ty(ty::ObjectTyKind::Tuple(this.alloc(tuple)), object_flags);
            this.instantiation_ty_map.insert(
                InstantiationTyMap::create_id(ty.id, ty_params.unwrap_or_default()),
                ty,
            );
            ty
        }

        let ty = create_tuple_target_type(self, element_flags, readonly);
        let prev = self.tuple_tys.insert(key, ty);
        assert!(prev.is_none());
        ty
    }

    fn add_ty_to_intersection(
        &self,
        set: &mut FxHashSet<TyID>,
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
            } else if *self.config.strict_null_checks() || !flags.intersects(TypeFlags::NULLABLE) {
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
        &self,
        set: &mut FxHashSet<TyID>,
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

    pub(super) fn get_intersection_ty(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        flags: IntersectionFlags,
        alias_symbol: Option<SymbolID>,
        alias_symbol_ty_args: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let mut set = fx_hashset_with_capacity(tys.len());
        let includes = self.add_tys_to_intersection(&mut set, TypeFlags::empty(), tys);
        let ty_set = set
            .into_iter()
            .map(|id| self.tys[id.as_usize()])
            .collect::<Vec<_>>();

        let object_flags = ObjectFlags::empty();

        if includes.intersects(TypeFlags::NEVER) {
            return self.never_ty;
        }

        let strict_null_checks = *self.config.strict_null_checks();

        if strict_null_checks
            && ((includes.intersects(TypeFlags::NULLABLE)
                && includes.intersects(
                    TypeFlags::OBJECT | TypeFlags::NON_PRIMITIVE | TypeFlags::INCLUDES_EMPTY_OBJECT,
                ))
                || (includes.intersects(TypeFlags::NON_PRIMITIVE)
                    && includes
                        .intersects(TypeFlags::DISJOINT_DOMAINS & !TypeFlags::DISJOINT_DOMAINS)))
            || includes.intersects(TypeFlags::STRING_LIKE)
                && includes.intersects(TypeFlags::DISJOINT_DOMAINS & !TypeFlags::STRING_LIKE)
            || includes.intersects(TypeFlags::NUMBER_LIKE)
                && includes.intersects(TypeFlags::DISJOINT_DOMAINS & !TypeFlags::NUMBER_LIKE)
            || includes.intersects(TypeFlags::BIG_INT_LIKE)
                && includes.intersects(TypeFlags::DISJOINT_DOMAINS & !TypeFlags::BIG_INT_LIKE)
            || includes.intersects(TypeFlags::ES_SYMBOL_LIKE)
                && includes.intersects(TypeFlags::DISJOINT_DOMAINS & !TypeFlags::ES_SYMBOL_LIKE)
            || includes.intersects(TypeFlags::VOID_LIKE)
                && includes.intersects(TypeFlags::DISJOINT_DOMAINS & !TypeFlags::VOID_LIKE)
        {
            return self.never_ty;
        }

        // if includes.intersects(TypeFlags::TEMPLATE_LITERAL | TypeFlags::STRING_MAPPING) && includes.intersects(TypeFlags::STRING_LIKE) &&

        if includes.intersects(TypeFlags::ANY) {
            if includes.intersects(TypeFlags::INCLUDES_WILDCARD) {
                return self.wildcard_ty;
            } else if includes.intersects(TypeFlags::INCLUDES_ERROR) {
                return self.error_ty;
            } else {
                return self.any_ty;
            }
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
        if includes.intersects(TypeFlags::STRING)
            && includes.intersects(
                TypeFlags::STRING_LITERAL | TypeFlags::TEMPLATE_LITERAL | TypeFlags::STRING_MAPPING,
            )
            || includes.intersects(TypeFlags::NUMBER)
                && includes.intersects(TypeFlags::NUMBER_LITERAL)
            || includes.intersects(TypeFlags::BIG_INT)
                && includes.intersects(TypeFlags::BIG_INT_LITERAL)
            || includes.intersects(TypeFlags::ES_SYMBOL)
                && includes.intersects(TypeFlags::UNIQUE_ES_SYMBOL)
            || includes.intersects(TypeFlags::VOID) && includes.intersects(TypeFlags::UNDEFINED)
            || includes.intersects(TypeFlags::INCLUDES_EMPTY_OBJECT)
                && includes.intersects(TypeFlags::DEFINITELY_NON_NULLABLE)
        {
            if flags != IntersectionFlags::NoSuperTypeReduction {
                todo!()
            };
        }

        if includes.intersects(TypeFlags::INCLUDES_MISSING_TYPE) {
            todo!()
        }

        if ty_set.is_empty() {
            return self.unknown_ty;
        } else if ty_set.len() == 1 {
            return ty_set[0];
        }

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
                    .intersects(TypeFlags::PRIMITIVE | TypeFlags::NON_PRIMITIVE))
            {
                todo!()
            }
        }

        if includes.intersects(TypeFlags::UNION) {
            todo!()
        } else {
            let tys = self.alloc(ty_set);
            self.create_intersection_ty(tys, object_flags, alias_symbol, alias_symbol_ty_args)
        }
    }
}

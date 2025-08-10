use bolt_ts_ast::{self as ast, ModifierKind};
use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;
use bolt_ts_utils::{fx_hashset_with_capacity, fx_indexmap_with_capacity};
use rustc_hash::FxHashSet;

use super::create_ty::IntersectionFlags;
use super::symbol_info::SymbolInfo;
use super::{SymbolLinks, errors};
use crate::ty::{self, CheckFlags, ObjectFlags, TypeFlags};
use crate::ty::{ObjectTy, ObjectTyKind, Ty, TyKind};
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID, SymbolName};

use super::{Ternary, TyChecker};

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub(super) enum RelationKind {
    Subtype,
    StrictSubtype,
    Assignable,
    Comparable,
    Identity,
    Enum,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(super) struct SigCheckMode: u8 {
        const BIVARIANT_CALLBACK    = 1 << 0;
        const STRICT_CALLBACK       = 1 << 1;
        const IGNORE_RETURN_TYPES   = 1 << 2;
        const STRICT_ARITY          = 1 << 3;
        const STRICT_TOP_SIGNATURE  = 1 << 4;
        const CALLBACK              = Self::BIVARIANT_CALLBACK.bits() | Self::STRICT_CALLBACK.bits();
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn is_excess_property_check_target(ty: &'cx Ty<'cx>) -> bool {
        if ty.kind.is_object() {
            let flags = ty.get_object_flags();
            !flags.intersects(ObjectFlags::OBJECT_LITERAL_PATTERN_WITH_COMPUTED_PROPERTIES)
        } else if ty.flags.intersects(TypeFlags::NON_PRIMITIVE) {
            true
        } else if let Some(s) = ty.kind.as_substitution_ty() {
            Self::is_excess_property_check_target(s.base_ty)
        } else if let Some(union) = ty.kind.as_union() {
            union
                .tys
                .iter()
                .any(|t| Self::is_excess_property_check_target(t))
        } else if let Some(i) = ty.kind.as_intersection() {
            i.tys
                .iter()
                .all(|t| Self::is_excess_property_check_target(t))
        } else {
            false
        }
    }

    pub(super) fn is_simple_type_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        use RelationKind::*;
        let s = source.flags;
        let t = target.flags;
        let strict_null_checks = self.config.strict_null_checks();
        let is_unknown_like_union_ty = |this: &mut Self| {
            strict_null_checks
                && target.kind.as_union().is_some_and(|u| {
                    // TODO: cache
                    u.tys.len() >= 3
                        && u.tys[0].flags.intersects(TypeFlags::UNDEFINED)
                        && u.tys[1].flags.intersects(TypeFlags::NULL)
                        && u.tys.iter().any(|ty| this.is_empty_anonymous_object_ty(ty))
                })
        };
        if t.intersects(TypeFlags::ANY)
            || s.intersects(TypeFlags::NEVER)
            || source == self.wildcard_ty
            || (t.intersects(TypeFlags::UNKNOWN)
                && !(relation != StrictSubtype && s.intersects(TypeFlags::ANY)))
        {
            true
        } else if t.intersects(TypeFlags::NEVER) {
            false
        } else if (s.intersects(TypeFlags::NUMBER_LIKE) && t.intersects(TypeFlags::NUMBER))
            || (s.intersects(TypeFlags::STRING_LIKE) && t.intersects(TypeFlags::STRING))
            || (s.intersects(TypeFlags::BIG_INT_LIKE) && t.intersects(TypeFlags::BIG_INT))
            || (s.intersects(TypeFlags::BOOLEAN_LIKE) && t.intersects(TypeFlags::BOOLEAN))
            || (s.intersects(TypeFlags::UNDEFINED)
                && (!strict_null_checks && !t.intersects(TypeFlags::UNION_OR_INTERSECTION)
                    || t.intersects(TypeFlags::UNDEFINED.union(TypeFlags::VOID))))
            || (s.intersects(TypeFlags::NULL)
                && (t.intersects(TypeFlags::NULL)
                    || (!strict_null_checks && !t.intersects(TypeFlags::UNION_OR_INTERSECTION))))
            || (s.intersects(TypeFlags::OBJECT)
                && t.intersects(TypeFlags::NON_PRIMITIVE)
                && !(relation == StrictSubtype
                    && self.is_empty_anonymous_object_ty(source)
                    && !source
                        .get_object_flags()
                        .intersects(ObjectFlags::FRESH_LITERAL)))
        {
            true
        } else if matches!(relation, Assignable | Comparable) {
            s.intersects(TypeFlags::ANY)
                || (s.intersects(TypeFlags::NUMBER)
                    && (t.intersects(TypeFlags::ENUM)
                        || t.contains(TypeFlags::NUMBER_LITERAL.union(TypeFlags::ENUM_LITERAL))))
                || (s.intersection(TypeFlags::NUMBER_LITERAL.union(TypeFlags::ENUM_LITERAL))
                    == TypeFlags::NUMBER_LITERAL
                    && (t.intersects(TypeFlags::ENUM)
                        || t.contains(TypeFlags::NUMBER_LITERAL.union(TypeFlags::ENUM_LITERAL))
                            && source.kind.as_number_lit().is_some_and(|s| {
                                target.kind.as_number_lit().is_some_and(|t| s.val == t.val)
                            })))
                || is_unknown_like_union_ty(self)
        } else {
            false
        }
    }

    pub(super) fn compare_props(
        &mut self,
        source: SymbolID,
        target: SymbolID,
        compare: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>, bool) -> Ternary + Copy,
    ) -> Ternary {
        if source == target {
            return Ternary::TRUE;
        }
        let source_prop_access = self.decl_modifier_flags_from_symbol(source)
            & ast::ModifierKind::NON_PUBLIC_ACCESSIBILITY_MODIFIER;
        let target_prop_access = self.decl_modifier_flags_from_symbol(target)
            & ast::ModifierKind::NON_PUBLIC_ACCESSIBILITY_MODIFIER;

        if source_prop_access != target_prop_access {
            return Ternary::FALSE;
        }

        if self.is_readonly_symbol(source) != self.is_readonly_symbol(target) {
            return Ternary::FALSE;
        }
        let s = self.get_type_of_symbol(source);
        let t = self.get_type_of_symbol(target);
        compare(self, s, t, false)
    }

    pub(super) fn is_prop_identical_to(
        &mut self,
        source_prop: SymbolID,
        target_prop: SymbolID,
    ) -> bool {
        self.compare_props(source_prop, target_prop, |this, s, t, _| {
            if this.is_type_related_to(s, t, RelationKind::Identity) {
                Ternary::TRUE
            } else {
                Ternary::FALSE
            }
        }) != Ternary::FALSE
    }

    pub(super) fn is_type_related_to(
        &mut self,
        mut source: &'cx Ty<'cx>,
        mut target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        if self.is_fresh_literal_ty(source) {
            source = self.get_regular_ty(source).unwrap();
        }
        if self.is_fresh_literal_ty(target) {
            target = self.get_regular_ty(target).unwrap();
        }
        if source == target {
            return true;
        }
        if relation != RelationKind::Identity {
            if (relation == RelationKind::Comparable
                && !target.flags.intersects(TypeFlags::NEVER)
                && self.is_simple_type_related_to(target, source, relation))
                || self.is_simple_type_related_to(source, target, relation)
            {
                return true;
            }
        } else if !(source.flags | target.flags).intersects(
            TypeFlags::UNION_OR_INTERSECTION
                .union(TypeFlags::INDEXED_ACCESS)
                .union(TypeFlags::CONDITIONAL)
                .union(TypeFlags::SUBSTITUTION),
        ) {
            if source.flags != target.flags {
                return false;
            }
            if source.flags.intersects(TypeFlags::SINGLETON) {
                return true;
            }
        }

        if source.kind.is_object() && target.kind.is_object() {
            // TODO: cache
        }

        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            self.check_type_related_to(source, target, relation, None)
        } else {
            false
        }
    }

    pub(super) fn check_type_assignable_to_and_optionally_elaborate(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        error_node: Option<ast::NodeID>,
        expr: Option<ast::NodeID>,
    ) -> Ternary {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            RelationKind::Assignable,
            error_node,
            expr,
            |this, span, source, target| {
                let source = if (source.kind.is_number_lit() && target.kind.is_number_lit())
                    || (source.kind.is_string_lit() && target.kind.is_string_lit())
                {
                    source
                } else {
                    this.get_base_ty_of_literal_ty(source)
                };
                Box::new(errors::TypeIsNotAssignableToType {
                    span,
                    ty1: this.print_ty(source).to_string(),
                    ty2: this.print_ty(target).to_string(),
                })
            },
        )
    }

    pub(super) fn check_type_related_to_and_optionally_elaborate(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        error_node: Option<ast::NodeID>,
        expr: Option<ast::NodeID>,
        error: impl FnOnce(&mut Self, Span, &'cx Ty<'cx>, &'cx Ty<'cx>) -> bolt_ts_middle::Diag,
    ) -> Ternary {
        if self.is_type_related_to(source, target, relation) {
            return Ternary::TRUE;
        }
        if error_node.is_none() || !self.elaborate_error(expr, source, target, relation, error_node)
        {
            if !self.check_type_related_to(source, target, relation, error_node) {
                if error_node.is_none() {
                    return Ternary::FALSE;
                }
                let span = self.p.node(error_node.unwrap()).span();
                let error = error(self, span, source, target);
                self.push_error(error);
                return Ternary::FALSE;
            } else {
                return Ternary::TRUE;
            }
        }

        Ternary::FALSE
    }

    pub(super) fn check_type_assignable_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> bool {
        self.check_type_related_to(source, target, RelationKind::Assignable, error_node)
    }

    pub(super) fn get_props_of_object_ty(&mut self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        if ty.kind.is_object() {
            self.resolve_structured_type_members(ty);
            self.properties_of_object_type(ty)
        } else {
            self.empty_array()
        }
    }

    pub(super) fn get_props_of_ty(&mut self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        let ty = self.get_reduced_apparent_ty(ty);
        if ty.kind.is_union_or_intersection() {
            self.get_props_of_union_or_intersection(ty)
        } else {
            self.get_props_of_object_ty(ty)
        }
    }

    pub(super) fn get_props_of_union_or_intersection(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx [SymbolID] {
        if let Some(props) = self.get_ty_links(ty.id).get_resolved_properties() {
            return props;
        }
        let Some(tys) = ty.kind.tys_of_union_or_intersection() else {
            unreachable!()
        };
        let mut members = fx_indexmap_with_capacity(64);
        for current in tys {
            for prop in self.get_props_of_ty(current) {
                let name = self.symbol(*prop).name;
                if let indexmap::map::Entry::Vacant(vac) = members.entry(name)
                    && let Some(combined_prop) = self.get_prop_of_union_or_intersection_ty(ty, name)
                    {
                        vac.insert(combined_prop);
                    }
            }
        }
        let props = members.into_values().collect::<Vec<_>>();
        let props = self.alloc(props);
        if self.get_ty_links(ty.id).get_resolved_properties().is_none() {
            self.get_mut_ty_links(ty.id).set_resolved_properties(props);
        } else {
            // cycle
            self.get_mut_ty_links(ty.id)
                .override_resolved_properties(props);
        }
        props
    }

    pub(super) fn get_prop_of_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let ty = self.get_reduced_apparent_ty(ty);
        if let TyKind::Object(_) = ty.kind {
            self.resolve_structured_type_members(ty);
            let symbol = self
                .expect_ty_links(ty.id)
                .expect_structured_members()
                .members
                .get(&name)
                .copied();
            if symbol.is_some() {
                return symbol;
            }

            let fn_ty = if ty.id == self.any_fn_ty().id {
                Some(self.global_array_ty())
            } else if self
                .get_ty_links(ty.id)
                .get_structured_members()
                .map(|item| !item.call_sigs.is_empty())
                .unwrap_or_default()
            {
                Some(self.global_callable_fn_ty())
            } else if self
                .get_ty_links(ty.id)
                .get_structured_members()
                .map(|item| !item.ctor_sigs.is_empty())
                .unwrap_or_default()
            {
                Some(self.global_newable_fn_ty())
            } else {
                None
            };

            if let Some(fn_ty) = fn_ty
                && let Some(symbol) = self.get_prop_of_object_ty(fn_ty, name) {
                    return Some(symbol);
                }

            self.get_prop_of_object_ty(self.global_object_ty(), name)
        } else if ty.kind.as_intersection().is_some() {
            if let Some(prop) = self.get_prop_of_union_or_intersection_ty(ty, name) {
                Some(prop)
            } else {
                None
            }
        } else if ty.kind.as_union().is_some() {
            self.get_prop_of_union_or_intersection_ty(ty, name)
        } else {
            None
        }
    }

    fn get_prop_of_union_or_intersection_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        self.get_union_or_intersection_prop(ty, name)
            .and_then(|prop| {
                if !self
                    .get_check_flags(prop)
                    .intersects(CheckFlags::READ_PARTIAL)
                {
                    Some(prop)
                } else {
                    None
                }
            })
    }

    fn get_union_or_intersection_prop(
        &mut self,
        containing_ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        // TODO: cache
        self.create_union_or_intersection_prop(containing_ty, name)
    }

    fn create_union_or_intersection_prop(
        &mut self,
        containing_ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let tys = containing_ty.kind.tys_of_union_or_intersection().unwrap();
        let is_union = containing_ty.kind.is_union();

        let mut optional_flag = None;

        let mut single_prop = None;
        let mut prop_set: Option<FxHashSet<_>> = None;
        let index_tys: Option<Vec<Ty<'cx>>> = None;

        let mut check_flags = if is_union {
            CheckFlags::empty()
        } else {
            CheckFlags::READONLY
        };

        for current in tys {
            let ty = self.get_apparent_ty(current);
            if self.is_error(ty) || ty.flags.intersects(TypeFlags::NEVER) {
                continue;
            }
            if let Some(prop) = self.get_prop_of_ty(ty, name) {
                let modifiers = self.get_declaration_modifier_flags_from_symbol(prop, None);
                let symbol_flags = self.symbol(prop).flags;
                if symbol_flags.intersects(SymbolFlags::CLASS_MEMBER) {
                    if optional_flag.is_none() {
                        optional_flag = Some(if is_union {
                            SymbolFlags::empty()
                        } else {
                            SymbolFlags::OPTIONAL
                        });
                    }
                    if is_union {
                        let flags = optional_flag.as_mut().unwrap();
                        *flags |= symbol_flags & SymbolFlags::OPTIONAL;
                    } else {
                        let flags = optional_flag.as_mut().unwrap();
                        *flags &= symbol_flags;
                    }
                }
                if let Some(single_prop) = single_prop {
                    if single_prop != prop {
                        if let Some(prop_set) = &mut prop_set {
                            prop_set.insert(prop);
                        } else {
                            let mut t = fx_hashset_with_capacity(tys.len());
                            t.insert(single_prop);
                            t.insert(prop);
                            prop_set = Some(t);
                        }
                    }
                } else {
                    single_prop = Some(prop);
                }

                match (is_union, self.is_readonly_symbol(prop)) {
                    (true, true) => check_flags |= CheckFlags::READONLY,
                    (false, false) => check_flags &= !CheckFlags::READONLY,
                    _ => (),
                };
                check_flags |=
                    if !modifiers.intersects(ModifierKind::NON_PUBLIC_ACCESSIBILITY_MODIFIER) {
                        CheckFlags::CONTAINS_PUBLIC
                    } else {
                        CheckFlags::empty()
                    } | if modifiers.intersects(ModifierKind::Protected) {
                        CheckFlags::CONTAINS_PROTECTED
                    } else {
                        CheckFlags::empty()
                    } | if modifiers.intersects(ModifierKind::Private) {
                        CheckFlags::CONTAINS_PRIVATE
                    } else {
                        CheckFlags::empty()
                    } | if modifiers.intersects(ModifierKind::Static) {
                        CheckFlags::CONTAINS_STATIC
                    } else {
                        CheckFlags::empty()
                    };
                // TODO: !is_prototype_prop
            } else if is_union {
                // TODO:
            }
        }
        let single_prop = single_prop?;

        if prop_set.is_none()
            && !check_flags.intersects(CheckFlags::READ_PARTIAL)
            && index_tys.is_none()
        {
            return Some(single_prop);
        }

        let props = prop_set
            .map(|set| set.into_iter().collect())
            .unwrap_or_else(|| vec![single_prop]);

        let mut first_ty = None;
        let mut name_ty = None;
        let mut prop_tys = Vec::with_capacity(props.len());
        let mut write_tys: Option<Vec<&'cx Ty<'cx>>> = None;

        for prop in props {
            let ty = self.get_type_of_symbol(prop);
            if first_ty.is_none() {
                first_ty = Some(ty);
                name_ty = self.get_symbol_links(prop).get_name_ty();
            }
            let write_ty = self.get_write_type_of_symbol(prop);
            if write_tys.is_some() || write_ty != ty {
                if let Some(write_tys) = &mut write_tys {
                    write_tys.push(write_ty);
                } else {
                    let mut t = prop_tys.clone();
                    t.push(write_ty);
                    write_tys = Some(t);
                }
            }
            // if first_ty.map_or(true, |first_ty| first_ty != ty)
            // {}
            prop_tys.push(ty);
        }

        let symbol_flags = SymbolFlags::PROPERTY.union(SymbolFlags::TRANSIENT)
            | optional_flag.unwrap_or(SymbolFlags::empty());
        let links = SymbolLinks::default()
            .with_containing_ty(containing_ty)
            .with_check_flags(CheckFlags::empty());
        let mut links = if let Some(name_ty) = name_ty {
            links.with_name_ty(name_ty)
        } else {
            links
        };
        let links = if prop_tys.len() > 2 {
            let prop_tys = self.alloc(prop_tys);
            links.config_check_flags(|config| config | CheckFlags::DEFERRED_TYPE);
            let links = links
                .with_deferral_parent(containing_ty)
                .with_deferral_constituents(prop_tys);
            if let Some(write_tys) = write_tys {
                links.with_deferral_constituents(self.alloc(write_tys))
            } else {
                links
            }
        } else {
            let ty = if is_union {
                self.get_union_ty(&prop_tys, ty::UnionReduction::Lit)
            } else {
                self.get_intersection_ty(&prop_tys, IntersectionFlags::None, None, None)
            };
            links.with_ty(ty)
        };

        // TODO: declarations
        let result =
            self.create_transient_symbol(name, symbol_flags, links, Default::default(), None);

        Some(result)
    }

    pub(super) fn get_prop_of_object_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let TyKind::Object(_) = ty.kind else {
            return None;
        };
        self.resolve_structured_type_members(ty);
        let symbol = self
            .expect_ty_links(ty.id)
            .expect_structured_members()
            .members
            .get(&name)
            .copied()?;
        self.symbol_is_value(symbol, false).then_some(symbol)
    }

    pub(super) fn symbol_is_value(
        &mut self,
        symbol: SymbolID,
        include_ty_only_members: bool,
    ) -> bool {
        let s = self.symbol(symbol).flags;
        s.intersects(SymbolFlags::VALUE)
            || (s.intersects(SymbolFlags::ALIAS)
                && self
                    .get_symbol_flags(symbol, !include_ty_only_members)
                    .intersects(SymbolFlags::VALUE))
    }

    pub fn get_unmatched_prop(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        require_optional_properties: bool,
    ) -> Option<(Vec<AtomId>, SymbolID)> {
        self.get_unmatched_props(source, target, require_optional_properties)
    }

    fn get_unmatched_props(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        require_optional_properties: bool,
    ) -> Option<(Vec<AtomId>, SymbolID)> {
        let properties = self.get_props_of_ty(target);
        let mut unmatched = Vec::with_capacity(properties.len());
        for target_prop in properties {
            let s = self.symbol(*target_prop);
            if require_optional_properties
                || !(s.flags.intersects(SymbolFlags::OPTIONAL)
                    || self
                        .get_check_flags(*target_prop)
                        .intersects(CheckFlags::PARTIAL))
            {
                let target_prop_name = s.name;
                let Some(source_prop) = self.get_prop_of_ty(source, target_prop_name) else {
                    if let Some(target_prop_name) = target_prop_name.as_atom() {
                        unmatched.push(target_prop_name);
                    }
                    continue;
                };
            }
        }
        if unmatched.is_empty() {
            None
        } else {
            let target = self.get_reduced_apparent_ty(target);
            let ty = target.kind.expect_object();
            fn recur(ty: &ObjectTy) -> SymbolID {
                match ty.kind {
                    ObjectTyKind::Reference(ty) => recur(ty.target.kind.expect_object()),
                    ObjectTyKind::Interface(ty) => ty.symbol,
                    ObjectTyKind::Anonymous(ty) => ty.symbol.unwrap(),
                    ObjectTyKind::Mapped(ty) => ty.symbol,
                    ObjectTyKind::Tuple(_) => Symbol::ERR,
                    _ => unreachable!("{ty:#?}"),
                }
            }
            let symbol = recur(ty);
            Some((unmatched, symbol))
        }
    }

    pub(super) fn is_type_assignable_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> bool {
        self.is_type_related_to(source, target, RelationKind::Assignable)
    }
}

use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;

use bolt_ts_utils::fx_hashset_with_capacity;

use super::create_ty::IntersectionFlags;
use super::{SymbolLinks, errors};
use crate::bind::{Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::ty::{self, CheckFlags, ObjectFlags, TypeFlags};
use crate::ty::{ObjectShape, ObjectTy, ObjectTyKind, Ty, TyKind};
use bolt_ts_ast as ast;

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
    pub(super) fn is_excess_property_check_target(&self, target: &'cx Ty<'cx>) -> bool {
        if target.kind.as_object().is_some() {
            // TODO: exclude literal computed name
            true
        } else if let Some(union) = target.kind.as_union() {
            union
                .tys
                .iter()
                .any(|ty| self.is_excess_property_check_target(ty))
        } else {
            false
        }
    }

    pub(super) fn is_simple_type_related_to(
        &self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        let s = source.flags;
        let t = target.flags;
        let strict_null_checks = *self.config.strict_null_checks();
        if t.intersects(TypeFlags::ANY) || s.intersects(TypeFlags::NEVER) {
            true
        } else if t.intersects(TypeFlags::UNKNOWN)
            && !(relation != RelationKind::StrictSubtype && s.intersects(TypeFlags::ANY))
        {
            true
        } else if t.intersects(TypeFlags::NEVER) {
            false
        } else if s.intersects(TypeFlags::NUMBER_LIKE) && t.intersects(TypeFlags::NUMBER) {
            true
        } else if s.intersects(TypeFlags::STRING_LIKE) && t.intersects(TypeFlags::STRING) {
            true
        } else if s.intersects(TypeFlags::BOOLEAN_LIKE) && t.intersects(TypeFlags::BOOLEAN) {
            true
        } else if s.intersects(TypeFlags::UNDEFINED)
            && ((!strict_null_checks && !target.kind.is_union_or_intersection())
                || t.intersects(TypeFlags::UNDEFINED | TypeFlags::VOID))
        {
            true
        } else if s.intersects(TypeFlags::NULL)
            && (t.intersects(TypeFlags::NULL)
                || (!strict_null_checks && !target.kind.is_union_or_intersection()))
        {
            true
        } else if source.kind.is_object()
            && t.intersects(TypeFlags::NON_PRIMITIVE)
            && !(relation == RelationKind::StrictSubtype
                && self.is_empty_anonymous_object_ty(source)
                && !(source
                    .get_object_flags()
                    .intersects(ObjectFlags::FRESH_LITERAL)))
        {
            true
        } else if matches!(
            relation,
            RelationKind::Assignable | RelationKind::Comparable
        ) {
            if s.intersects(TypeFlags::ANY) {
                true
            } else {
                // TODO:
                false
            }
        } else {
            false
        }
    }

    pub(super) fn is_type_related_to(
        &mut self,
        mut source: &'cx Ty<'cx>,
        mut target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        if self.is_fresh_literal_ty(source) {
            source = self.ty_links[&source.id].get_regular_ty().unwrap();
        }
        if self.is_fresh_literal_ty(target) {
            target = self.ty_links[&target.id].get_regular_ty().unwrap();
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
                | TypeFlags::INDEXED_ACCESS
                | TypeFlags::CONDITIONAL
                | TypeFlags::SUBSTITUTION,
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
            self.check_type_related_to(source, target, relation, None) != Ternary::FALSE
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
        error: impl FnOnce(&mut Self, Span, &'cx Ty<'cx>, &'cx Ty<'cx>) -> crate::Diag,
    ) -> Ternary {
        if self.is_type_related_to(source, target, relation) {
            return Ternary::TRUE;
        }
        if error_node.is_none() || !self.elaborate_error(expr, source, target, relation, error_node)
        {
            if self.check_type_related_to(source, target, relation, error_node) == Ternary::FALSE {
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
    ) -> Ternary {
        self.check_type_related_to(source, target, RelationKind::Assignable, error_node)
    }

    pub(super) fn get_props_of_object_ty(&mut self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        if ty.kind.is_object() {
            self.resolve_structured_type_members(ty);
            self.properties_of_object_type(ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_props_of_ty(&mut self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        if ty.kind.is_object() {
            self.get_props_of_object_ty(ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_prop_of_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        let ty = self.get_reduced_apparent_ty(ty);
        if let TyKind::Object(object_ty) = ty.kind {
            self.resolve_structured_type_members(ty);

            let symbol = if object_ty.kind.is_interface()
                || object_ty.kind.is_anonymous()
                || object_ty.kind.is_reference()
                || object_ty.kind.is_mapped()
            {
                self.expect_ty_links(ty.id)
                    .expect_structured_members()
                    .members
                    .get(&name)
                    .copied()
            } else if let Some(ty) = object_ty.kind.as_tuple() {
                ObjectShape::get_member(ty, &name)
            } else {
                unreachable!("ty: {ty:#?}")
            };

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

            if let Some(fn_ty) = fn_ty {
                if let Some(symbol) = self.get_prop_of_object_ty(fn_ty, name) {
                    return Some(symbol);
                }
            }

            self.get_prop_of_object_ty(self.global_object_ty(), name)
        } else if ty.kind.as_union().is_some() {
            self.get_prop_of_union_or_intersection_ty(ty, name)
        } else if ty.kind.as_intersection().is_some() {
            if let Some(prop) = self.get_prop_of_union_or_intersection_ty(ty, name) {
                return Some(prop);
            } else {
                None
            }
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
        let (is_union, tys) = if let Some(union) = containing_ty.kind.as_union() {
            (true, union.tys)
        } else if let Some(i) = containing_ty.kind.as_intersection() {
            (false, i.tys)
        } else {
            unreachable!("containing_ty: {containing_ty:#?}")
        };

        let mut single_prop = None;
        let mut prop_set = fx_hashset_with_capacity(tys.len());

        let check_flags = if is_union {
            CheckFlags::empty()
        } else {
            CheckFlags::READONLY
        };

        for current in tys {
            let ty = self.get_apparent_ty(current);
            if ty != self.error_ty && ty != self.never_ty {
                if let Some(prop) = self.get_prop_of_ty(ty, name) {
                    if let Some(single_prop) = single_prop {
                        if single_prop != prop {
                            if prop_set.is_empty() {
                                prop_set.insert(single_prop);
                            }
                            prop_set.insert(prop);
                        }
                    } else {
                        single_prop = Some(prop);
                    }
                }
            }
        }

        let props = if prop_set.is_empty() {
            if let Some(single_prop) = single_prop {
                vec![single_prop]
            } else {
                vec![]
            }
        } else {
            prop_set.into_iter().collect::<Vec<_>>()
        };

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

        let symbol_flags = SymbolFlags::PROPERTY;
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

        let result = self.create_transient_symbol(name, symbol_flags, None, links);

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
        self.expect_ty_links(ty.id)
            .expect_structured_members()
            .members
            .get(&name)
            .copied()
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
                || !(s.flags().intersects(SymbolFlags::OPTIONAL)
                    || self
                        .get_check_flags(*target_prop)
                        .intersects(CheckFlags::PARTIAL))
            {
                let target_prop_name = s.name();
                let Some(source_prop) = self.get_prop_of_ty(source, target_prop_name) else {
                    unmatched.push(target_prop_name.expect_atom());
                    continue;
                };
            }
        }
        if unmatched.is_empty() {
            None
        } else {
            let ty = target.kind.expect_object();
            fn recur(ty: &ObjectTy) -> SymbolID {
                match ty.kind {
                    ObjectTyKind::Reference(ty) => recur(ty.target.kind.expect_object()),
                    ObjectTyKind::Interface(ty) => ty.symbol,
                    ObjectTyKind::Anonymous(ty) => ty.symbol,
                    ObjectTyKind::Mapped(ty) => ty.symbol,
                    ObjectTyKind::Tuple(ty) => Symbol::ERR,
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

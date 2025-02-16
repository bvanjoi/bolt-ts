use bolt_ts_utils::fx_hashset_with_capacity;
use rustc_hash::FxHashSet;

use super::errors;
use super::get_variances::VarianceFlags;
use super::relation::{RelationKind, SigCheckMode};
use super::utils::contains_ty;
use crate::ast;
use crate::bind::{SymbolFlags, SymbolID};
use crate::keyword::IDENT_LENGTH;
use crate::ty::{self, ElementFlags, ObjectFlags, Sig, SigFlags, SigKind, TypeFlags};
use crate::ty::{Ty, TyKind};

use super::{Ternary, TyChecker};

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    struct RecursionFlags: u8 {
        const SOURCE = 1 << 0;
        const TARGET = 1 << 1;
        const BOTH   = Self::SOURCE.bits() | Self::TARGET.bits();
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    struct IntersectionState: u8 {
        const SOURCE = 1 << 0;
        const TARGET = 1 << 1;
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_type_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        error_node: Option<ast::NodeID>,
    ) -> Ternary {
        let mut c = TypeRelatedChecker::new(self, relation, error_node);
        c.is_related_to(
            source,
            target,
            RecursionFlags::BOTH,
            error_node.is_some(),
            IntersectionState::empty(),
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RelationKey {
    source: ty::TyID,
    target: ty::TyID,
    relation: RelationKind,
}

pub(super) struct TypeRelatedChecker<'cx, 'checker> {
    pub(super) c: &'checker mut TyChecker<'cx>,
    relation: RelationKind,
    maybe_keys_set: FxHashSet<RelationKey>,
    expanding_flags: RecursionFlags,
    source_stack: Vec<&'cx ty::Ty<'cx>>,
    target_stack: Vec<&'cx ty::Ty<'cx>>,
    error_node: Option<ast::NodeID>,
}

impl<'cx, 'checker> TypeRelatedChecker<'cx, 'checker> {
    pub(super) fn new(
        c: &'checker mut TyChecker<'cx>,
        relation: RelationKind,
        error_node: Option<ast::NodeID>,
    ) -> Self {
        TypeRelatedChecker {
            c,
            relation,
            maybe_keys_set: fx_hashset_with_capacity(32),
            expanding_flags: RecursionFlags::empty(),
            source_stack: Vec::with_capacity(32),
            target_stack: Vec::with_capacity(32),
            error_node,
        }
    }

    fn is_related_to(
        &mut self,
        original_source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        recursion_flags: RecursionFlags,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if original_source.id == target.id {
            return Ternary::TRUE;
        } else if original_source.kind.is_object() && target.flags.intersects(TypeFlags::PRIMITIVE)
        {
            if self.relation == RelationKind::Comparable
                && !target.flags.intersects(TypeFlags::NEVER)
                && (self
                    .c
                    .is_simple_type_related_to(target, original_source, self.relation)
                    || self
                        .c
                        .is_simple_type_related_to(original_source, target, self.relation))
            {
                return Ternary::TRUE;
            } else {
                return Ternary::FALSE;
            }
        }

        let source = self.c.get_normalized_ty(original_source, false);
        let target = self.c.get_normalized_ty(target, true);

        if self.relation == RelationKind::Identity {
            return if source.flags != target.flags {
                Ternary::FALSE
            } else if source.flags.intersects(TypeFlags::SINGLETON) {
                Ternary::TRUE
            } else {
                self.recur_ty_related_to(
                    source,
                    target,
                    false,
                    IntersectionState::empty(),
                    recursion_flags,
                )
            };
        }

        if source.kind.is_param()
            && self
                .c
                .get_constraint_of_ty(source)
                .is_some_and(|constraint| constraint == target)
        {
            return Ternary::TRUE;
        }

        if source.flags.intersects(TypeFlags::DEFINITELY_NON_NULLABLE) && target.kind.is_union() {
            let t = target.kind.expect_union();
            let candidate = match t.tys.len() {
                2 if t.tys[0].flags.intersects(TypeFlags::NULLABLE) => Some(t.tys[1]),
                3 if t.tys[0].flags.intersects(TypeFlags::NULLABLE)
                    && t.tys[1].flags.intersects(TypeFlags::NULLABLE) =>
                {
                    Some(t.tys[1])
                }
                _ => None,
            };
            if let Some(candidate) = candidate {
                if !candidate.flags.intersects(TypeFlags::NULLABLE) && candidate.id == source.id {
                    return Ternary::TRUE;
                }
            }
        }

        if (self.relation == RelationKind::Comparable
            && !target.flags.intersects(TypeFlags::NEVER)
            && self
                .c
                .is_simple_type_related_to(target, source, self.relation))
            || self
                .c
                .is_simple_type_related_to(source, target, self.relation)
        {
            return Ternary::TRUE;
        }

        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            let is_performing_excess_property_check = !intersection_state
                .intersects(IntersectionState::TARGET)
                && source
                    .get_object_flags()
                    .contains(ObjectFlags::OBJECT_LITERAL | ObjectFlags::FRESH_LITERAL);
            if is_performing_excess_property_check
                && self.has_excess_properties(source, target, report_error)
            {
                if report_error {
                    return Ternary::TRUE;
                } else {
                    return Ternary::FALSE;
                }
            }
            return self.recur_ty_related_to(
                source,
                target,
                report_error,
                intersection_state,
                recursion_flags,
            );
        }

        Ternary::FALSE
    }

    fn get_relation_key(
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> RelationKey {
        RelationKey {
            source: source.id,
            target: target.id,
            relation,
        }
    }

    fn is_property_symbol_ty_related(
        &mut self,
        source_prop: SymbolID,
        target_prop: SymbolID,
        get_ty_of_source_prop: impl Fn(&mut Self, SymbolID) -> &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let effective_target = self.c.get_non_missing_type_of_symbol(target_prop);
        let effective_source = get_ty_of_source_prop(self, source_prop);
        self.is_related_to(
            effective_source,
            effective_target,
            RecursionFlags::BOTH,
            report_error,
            intersection_state,
        )
    }

    fn prop_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        source_prop: SymbolID,
        target_prop: SymbolID,
        get_ty_of_source_prop: impl Fn(&mut Self, SymbolID) -> &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let related = self.is_property_symbol_ty_related(
            source_prop,
            target_prop,
            get_ty_of_source_prop,
            report_error,
            intersection_state,
        );
        if related == Ternary::FALSE {
            if report_error {
                // TODO:
            }
            return Ternary::FALSE;
        }
        related
    }

    fn props_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if self.relation == RelationKind::Identity {
            // TODO:
        }

        let mut result = Ternary::TRUE;

        if let Some(t_tuple) = target.as_tuple() {
            if self.c.is_array_or_tuple(source) {
                let Some(s) = source.kind.as_object_reference() else {
                    unreachable!()
                };
                let source_arity = TyChecker::get_ty_reference_arity(s);
                let target_arity = if let Some(t) = target.kind.as_object_reference() {
                    TyChecker::get_ty_reference_arity(t)
                } else if let Some(t) = target.kind.as_object_tuple() {
                    t.ty_params().map_or(0, |ty_params| ty_params.len())
                } else {
                    unreachable!()
                };
                let source_rest_flags = if let Some(s_tuple) = s.target.kind.as_object_tuple() {
                    s_tuple.combined_flags.intersection(ElementFlags::REST)
                } else {
                    ElementFlags::REST
                };
                let target_has_rest_elem =
                    t_tuple.combined_flags.intersects(ElementFlags::VARIABLE);
                let source_min_length = if let Some(s_tuple) = s.target.kind.as_object_tuple() {
                    s_tuple.min_length
                } else {
                    0
                };
                let target_min_length = t_tuple.min_length;
                if !source_rest_flags.is_empty() && source_arity < target_min_length {
                    // TODO: report
                    return Ternary::FALSE;
                } else if !target_has_rest_elem && target_arity < source_min_length {
                    // TODO: report
                    return Ternary::FALSE;
                } else if !target_has_rest_elem
                    && (!source_rest_flags.is_empty() || target_arity < source_arity)
                {
                    // TODO: report
                    return Ternary::FALSE;
                }
                let source_ty_args = self.c.get_ty_arguments(source);
                let target_ty_args = self.c.get_ty_arguments(target);
                let target_start_count = t_tuple.get_start_elem_count(ElementFlags::NON_REST);
                let target_end_count = t_tuple.get_end_elem_count(ElementFlags::NON_REST);
                for source_pos in 0..source_arity {
                    let source_flags = if let Some(s_tuple) = s.target.kind.as_object_tuple() {
                        s_tuple.element_flags[source_pos]
                    } else {
                        ElementFlags::REST
                    };
                    let source_pos_from_end = source_arity - 1 - source_pos;
                    let target_pos = if target_has_rest_elem && source_pos >= target_start_count {
                        target_arity - 1 - usize::min(source_pos_from_end, target_end_count)
                    } else {
                        source_pos
                    };
                    let target_flags = t_tuple.element_flags[target_pos];

                    if target_flags.intersects(ElementFlags::VARIADIC)
                        && !source_flags.intersects(ElementFlags::VARIADIC)
                    {
                        // TODO: report
                        return Ternary::FALSE;
                    } else if source_flags.intersects(ElementFlags::VARIADIC)
                        && !target_flags.intersects(ElementFlags::VARIABLE)
                    {
                        // TODO: report
                        return Ternary::FALSE;
                    } else if target_flags.intersects(ElementFlags::REQUIRED)
                        && !source_flags.intersects(ElementFlags::REQUIRED)
                    {
                        // TODO: report
                        return Ternary::FALSE;
                    }

                    let source_ty = source_ty_args[source_pos];
                    let target_ty = target_ty_args[target_pos];
                    let target_check_ty = if source_flags.intersects(ElementFlags::VARIADIC)
                        && target_flags.intersects(ElementFlags::REST)
                    {
                        self.c.create_array_ty(target_ty, false)
                    } else {
                        target_ty
                    };
                    let is_related = self.is_related_to(
                        source_ty,
                        target_check_ty,
                        RecursionFlags::BOTH,
                        report_error,
                        intersection_state,
                    );
                    if is_related == Ternary::FALSE {
                        // TODO: report error
                        return Ternary::FALSE;
                    }
                    result &= is_related;
                }
                return result;
            } else if t_tuple.combined_flags.intersects(ElementFlags::VARIABLE) {
                return Ternary::FALSE;
            }
        }

        let require_optional_properties = matches!(
            self.relation,
            RelationKind::Subtype | RelationKind::StrictSubtype
        ) && !source.is_object_literal()
            && !self.c.is_empty_array_lit_ty(source)
            && !source.is_tuple();
        if let Some((unmatched, target_symbol)) =
            self.c
                .get_unmatched_prop(source, target, require_optional_properties)
        {
            assert!(!unmatched.is_empty());
            if report_error {
                let symbol = self.c.binder.symbol(target_symbol);
                let target_span = if symbol.flags.intersects(SymbolFlags::CLASS) {
                    self.c
                        .p
                        .node(symbol.expect_class().decl)
                        .as_class_decl()
                        .unwrap()
                        .name
                        .span
                } else if symbol.flags.intersects(SymbolFlags::INTERFACE) {
                    self.c
                        .p
                        .node(symbol.expect_interface().decls[0])
                        .as_interface_decl()
                        .unwrap()
                        .name
                        .span
                } else if symbol.flags.intersects(SymbolFlags::OBJECT_LITERAL) {
                    self.c.p.node(symbol.expect_object().decl).span()
                } else {
                    self.c.p.node(symbol.expect_ty_lit().decl).span()
                };
                let Some(source_symbol) = source.symbol() else {
                    // TODO: unreachable!()
                    return Ternary::TRUE;
                };
                let mut unmatched = unmatched;
                unmatched.sort();
                if unmatched.len() < 3 {
                    for name in unmatched {
                        let field = self.c.atoms.get(name).to_string();
                        let defined = errors::DefinedHere {
                            span: target_span,
                            kind: errors::DeclKind::Property,
                            name: field.to_string(),
                        };
                        let span = self.c.p.node(self.error_node.unwrap()).span();
                        let error = errors::PropertyXIsMissing {
                            span,
                            field,
                            related: [defined],
                        };
                        self.c.push_error(Box::new(error));
                    }
                } else {
                    let props = vec![
                        self.c.atoms.get(unmatched[0]).to_string(),
                        self.c.atoms.get(unmatched[1]).to_string(),
                    ];
                    let len = unmatched.len() - 2;
                    let span = self.c.p.node(self.error_node.unwrap()).span();
                    let error =
                        errors::Type0IsMissingTheFollowingPropertiesFromType1Colon2And3More {
                            span,
                            ty1: self.c.print_ty(source).to_string(),
                            ty2: self.c.print_ty(target).to_string(),
                            props,
                            len,
                        };
                    self.c.push_error(Box::new(error));
                }
                return Ternary::TRUE;
            }

            return Ternary::FALSE;
        }

        let props = self.c.properties_of_ty(target);
        let numeric_names_only = source.is_tuple() && target.is_tuple();
        for target_prop in props {
            let s = self.c.symbol(*target_prop);
            let name = s.name();
            if !s.flags().intersects(SymbolFlags::PROTOTYPE)
                && (!numeric_names_only || name.is_numeric() || name.expect_atom() == IDENT_LENGTH)
            {
                if let Some(source_prop) = self.c.get_prop_of_ty(source, name) {
                    if !source_prop.eq(target_prop) {
                        let related = self.prop_related_to(
                            source,
                            target,
                            source_prop,
                            *target_prop,
                            |this, symbol| this.c.get_non_missing_type_of_symbol(symbol),
                            report_error,
                            intersection_state,
                        );
                        if related == Ternary::FALSE {
                            return Ternary::FALSE;
                        }
                        result &= related;
                    }
                }
            }
        }

        result
    }

    fn type_args_related_to(
        &mut self,
        source_ty_args: ty::Tys<'cx>,
        target_ty_args: ty::Tys<'cx>,
        variances: &'cx [VarianceFlags],
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if source_ty_args.len() != target_ty_args.len() && self.relation != RelationKind::Identity {
            return Ternary::FALSE;
        }
        let len = usize::min(source_ty_args.len(), target_ty_args.len());
        let mut result = Ternary::TRUE;
        for i in 0..len {
            let variance_flags = variances
                .get(i)
                .copied()
                .unwrap_or(VarianceFlags::COVARIANT);

            let variance = variance_flags & VarianceFlags::VARIANCE_MASK;
            if variance != VarianceFlags::INDEPENDENT {
                let source = source_ty_args[i];
                let target = target_ty_args[i];
                let mut related = Ternary::TRUE;
                if variance_flags.intersects(VarianceFlags::UNMEASURABLE) {
                    related = if self.relation == RelationKind::Identity {
                        self.is_related_to(
                            source,
                            target,
                            RecursionFlags::BOTH,
                            false,
                            IntersectionState::empty(),
                        )
                    } else {
                        todo!()
                    }
                } else if variance == VarianceFlags::COVARIANT {
                    related = self.is_related_to(
                        source,
                        target,
                        RecursionFlags::BOTH,
                        report_error,
                        intersection_state,
                    );
                } else if variance == VarianceFlags::CONTRAVARIANT {
                    related = self.is_related_to(
                        target,
                        source,
                        RecursionFlags::BOTH,
                        report_error,
                        intersection_state,
                    );
                } else if variance == VarianceFlags::BIVARIANT {
                    related = self.is_related_to(
                        target,
                        source,
                        RecursionFlags::BOTH,
                        false,
                        IntersectionState::empty(),
                    );
                    if related == Ternary::FALSE {
                        related = self.is_related_to(
                            source,
                            target,
                            RecursionFlags::BOTH,
                            report_error,
                            intersection_state,
                        );
                    }
                } else {
                    related = self.is_related_to(
                        source,
                        target,
                        RecursionFlags::BOTH,
                        report_error,
                        intersection_state,
                    );
                    if related != Ternary::FALSE {
                        related = self.is_related_to(
                            target,
                            source,
                            RecursionFlags::BOTH,
                            report_error,
                            intersection_state,
                        );
                    }
                }

                if related == Ternary::FALSE {
                    return Ternary::FALSE;
                }
                result &= related;
            }
        }
        result
    }

    fn relate_variances(
        &mut self,
        source: ty::Tys<'cx>,
        target: ty::Tys<'cx>,
        variances: &'cx [VarianceFlags],
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Option<Ternary> {
        let res =
            self.type_args_related_to(source, target, variances, report_error, intersection_state);
        Some(res)
    }

    fn each_type_related_to_some_type(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> Ternary {
        let source_tys = if let TyKind::Union(s) = source.kind {
            s.tys
        } else if let TyKind::Intersection(i) = source.kind {
            i.tys
        } else {
            unreachable!()
        };
        let target_tys = if let Some(u) = target.kind.as_union() {
            u.tys
        } else if let Some(i) = target.kind.as_intersection() {
            i.tys
        } else {
            unreachable!()
        };
        let mut result = Ternary::TRUE;
        for source_ty in source_tys {
            let related = self.ty_related_to_some_ty(
                source_ty,
                target,
                target_tys,
                false,
                IntersectionState::empty(),
            );
            if related == Ternary::FALSE {
                return Ternary::FALSE;
            }
            result &= related;
        }
        result
    }

    fn some_type_related_to_type(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let Some(tys) = source.kind.tys_of_union_or_intersection() else {
            unreachable!()
        };
        if source.kind.is_union() && contains_ty(tys, target) {
            return Ternary::TRUE;
        }
        for i in 0..tys.len() {
            let related = self.is_related_to(
                tys[i],
                target,
                RecursionFlags::SOURCE,
                report_error && i == tys.len() - 1,
                intersection_state,
            );
            if related != Ternary::FALSE {
                return related;
            }
        }
        Ternary::FALSE
    }

    fn each_type_related_to_type(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let TyKind::Union(s) = source.kind else {
            unreachable!()
        };
        let mut result = Ternary::TRUE;

        let undefined_stripped_target = target;
        let source_tys = s.tys;
        for (i, source_ty) in source_tys.iter().enumerate() {
            if let Some(target_union) = undefined_stripped_target.kind.as_union() {
                if source_tys.len() >= target_union.tys.len()
                    && source_tys.len() % target_union.tys.len() == 0
                {
                    let related = self.is_related_to(
                        source_ty,
                        target_union.tys[i % target_union.tys.len()],
                        RecursionFlags::BOTH,
                        false,
                        intersection_state,
                    );
                    if related != Ternary::FALSE {
                        result &= related;
                        continue;
                    }
                }
            }
            let related = self.is_related_to(
                source_ty,
                target,
                RecursionFlags::SOURCE,
                report_error,
                intersection_state,
            );
            if related == Ternary::FALSE {
                return Ternary::FALSE;
            }
            result &= related;
        }
        result
    }

    fn union_or_intersection_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if source.kind.is_union() {
            // if let TyKind::Union(t) = target.kind {
            // } else {
            // }
            return if self.relation == RelationKind::Comparable {
                self.some_type_related_to_type(
                    source,
                    target,
                    report_error && !source.flags.intersects(TypeFlags::PRIMITIVE),
                    intersection_state,
                )
            } else {
                self.each_type_related_to_type(
                    source,
                    target,
                    report_error && !source.flags.intersects(TypeFlags::PRIMITIVE),
                    intersection_state,
                )
            };
        } else if let Some(target_union) = target.kind.as_union() {
            self.ty_related_to_some_ty(
                source,
                target,
                target_union.tys,
                report_error,
                intersection_state,
            )
        } else if let Some(i) = target.kind.as_intersection() {
            self.ty_related_to_each_ty(
                source,
                target,
                i.tys,
                report_error,
                IntersectionState::TARGET,
            )
        } else {
            self.some_type_related_to_type(source, target, false, IntersectionState::SOURCE)
        }
    }

    fn ty_related_to_each_ty(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        target_tys: ty::Tys<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::TRUE;
        for target_ty in target_tys {
            let related = self.is_related_to(
                source,
                target_ty,
                RecursionFlags::TARGET,
                report_error,
                intersection_state,
            );
            if related == Ternary::FALSE {
                return Ternary::FALSE;
            }
            result &= related;
        }
        result
    }

    fn ty_related_to_some_ty(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        target_tys: ty::Tys<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if let Some(unions) = target.kind.as_union() {
            if contains_ty(unions.tys, source) {
                return Ternary::TRUE;
            }
            // TODO:
        }

        for ty in target_tys {
            let related = self.is_related_to(
                source,
                ty,
                RecursionFlags::TARGET,
                false,
                intersection_state,
            );
            if related != Ternary::FALSE {
                return related;
            }
        }

        Ternary::FALSE
    }

    fn structured_ty_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::TRUE;
        let mut source_flags = source.flags;
        let target_flags = target.flags;
        if self.relation == RelationKind::Identity {
            if source.kind.is_union_or_intersection() {
                let mut result = self.each_type_related_to_some_type(source, target);
                if result != Ternary::FALSE {
                    result &= self.each_type_related_to_some_type(target, source);
                }
                return result;
            }
        } else if source.kind.is_union_or_intersection() || target.kind.is_union_or_intersection() {
            result = self.union_or_intersection_related_to(
                source,
                target,
                report_error,
                intersection_state,
            );

            if result != Ternary::FALSE {
                return result;
            }
        }

        // TODO: handle `source/target.kind.readonly` and `isMutableArrayOrTuple`
        if (source.kind.is_single_element_generic_tuple_type() && {
            let ty_arg = self.c.get_ty_arguments(source)[0];
            result = self.is_related_to(
                ty_arg,
                target,
                RecursionFlags::SOURCE,
                false,
                IntersectionState::empty(),
            );
            result != Ternary::FALSE
        }) || (target.kind.is_single_element_generic_tuple_type() && {
            let ty_arg = self.c.get_ty_arguments(target)[0];
            result = self.is_related_to(
                source,
                ty_arg,
                RecursionFlags::TARGET,
                false,
                IntersectionState::empty(),
            );
            result != Ternary::FALSE
        }) {
            return result;
        }

        if target_flags.intersects(TypeFlags::INDEX) {
            if source_flags.intersects(TypeFlags::INDEX) {
                let source_ty = source.kind.expect_index_ty().ty;
                let target_ty = target.kind.expect_index_ty().ty;
                result = self.is_related_to(
                    target_ty,
                    source_ty,
                    RecursionFlags::BOTH,
                    false,
                    IntersectionState::empty(),
                );
                if result != Ternary::FALSE {
                    return result;
                }
            } else if target.is_tuple() {
                todo!()
            } else {
                let target_ty = target.kind.expect_index_ty();
                if let Some(constraint) = self.c.get_simplified_ty_or_constraint(target_ty.ty) {
                    let index_flags = target_ty.index_flags | ty::IndexFlags::NO_REDUCIBLE_CHECK;
                    let index_ty = self.c.get_index_ty(constraint, index_flags);
                    if self.is_related_to(
                        source,
                        index_ty,
                        RecursionFlags::TARGET,
                        report_error,
                        IntersectionState::empty(),
                    ) == Ternary::TRUE
                    {
                        return Ternary::TRUE;
                    }
                }
                // TODO: is_generic_mapped_ty(target_ty)
            }
        } else if let Some(_) = target.kind.as_cond_ty() {
            if self.c.is_deeply_nested_type(target, &self.target_stack, 10) {
                return Ternary::FALSE;
            }
        }

        if source.flags.intersects(TypeFlags::TYPE_VARIABLE) {
            if !(source.kind.is_indexed_access() && target.kind.is_indexed_access()) {
                let constraint = self
                    .c
                    .get_constraint_of_ty(source)
                    .unwrap_or(self.c.unknown_ty);
                result = self.is_related_to(
                    constraint,
                    target,
                    RecursionFlags::SOURCE,
                    false,
                    intersection_state,
                );
                if result != Ternary::FALSE {
                    return result;
                }
            }
        } else if let Some(source_cond) = source.kind.as_cond_ty() {
            if self.c.is_deeply_nested_type(source, &self.target_stack, 10) {
                return Ternary::FALSE;
            } else if let Some(target_cond) = target.kind.as_cond_ty() {
                let source_extends = source_cond.extends_ty;
                let mapper = None;
                if self
                    .c
                    .is_type_identical_to(source_extends, target_cond.extends_ty)
                    && (self.is_related_to(
                        source_cond.check_ty,
                        target_cond.check_ty,
                        RecursionFlags::BOTH,
                        false,
                        IntersectionState::empty(),
                    ) != Ternary::FALSE
                        || self.is_related_to(
                            target_cond.check_ty,
                            source_cond.check_ty,
                            RecursionFlags::BOTH,
                            false,
                            IntersectionState::empty(),
                        ) != Ternary::FALSE)
                {
                    result = {
                        let source_true_ty = {
                            let true_ty = self.c.get_true_ty_from_cond_ty(source, source_cond);
                            self.c.instantiate_ty(true_ty, mapper)
                        };
                        let target_true_ty = self.c.get_true_ty_from_cond_ty(target, target_cond);
                        self.is_related_to(
                            source_true_ty,
                            target_true_ty,
                            RecursionFlags::BOTH,
                            report_error,
                            IntersectionState::empty(),
                        )
                    };
                    if result != Ternary::FALSE {
                        let source_false_ty = self.c.get_false_ty_from_cond_ty(source, source_cond);
                        let target_false_ty = self.c.get_false_ty_from_cond_ty(target, target_cond);
                        result &= self.is_related_to(
                            source_false_ty,
                            target_false_ty,
                            RecursionFlags::BOTH,
                            report_error,
                            IntersectionState::empty(),
                        );
                    }

                    if result != Ternary::FALSE {
                        return result;
                    }
                }
            }
            let default_constraint = self.c.get_default_constraint_of_cond_ty(source);
            result = self.is_related_to(
                default_constraint,
                target,
                RecursionFlags::SOURCE,
                report_error,
                IntersectionState::empty(),
            );
            if result != Ternary::FALSE {
                return result;
            }
            let distributive_constraint = if !target_flags.intersects(TypeFlags::CONDITIONAL)
                && self.c.has_non_circular_constraint(source)
            {
                self.c.get_constraint_of_distributive_cond_ty(source)
            } else {
                None
            };
            if let Some(distributive_constraint) = distributive_constraint {
                // TODO: resetErrorInfo(saveErrorInfo);
                result = self.is_related_to(
                    distributive_constraint,
                    target,
                    RecursionFlags::SOURCE,
                    report_error,
                    IntersectionState::empty(),
                );
                if result != Ternary::FALSE {
                    return result;
                }
            }
        } else {
            if !source.is_tuple() {
                if let Some(source_refer) = source.kind.as_object_reference() {
                    if let Some(target_refer) = target.kind.as_object_reference() {
                        if source_refer.target == target_refer.target
                            && !source.is_tuple()
                            && !(self.c.is_marker_ty(source) || self.c.is_marker_ty(target))
                        {
                            if self.c.is_empty_array_lit_ty(source) {
                                return Ternary::TRUE;
                            }
                            let variances = self.c.get_variances(source_refer.target);
                            if variances == self.c.empty_array() {
                                // cycle
                                return Ternary::UNKNOWN;
                            }
                            let source_ty_args = self.c.get_ty_arguments(source);
                            let target_ty_args = self.c.get_ty_arguments(target);
                            if let Some(result) = self.relate_variances(
                                source_ty_args,
                                target_ty_args,
                                variances,
                                report_error,
                                intersection_state,
                            ) {
                                return result;
                            }
                        }
                    }
                }
            }

            if target.kind.is_array(self.c)
                && self
                    .c
                    .every_type(source, |this, t| this.is_array_or_tuple(t))
            {
                return if self.relation != RelationKind::Identity {
                    let source = self
                        .c
                        .get_index_ty_of_ty(source, self.c.number_ty)
                        .unwrap_or(self.c.any_ty);
                    let target = self
                        .c
                        .get_index_ty_of_ty(target, self.c.number_ty)
                        .unwrap_or(self.c.any_ty);
                    self.is_related_to(
                        source,
                        target,
                        RecursionFlags::BOTH,
                        report_error,
                        IntersectionState::empty(),
                    )
                } else {
                    Ternary::FALSE
                };
            } else if let Some(target_index) = target.kind.as_index_ty() {
                let target_ty = target_index.ty;
                if let Some(constraint) = self.c.get_simplified_ty_or_constraint(target_ty) {
                    let index_ty = self.c.get_index_ty(
                        constraint,
                        target_index.index_flags | ty::IndexFlags::NO_REDUCIBLE_CHECK,
                    );
                    if self.is_related_to(
                        source,
                        index_ty,
                        RecursionFlags::TARGET,
                        report_error,
                        IntersectionState::empty(),
                    ) == Ternary::TRUE
                    {
                        return Ternary::TRUE;
                    }
                }
            }

            let source_is_primitive = source.flags.intersects(TypeFlags::PRIMITIVE);

            let source = if self.relation != RelationKind::Identity {
                self.c.get_apparent_ty(source)
            } else {
                source
            };

            if source.kind.is_object_or_intersection() && target.kind.is_object() {
                let report_error = report_error && !source_is_primitive;

                let mut res =
                    self.props_related_to(source, target, report_error, intersection_state);
                if res != Ternary::FALSE {
                    res &= self.sigs_related_to(
                        source,
                        target,
                        SigKind::Call,
                        report_error,
                        intersection_state,
                    );
                    if res != Ternary::FALSE {
                        res &= self.sigs_related_to(
                            source,
                            target,
                            SigKind::Constructor,
                            report_error,
                            intersection_state,
                        );
                        if res != Ternary::FALSE {
                            res &= self.index_sigs_related_to(
                                source,
                                target,
                                source_is_primitive,
                                report_error,
                                intersection_state,
                            );
                        }
                    }
                }
                return res;
            }
        }

        Ternary::FALSE
    }

    fn recur_ty_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> Ternary {
        let key = Self::get_relation_key(source, target, self.relation);
        if !self.maybe_keys_set.insert(key) {
            // already being compared.
            return Ternary::MAYBE;
        }

        if recursion_flags.intersects(RecursionFlags::SOURCE) {
            self.source_stack.push(source);
            if !self.expanding_flags.intersects(RecursionFlags::SOURCE)
                && self.c.is_deeply_nested_type(source, &self.source_stack, 3)
            {
                self.expanding_flags |= RecursionFlags::SOURCE;
            }
        }

        if recursion_flags.intersects(RecursionFlags::TARGET) {
            self.target_stack.push(target);
            if !self.expanding_flags.intersects(RecursionFlags::TARGET)
                && self.c.is_deeply_nested_type(target, &self.target_stack, 3)
            {
                self.expanding_flags |= RecursionFlags::TARGET;
            }
        }

        let res = if self.expanding_flags == RecursionFlags::BOTH {
            Ternary::MAYBE
        } else {
            self.structured_ty_related_to(source, target, report_error, intersection_state)
        };

        if recursion_flags.intersects(RecursionFlags::TARGET) {
            self.target_stack.pop();
        }

        if recursion_flags.intersects(RecursionFlags::SOURCE) {
            self.source_stack.pop();
        }

        self.maybe_keys_set.remove(&key);
        res
    }

    fn index_info_related_to(
        &mut self,
        source: &'cx ty::IndexInfo<'cx>,
        target: &'cx ty::IndexInfo<'cx>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let res = self.is_related_to(
            source.val_ty,
            target.val_ty,
            RecursionFlags::BOTH,
            false,
            intersection_state,
        );
        if res == Ternary::FALSE {
            if source.key_ty == target.key_ty {
                let decl = self.c.binder.symbol(source.symbol).expect_index().decl;
                let span = self.c.p.node(decl).expect_index_sig_decl().ty.span();
                let error = errors::IndexSignaturesAreIncompatible {
                    span,
                    ty: target.val_ty.to_string(self.c),
                };
                self.c.push_error(Box::new(error));
            } else {
                todo!()
            }
        }
        Ternary::TRUE
    }

    fn members_related_to_index_info(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx ty::IndexInfo<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut res = Ternary::TRUE;
        let props = self.c.get_props_of_ty(source);
        for prop in props {
            let prop_ty = self.c.get_non_missing_type_of_symbol(*prop);
            let ty = prop_ty;
            let related = self.is_related_to(
                ty,
                target.val_ty,
                RecursionFlags::BOTH,
                report_error,
                intersection_state,
            );
            if related == Ternary::FALSE {
                return Ternary::FALSE;
            }
            res &= related;
        }

        for info in self.c.get_index_infos_of_ty(source) {
            if self.c.is_applicable_index_ty(info.key_ty, target.key_ty) {
                let related = self.index_info_related_to(info, target, intersection_state);
                if related == Ternary::FALSE {
                    return Ternary::FALSE;
                }
                res &= related;
            }
        }
        res
    }

    fn type_related_to_index_info(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx ty::IndexInfo<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if let Some(source) = self.c.get_applicable_index_info(source, target.key_ty) {
            self.index_info_related_to(source, target, intersection_state)
        } else if !intersection_state.intersects(IntersectionState::SOURCE)
            && (self.relation != RelationKind::StrictSubtype
                || source
                    .get_object_flags()
                    .intersects(ObjectFlags::FRESH_LITERAL))
            && self.c.is_object_ty_with_inferable_index(source)
        {
            self.members_related_to_index_info(source, target, report_error, intersection_state)
        } else {
            Ternary::FALSE
        }
    }

    fn index_sigs_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        source_is_primitive: bool,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if self.relation == RelationKind::Identity {
            return Ternary::TRUE;
        }
        let index_infos = self.c.get_index_infos_of_ty(target);
        let target_has_string_index = index_infos.iter().any(|i| i.key_ty == self.c.string_ty);
        let mut result = Ternary::TRUE;
        for target_info in index_infos {
            let related = if self.relation != RelationKind::StrictSubtype
                && !source_is_primitive
                && target_has_string_index
                && target_info.val_ty.flags.intersects(TypeFlags::ANY)
            {
                Ternary::TRUE
            } else {
                self.type_related_to_index_info(
                    source,
                    target_info,
                    report_error,
                    intersection_state,
                )
            };
            if related == Ternary::FALSE {
                return Ternary::FALSE;
            }
            result &= related;
        }
        result
    }

    fn index_sigs_identical_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> Ternary {
        let source_infos = self.c.get_index_infos_of_ty(source);
        let target_infos = self.c.get_index_infos_of_ty(target);
        if source_infos.len() != target_infos.len() {
            return Ternary::FALSE;
        }
        for target_info in target_infos {
            let Some(source_info) = self.c.get_index_info_of_ty(source, target_info.key_ty) else {
                return Ternary::FALSE;
            };
            let source_val_ty = source_info.val_ty;
            if self.is_related_to(
                source_val_ty,
                target_info.val_ty,
                RecursionFlags::BOTH,
                false,
                IntersectionState::empty(),
            ) == Ternary::FALSE
            {
                return Ternary::FALSE;
            }
            // TODO: source_info.is_readonly != target_info.is_readonly then return false
        }
        Ternary::TRUE
    }

    fn sigs_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        kind: SigKind,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if self.relation == RelationKind::Identity {
            return self.index_sigs_identical_to(source, target);
        };

        if source == self.c.any_fn_ty() || target == self.c.any_fn_ty() {
            return Ternary::TRUE;
        }

        let source_sigs = self.c.get_signatures_of_type(source, kind);
        let target_sigs = self.c.get_signatures_of_type(target, kind);

        if kind == SigKind::Constructor && !source_sigs.is_empty() && !target_sigs.is_empty() {
            let source_is_abstract = source_sigs[0].flags.intersects(SigFlags::ABSTRACT);
            let target_is_abstract = target_sigs[0].flags.intersects(SigFlags::ABSTRACT);
            if source_is_abstract && !target_is_abstract {
                return Ternary::FALSE;
            }
        }

        if source_sigs.len() == 1 && target_sigs.len() == 1 {
            let erase_generics = self.relation == RelationKind::Comparable;
            let source_sig = source_sigs[0];
            let target_sig = target_sigs[0];
            return self.sig_related_to(
                source_sig,
                target_sig,
                erase_generics,
                report_error,
                intersection_state,
            );
        } else {
            for t in target_sigs {
                for s in source_sigs {
                    if self.sig_related_to(s, t, true, report_error, intersection_state)
                        != Ternary::FALSE
                    {
                        continue;
                    }
                }
                return Ternary::FALSE;
            }
        }
        Ternary::TRUE
    }

    fn sig_related_to(
        &mut self,
        source: &'cx Sig<'cx>,
        target: &'cx Sig<'cx>,
        erase: bool,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let check_mode = match self.relation {
            RelationKind::Subtype => SigCheckMode::STRICT_TOP_SIGNATURE,
            RelationKind::StrictSubtype => {
                SigCheckMode::STRICT_TOP_SIGNATURE | SigCheckMode::STRICT_ARITY
            }
            _ => SigCheckMode::empty(),
        };

        self.compare_sig_related(
            source,
            target,
            check_mode,
            report_error,
            |this, source, target, report_error| {
                this.is_related_to(
                    source,
                    target,
                    RecursionFlags::BOTH,
                    report_error,
                    intersection_state,
                )
            },
        )
    }

    pub(super) fn compare_sig_related(
        &mut self,
        mut source: &'cx Sig<'cx>,
        mut target: &'cx Sig<'cx>,
        check_mode: SigCheckMode,
        report_error: bool,
        compare: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>, bool) -> Ternary + Copy,
    ) -> Ternary {
        if source == target {
            return Ternary::TRUE;
        }

        let mut result = Ternary::TRUE;
        let target_count = target.get_param_count(self.c);
        let source_has_more_params = !self.c.has_effective_rest_param(target)
            && (if check_mode.intersects(SigCheckMode::STRICT_ARITY) {
                self.c.has_effective_rest_param(source)
                    || source.get_param_count(self.c) > target_count
            } else {
                self.c.get_min_arg_count(source) > target_count
            });
        if source_has_more_params {
            return Ternary::FALSE;
        }
        if let Some(ty_params) = source.ty_params {
            if target.ty_params.map_or(true, |target_ty_params| {
                !std::ptr::eq(ty_params, target_ty_params)
            }) {
                // when compare signatures, such as:
                // `<G>() => G` and `<T>() => T`
                // we should canonical the type parameters `G` and `T` into the same type parameter
                target = self.c.get_canonical_sig(target);
                source = self.c.instantiate_sig_in_context_of(source, target, None);
            }
        }
        let source_count = source.get_param_count(self.c);
        let source_rest_ty = source.get_non_array_rest_ty(self.c);
        let target_rest_ty = target.get_non_array_rest_ty(self.c);

        let (param_count, rest_index) = if source_rest_ty.is_some() || target_rest_ty.is_some() {
            let param_count = usize::min(source_count, target_count);
            (param_count, param_count - 1)
        } else {
            (usize::max(source_count, target_count), usize::MAX)
        };
        let strict_variance = false;

        for i in 0..param_count {
            let source_ty = if i == rest_index {
                todo!()
            } else {
                self.c.try_get_ty_at_pos(source, i)
            };
            let target_ty = if i == rest_index {
                todo!()
            } else {
                self.c.try_get_ty_at_pos(target, i)
            };
            if let Some(source_ty) = source_ty {
                if let Some(target_ty) = target_ty {
                    if (source_ty != target_ty) || check_mode.intersects(SigCheckMode::STRICT_ARITY)
                    {
                        let source_sig = if check_mode.intersects(SigCheckMode::CALLBACK) {
                            None
                        } else {
                            let source_ty = self.c.get_non_nullable_ty(source_ty);
                            self.c.get_single_call_sig(source_ty)
                        };
                        let target_sig = if check_mode.intersects(SigCheckMode::CALLBACK) {
                            None
                        } else {
                            let target_ty = self.c.get_non_nullable_ty(target_ty);
                            self.c.get_single_call_sig(target_ty)
                        };
                        let callbacks = match (source_sig, target_sig) {
                            (Some(_), Some(_)) => false,
                            _ => false,
                        };
                        let mut related = if callbacks {
                            let target_sig = target_sig.unwrap();
                            let source_sig = source_sig.unwrap();
                            self.compare_sig_related(
                                target_sig,
                                source_sig,
                                check_mode & SigCheckMode::STRICT_ARITY,
                                report_error,
                                compare,
                            )
                        } else if !check_mode.intersects(SigCheckMode::CALLBACK) && !strict_variance
                        {
                            let res = compare(self, source_ty, target_ty, false);
                            if res == Ternary::FALSE {
                                // TODO: use report_error param
                                compare(self, target_ty, source_ty, false)
                            } else {
                                res
                            }
                        } else {
                            // TODO: use report_error param
                            compare(self, target_ty, source_ty, false)
                        };

                        if related != Ternary::FALSE
                            && check_mode.intersects(SigCheckMode::STRICT_ARITY)
                            && i >= self.c.get_min_arg_count(source)
                            && i < self.c.get_min_arg_count(target)
                            && compare(self, source_ty, target_ty, false) != Ternary::FALSE
                        {
                            related = Ternary::FALSE;
                        }

                        if related == Ternary::FALSE {
                            return Ternary::FALSE;
                        }
                        result &= related;
                    }
                }
            }
        }

        if !check_mode.intersects(SigCheckMode::IGNORE_RETURN_TYPES) {
            let ret_ty = |this: &mut Self, sig: &'cx ty::Sig<'cx>| {
                // TODO: cycle
                this.c.get_ret_ty_of_sig(sig)
            };
            let target_ret_ty = ret_ty(self, target);
            if target_ret_ty == self.c.any_ty || target_ret_ty == self.c.void_ty {
                return Ternary::TRUE;
            } else {
                let source_ret_ty = ret_ty(self, source);
                // TODO: ty_predict
                let related = if check_mode.intersects(SigCheckMode::BIVARIANT_CALLBACK) {
                    compare(self, source_ret_ty, target_ret_ty, false)
                } else {
                    compare(self, source_ret_ty, target_ret_ty, report_error)
                };

                if related == Ternary::FALSE {
                    return Ternary::FALSE;
                }
                result &= related;
            }
        }

        result
    }

    fn has_excess_properties(
        &mut self,
        source: &'cx Ty<'cx>,
        target_ty: &'cx Ty<'cx>,
        report_error: bool,
    ) -> bool {
        if !self.c.is_excess_property_check_target(target_ty) {
            return false;
        }
        for prop in self.c.get_props_of_ty(source) {
            let name = self.c.symbol(*prop).name();
            if !self.c.is_known_prop(target_ty, name) {
                if report_error {
                    let span = self.c.p.node(self.c.get_symbol_decl(*prop).unwrap()).span();
                    let field = self.c.atoms.get(name.expect_atom()).to_string();
                    let error = errors::ObjectLitMayOnlySpecifyKnownPropAndFieldDoesNotExist {
                        span,
                        field,
                    };
                    self.c.push_error(Box::new(error));
                }
                return true;
            }
        }

        false
    }
}

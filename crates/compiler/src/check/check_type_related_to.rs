use bolt_ts_utils::fx_hashset_with_capacity;
use rustc_hash::FxHashSet;

use super::errors;
use super::relation::{RelationKind, SigCheckMode};
use crate::ast;
use crate::bind::{SymbolFlags, SymbolID};
use crate::keyword::IDENT_LENGTH;
use crate::ty::{self, ObjectFlags, SigFlags, SigKind};
use crate::ty::{Sig, Ty, TyKind, Tys};

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
        let mut c = TypeRelatedChecker {
            c: self,
            relation,
            maybe_keys_set: fx_hashset_with_capacity(32),
            expanding_flags: RecursionFlags::empty(),
            source_stack: Vec::with_capacity(32),
            target_stack: Vec::with_capacity(32),
            error_node,
        };
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

struct TypeRelatedChecker<'cx, 'checker> {
    c: &'checker mut TyChecker<'cx>,
    relation: RelationKind,
    maybe_keys_set: FxHashSet<RelationKey>,
    expanding_flags: RecursionFlags,
    source_stack: Vec<&'cx ty::Ty<'cx>>,
    target_stack: Vec<&'cx ty::Ty<'cx>>,
    error_node: Option<ast::NodeID>,
}

impl<'cx, 'checker> TypeRelatedChecker<'cx, 'checker> {
    fn is_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        recursion_flags: RecursionFlags,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if source.id == target.id {
            return Ternary::TRUE;
        } else if source.kind.is_object() && target.kind.is_primitive() {
            if self.relation == RelationKind::Comparable
                && !target.kind.is_never()
                && (self
                    .c
                    .is_simple_type_related_to(target, source, self.relation)
                    || self
                        .c
                        .is_simple_type_related_to(source, target, self.relation))
            {
                return Ternary::TRUE;
            } else {
                return Ternary::FALSE;
            }
        }

        if source.kind.is_param()
            && self
                .c
                .get_constraint_of_ty(source)
                .is_some_and(|constraint| constraint == target)
        {
            return Ternary::TRUE;
        }

        if source.kind.definitely_non_nullable() && target.kind.is_union() {
            let t = target.kind.expect_union();
            let candidate = match t.tys.len() {
                2 if t.tys[0].kind.is_nullable() => Some(t.tys[1]),
                3 if t.tys[0].kind.is_nullable() && t.tys[1].kind.is_nullable() => Some(t.tys[1]),
                _ => None,
            };
            if let Some(candidate) = candidate {
                if !candidate.kind.is_nullable() && candidate.id == source.id {
                    return Ternary::TRUE;
                }
            }
        }

        if (self.relation == RelationKind::Comparable
            && !target.kind.is_never()
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
            let is_performing_excess_property_check = source
                .kind
                .as_object()
                .is_some_and(|object| object.flags.intersects(ObjectFlags::OBJECT_LITERAL));
            if is_performing_excess_property_check
                && self.has_excess_properties(source, target, report_error)
            {
                if report_error {
                    return Ternary::TRUE;
                } else {
                    return Ternary::FALSE;
                }
            }
            return self.recur_related_to(
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

        if target.kind.is_tuple() || target.kind.is_object_tuple() {
            if self.c.is_array_or_tuple(source) || source.kind.is_object_tuple() {
                // TODO:
                return Ternary::TRUE;
            }
        }

        let unmatched = self.c.get_unmatched_prop(source, target);
        if let Some((unmatched, target_symbol)) = unmatched {
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
            if !unmatched.is_empty() && report_error {
                let Some(source_symbol) = source.symbol() else {
                    unreachable!()
                };
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
                return Ternary::TRUE;
            }

            return Ternary::FALSE;
        }

        let mut result = Ternary::TRUE;

        let props = self.c.properties_of_ty(target);
        let numeric_names_only = source.kind.is_tuple() && target.kind.is_tuple();
        for target_prop in props {
            let name = self.c.binder.symbol(*target_prop).name;
            if !self
                .c
                .binder
                .symbol(*target_prop)
                .flags
                .intersects(SymbolFlags::PROTOTYPE)
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
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if source_ty_args.len() != target_ty_args.len() && self.relation != RelationKind::Identity {
            return Ternary::FALSE;
        }
        let len = usize::min(source_ty_args.len(), target_ty_args.len());
        for i in 0..len {
            let source = source_ty_args[i];
            let target = target_ty_args[i];
            if self.is_related_to(
                source,
                target,
                RecursionFlags::BOTH,
                report_error,
                intersection_state,
            ) == Ternary::FALSE
            {
                return Ternary::FALSE;
            }
        }
        Ternary::TRUE
    }

    fn relate_variances(
        &mut self,
        source: ty::Tys<'cx>,
        target: ty::Tys<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Option<Ternary> {
        let res = self.type_args_related_to(source, target, report_error, intersection_state);
        Some(res)
    }

    fn each_type_related_to_type(
        &mut self,
        source: &'cx Ty<'cx>,
        sources: Tys<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let res = Ternary::TRUE;
        for (idx, source_ty) in sources.iter().enumerate() {
            // if idx <= targets.len() {
            //     let related = self.is_related_to(source_ty, targets[idx]);
            // }
            let related = self.is_related_to(
                source_ty,
                target,
                RecursionFlags::BOTH,
                report_error,
                intersection_state,
            );
            if related == Ternary::FALSE {
                return Ternary::FALSE;
            }
        }
        res
    }

    fn union_or_intersection_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if let TyKind::Union(s) = source.kind {
            // if let TyKind::Union(t) = target.kind {
            // } else {
            // }
            self.each_type_related_to_type(source, s.tys, target, report_error, intersection_state)
        } else if target.kind.is_union() {
            self.ty_related_to_some_ty(source, target, report_error)
        } else {
            Ternary::FALSE
        }
    }

    fn ty_related_to_some_ty(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
    ) -> Ternary {
        if let Some(unions) = target.kind.as_union() {
            if unions.tys.contains(&source) {
                return Ternary::TRUE;
            }
            // TODO: compare
            Ternary::FALSE
        } else {
            Ternary::FALSE
        }
    }

    fn structured_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if source.kind.is_union_or_intersection() || target.kind.is_union_or_intersection() {
            return self.union_or_intersection_related_to(
                source,
                target,
                report_error,
                intersection_state,
            );
        }

        if !source.kind.is_tuple() {
            if let Some(source_refer) = source.kind.as_object_reference() {
                if let Some(target_refer) = target.kind.as_object_reference() {
                    if source_refer.target == target_refer.target {
                        if self.c.is_empty_array_lit_ty(source) {
                            return Ternary::TRUE;
                        }
                        if let Some(result) = self.relate_variances(
                            source_refer.resolved_ty_args,
                            target_refer.resolved_ty_args,
                            report_error,
                            intersection_state,
                        ) {
                            return result;
                        }
                    }
                }
            }
        }

        let source_is_primitive = source.kind.is_primitive();

        let source = if self.relation != RelationKind::Identity {
            self.c.get_apparent_ty(source)
        } else {
            source
        };

        if source.kind.is_object_or_intersection() && target.kind.is_object() {
            let report_error = report_error && !source_is_primitive;

            let mut res = self.props_related_to(source, target, report_error, intersection_state);
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
            res
        } else {
            Ternary::FALSE
        }
    }

    fn recur_related_to(
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
            self.structured_related_to(source, target, report_error, intersection_state)
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
            if self.c.is_applicable_index_ty(info.key_ty, target.key_ty) != Ternary::FALSE {
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
        let target_has_string_index = index_infos.iter().any(|i| i.key_ty == self.c.string_ty());
        let mut result = Ternary::TRUE;
        for target_info in index_infos {
            let related = if self.relation != RelationKind::StrictSubtype
                && !source_is_primitive
                && target_has_string_index
                && target_info.val_ty.kind.is_any()
            {
                Ternary::TRUE
            } else {
                self.type_related_to_index_info(
                    source,
                    &target_info,
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

    fn sigs_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        kind: SigKind,
        report_error: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if self.relation == RelationKind::Identity {
            todo!()
        };

        if source == self.c.any_ty() || target == self.c.any_ty() {
            return Ternary::TRUE;
        }

        let source_sigs = {
            let Some(ty) = source.kind.as_object() else {
                return Ternary::TRUE;
            };
            self.c.get_signatures_of_type(source, kind)
        };
        let target_sigs = {
            let Some(ty) = target.kind.as_object() else {
                return Ternary::TRUE;
            };
            self.c.get_signatures_of_type(target, kind)
        };

        if kind == SigKind::Constructor && !source_sigs.is_empty() && !target_sigs.is_empty() {
            let source_is_abstract = source_sigs[0].flags.intersects(SigFlags::HAS_ABSTRACT);
            let target_is_abstract = target_sigs[0].flags.intersects(SigFlags::HAS_ABSTRACT);
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

    fn compare_sig_related(
        &mut self,
        mut source: &'cx Sig<'cx>,
        target: &'cx Sig<'cx>,
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
                            self.c.get_single_call_sig(source_ty)
                        };
                        let target_sig = if check_mode.intersects(SigCheckMode::CALLBACK) {
                            None
                        } else {
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
                        } else {
                            if !check_mode.intersects(SigCheckMode::CALLBACK) {
                                let res = compare(self, source_ty, target_ty, false);
                                if res == Ternary::FALSE {
                                    compare(self, target_ty, source_ty, false)
                                } else {
                                    res
                                }
                            } else {
                                compare(self, target_ty, source_ty, false)
                            }
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
            if target_ret_ty == self.c.any_ty() || target_ret_ty == self.c.void_ty() {
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
            let name = self.c.binder.symbol(*prop).name;
            if !self.c.is_known_prop(target_ty, name) {
                if report_error {
                    let span = self.c.p.node(prop.decl(self.c.binder)).span();
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

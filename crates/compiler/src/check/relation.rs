use bolt_ts_span::Span;
use rustc_hash::FxHashSet;

use super::errors;
use crate::ast;
use crate::atoms::AtomId;
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::ty::{self, SigFlags, SigKind};
use crate::ty::{ObjectShape, ObjectTy, ObjectTyKind, Sig, Ty, TyKind, Tys};

use super::{Ternary, TyChecker};

#[derive(Clone, Copy, Debug, PartialEq)]
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
    fn recur_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        report_error: bool,
    ) -> bool {
        self.structured_related_to(source, target, relation, report_error)
    }

    fn has_excess_properties(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        let source = source.kind.expect_object_lit();
        let Some(target) = target.kind.as_object_lit() else {
            return false;
        };
        for (name, symbol) in &self.binder.symbol(source.symbol).expect_object().members {
            if target.members.contains_key(name) {
                continue;
            } else {
                let span = self
                    .p
                    .node(self.binder.symbol(*symbol).expect_prop().decl)
                    .span();
                let field = self.atoms.get(name.expect_atom()).to_string();
                let error =
                    errors::ObjectLitMayOnlySpecifyKnownPropAndFieldDoesNotExist { span, field };
                self.push_error(span.module, Box::new(error));
                return true;
            }
        }

        false
    }

    fn is_simple_type_related_to(&self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        if source.kind.is_number_like() && target.kind.is_number() {
            true
        } else if source.kind.is_string_like() && target.kind.is_string() {
            true
        } else {
            source.kind.is_any()
        }
    }

    pub(super) fn is_type_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        if source.id == target.id {
            assert!(std::ptr::eq(&source.kind, &target.kind));
            return true;
        }
        if self.is_simple_type_related_to(source, target)
            || self.is_simple_type_related_to(target, source)
        {
            return true;
        }

        if source.kind.is_object() && target.kind.is_object() {
            // skip
        }

        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            self.check_type_related_to(source, target, relation, None)
        } else {
            false
        }
    }

    fn is_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        report_error: bool,
    ) -> bool {
        if source.id == target.id {
            return true;
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
                    return true;
                }
            }
        }

        if self.is_simple_type_related_to(source, target) {
            return true;
        }

        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            let is_performing_excess_property_check = source.kind.as_object_lit().is_some();
            if is_performing_excess_property_check && self.has_excess_properties(source, target) {
                return true;
            }
            self.recur_related_to(source, target, relation, report_error)
        } else {
            false
        }
    }

    fn props_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        report_error: bool,
    ) -> Ternary {
        let unmatched = self.get_unmatched_prop(source, target);
        if let Some((unmatched, target_symbol)) = unmatched {
            let symbol = self.binder.symbol(target_symbol);
            let span = if symbol.flags.intersects(SymbolFlags::CLASS) {
                self.p
                    .node(symbol.expect_class().decl)
                    .as_class_decl()
                    .unwrap()
                    .name
                    .span
            } else if symbol.flags.intersects(SymbolFlags::INTERFACE) {
                self.p
                    .node(symbol.expect_interface().decl)
                    .as_interface_decl()
                    .unwrap()
                    .name
                    .span
            } else {
                self.p.node(symbol.expect_object().decl).span()
            };
            if !unmatched.is_empty() && report_error {
                for name in unmatched {
                    let field = self.atoms.get(name).to_string();
                    let error = errors::PropertyXIsMissing { span, field };
                    self.push_error(span.module, Box::new(error));
                }
                return Ternary::TRUE;
            }

            Ternary::FALSE
        } else {
            Ternary::TRUE
        }
    }

    fn type_arg_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        self.is_related_to(source, target, relation, false)
    }

    fn relate_variances(
        &mut self,
        source: ty::Tys<'cx>,
        target: ty::Tys<'cx>,
        relation: RelationKind,
    ) -> Option<bool> {
        for i in 0..source.len() {
            let source = source[i];
            let target = target[i];
            if !self.is_related_to(source, target, relation, false) {
                return Some(false);
            }
        }
        Some(true)
    }

    fn is_empty_array_lit_ty(&self, ty: &'cx Ty<'cx>) -> bool {
        if ty.kind.is_array(self) {
            ty.kind
                .as_object_reference()
                .map(|refer| refer.resolved_ty_args[0].kind.is_ty_var())
                .unwrap_or_default()
        } else {
            false
        }
    }

    fn structured_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        report_error: bool,
    ) -> bool {
        if source.kind.is_union_or_intersection() || target.kind.is_union_or_intersection() {
            return self.union_or_intersection_related_to(source, target, relation, report_error);
        }

        if !source.kind.is_tuple() {
            if let Some(source_refer) = source.kind.as_object_reference() {
                if let Some(target_refer) = target.kind.as_object_reference() {
                    if source_refer.target == target_refer.target {
                        if self.is_empty_array_lit_ty(source) {
                            return true;
                        }
                        if let Some(result) = self.relate_variances(
                            source_refer.resolved_ty_args,
                            target_refer.resolved_ty_args,
                            relation,
                        ) {
                            return result;
                        }
                    }
                }
            }
        }

        let source_is_primitive = source.kind.is_primitive();

        let source = if relation != RelationKind::Identity {
            self.get_apparent_ty(source)
        } else {
            source
        };

        if source.kind.is_object_or_intersection() && target.kind.is_object() {
            let report_error = report_error && !source_is_primitive;

            let mut res = self.props_related_to(source, target, report_error);
            if res != Ternary::FALSE {
                // TODO: `sigs_related_to` with call tag
                res &= self.sigs_related_to(source, target, SigKind::Call, relation, report_error);
                if res != Ternary::FALSE {
                    res &= self.sigs_related_to(
                        source,
                        target,
                        SigKind::Constructor,
                        relation,
                        report_error,
                    );
                    if res != Ternary::FALSE {
                        res &= self.index_sig_related_to(source, target, relation);
                    }
                }
                res != Ternary::FALSE
            } else {
                false
            }
        } else {
            true
        }
    }

    fn index_info_related_to(
        &mut self,
        source: &'cx ty::IndexInfo<'cx>,
        target: &'cx ty::IndexInfo<'cx>,
        relation: RelationKind,
    ) -> bool {
        let res = self.is_related_to(source.val_ty, target.val_ty, relation, false);
        if !res {
            if source.key_ty == target.key_ty {
                let decl = self.binder.symbol(source.symbol).expect_index().decl;
                let span = self.p.node(decl).expect_index_sig_decl().ty.span();
                let error = errors::IndexSignaturesAreIncompatible {
                    span,
                    ty: target.val_ty.to_string(self),
                };
                self.push_error(span.module, Box::new(error));
            } else {
                todo!()
            }
        }
        true
    }

    fn type_related_to_index_info(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx ty::IndexInfo<'cx>,
        relation: RelationKind,
    ) -> Ternary {
        if let Some(source) = self.get_applicable_index_info(source, target.key_ty) {
            self.index_info_related_to(source, target, relation);
            Ternary::TRUE
        } else {
            Ternary::FALSE
        }
    }

    fn index_sig_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> Ternary {
        if relation == RelationKind::Identity {
            return Ternary::TRUE;
        }
        for info in self.index_infos_of_ty(target) {
            self.type_related_to_index_info(source, info, relation);
        }
        Ternary::TRUE
    }

    fn sigs_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        kind: SigKind,
        relation: RelationKind,
        report_error: bool,
    ) -> Ternary {
        if relation == RelationKind::Identity {
            todo!()
        };

        if source == self.any_ty() || target == self.any_ty() {
            return Ternary::TRUE;
        }

        let source_sigs = {
            let Some(ty) = source.kind.as_object() else {
                return Ternary::TRUE;
            };
            self.signatures_of_type(source, kind)
        };
        let target_sigs = {
            let Some(ty) = target.kind.as_object() else {
                return Ternary::TRUE;
            };
            self.signatures_of_type(target, kind)
        };

        if kind == SigKind::Constructor && !source_sigs.is_empty() && !target_sigs.is_empty() {
            let source_is_abstract = source_sigs[0].flags.intersects(SigFlags::HAS_ABSTRACT);
            let target_is_abstract = target_sigs[0].flags.intersects(SigFlags::HAS_ABSTRACT);
            if source_is_abstract && !target_is_abstract {
                return Ternary::FALSE;
            }
        }

        if source_sigs.len() == 1 && target_sigs.len() == 1 {
            let erase_generics = relation == RelationKind::Comparable;
            let source_sig = source_sigs[0];
            let target_sig = target_sigs[0];
            return self.sig_related_to(
                source_sig,
                target_sig,
                erase_generics,
                relation,
                report_error,
            );
        } else {
            for t in target_sigs {
                for s in source_sigs {
                    if self.sig_related_to(s, t, true, relation, report_error) != Ternary::FALSE {
                        continue;
                    }
                }
                return Ternary::FALSE;
            }
        }
        Ternary::TRUE
    }

    fn compare_sig_related(
        &mut self,
        source: &'cx Sig<'cx>,
        target: &'cx Sig<'cx>,
        check_mode: SigCheckMode,
        report_error: bool,
        compare: impl FnOnce(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>, bool) -> bool,
    ) -> Ternary {
        if source == target {
            return Ternary::TRUE;
        }

        let target_count = target.get_param_count(self);
        let source_has_more_params = !self.has_effective_rest_param(target)
            && (if check_mode.intersects(SigCheckMode::STRICT_ARITY) {
                self.has_effective_rest_param(source) || source.get_param_count(self) > target_count
            } else {
                self.get_min_arg_count(source) > target_count
            });
        if source_has_more_params {
            return Ternary::FALSE;
        }
        // if let Some(ty_params) = source.ty_params {
        //     if std::ptr::eq(ty_params, target.ty_params) {

        //     }
        // }
        let source_count = source.get_param_count(self);
        let source_rest_ty = source.get_non_array_rest_ty(self);
        let target_rest_ty = target.get_non_array_rest_ty(self);

        if !check_mode.intersects(SigCheckMode::IGNORE_RETURN_TYPES) {
            let ret_ty = |this: &mut Self, sig: &'cx ty::Sig<'cx>| {
                // TODO: cycle
                this.get_ret_ty_of_sig(sig)
            };
            let target_ret_ty = ret_ty(self, target);
            if target_ret_ty == self.any_ty() || target_ret_ty == self.void_ty() {
                return Ternary::TRUE;
            } else {
                let source_ret_ty = ret_ty(self, source);
                // TODO: ty_predict
                let res = if check_mode.intersects(SigCheckMode::BIVARIANT_CALLBACK) {
                    compare(self, source_ret_ty, target_ret_ty, false)
                } else {
                    compare(self, source_ret_ty, target_ret_ty, report_error)
                };

                if !res {
                    return Ternary::FALSE;
                }
            }
        }

        Ternary::TRUE
    }

    fn sig_related_to(
        &mut self,
        source: &'cx Sig<'cx>,
        target: &'cx Sig<'cx>,
        erase: bool,
        relation: RelationKind,
        report_error: bool,
    ) -> Ternary {
        let check_mode = if relation == RelationKind::Subtype {
            SigCheckMode::STRICT_TOP_SIGNATURE
        } else if relation == RelationKind::StrictSubtype {
            SigCheckMode::STRICT_TOP_SIGNATURE | SigCheckMode::STRICT_ARITY
        } else {
            SigCheckMode::empty()
        };

        self.compare_sig_related(
            source,
            target,
            check_mode,
            report_error,
            |this, source, target, report_error| {
                this.is_related_to(source, target, relation, report_error)
            },
        )
    }

    pub(super) fn check_type_assignable_to_and_optionally_elaborate(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            RelationKind::Assignable,
            error_node,
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
        );
    }

    pub(super) fn check_type_related_to_and_optionally_elaborate(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        error_node: Option<ast::NodeID>,
        error: impl FnOnce(&mut Self, Span, &'cx Ty<'cx>, &'cx Ty<'cx>) -> crate::Diag,
    ) -> bool {
        if self.is_type_related_to(source, target, relation) {
            return true;
        }
        if error_node.is_some() {
            if !self.check_type_related_to(source, target, relation, error_node) {
                let span = self.p.node(error_node.unwrap()).span();
                let error = error(self, span, source, target);
                self.push_error(span.module, error);
                false
            } else {
                true
            }
        } else {
            false
        }
    }

    pub(super) fn check_type_assignable_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) {
        self.check_type_related_to(source, target, RelationKind::Assignable, error_node);
    }

    fn check_type_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        error_node: Option<ast::NodeID>,
    ) -> bool {
        self.is_related_to(source, target, relation, error_node.is_some())
    }

    fn each_type_related_to_type(
        &mut self,
        source: &'cx Ty<'cx>,
        sources: Tys<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        report_error: bool,
    ) -> bool {
        let res = true;
        for (idx, source_ty) in sources.iter().enumerate() {
            // if idx <= targets.len() {
            //     let related = self.is_related_to(source_ty, targets[idx]);
            // }
            let related = self.is_related_to(source_ty, target, relation, report_error);
            if !related {
                return false;
            }
        }
        res
    }

    fn union_or_intersection_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        report_error: bool,
    ) -> bool {
        if let TyKind::Union(s) = source.kind {
            // if let TyKind::Union(t) = target.kind {
            // } else {
            // }
            self.each_type_related_to_type(source, s.tys, target, relation, report_error)
        } else {
            false
        }
    }

    fn get_props_of_object_ty(
        &self,
        self_ty: &'cx ty::Ty<'cx>,
        ty: &'cx ObjectTy<'cx>,
    ) -> &'cx [SymbolID] {
        if let ObjectTyKind::Interface(_) = ty.kind {
            self.properties_of_object_type(self_ty)
        } else if let ObjectTyKind::ObjectLit(ty) = ty.kind {
            ObjectShape::props(ty)
        } else if let ObjectTyKind::Tuple(ty) = ty.kind {
            ObjectShape::props(ty)
        } else if let ObjectTyKind::Reference(_) = ty.kind {
            self.properties_of_object_type(self_ty)
        } else if let ObjectTyKind::Anonymous(_) = ty.kind {
            self.properties_of_object_type(self_ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_props_of_ty(&mut self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        self.resolve_structured_type_members(ty);
        if let TyKind::Object(object_ty) = ty.kind {
            self.get_props_of_object_ty(ty, object_ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_prop_of_ty(
        &mut self,
        ty: &'cx Ty<'cx>,
        name: SymbolName,
    ) -> Option<SymbolID> {
        self.resolve_structured_type_members(ty);
        let TyKind::Object(object_ty) = ty.kind else {
            return None;
        };

        if object_ty.kind.as_interface().is_some() {
            self.ty_structured_members[&ty.id]
                .members
                .get(&name)
                .copied()
        } else if let Some(ty) = object_ty.kind.as_object_lit() {
            ObjectShape::get_member(ty, &name)
        } else if let Some(ty) = object_ty.kind.as_tuple() {
            ObjectShape::get_member(ty, &name)
        } else if let Some(refer) = object_ty.kind.as_reference() {
            if !self.ty_structured_members.contains_key(&ty.id) {
                self.get_prop_of_ty(refer.target, name)
            } else {
                self.ty_structured_members[&ty.id]
                    .members
                    .get(&name)
                    .copied()
            }
        } else if let Some(_) = object_ty.kind.as_anonymous() {
            self.ty_structured_members[&ty.id]
                .members
                .get(&name)
                .copied()
        } else {
            unreachable!("ty: {ty:#?}")
        }
    }

    pub fn get_unmatched_prop(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> Option<(FxHashSet<AtomId>, SymbolID)> {
        self.get_unmatched_props(source, target)
    }

    fn get_unmatched_props(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> Option<(FxHashSet<AtomId>, SymbolID)> {
        let symbols = self.get_props_of_ty(target);
        let set: FxHashSet<_> = symbols
            .iter()
            .filter_map(|symbol| {
                let name = self.binder.symbol(*symbol).name;
                if self.get_prop_of_ty(source, name).is_none() {
                    Some(name.expect_atom())
                } else {
                    None
                }
            })
            .collect();
        if set.is_empty() {
            None
        } else {
            let ty = target.kind.expect_object();
            fn recur(ty: &ObjectTy) -> SymbolID {
                match ty.kind {
                    ObjectTyKind::Reference(ty) => recur(ty.target.kind.expect_object()),
                    ObjectTyKind::Interface(ty) => ty.symbol,
                    ObjectTyKind::ObjectLit(ty) => ty.symbol,
                    ObjectTyKind::Anonymous(ty) => ty.symbol,
                    _ => unreachable!(),
                }
            }
            let symbol = recur(ty);
            Some((set, symbol))
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

use bolt_ts_span::Span;
use rustc_hash::FxHashSet;

use super::errors;
use crate::atoms::AtomId;
use crate::bind::{SymbolFlags, SymbolID, SymbolName};
use crate::ty;
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

impl<'cx> TyChecker<'cx> {
    fn recur_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        self.structured_related_to(source, target, relation)
    }

    fn has_excess_properties(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        let source = source.kind.expect_object_lit();
        let Some(target) = target.kind.as_object_lit() else {
            return false;
        };
        for (name, symbol) in &self.binder.symbol(source.symbol).expect_object().members {
            if target.members.contains_key(&name) {
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

        return false;
    }

    fn is_simple_type_related_to(&self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        if source.kind.is_number_like() && target.kind.is_number() {
            true
        } else if source.kind.is_string_like() && target.kind.is_string() {
            true
        } else if source.kind.is_any() {
            true
        } else {
            false
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
            self.check_type_related_to(source, target, relation)
        } else {
            false
        }
    }

    fn is_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
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
            self.recur_related_to(source, target, relation)
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
        self.is_related_to(source, target, relation)
    }

    fn relate_variances(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> Option<bool> {
        Some(self.type_arg_related_to(source, target, relation))
    }

    fn is_empty_array_lit_ty(&self, ty: &'cx Ty<'cx>) -> bool {
        if ty.kind.is_array(self) {
            ty.kind
                .as_object_reference()
                .map(|refer| refer.ty_args[0].kind.is_ty_var())
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
    ) -> bool {
        if source.kind.is_union_or_intersection() || target.kind.is_union_or_intersection() {
            return self.union_or_intersection_related_to(source, target, relation);
        }

        if source.kind.is_array(self) && target.kind.is_array(self) {
            if self.is_empty_array_lit_ty(source) {
                return true;
            }
            let actual = source
                .kind
                .as_object_reference()
                .map(|refer| refer.ty_args[0])
                .unwrap();
            let expect = target
                .kind
                .as_object_reference()
                .map(|refer| refer.ty_args[0])
                .unwrap();
            if let Some(result) = self.relate_variances(actual, expect, relation) {
                return result;
            }
        }

        let source_is_primitive = source.kind.is_primitive();

        let source = if relation != RelationKind::Identity {
            self.get_apparent_ty(source)
        } else {
            source
        };

        if source.kind.is_object_or_intersection() && target.kind.is_object() {
            let report_error = !source_is_primitive;
            let mut res = self.props_related_to(source, target, report_error);
            if res != Ternary::FALSE {
                // TODO: `sigs_related_to` with call tag
                res &= self.sigs_related_to(source, target);
                res &= self.index_sig_related_to(source, target, relation);
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
        let res = self.is_related_to(source.val_ty, target.val_ty, relation);
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
        for info in self.index_infos(target) {
            self.type_related_to_index_info(source, info, relation);
        }
        Ternary::TRUE
    }

    fn sigs_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> Ternary {
        if source == self.any_ty() || target == self.any_ty() {
            return Ternary::TRUE;
        }

        let source_sigs = {
            let Some(ty) = source.kind.as_object() else {
                return Ternary::TRUE;
            };
            self.get_sigs_of_ty(ty)
        };
        let target_sigs = {
            let Some(ty) = target.kind.as_object() else {
                return Ternary::TRUE;
            };
            self.get_sigs_of_ty(ty)
        };

        for t in target_sigs {
            for s in source_sigs {
                if self.sig_related_to(s, t) != Ternary::FALSE {
                    continue;
                }
            }
            return Ternary::FALSE;
        }

        Ternary::TRUE
    }

    fn sig_related_to(&mut self, source: &'cx Sig<'cx>, target: &'cx Sig<'cx>) -> Ternary {
        // TODO:
        Ternary::FALSE
    }

    pub(super) fn check_type_assignable_to_and_optionally_elaborate(
        &mut self,
        span: Span,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) {
        self.check_type_related_to_and_optionally_elaborate(
            span,
            source,
            target,
            RelationKind::Assignable,
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
        span: Span,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        error: impl FnOnce(&mut Self, Span, &'cx Ty<'cx>, &'cx Ty<'cx>) -> crate::Diag,
    ) {
        if !self.is_type_related_to(source, target, relation) {
            let err = error(self, span, source, target);
            self.push_error(span.module, err);
        }
    }

    pub(super) fn check_type_assignable_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) {
        self.check_type_related_to(source, target, RelationKind::Assignable);
    }

    fn check_type_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        self.is_related_to(source, target, relation)
    }

    fn each_type_related_to_type(
        &mut self,
        source: &'cx Ty<'cx>,
        sources: Tys<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        let mut res = true;
        for (idx, source_ty) in sources.iter().enumerate() {
            // if idx <= targets.len() {
            //     let related = self.is_related_to(source_ty, targets[idx]);
            // }
            let related = self.is_related_to(source_ty, target, relation);
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
    ) -> bool {
        if let TyKind::Union(s) = source.kind {
            // if let TyKind::Union(t) = target.kind {
            // } else {
            // }
            self.each_type_related_to_type(source, s.tys, target, relation)
        } else {
            false
        }
    }

    fn get_props_of_object_ty(&self, ty: &'cx ObjectTy<'cx>) -> &'cx [SymbolID] {
        if let ObjectTyKind::Interface(ty) = ty.kind {
            ObjectShape::props(ty)
        } else if let ObjectTyKind::ObjectLit(ty) = ty.kind {
            ObjectShape::props(ty)
        } else if let ObjectTyKind::Tuple(ty) = ty.kind {
            ObjectShape::props(ty)
        } else if let ObjectTyKind::Reference(ty) = ty.kind {
            let target = ty.target.kind.expect_object();
            self.get_props_of_object_ty(target)
        } else {
            &[]
        }
    }

    fn get_sigs_of_ty(&mut self, ty: &'cx ObjectTy<'cx>) -> &'cx [&'cx Sig<'cx>] {
        if let ObjectTyKind::Interface(ty) = ty.kind {
            ObjectShape::declared_call_sigs(ty)
        } else if let ObjectTyKind::ObjectLit(ty) = ty.kind {
            ObjectShape::declared_call_sigs(ty)
        } else if let ObjectTyKind::Tuple(ty) = ty.kind {
            ObjectShape::declared_call_sigs(ty)
        } else if let ObjectTyKind::Fn(ty) = ty.kind {
            ObjectShape::declared_call_sigs(ty)
        } else if let ObjectTyKind::Reference(ty) = ty.kind {
            let target = ty.target.kind.expect_object();
            self.get_sigs_of_ty(target)
        } else {
            &[]
        }
    }

    fn get_props_of_ty(&self, ty: &'cx Ty<'cx>) -> &'cx [SymbolID] {
        if let TyKind::Object(ty) = ty.kind {
            self.get_props_of_object_ty(ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_prop_of_ty(&self, ty: &'cx Ty<'cx>, name: SymbolName) -> Option<SymbolID> {
        let TyKind::Object(ty) = ty.kind else {
            return None;
        };

        if let Some(ty) = ty.kind.as_interface() {
            ObjectShape::get_member(ty, &name)
        } else if let Some(ty) = ty.kind.as_object_lit() {
            ObjectShape::get_member(ty, &name)
        } else if let Some(ty) = ty.kind.as_tuple() {
            ObjectShape::get_member(ty, &name)
        } else if let Some(ty) = ty.kind.as_reference() {
            self.get_prop_of_ty(ty.target, name)
        } else {
            unreachable!("ty: {ty:#?}")
        }
    }

    fn get_unmatched_prop(
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

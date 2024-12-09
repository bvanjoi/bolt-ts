use bolt_ts_span::Span;
use rustc_hash::FxHashSet;

use crate::atoms::AtomId;
use crate::bind::{SymbolID, SymbolName};
use crate::errors;
use crate::ty::{ObjectLikeTy, ObjectTy, ObjectTyKind, Ty, TyKind, Tys};

use super::TyChecker;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum RelationKind {
    Assignable,
    StrictSubtype,
    Identity,
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
        for (name, symbol) in &self
            .binder
            .symbol(source.symbol)
            .kind
            .expect_object()
            .members
        {
            if target.members.contains_key(&name) {
                continue;
            } else {
                let span = self
                    .p
                    .node(self.binder.symbol(*symbol).kind.expect_prop())
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
    ) -> bool {
        let unmatched = self.get_unmatched_prop(source, target);
        if let Some((unmatched, target_symbol)) = unmatched {
            let symbol = self.binder.symbol(target_symbol);
            let decl = if let Some(symbol) = symbol.kind.as_class() {
                symbol.decl
            } else {
                symbol.kind.expect_object().decl
            };
            if !unmatched.is_empty() && report_error {
                for name in unmatched {
                    let span = self.p.node(decl).span();
                    let field = self.atoms.get(name).to_string();
                    let error = errors::PropertyXIsMissing { span, field };
                    self.push_error(span.module, Box::new(error));
                }
                return true;
            }

            false
        } else {
            true
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
        if let Some(ty) = ty.kind.as_object_array() {
            ty.ty.kind.is_ty_var()
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

        if let Some(actual) = source.kind.as_object_array() {
            if let Some(expect) = target.kind.as_object_array() {
                if self.is_empty_array_lit_ty(source) {
                    return true;
                } else if let Some(result) = self.relate_variances(actual.ty, expect.ty, relation) {
                    return result;
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
            let report_error = !source_is_primitive;
            self.props_related_to(source, target, report_error)
        } else {
            true
        }
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
                if let TyKind::NumberLit(source) = source.kind {
                    if let TyKind::NumberLit(target) = target.kind {
                        return Box::new(errors::TypeIsNotAssignableToType {
                            span,
                            ty1: source.val.to_string(),
                            ty2: target.val.to_string(),
                        });
                    }
                }
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
            ObjectLikeTy::props(ty)
        } else if let ObjectTyKind::ObjectLit(ty) = ty.kind {
            ObjectLikeTy::props(ty)
        } else if let ObjectTyKind::Tuple(ty) = ty.kind {
            ObjectLikeTy::props(ty)
        } else {
            unreachable!()
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
        let members = if let Some(ty) = ty.kind.as_interface() {
            ObjectLikeTy::members(ty)
        } else if let Some(ty) = ty.kind.as_object_lit() {
            ObjectLikeTy::members(ty)
        } else if let Some(ty) = ty.kind.as_tuple() {
            ObjectLikeTy::members(ty)
        } else {
            unreachable!()
        };
        members.get(&name).map(|symbol| *symbol)
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
            let symbol = if let ObjectTyKind::Interface(ty) = ty.kind {
                ty.symbol
            } else if let ObjectTyKind::ObjectLit(ty) = ty.kind {
                ty.symbol
            } else {
                unreachable!()
            };
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

use rts_span::Span;
use rustc_hash::FxHashSet;

use crate::{
    atoms::AtomId,
    bind::SymbolID,
    errors,
    ty::{Ty, TyKind, Tys},
};

use super::TyChecker;

#[derive(Clone, Copy, Debug)]
pub(super) enum RelationKind {
    Assignable,
    StrictSubtype,
}

impl<'cx> TyChecker<'cx> {
    fn recur_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        self.structured_related_to(source, target)
    }

    fn has_excess_properties(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        let Some(source) = source.kind.as_object_lit() else {
            unreachable!()
        };
        let Some(target) = target.kind.as_object_lit() else {
            return false;
        };
        for (name, symbol) in &self.symbols.get(source.symbol).kind.expect_object().members {
            let name = name.expect_atom();
            if target.members.contains_key(&name) {
                continue;
            } else {
                let span = self
                    .nodes
                    .get(self.symbols.get(*symbol).kind.expect_prop())
                    .span();
                let field = self.atoms.get(name).to_string();
                let error =
                    errors::ObjectLitMayOnlySpecifyKnownPropAndFieldDoesNotExist { span, field };
                self.push_error(span.module, Box::new(error));
                return true;
            }
        }

        return false;
    }

    fn is_simple_type_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
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
            self.check_type_related_to(source, target)
        } else {
            false
        }
    }
    fn is_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        if source.id == target.id {
            return true;
        }

        if source.kind.definitely_non_nullable() && target.kind.is_union() {
            let TyKind::Union(t) = target.kind else {
                unreachable!()
            };
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

        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            let is_performing_excess_property_check = source.kind.as_object_lit().is_some();
            if is_performing_excess_property_check && self.has_excess_properties(source, target) {
                return true;
            }
            self.recur_related_to(source, target)
        } else {
            false
        }
    }

    fn props_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        let unmatched = self.get_unmatched_prop(source, target);
        if let Some((unmatched, target_symbol)) = unmatched {
            let target = self.symbols.get(target_symbol).kind.expect_object();
            for name in unmatched {
                let span = self.nodes.get(target.decl).span();
                let field = self.atoms.get(name).to_string();
                let error = errors::PropertyXIsMissing { span, field };
                self.push_error(span.module, Box::new(error));
            }
            true
        } else {
            true
        }
    }

    fn type_arg_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        self.is_related_to(source, target)
    }

    fn relate_variances(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        self.type_arg_related_to(source, target)
    }

    fn structured_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        if source.kind.is_union_or_intersection() || target.kind.is_union_or_intersection() {
            return self.union_or_intersection_related_to(source, target);
        }

        if let Some(source) = source.kind.as_array() {
            if let Some(target) = target.kind.as_array() {
                let result = self.relate_variances(source.ty, target.ty);
                if result {
                    return result;
                }
            }
        }

        if source.kind.is_object_or_intersection() && target.kind.is_object() {
            self.props_related_to(source, target)
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

    fn check_type_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        self.is_related_to(source, target)
    }

    fn each_type_related_to_type(
        &mut self,
        source: &'cx Ty<'cx>,
        sources: Tys<'cx>,
        target: &'cx Ty<'cx>,
        targets: Tys<'cx>,
    ) -> bool {
        let mut res = true;
        for (idx, source_ty) in sources.iter().enumerate() {
            // if idx <= targets.len() {
            //     let related = self.is_related_to(source_ty, targets[idx]);
            // }
            let related = self.is_related_to(source_ty, target);
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
    ) -> bool {
        if let TyKind::Union(s) = source.kind {
            if let TyKind::Union(t) = target.kind {
                self.each_type_related_to_type(source, s.tys, target, t.tys)
            } else {
                false
            }
        } else {
            false
        }
    }

    fn get_unmatched_prop(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> Option<(FxHashSet<AtomId>, SymbolID)> {
        let Some(target) = target.kind.as_object_lit() else {
            unreachable!()
        };
        let set: FxHashSet<_> = target
            .members
            .iter()
            .filter_map(|(name, _)| {
                if let Some(source) = source.kind.as_object_lit() {
                    if !source.members.contains_key(&name) {
                        return Some(*name);
                    }
                }
                None
            })
            .collect();
        if set.is_empty() {
            None
        } else {
            Some((set, target.symbol))
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

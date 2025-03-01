use bolt_ts_ast::keyword;
use bolt_ts_atom::AtomId;

use super::TyChecker;
use crate::ty::{self, TypeFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn is_valid_index_key_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.flags
            .intersects(TypeFlags::STRING | TypeFlags::NUMBER | TypeFlags::ES_SYMBOL)
            || ty.kind.as_intersection().is_some_and(|i| {
                !self.is_generic(ty) && i.tys.iter().any(|ty| self.is_valid_index_key_ty(ty))
            })
            || ty.is_pattern_lit_ty()
    }

    pub(super) fn is_valid_base_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if let Some(param_ty) = ty.kind.as_param() {
            if let Some(constraint) = self.get_base_constraint_of_ty(ty) {
                return self.is_valid_base_ty(constraint);
            }
        }

        if ty.kind.is_object()
            || ty
                .flags
                .intersects(TypeFlags::NON_PRIMITIVE | TypeFlags::ANY)
        {
            true
            // TODO: !is_generic_mapped_ty
        } else {
            ty.kind.is_intersection()
        }
    }

    pub(super) fn is_valid_number_string(&self, v: AtomId, round_trip_only: bool) -> bool {
        if v == keyword::IDENT_EMPTY {
            false
        } else {
            let n = self.atoms.get(v);
            // TODO: NaN, Infinity...
            n.parse::<f64>().is_ok()
        }
    }

    pub(super) fn is_valid_ty_for_template_lit_placeholder(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        if let Some(i) = target.kind.as_intersection() {
            i.tys.iter().all(|t| {
                *t == self.empty_ty_literal_ty()
                    || self.is_valid_ty_for_template_lit_placeholder(source, t)
            })
        } else if target.flags.intersects(TypeFlags::STRING)
            || self.is_type_assignable_to(source, target)
        {
            true
        } else if let Some(s) = source.kind.as_string_lit() {
            let v = s.val;
            (target.flags.intersects(TypeFlags::NUMBER) && self.is_valid_number_string(v, false))
                || (target.flags.intersects(TypeFlags::BIG_INT) && false/* TODO: handle bigint */)
                || (target
                    .flags
                    .intersects(TypeFlags::BOOLEAN_LITERAL | TypeFlags::NULLABLE)
                    && v == target.intrinsic_name().unwrap())
                || (target.flags.intersects(TypeFlags::STRING_MAPPING) && false/* TODO: handle string mapping */)
                || (target.flags.intersects(TypeFlags::TEMPLATE_LITERAL)
                    && self.is_ty_matched_by_template_lit_ty(source, target))
        } else if let Some(s) = source.kind.as_template_lit_ty() {
            let texts = s.texts;
            texts.len() == 2
                && texts[0] == keyword::IDENT_EMPTY
                && texts[0] == keyword::IDENT_EMPTY
                && self.is_type_assignable_to(s.tys[0], target)
        } else {
            false
        }
    }
}

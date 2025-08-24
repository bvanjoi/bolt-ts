use super::TyChecker;
use crate::ty::{self, TypeFlags};

impl<'cx> TyChecker<'cx> {
    fn get_base_type_of_literal_type_union(
        &mut self,
        ty: &'cx ty::UnionTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let tys = ty
            .tys
            .iter()
            .map(|ty| self.get_base_ty_of_literal_ty(ty))
            .collect::<Vec<_>>();
        self.get_union_ty(&tys, ty::UnionReduction::None, false, None, None)
    }

    pub(super) fn get_base_ty_of_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(TypeFlags::ENUM_LIKE) {
            // TODO: get_base_ty_of_enum_like_ty(ty)
            ty
        } else if ty.flags.intersects(
            TypeFlags::STRING_LITERAL
                .union(TypeFlags::TEMPLATE_LITERAL)
                .union(TypeFlags::STRING_MAPPING),
        ) {
            self.string_ty
        } else if ty.flags.intersects(TypeFlags::NUMBER_LITERAL) {
            self.number_ty
        } else if ty.flags.intersects(TypeFlags::BIG_INT_LITERAL) {
            self.bigint_ty
        } else if ty.flags.intersects(TypeFlags::BOOLEAN_LITERAL) {
            self.boolean_ty()
        } else if let Some(union) = ty.kind.as_union() {
            debug_assert!(ty.flags.intersects(TypeFlags::UNION));
            self.get_base_type_of_literal_type_union(union)
        } else {
            ty
        }
    }

    pub(super) fn get_base_ty_of_literal_ty_for_comparison(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(
            TypeFlags::STRING_LITERAL
                .union(TypeFlags::TEMPLATE_LITERAL)
                .union(TypeFlags::STRING_MAPPING),
        ) {
            self.string_ty
        } else if ty
            .flags
            .intersects(TypeFlags::NUMBER_LITERAL.union(TypeFlags::ENUM))
        {
            self.number_ty
        } else if ty.flags.intersects(TypeFlags::BIG_INT_LITERAL) {
            self.bigint_ty
        } else if ty.flags.intersects(TypeFlags::BOOLEAN_LITERAL) {
            self.boolean_ty()
        } else if ty.flags.intersects(TypeFlags::UNION) {
            self.map_ty(
                ty,
                |this, t| Some(this.get_base_ty_of_literal_ty_for_comparison(t)),
                false,
            )
            .unwrap()
        } else {
            ty
        }
    }
}

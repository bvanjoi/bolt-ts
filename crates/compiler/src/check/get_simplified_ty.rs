use crate::ty::{self, TypeFlags};

use super::{TyChecker, create_ty::IntersectionFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_simplified_ty_or_constraint(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let simplified = self.get_simplified_ty(ty, false);
        if simplified != ty {
            Some(simplified)
        } else {
            self.get_constraint_of_ty(ty)
        }
    }

    pub(super) fn get_simplified_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        writing: bool,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(indexed_access_ty) = ty.kind.as_indexed_access() {
            self.get_simplified_index_access_ty(ty, indexed_access_ty, writing)
        } else if ty.kind.as_cond_ty().is_some() {
            self.get_simplified_cond_ty(ty, writing)
        } else {
            ty
        }
    }

    fn get_simplified_cond_ty(&mut self, ty: &'cx ty::Ty<'cx>, writing: bool) -> &'cx ty::Ty<'cx> {
        let cond_ty = ty.kind.expect_cond_ty();
        let true_ty = self.get_true_ty_from_cond_ty(ty, cond_ty);
        let false_ty = self.get_false_ty_from_cond_ty(ty, cond_ty);
        if false_ty.flags.intersects(TypeFlags::NEVER)
            && self.get_actual_ty_variable(true_ty) == self.get_actual_ty_variable(cond_ty.check_ty)
        {
            if cond_ty.check_ty.flags.intersects(TypeFlags::ANY) || {
                let source = self.get_restrictive_instantiation(cond_ty.check_ty);
                let target = self.get_restrictive_instantiation(cond_ty.extends_ty);
                self.is_type_assignable_to(source, target)
            } {
                return self.get_simplified_ty(ty, writing);
            } else if self.is_intersection_empty(cond_ty.check_ty, cond_ty.extends_ty) {
                return self.never_ty;
            }
        } else if true_ty.flags.intersects(TypeFlags::NEVER)
            && self.get_actual_ty_variable(false_ty)
                == self.get_actual_ty_variable(cond_ty.check_ty)
        {
            if !cond_ty.check_ty.flags.intersects(TypeFlags::ANY) && {
                let source = self.get_restrictive_instantiation(cond_ty.check_ty);
                let target = self.get_restrictive_instantiation(cond_ty.extends_ty);
                self.is_type_assignable_to(source, target)
            } {
                return self.never_ty;
            } else if cond_ty.check_ty.flags.intersects(TypeFlags::ANY)
                || self.is_intersection_empty(cond_ty.check_ty, cond_ty.extends_ty)
            {
                return self.get_simplified_ty(ty, writing);
            }
        }
        ty
    }

    fn is_intersection_empty(&mut self, ty1: &'cx ty::Ty<'cx>, ty2: &'cx ty::Ty<'cx>) -> bool {
        let i = self.get_intersection_ty(&[ty1, ty2], IntersectionFlags::None, None, None);
        let u = self.get_union_ty(&[i, self.never_ty], ty::UnionReduction::Lit);
        u.flags.intersects(TypeFlags::NEVER)
    }

    fn get_simplified_index_access_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        indexed_access_ty: &'cx ty::IndexedAccessTy<'cx>,
        writing: bool,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: cache
        let object_ty = self.get_simplified_ty(indexed_access_ty.object_ty, writing);
        let index_ty = self.get_simplified_ty(indexed_access_ty.index_ty, writing);

        // if !index_ty.flags.intersects(TypeFlags::INSTANTIABLE) {}

        if object_ty.kind.is_generic_tuple_type()
            && index_ty.flags.intersects(TypeFlags::NUMBER_LIKE)
        {
            // TODO:
        }

        if self.is_generic_mapped_ty(object_ty)
            && self.get_mapped_ty_name_ty_kind(object_ty) != ty::MappedTyNameTyKind::Remapping
        {
            let ty = self.substitute_indexed_mapped_ty(object_ty, indexed_access_ty.index_ty);
            self.map_ty(
                ty,
                |this, t| Some(this.get_simplified_ty(t, writing)),
                false,
            )
            .unwrap()
        } else {
            ty
        }
        // TODO: cache
    }
}

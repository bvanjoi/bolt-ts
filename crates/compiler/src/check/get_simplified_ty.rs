use crate::ty::{self, TypeFlags};

use super::TyChecker;

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
        &self,
        ty: &'cx ty::Ty<'cx>,
        writing: bool,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(indexed_access_ty) = ty.kind.as_indexed_access() {
            self.get_simplified_index_access_ty(ty, indexed_access_ty, writing)
        } else if ty.kind.as_cond_ty().is_some() {
            // TODO: handle indexed_cond
            ty
        } else {
            ty
        }
    }

    fn get_simplified_index_access_ty(
        &self,
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

        return ty;
    }
}

use super::TyChecker;
use crate::ty;

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
        self.create_union_type(tys)
    }

    pub(super) fn get_base_ty_of_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_number_lit() {
            self.number_ty()
        } else if ty.kind.is_string_lit() {
            self.string_ty()
        } else if let Some(union) = ty.kind.as_union() {
            self.get_base_type_of_literal_type_union(union)
        } else {
            ty
        }
    }
}

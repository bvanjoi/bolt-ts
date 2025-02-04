use super::TyChecker;
use crate::ty;

#[derive(Debug, Clone, Copy)]
pub(super) struct PermissiveMapper;

impl<'cx> ty::TyMap<'cx> for PermissiveMapper {
    fn get_mapped_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        checker: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_param() {
            checker.wildcard_ty
        } else {
            ty
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct RestrictiveMapper;

impl<'cx> ty::TyMap<'cx> for RestrictiveMapper {
    fn get_mapped_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        checker: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(param) = ty.kind.as_param() else {
            return ty;
        };
        let use_self = if let Some(c) = checker.param_ty_constraint(ty) {
            c == checker.no_constraint_ty()
        } else {
            checker.get_constraint_decl(ty).is_none()
        };
        if use_self {
            ty
        } else if let Some(restrictive_instantiation) =
            checker.get_ty_links(ty.id).get_restrictive_instantiation()
        {
            restrictive_instantiation
        } else {
            let restrictive_instantiation =
                checker.create_param_ty(param.symbol, param.offset, false);
            let no_constraint_ty = checker.no_constraint_ty();
            checker
                .get_mut_ty_links(restrictive_instantiation.id)
                .set_param_ty_constraint(no_constraint_ty);
            checker
                .get_mut_ty_links(ty.id)
                .set_restrictive_instantiation(restrictive_instantiation);
            restrictive_instantiation
        }
    }
}

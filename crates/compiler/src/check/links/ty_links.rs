use super::links;
use crate::ty;

links!(
    TyLinks,
    (structured_members, &'cx ty::StructuredMembers<'cx>),
    (resolved_base_tys, ty::Tys<'cx>),
    (resolved_base_ctor_ty, &'cx ty::Ty<'cx>),
    (param_ty_mapper, &'cx ty::TyMapper<'cx>),
);

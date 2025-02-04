use super::links;
use crate::ty;

links!(
    TyLinks,
    (structured_members, &'cx ty::StructuredMembers<'cx>),
    (resolved_base_tys, ty::Tys<'cx>),
    (resolved_base_ctor_ty, &'cx ty::Ty<'cx>),
    (resolved_base_constraint, &'cx ty::Ty<'cx>),
    (resolved_string_index_ty, &'cx ty::Ty<'cx>),
    (resolved_index_ty, &'cx ty::Ty<'cx>),
    (immediate_base_constraint, &'cx ty::Ty<'cx>),
    (param_ty_mapper, &'cx dyn ty::TyMap<'cx>),
    (param_ty_constraint, &'cx ty::Ty<'cx>),
    (non_existent_prop_checked, bool),
    (literal_ty, &'cx ty::Ty<'cx>),
    (permissive_instantiation, &'cx ty::Ty<'cx>),
    (restrictive_instantiation, &'cx ty::Ty<'cx>)
);

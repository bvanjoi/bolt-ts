use super::links;
use crate::ty;

// TODO: split them into type def.
links!(
    TyLinks,
    (structured_members, &'cx ty::StructuredMembers<'cx>),
    (resolved_base_tys, ty::Tys<'cx>),
    (resolved_base_ctor_ty, &'cx ty::Ty<'cx>),
    (resolved_base_constraint, &'cx ty::Ty<'cx>),
    (resolved_string_index_ty, &'cx ty::Ty<'cx>),
    (resolved_index_ty, &'cx ty::Ty<'cx>),
    (resolved_ty_args, ty::Tys<'cx>),
    (immediate_base_constraint, &'cx ty::Ty<'cx>),
    // parameter type
    (param_ty_mapper, &'cx dyn ty::TyMap<'cx>),
    (param_ty_constraint, &'cx ty::Ty<'cx>),
    (non_existent_prop_checked, bool),
    // reference type
    (literal_ty, &'cx ty::Ty<'cx>),
    // conditional type
    (resolved_default_constraint, &'cx ty::Ty<'cx>),
    (resolved_inferred_true_ty, &'cx ty::Ty<'cx>),
    (resolved_true_ty, &'cx ty::Ty<'cx>),
    (resolved_false_ty, &'cx ty::Ty<'cx>),
    (resolved_constraint_of_distribute, Option<&'cx ty::Ty<'cx>>),
    // mapped type
    (named_ty, &'cx ty::Ty<'cx>),
    //
    (permissive_instantiation, &'cx ty::Ty<'cx>),
    (restrictive_instantiation, &'cx ty::Ty<'cx>)
);

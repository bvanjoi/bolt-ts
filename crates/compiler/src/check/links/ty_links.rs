use super::links;
use crate::bind::SymbolID;
use crate::ty;

// TODO: split them into type def.
links!(
    TyLinks,
    (structured_members, &'cx ty::StructuredMembers<'cx>),
    (resolved_base_tys, ty::Tys<'cx>),
    (resolved_base_ctor_ty, &'cx ty::Ty<'cx>),
    (resolved_base_constraint, &'cx ty::Ty<'cx>),
    (resolved_properties, &'cx [SymbolID]),
    (resolved_index_ty, &'cx ty::Ty<'cx>),
    (resolved_string_index_ty, &'cx ty::Ty<'cx>),
    (resolved_ty_args, ty::Tys<'cx>),
    (immediate_base_constraint, &'cx ty::Ty<'cx>),
    // fresh type
    (fresh_ty, &'cx ty::Ty<'cx>),
    (regular_ty, &'cx ty::Ty<'cx>),
    // parameter type
    (param_ty_mapper, &'cx dyn ty::TyMap<'cx>),
    (param_ty_constraint, &'cx ty::Ty<'cx>),
    (non_existent_prop_checked, bool),
    (default, &'cx ty::Ty<'cx>),
    // reference type
    (literal_ty, &'cx ty::Ty<'cx>),
    // conditional type
    (resolved_default_constraint, &'cx ty::Ty<'cx>),
    (resolved_inferred_true_ty, &'cx ty::Ty<'cx>),
    (resolved_true_ty, &'cx ty::Ty<'cx>),
    (resolved_false_ty, &'cx ty::Ty<'cx>),
    (resolved_constraint_of_distribute, Option<&'cx ty::Ty<'cx>>),
    // mapped type
    (mapped_ty_param, &'cx ty::Ty<'cx>),
    (mapped_constraint_ty, &'cx ty::Ty<'cx>),
    (mapped_named_ty, &'cx ty::Ty<'cx>),
    (mapped_key_ty, &'cx ty::Ty<'cx>),
    (mapped_template_ty, &'cx ty::Ty<'cx>),
    (mapped_modifiers_ty, &'cx ty::Ty<'cx>),
    (mapped_contains_error, bool),
    //
    (permissive_instantiation, &'cx ty::Ty<'cx>),
    (restrictive_instantiation, &'cx ty::Ty<'cx>)
);

use super::links;
use crate::ty;
use bolt_ts_binder::SymbolID;

// TODO: split them into type def.
links!(
    TyLinks,
    (structured_members, &'cx ty::StructuredMembers<'cx>),
    (resolved_base_tys, ty::Tys<'cx>),
    (base_tys_resolved, bool),
    (resolved_base_ctor_ty, &'cx ty::Ty<'cx>),
    (resolved_base_constraint, &'cx ty::Ty<'cx>),
    (resolved_properties, &'cx [SymbolID]),
    (resolved_index_ty, &'cx ty::Ty<'cx>),
    (resolved_string_index_ty, &'cx ty::Ty<'cx>),
    (resolved_ty_args, ty::Tys<'cx>),
    // parameter type
    (param_ty_mapper, &'cx dyn ty::TyMap<'cx>),
    (param_ty_constraint, &'cx ty::Ty<'cx>),
    (default, &'cx ty::Ty<'cx>),
    // reference type
    (literal_ty, &'cx ty::Ty<'cx>),
    // conditional type
    (resolved_default_constraint, &'cx ty::Ty<'cx>),
    (resolved_inferred_true_ty, &'cx ty::Ty<'cx>),
    (resolved_true_ty, &'cx ty::Ty<'cx>),
    (resolved_false_ty, &'cx ty::Ty<'cx>),
    (resolved_constraint_of_distribute, Option<&'cx ty::Ty<'cx>>),
    // indexed access type
    (reading_simplified_ty, &'cx ty::Ty<'cx>),
    (writing_simplified_ty, &'cx ty::Ty<'cx>),
    // object flags
    (could_contain_ty_variables, bool),
);

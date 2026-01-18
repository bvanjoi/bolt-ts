use super::links;
use crate::check::get_variances::VarianceFlags;
use crate::ty;
use bolt_ts_binder::{SymbolID, SymbolTable};

links!(
    SymbolLinks,
    (ty, &'cx ty::Ty<'cx>),
    (write_ty, &'cx ty::Ty<'cx>),
    (declared_ty, &'cx ty::Ty<'cx>),
    (ty_params, ty::Tys<'cx>),
    (target, SymbolID),
    (ty_mapper, &'cx dyn ty::TyMap<'cx>),
    (name_ty, &'cx ty::Ty<'cx>),
    (containing_ty, &'cx ty::Ty<'cx>),
    (variances, &'cx [VarianceFlags]),
    (late_symbol, SymbolID),
    (unique_es_symbol_ty, &'cx ty::Ty<'cx>),
    // transient symbol
    (check_flags, ty::CheckFlags),
    // mapped symbol
    (mapped_ty, &'cx ty::Ty<'cx>),
    (named_ty, &'cx ty::Ty<'cx>),
    (key_ty, &'cx ty::Ty<'cx>),
    (prop_ty, &'cx ty::Ty<'cx>),
    (constraint_ty, &'cx ty::Ty<'cx>),
    (synthetic_origin, SymbolID),
    // deferral
    (deferral_parent, &'cx ty::Ty<'cx>),
    (deferral_constituents, ty::Tys<'cx>),
    (deferral_write_constituents, ty::Tys<'cx>),
    // name resolution
    (alias_target, SymbolID),
    (exports_checked, bool),
    (exports, &'cx SymbolTable),
    (members, &'cx SymbolTable),
    (resolved_exports, &'cx SymbolTable),
    (resolved_members, &'cx SymbolTable),
);

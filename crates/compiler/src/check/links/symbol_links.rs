use super::links;
use crate::bind::{SymbolID, SymbolName};
use crate::check::get_variances::VarianceFlags;
use crate::ty;

use rustc_hash::FxHashMap;

links!(
    SymbolLinks,
    (ty, &'cx ty::Ty<'cx>),
    (write_ty, &'cx ty::Ty<'cx>),
    (declared_ty, &'cx ty::Ty<'cx>),
    (ty_params, ty::Tys<'cx>),
    (target, SymbolID),
    (ty_mapper, &'cx dyn ty::TyMap<'cx>),
    (resolved_members, &'cx FxHashMap<SymbolName, SymbolID>),
    (resolved_exports, &'cx FxHashMap<SymbolName, SymbolID>),
    (name_ty, &'cx ty::Ty<'cx>),
    (containing_ty, &'cx ty::Ty<'cx>),
    (variances, &'cx [VarianceFlags]),
    // transient symbol
    (check_flags, ty::CheckFlags),
    // mapped symbol
    (mapped_ty, &'cx ty::Ty<'cx>),
    (named_ty, &'cx ty::Ty<'cx>),
    (key_ty, &'cx ty::Ty<'cx>),
    // deferral
    (deferral_parent, &'cx ty::Ty<'cx>),
    (deferral_constituents, ty::Tys<'cx>),
    (deferral_write_constituents, ty::Tys<'cx>),
    // resolve
    (alias_target, SymbolID),
);

use super::links;
use crate::bind::{SymbolID, SymbolName};
use crate::ty;

use rustc_hash::FxHashMap;

links!(
    SymbolLinks,
    (ty, &'cx ty::Ty<'cx>),
    (declared_ty, &'cx ty::Ty<'cx>),
    (ty_params, ty::Tys<'cx>),
    (check_flags, ty::CheckFlags),
    (target, SymbolID),
    (ty_mapper, &'cx ty::TyMapper<'cx>),
    (resolved_members, &'cx FxHashMap<SymbolName, SymbolID>),
);

use super::super::type_predicate::TyPred;
use super::links;
use crate::ty;

links!(
    SigLinks,
    (resolved_ret_ty, &'cx ty::Ty<'cx>),
    (canonical_sig, &'cx ty::Sig<'cx>),
    (resolved_ty_pred, &'cx TyPred<'cx>),
    (ty_params, ty::Tys<'cx>),
);

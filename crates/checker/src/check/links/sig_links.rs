use super::super::ty;
use super::super::type_predicate::TyPred;
use super::links;

links!(
    SigLinks,
    (resolved_ret_ty, &'cx ty::Ty<'cx>),
    (canonical_sig, &'cx ty::Sig<'cx>),
    (resolved_ty_pred, &'cx TyPred<'cx>),
    (ty_params, ty::Tys<'cx>),
    (this_param, bolt_ts_binder::SymbolID),
    (inner_optional_call_sig, &'cx ty::Sig<'cx>),
    (outer_optional_call_sig, &'cx ty::Sig<'cx>),
);

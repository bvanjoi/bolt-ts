use super::links;
use crate::ty;

links!(
    SigLinks,
    (resolved_ret_ty, &'cx ty::Ty<'cx>),
    (canonical_sig, &'cx ty::Sig<'cx>),
);

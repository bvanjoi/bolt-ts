use super::links;
use crate::check::NodeCheckFlags;
use crate::check::get_declared_ty::EnumMemberValue;
use crate::ty;

links!(
    NodeLinks,
    (resolved_ty, &'cx ty::Ty<'cx>),
    (resolved_sig, &'cx ty::Sig<'cx>),
    (resolved_symbol, bolt_ts_binder::SymbolID),
    (flags, NodeCheckFlags),
    (outer_ty_params, ty::Tys<'cx>),
    (effects_sig, &'cx ty::Sig<'cx>),
    (skip_direct_inference, bool),
    (non_existent_prop_checked, bool),
    (enum_member_value, EnumMemberValue)
);

impl NodeLinks<'_> {
    pub fn flags(&self) -> NodeCheckFlags {
        self.get_flags().unwrap()
    }
}

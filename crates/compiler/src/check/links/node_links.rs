use super::links;
use crate::check::NodeCheckFlags;
use crate::{bind, ty};

links!(
    NodeLinks,
    (resolved_ty, &'cx ty::Ty<'cx>),
    (resolved_sig, &'cx ty::Sig<'cx>),
    (resolved_symbol, bind::SymbolID),
    (flags, NodeCheckFlags),
    (outer_ty_params, ty::Tys<'cx>),
    (effects_sig, &'cx ty::Sig<'cx>),
    (skip_direct_inference, bool),
    (non_existent_prop_checked, bool),
);

impl NodeLinks<'_> {
    pub fn flags(&self) -> NodeCheckFlags {
        self.get_flags().unwrap()
    }
}

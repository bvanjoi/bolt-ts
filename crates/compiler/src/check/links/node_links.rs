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
);

impl NodeLinks<'_> {
    pub fn flags(&self) -> NodeCheckFlags {
        self.get_flags().unwrap()
    }
}

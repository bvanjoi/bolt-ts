use super::super::NodeFlags;
use super::links;
use crate::{bind, ty};

links!(
    NodeLinks,
    (resolved_ty, &'cx ty::Ty<'cx>),
    (resolved_sig, &'cx ty::Sig<'cx>),
    (resolved_symbol, bind::SymbolID),
    (flags, NodeFlags),
    (outer_ty_params, ty::Tys<'cx>),
    (effects_sig, &'cx ty::Sig<'cx>),
);

impl NodeLinks<'_> {
    pub fn flags(&self) -> NodeFlags {
        self.get_flags().unwrap()
    }
}

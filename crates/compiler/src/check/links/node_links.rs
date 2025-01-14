use super::super::NodeFlags;
use super::links;
use crate::ty;

links!(
    NodeLinks,
    (resolved_ty, &'cx ty::Ty<'cx>),
    (resolved_sig, &'cx ty::Sig<'cx>),
    (flags, NodeFlags)
);

impl NodeLinks<'_> {
    pub fn flags(&self) -> NodeFlags {
        self.get_flags().unwrap()
    }
}

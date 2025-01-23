use crate::ir::ArrayTyLike;
use crate::ty;

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_array_or_tuple_target_ty(
        &mut self,
        node: &impl ArrayTyLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let ty_node = self.p.node(node.id()).as_ty().unwrap();
        if Self::get_array_ele_ty_node(&ty_node).is_some() {
            return self.global_array_ty();
        };
        let element_flags: Vec<_> = node
            .elements()
            .unwrap()
            .iter()
            .map(|ty| Self::get_tuple_element_flags(ty))
            .collect();
        let element_flags = self.alloc(element_flags);
        self.get_tuple_target_ty(element_flags, false)
    }
}

use crate::ir::ArrayTyLike;
use crate::ty::{self, ElementFlags};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    // fn get_ty_from_array_or_tuple_ty(&mut self, node: &impl ArrayTyLike<'cx>) -> &'cx ty::Ty<'cx> {
    //     if let Some(ty) = self.get_node_links(node.id()).get_resolved_ty() {
    //         return ty;
    //     }
    //     let is_tuple = node.elements().is_some();
    //     let target = self.get_array_or_tuple_target_ty(node);
    //     let resolved_ty = if target == self.empty_generic_ty() {
    //         self.empty_object_ty()
    //     } else if !node
    //         .elements()
    //         .is_some_and(|elems| elems.iter().any(|ele| Self::get_tuple_element_flags(ele)))
    //     {
    //         // tuple type
    //     } else {
    //         // array type
    //         let ty = self.get_ty_from_type_node(node.element().unwrap());
    //         self.create_normalized_ty_reference(target, self.alloc([ty]))
    //     };
    //     resolved_ty
    // }

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

    fn get_tuple_target_ty(
        &mut self,
        element_flags: &'cx [ElementFlags],
        readonly: bool,
    ) -> &'cx ty::Ty<'cx> {
        if element_flags.len() == 1 && element_flags[0].intersects(ElementFlags::REST) {
            return if readonly {
                self.global_array_ty()
            } else {
                self.global_array_ty()
            };
        }
        let key = element_flags.iter().map(|flag| {
            if flag.intersects(ElementFlags::REQUIRED) {
                b'#'
            } else if flag.intersects(ElementFlags::OPTIONAL) {
                b'?'
            } else if flag.intersects(ElementFlags::REST) {
                b'.'
            } else {
                b'*'
            }
        });
        let key = if readonly {
            key.chain([b'R'].into_iter()).collect::<Vec<_>>()
        } else {
            key.collect::<Vec<_>>()
        };
        let key = xxhash_rust::xxh3::xxh3_64(&key);
        if let Some(ty) = self.tuple_tys.get(&key) {
            return ty;
        }
        let ty = self.create_tuple_target_type(element_flags, readonly);
        let prev = self.tuple_tys.insert(key, ty);
        assert!(prev.is_none());
        ty
    }
}

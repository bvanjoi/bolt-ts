use super::TyChecker;

use super::ty::{self, ObjectFlags};

use bolt_ts_binder::{SymbolFlags, SymbolID};
use bolt_ts_ty::TypeFlags;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum RecursionId {
    Ty(ty::TyID),
    Symbol(SymbolID),
    Node(bolt_ts_ast::NodeID),
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_recursion_id(&self, ty: &'cx ty::Ty<'cx>) -> RecursionId {
        if ty.flags.contains(TypeFlags::OBJECT) && !ty.is_object_or_array_literal() {
            if ty.get_object_flags().contains(ObjectFlags::REFERENCE)
                && let Some(r) = ty.kind.as_object_reference()
                && let Some(node) = r.node
            {
                return RecursionId::Node(node);
            } else if let Some(symbol) = ty.symbol()
                && !(ty.kind.is_object_anonymous()
                    && self.symbol(symbol).flags.contains(SymbolFlags::CLASS))
            {
                return RecursionId::Symbol(symbol);
            } else if ty.is_tuple() {
                return if let Some(refer) = ty.kind.as_object_reference() {
                    assert!(refer.target.kind.is_object_tuple());
                    RecursionId::Ty(refer.target.id)
                } else if ty.kind.is_object_tuple() {
                    RecursionId::Ty(ty.id)
                } else {
                    unreachable!()
                };
            }
        }
        if let Some(param) = ty.kind.as_param()
            // It will return undefined if the symbol is `None` in typescript,
            // but we fallback into `RecursionId::Ty` in that case, so it's fine
            && let Some(symbol) = param.symbol
        {
            return RecursionId::Symbol(symbol);
        } else if let Some(indexed_access) = ty.kind.as_indexed_access() {
            let mut most_left = indexed_access.object_ty;
            while let Some(indexed_access) = most_left.kind.as_indexed_access() {
                most_left = indexed_access.object_ty;
            }
            return RecursionId::Ty(most_left.id);
        } else if let Some(cond) = ty.kind.as_cond_ty() {
            //TODO: maybe use `ptr(cond.root)`?
            return RecursionId::Node(cond.root.node.id);
        }
        RecursionId::Ty(ty.id)
    }

    fn has_matching_recursion_ident(&self, ty: &'cx ty::Ty<'cx>, id: RecursionId) -> bool {
        if ty
            .get_object_flags()
            .contains(ObjectFlags::INSTANTIATED_MAPPED)
        {
            // TODO:
        }
        if let Some(i) = ty.kind.as_intersection() {
            i.tys
                .iter()
                .any(|t| self.has_matching_recursion_ident(t, id))
        } else {
            self.get_recursion_id(ty) == id
        }
    }

    pub(super) fn is_deeply_nested_type(
        &self,
        ty: &'cx ty::Ty<'cx>,
        stack: &[&'cx ty::Ty<'cx>],
        max_depth: usize,
    ) -> bool {
        let depth = stack.len();
        if depth < max_depth {
            return false;
        }
        if ty
            .get_object_flags()
            .contains(ObjectFlags::INSTANTIATED_MAPPED)
        {
            // TODO:
        }
        if let Some(i) = ty.kind.as_intersection() {
            i.tys
                .iter()
                .any(|t| self.is_deeply_nested_type(t, stack, max_depth))
        } else {
            let id = self.get_recursion_id(ty);
            let mut count = 0;
            let mut last_type_id = 0;
            for t in stack.iter().take(depth) {
                if self.has_matching_recursion_ident(t, id) {
                    if t.id.as_u32() >= last_type_id {
                        count += 1;
                        if count >= max_depth {
                            return true;
                        }
                    }
                    last_type_id = t.id.as_u32();
                }
            }
            false
        }
    }
}

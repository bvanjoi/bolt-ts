use super::bind::{SymbolFlags, SymbolID};
use crate::ty::{self, ObjectFlags};

use super::TyChecker;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum RecursionId {
    Ty(ty::TyID),
    Symbol(SymbolID),
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_recursion_id(&self, ty: &'cx ty::Ty<'cx>) -> RecursionId {
        if ty.kind.is_object() && !ty.is_object_or_array_literal() {
            // if ty.kind.is_object_reference()
            if let Some(symbol) = ty.symbol() {
                if !(ty.kind.is_object_anonymous()
                    && self
                        .binder
                        .symbol(symbol)
                        .flags
                        .intersects(SymbolFlags::CLASS))
                {
                    return RecursionId::Symbol(symbol);
                }
                if ty.is_tuple() {
                    // return ty;
                }
            }
        }

        RecursionId::Ty(ty.id)
    }

    fn has_matching_recursion_ident(&self, ty: &'cx ty::Ty<'cx>, id: RecursionId) -> bool {
        self.get_recursion_id(ty) == id
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
        if ty.get_object_flags() == ObjectFlags::INSTANTIATED_MAPPED {
            todo!()
        }
        if ty.kind.is_intersection() {
            todo!()
        }

        let id = self.get_recursion_id(ty);
        let mut count = 0;
        let mut last_type_id = 0;
        for i in 0..depth {
            let t = stack[i];
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

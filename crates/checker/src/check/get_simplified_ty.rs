use crate::ty::{self, IndexFlags, TypeFlags};

use super::{TyChecker, create_ty::IntersectionFlags};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimplifiedKind {
    Writing,
    Reading,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_simplified_ty_or_constraint(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let simplified = self.get_simplified_ty(ty, SimplifiedKind::Reading);
        if simplified != ty {
            Some(simplified)
        } else {
            self.get_constraint_of_ty(ty)
        }
    }

    pub(super) fn get_simplified_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SimplifiedKind,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(indexed_access_ty) = ty.kind.as_indexed_access() {
            self.get_simplified_index_access_ty(ty, indexed_access_ty, kind)
        } else if ty.kind.as_cond_ty().is_some() {
            self.get_simplified_cond_ty(ty, kind)
        } else {
            ty
        }
    }

    fn get_simplified_cond_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SimplifiedKind,
    ) -> &'cx ty::Ty<'cx> {
        let cond_ty = ty.kind.expect_cond_ty();
        let true_ty = self.get_true_ty_from_cond_ty(ty, cond_ty);
        let false_ty = self.get_false_ty_from_cond_ty(ty, cond_ty);
        if false_ty.flags.intersects(TypeFlags::NEVER)
            && self.get_actual_ty_variable(true_ty) == self.get_actual_ty_variable(cond_ty.check_ty)
        {
            if cond_ty.check_ty.flags.intersects(TypeFlags::ANY) || {
                let source = self.get_restrictive_instantiation(cond_ty.check_ty);
                let target = self.get_restrictive_instantiation(cond_ty.extends_ty);
                self.is_type_assignable_to(source, target)
            } {
                return self.get_simplified_ty(ty, kind);
            } else if self.is_intersection_empty(cond_ty.check_ty, cond_ty.extends_ty) {
                return self.never_ty;
            }
        } else if true_ty.flags.intersects(TypeFlags::NEVER)
            && self.get_actual_ty_variable(false_ty)
                == self.get_actual_ty_variable(cond_ty.check_ty)
        {
            if !cond_ty.check_ty.flags.intersects(TypeFlags::ANY) && {
                let source = self.get_restrictive_instantiation(cond_ty.check_ty);
                let target = self.get_restrictive_instantiation(cond_ty.extends_ty);
                self.is_type_assignable_to(source, target)
            } {
                return self.never_ty;
            } else if cond_ty.check_ty.flags.intersects(TypeFlags::ANY)
                || self.is_intersection_empty(cond_ty.check_ty, cond_ty.extends_ty)
            {
                return self.get_simplified_ty(false_ty, kind);
            }
        }
        ty
    }

    fn is_intersection_empty(&mut self, ty1: &'cx ty::Ty<'cx>, ty2: &'cx ty::Ty<'cx>) -> bool {
        let i = self.get_intersection_ty(&[ty1, ty2], IntersectionFlags::None, None, None);
        let u = self.get_union_ty(
            &[i, self.never_ty],
            ty::UnionReduction::Lit,
            false,
            None,
            None,
        );
        u.flags.intersects(TypeFlags::NEVER)
    }

    fn distribute_object_over_object_ty(
        &mut self,
        object_ty: &'cx ty::Ty<'cx>,
        index_ty: &'cx ty::Ty<'cx>,
        kind: SimplifiedKind,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(tys) = object_ty.kind.tys_of_union_or_intersection()
            && !self.should_defer_index_ty(object_ty, IndexFlags::empty())
        {
            let tys = tys
                .iter()
                .map(|t| {
                    let a = self.get_indexed_access_ty(t, index_ty, None, None);
                    self.get_simplified_ty(a, kind)
                })
                .collect::<Vec<_>>();
            return if object_ty.flags.intersects(TypeFlags::INTERSECTION)
                || kind == SimplifiedKind::Writing
            {
                Some(self.get_intersection_ty(&tys, IntersectionFlags::None, None, None))
            } else {
                Some(self.get_union_ty(&tys, ty::UnionReduction::Lit, false, None, None))
            };
        }
        None
    }

    fn distribute_object_over_index_ty(
        &mut self,
        object_ty: &'cx ty::Ty<'cx>,
        index_ty: &'cx ty::Ty<'cx>,
        kind: SimplifiedKind,
    ) -> Option<&'cx ty::Ty<'cx>> {
        index_ty.kind.as_union().map(|u| {
            let tys = u
                .tys
                .iter()
                .map(|t| {
                    let a = self.get_indexed_access_ty(object_ty, t, None, None);
                    self.get_simplified_ty(a, SimplifiedKind::Writing)
                })
                .collect::<Vec<_>>();
            if kind == SimplifiedKind::Writing {
                self.get_intersection_ty(&tys, IntersectionFlags::None, None, None)
            } else {
                self.get_union_ty(&tys, ty::UnionReduction::Lit, false, None, None)
            }
        })
    }

    fn get_simplified_index_access_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        indexed_access_ty: &'cx ty::IndexedAccessTy<'cx>,
        kind: SimplifiedKind,
    ) -> &'cx ty::Ty<'cx> {
        assert!(
            ty.kind
                .as_indexed_access()
                .is_some_and(|t| std::ptr::addr_eq(t, indexed_access_ty))
        );
        // check cache
        if let Some(cached) = match kind {
            SimplifiedKind::Writing => self.get_ty_links(ty.id).get_writing_simplified_ty(),
            SimplifiedKind::Reading => self.get_ty_links(ty.id).get_reading_simplified_ty(),
        } {
            return if cached == self.circular_constraint_ty() {
                ty
            } else {
                cached
            };
        }

        let placeholder = self.circular_constraint_ty();
        match kind {
            SimplifiedKind::Writing => self
                .get_mut_ty_links(ty.id)
                .set_writing_simplified_ty(placeholder),
            SimplifiedKind::Reading => self
                .get_mut_ty_links(ty.id)
                .set_reading_simplified_ty(placeholder),
        }

        let set_cache = |this: &mut Self, t: &'cx ty::Ty<'cx>| {
            let links = this.get_mut_ty_links(ty.id);
            match kind {
                SimplifiedKind::Writing => links.override_writing_simplified_ty(t),
                SimplifiedKind::Reading => links.override_reading_simplified_ty(t),
            }
        };

        let object_ty = self.get_simplified_ty(indexed_access_ty.object_ty, kind);
        let index_ty = self.get_simplified_ty(indexed_access_ty.index_ty, kind);

        if let Some(distributed_over_index) =
            self.distribute_object_over_index_ty(object_ty, index_ty, kind)
        {
            set_cache(self, distributed_over_index);
            return distributed_over_index;
        }

        if !index_ty.flags.intersects(TypeFlags::INSTANTIABLE)
            && let Some(distributed_over_object) =
                self.distribute_object_over_object_ty(object_ty, index_ty, kind)
        {
            set_cache(self, distributed_over_object);
            return distributed_over_object;
        }

        if object_ty.kind.is_generic_tuple_type()
            && index_ty.flags.intersects(TypeFlags::NUMBER_LIKE)
        {
            // TODO:
        }

        if self.is_generic_mapped_ty(object_ty)
            && self.get_mapped_ty_name_ty_kind(object_ty) != ty::MappedTyNameTyKind::Remapping
        {
            let t = self.substitute_indexed_mapped_ty(object_ty, indexed_access_ty.index_ty);
            let t = self
                .map_ty(t, |this, t| Some(this.get_simplified_ty(t, kind)), false)
                .unwrap();
            set_cache(self, t);
            return t;
        }
        set_cache(self, ty);
        ty
    }
}

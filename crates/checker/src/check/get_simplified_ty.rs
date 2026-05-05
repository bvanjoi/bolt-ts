use super::TyChecker;
use super::create_ty::IntersectionFlags;
use super::ty::{self, IndexFlags, TypeFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_simplified_ty_or_constraint(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let simplified = self.get_simplified_ty::<false>(ty);
        if simplified != ty {
            Some(simplified)
        } else {
            self.get_constraint_of_ty(ty)
        }
    }

    pub(super) fn get_simplified_ty<const WRITING: bool>(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        match ty.kind {
            ty::TyKind::IndexedAccess(n) => self.get_simplified_indexed_access_ty::<WRITING>(ty, n),
            ty::TyKind::Cond(n) => self.get_simplified_cond_ty::<WRITING>(n).unwrap_or(ty),
            ty::TyKind::Index(n) => self.get_simplified_index_ty(n).unwrap_or(ty),
            _ => ty,
        }
    }

    fn get_simplified_index_ty(
        &mut self,
        index_ty: &'cx ty::IndexTy<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.is_generic_mapped_ty(index_ty.ty)
            && let mapped_ty = index_ty.ty.kind.expect_object_mapped()
            && self.get_name_ty_from_mapped_ty(mapped_ty).is_some()
            && !self.is_mapped_ty_with_keyof_constraint_decl(mapped_ty)
        {
            Some(self.get_index_ty_for_mapped_ty(index_ty.ty, IndexFlags::empty()))
        } else {
            None
        }
    }

    fn get_simplified_cond_ty<const WRITING: bool>(
        &mut self,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let true_ty = self.get_true_ty_from_cond_ty(cond_ty);
        let false_ty = self.get_false_ty_from_cond_ty(cond_ty);
        if false_ty.flags.contains(TypeFlags::NEVER)
            && self.get_actual_ty_variable(true_ty) == self.get_actual_ty_variable(cond_ty.check_ty)
        {
            if cond_ty.check_ty.flags.intersects(TypeFlags::ANY) || {
                let source = self.get_restrictive_instantiation(cond_ty.check_ty);
                let target = self.get_restrictive_instantiation(cond_ty.extends_ty);
                self.is_type_assignable_to(source, target)
            } {
                return Some(self.get_simplified_ty::<WRITING>(true_ty));
            } else if self.is_intersection_empty(cond_ty.check_ty, cond_ty.extends_ty) {
                return Some(self.never_ty);
            }
        } else if true_ty.flags.contains(TypeFlags::NEVER)
            && self.get_actual_ty_variable(false_ty)
                == self.get_actual_ty_variable(cond_ty.check_ty)
        {
            if !cond_ty.check_ty.flags.intersects(TypeFlags::ANY) && {
                let source = self.get_restrictive_instantiation(cond_ty.check_ty);
                let target = self.get_restrictive_instantiation(cond_ty.extends_ty);
                self.is_type_assignable_to(source, target)
            } {
                return Some(self.never_ty);
            } else if cond_ty.check_ty.flags.intersects(TypeFlags::ANY)
                || self.is_intersection_empty(cond_ty.check_ty, cond_ty.extends_ty)
            {
                return Some(self.get_simplified_ty::<WRITING>(false_ty));
            }
        }
        None
    }

    fn is_intersection_empty(&mut self, ty1: &'cx ty::Ty<'cx>, ty2: &'cx ty::Ty<'cx>) -> bool {
        let i = self.get_intersection_ty(&[ty1, ty2], IntersectionFlags::None, None, None);
        let u = self.get_union_ty::<false>(
            &[i, self.never_ty],
            ty::UnionReduction::Lit,
            None,
            None,
            None,
        );
        u.flags.intersects(TypeFlags::NEVER)
    }

    pub(super) fn distribute_object_over_object_ty<const WRITING: bool>(
        &mut self,
        object_ty: &'cx ty::Ty<'cx>,
        index_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // (T | U)[K] -> T[K] | U[K] (reading)
        // (T | U)[K] -> T[K] & U[K] (writing)
        // (T & U)[K] -> T[K] & U[K]
        if let Some(tys) = object_ty.kind.tys_of_union_or_intersection()
            && !self.should_defer_index_ty(object_ty, IndexFlags::empty())
        {
            let tys = tys
                .iter()
                .map(|t| {
                    let a = self.get_indexed_access_ty(t, index_ty, None, None, None, None);
                    self.get_simplified_ty::<WRITING>(a)
                })
                .collect::<Vec<_>>();
            return if WRITING || object_ty.flags.intersects(TypeFlags::INTERSECTION) {
                Some(self.get_intersection_ty(&tys, IntersectionFlags::None, None, None))
            } else {
                Some(self.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None))
            };
        }
        None
    }

    pub(super) fn distribute_index_over_object_ty<const WRITING: bool>(
        &mut self,
        object_ty: &'cx ty::Ty<'cx>,
        index_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if object_ty.flags.contains(TypeFlags::UNION)
            || object_ty.flags.contains(TypeFlags::INTERSECTION)
                && !self.should_defer_index_ty(object_ty, IndexFlags::empty())
        {
            let tys = object_ty.kind.tys_of_union_or_intersection().unwrap();
            let tys = tys
                .iter()
                .map(|t| {
                    let t = self.get_indexed_access_ty(t, index_ty, None, None, None, None);
                    self.get_simplified_ty::<WRITING>(t)
                })
                .collect::<Vec<_>>();
            Some(
                if WRITING || object_ty.flags.contains(TypeFlags::INTERSECTION) {
                    self.get_intersection_ty(&tys, IntersectionFlags::None, None, None)
                } else {
                    self.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None)
                },
            )
        } else {
            None
        }
    }

    fn distribute_object_over_index_ty<const WRITING: bool>(
        &mut self,
        object_ty: &'cx ty::Ty<'cx>,
        index_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        index_ty.kind.as_union().map(|u| {
            let tys = u
                .tys
                .iter()
                .map(|t| {
                    let a = self.get_indexed_access_ty(object_ty, t, None, None, None, None);
                    self.get_simplified_ty::<true>(a)
                })
                .collect::<Vec<_>>();
            if WRITING {
                self.get_intersection_ty(&tys, IntersectionFlags::None, None, None)
            } else {
                self.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None)
            }
        })
    }

    fn get_simplified_indexed_access_ty<const WRITING: bool>(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        indexed_access_ty: &'cx ty::IndexedAccessTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        assert!(
            ty.kind
                .as_indexed_access()
                .is_some_and(|t| std::ptr::addr_eq(t, indexed_access_ty))
        );
        // check cache
        if let Some(cached) = match WRITING {
            true => self.get_ty_links(ty.id).get_writing_simplified_ty(),
            false => self.get_ty_links(ty.id).get_reading_simplified_ty(),
        } {
            return if cached == self.circular_constraint_ty() {
                ty
            } else {
                cached
            };
        }

        let placeholder = self.circular_constraint_ty();
        let links = self.get_mut_ty_links(ty.id);
        if WRITING {
            links.set_writing_simplified_ty(placeholder);
        } else {
            links.set_reading_simplified_ty(placeholder);
        }

        let set_cache = |this: &mut Self, t: &'cx ty::Ty<'cx>| {
            let links = this.get_mut_ty_links(ty.id);
            if WRITING {
                links.override_writing_simplified_ty(t);
            } else {
                links.override_reading_simplified_ty(t);
            }
        };

        let object_ty = self.get_simplified_ty::<WRITING>(indexed_access_ty.object_ty);
        let index_ty = self.get_simplified_ty::<WRITING>(indexed_access_ty.index_ty);

        if let Some(distributed_over_index) =
            self.distribute_object_over_index_ty::<WRITING>(object_ty, index_ty)
        {
            set_cache(self, distributed_over_index);
            return distributed_over_index;
        }

        if !index_ty.flags.intersects(TypeFlags::INSTANTIABLE)
            && let Some(distributed_over_object) =
                self.distribute_index_over_object_ty::<WRITING>(object_ty, index_ty)
        {
            set_cache(self, distributed_over_object);
            return distributed_over_object;
        }

        if object_ty.kind.is_generic_tuple_type()
            && index_ty.flags.intersects(TypeFlags::NUMBER_LIKE)
        {
            let element_ty = self.get_element_ty_of_slice_of_tuple_ty::<WRITING, false>(
                object_ty,
                if index_ty.flags.contains(TypeFlags::NUMBER) {
                    0
                } else {
                    object_ty.as_tuple().unwrap().fixed_length
                },
                0,
            );
            if let Some(element_ty) = element_ty {
                set_cache(self, element_ty);
                return element_ty;
            }
        }

        if self.is_generic_mapped_ty(object_ty)
            && self.get_mapped_ty_name_ty_kind(object_ty) != ty::MappedTyNameTyKind::Remapping
        {
            let t = self.substitute_indexed_mapped_ty(object_ty, indexed_access_ty.index_ty);
            let t = self
                .map_ty(
                    t,
                    |this, t| Some(this.get_simplified_ty::<WRITING>(t)),
                    false,
                )
                .unwrap();
            set_cache(self, t);
            return t;
        }
        set_cache(self, ty);
        ty
    }
}

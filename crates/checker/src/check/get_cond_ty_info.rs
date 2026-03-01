use super::TyChecker;
use super::get_simplified_ty::SimplifiedKind;
use super::ty;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_true_ty_from_cond_ty(
        &mut self,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let id = cond_ty.conditional_links;
        if let Some(cached) = self.conditional_links_arena[id].get_resolved_true_ty() {
            return cached;
        }
        let true_ty = self.get_ty_from_type_node(cond_ty.root.node.true_ty);
        let true_ty = self.instantiate_ty(true_ty, cond_ty.mapper);
        self.conditional_links_arena[id].set_resolved_true_ty(true_ty);
        true_ty
    }

    pub(super) fn get_false_ty_from_cond_ty(
        &mut self,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let id = cond_ty.conditional_links;
        if let Some(cached) = self.conditional_links_arena[id].get_resolved_false_ty() {
            return cached;
        }
        let false_ty = self.get_ty_from_type_node(cond_ty.root.node.false_ty);
        let false_ty = self.instantiate_ty(false_ty, cond_ty.mapper);
        self.conditional_links_arena[id].set_resolved_false_ty(false_ty);
        false_ty
    }

    pub(super) fn get_constraint_of_distributive_cond_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let cond_ty = ty.kind.expect_cond_ty();
        let id = cond_ty.conditional_links;
        if let Some(ty) = self.conditional_links_arena[id].get_resolved_constraint_of_distribute() {
            return ty;
        }
        if cond_ty.root.is_distributive
            && self.common_ty_links_arena[ty.links].get_restrictive_instantiation() != Some(ty)
        {
            let simplified = self.get_simplified_ty(cond_ty.check_ty, SimplifiedKind::Reading);
            let constraint = if simplified == cond_ty.check_ty {
                self.get_constraint_of_ty(simplified)
            } else {
                Some(simplified)
            };
            if let Some(constraint) = constraint
                && constraint != cond_ty.check_ty
            {
                let mapper =
                    self.prepend_ty_mapping(cond_ty.root.check_ty, constraint, cond_ty.mapper);
                let instantiated = self.get_cond_ty_instantiation(ty, mapper, None, None);
                self.conditional_links_arena[id]
                    .set_resolved_constraint_of_distribute(Some(instantiated));
                return Some(instantiated);
            }
        }
        self.conditional_links_arena[id].set_resolved_constraint_of_distribute(None);
        None
    }

    pub(super) fn get_default_constraint_of_cond_ty(
        &mut self,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let id = cond_ty.conditional_links;
        if let Some(ty) = self.conditional_links_arena[id].get_resolved_default_constraint() {
            return ty;
        }
        let true_constraint = self.get_inferred_true_ty_from_cond_ty(cond_ty);
        let false_constraint = self.get_false_ty_from_cond_ty(cond_ty);
        let res = if self.is_type_any(true_constraint) {
            false_constraint
        } else if self.is_type_any(false_constraint) {
            true_constraint
        } else {
            self.get_union_ty::<false>(
                &[true_constraint, false_constraint],
                ty::UnionReduction::Lit,
                None,
                None,
                None,
            )
        };
        self.conditional_links_arena[id].set_resolved_default_constraint(res);
        res
    }

    pub(super) fn get_inferred_true_ty_from_cond_ty(
        &mut self,
        cond_ty: &'cx ty::CondTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let id = cond_ty.conditional_links;
        if let Some(ty) = self.conditional_links_arena[id].get_resolved_inferred_true_ty() {
            return ty;
        }
        let res = if let Some(m) = cond_ty.combined_mapper {
            let ty = self.get_ty_from_type_node(cond_ty.root.node.true_ty);
            self.instantiate_ty(ty, Some(m))
        } else {
            self.get_true_ty_from_cond_ty(cond_ty)
        };
        self.conditional_links_arena[id].set_resolved_inferred_true_ty(res);
        res
    }
}

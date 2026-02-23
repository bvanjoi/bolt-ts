use super::TyChecker;
use super::instantiation_ty_map::hash_ty_args;
use super::ty;
use super::ty::{IndexFlags, TypeFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_index_ty_for_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        index_flags: IndexFlags,
    ) -> &'cx ty::Ty<'cx> {
        let map = ty.kind.expect_object_mapped();
        let name_ty = self
            .get_name_ty_from_mapped_ty(map.target.map_or(map, |t| t.kind.expect_object_mapped()));
        if name_ty.is_none() && !index_flags.intersects(IndexFlags::NO_INDEX_SIGNATURES) {
            return self.get_constraint_ty_from_mapped_ty(map);
        }
        let mut key_tys = vec![];
        let mut add_member_for_key_ty = |this: &mut Self, key_ty: &'cx ty::Ty<'cx>| {
            let prop_name_ty = match name_ty {
                Some(name_ty) => {
                    let source = this.get_ty_param_from_mapped_ty(map);
                    let mapper = this.append_ty_mapping(map.mapper, source, key_ty);
                    this.instantiate_ty(name_ty, Some(mapper))
                }
                None => key_ty,
            };
            if prop_name_ty == this.string_ty {
                key_tys.push(this.string_or_number_ty());
            } else {
                key_tys.push(prop_name_ty);
            }
        };
        let constraint_ty = self.get_constraint_ty_from_mapped_ty(map);
        if self.is_generic_index_ty(constraint_ty) {
            if self.is_mapped_ty_with_keyof_constraint_decl(map) {
                return self.get_index_ty_for_generic_ty(ty, index_flags);
            }
            self.for_each_ty(constraint_ty, |this, key_ty| {
                add_member_for_key_ty(this, key_ty)
            });
        } else if self.is_mapped_ty_with_keyof_constraint_decl(map) {
            let modifiers_ty = {
                let t = self.get_modifiers_ty_from_mapped_ty(map);
                self.get_apparent_ty(t)
            };
            self.for_each_mapped_ty_prop_key_ty_and_index_sig_key_ty(
                modifiers_ty,
                TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE,
                index_flags.intersects(IndexFlags::STRINGS_ONLY),
                &mut add_member_for_key_ty,
            );
        } else {
            let t = self.get_lower_bound_of_key_ty(constraint_ty);
            self.for_each_ty(t, add_member_for_key_ty);
        };

        let result = if index_flags.intersects(IndexFlags::NO_INDEX_SIGNATURES) {
            let t = self.get_union_ty::<false>(&key_tys, ty::UnionReduction::Lit, None, None, None);
            self.filter_type(t, |_, t| {
                !t.flags.intersects(TypeFlags::ANY | TypeFlags::STRING)
            })
        } else {
            self.get_union_ty::<false>(&key_tys, ty::UnionReduction::Lit, None, None, None)
        };

        if let Some(result) = result.kind.as_union()
            && let Some(c) = constraint_ty.kind.as_union()
            && hash_ty_args(result.tys) == hash_ty_args(c.tys)
        {
            return constraint_ty;
        }
        result
    }

    pub(super) fn get_constraint_decl_for_mapped_ty(
        &self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> Option<&'cx bolt_ts_ast::Ty<'cx>> {
        self.get_effective_constraint_of_ty_param(ty.decl.ty_param)
    }

    pub(super) fn get_ty_param_from_mapped_ty(
        &mut self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(cached) = self.object_mapped_ty_links_arena[ty.links].get_ty_param() {
            return cached;
        }
        let param_ty = self.get_symbol_of_decl(ty.decl.ty_param.id);
        let t = self.get_declared_ty_of_ty_param(param_ty);
        self.object_mapped_ty_links_arena[ty.links].set_ty_param(t);
        t
    }

    pub(super) fn get_constraint_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(cached) = self.object_mapped_ty_links_arena[ty.links].get_constraint_ty() {
            return cached;
        }
        let ty_param = self.get_ty_param_from_mapped_ty(ty);
        let t = self
            .get_constraint_of_ty_param(ty_param)
            .unwrap_or(self.error_ty);
        self.object_mapped_ty_links_arena[ty.links].set_constraint_ty(t);
        t
    }

    pub(super) fn get_name_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        ty.decl.name_ty.map(|name_ty| {
            if let Some(t) = self.object_mapped_ty_links_arena[ty.links].get_named_ty() {
                return t;
            }
            let name_ty = self.get_ty_from_type_node(name_ty);
            let name_ty = self.instantiate_ty(name_ty, ty.mapper);
            let links = &mut self.object_mapped_ty_links_arena[ty.links];
            if links.get_named_ty().is_some() {
                // cycle
                links.override_named_ty(name_ty);
            } else {
                links.set_named_ty(name_ty);
            }
            name_ty
        })
    }

    pub(super) fn get_template_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::MappedTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(cached) = self.object_mapped_ty_links_arena[ty.links].get_template_ty() {
            return cached;
        }
        let template_ty = if let Some(decl_ty) = ty.decl.ty {
            let decl_ty = self.get_ty_from_type_node(decl_ty);
            let is_optional = ty
                .decl
                .get_modifiers()
                .contains(bolt_ts_ast::MappedTyModifiers::INCLUDE_OPTIONAL);
            let t = self.add_optionality::<true>(decl_ty, is_optional);
            self.instantiate_ty(t, ty.mapper)
        } else {
            self.error_ty
        };
        self.object_mapped_ty_links_arena[ty.links].set_template_ty(template_ty);
        template_ty
    }

    pub(super) fn get_apparent_ty_of_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let m = ty.kind.expect_object_mapped();
        if let Some(resolved) =
            self.object_mapped_ty_links_arena[m.links].get_resolved_apparent_ty()
        {
            return resolved;
        }
        let t = self.get_resolved_apparent_ty_of_mapped_ty(ty);
        self.object_mapped_ty_links_arena[m.links].set_resolved_apparent_ty(t);
        t
    }

    fn get_resolved_apparent_ty_of_mapped_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let m = ty.kind.expect_object_mapped();
        let target = m.target.unwrap_or(ty);
        let target_m = target.kind.expect_object_mapped();
        let ty_var = self.get_homomorphic_ty_var(target_m);
        if let Some(ty_var) = ty_var
            && target_m.decl.name_ty.is_none()
            && let modifier_ty = self.get_modifiers_ty_from_mapped_ty(m)
            && let Some(base_constraint) = if self.is_generic_mapped_ty(modifier_ty) {
                Some(self.get_apparent_ty_of_mapped_ty(modifier_ty))
            } else {
                self.get_base_constraint_of_ty(modifier_ty)
            }
            && self.every_type(base_constraint, |this, t| {
                this.is_array_or_tuple(t) || this.is_array_or_tuple_intersection(t)
            })
        {
            let mapper = self.prepend_ty_mapping(ty_var, base_constraint, m.mapper);
            return self.instantiate_ty(target, Some(mapper));
        }
        ty
    }
}

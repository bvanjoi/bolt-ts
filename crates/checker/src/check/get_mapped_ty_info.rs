use super::TyChecker;
use super::instantiation_ty_map::hash_ty_args;
use super::ty;
use crate::ty::{IndexFlags, TypeFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_index_ty_for_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        index_flags: IndexFlags,
    ) -> &'cx ty::Ty<'cx> {
        let map = ty.kind.expect_object_mapped();
        let name_ty = self.get_name_ty_from_mapped_ty(map.target.unwrap_or(ty));
        if name_ty.is_none() && !index_flags.intersects(IndexFlags::NO_INDEX_SIGNATURES) {
            return self.get_constraint_ty_from_mapped_ty(ty);
        }
        let mut key_tys = vec![];
        let mut add_member_for_key_ty = |this: &mut Self, key_ty: &'cx ty::Ty<'cx>| {
            let prop_name_ty = match name_ty {
                Some(name_ty) => {
                    let source = this.get_ty_param_from_mapped_ty(ty);
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
        let constraint_ty = self.get_constraint_ty_from_mapped_ty(ty);
        if self.is_generic_index_ty(constraint_ty) {
            if self.is_mapped_ty_with_keyof_constraint_decl(map) {
                return self.get_index_ty_for_generic_ty(ty, index_flags);
            }
            self.for_each_ty(constraint_ty, |this, key_ty| {
                add_member_for_key_ty(this, key_ty)
            });
        } else if self.is_mapped_ty_with_keyof_constraint_decl(map) {
            let modifiers_ty = {
                let t = self.get_modifiers_ty_from_mapped_ty(ty);
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
            let t = self.get_union_ty(&key_tys, ty::UnionReduction::Lit, false, None, None);
            self.filter_type(t, |_, t| {
                !t.flags.intersects(TypeFlags::ANY | TypeFlags::STRING)
            })
        } else {
            self.get_union_ty(&key_tys, ty::UnionReduction::Lit, false, None, None)
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

    pub(super) fn get_ty_param_from_mapped_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let mapped_ty = ty.kind.expect_object_mapped();
        if let Some(t) = self.object_mapped_ty_links_arena[mapped_ty.links].get_ty_param() {
            return t;
        }
        let param_ty = self.get_symbol_of_decl(mapped_ty.decl.ty_param.id);
        let t = self.get_declared_ty_of_ty_param(param_ty);
        self.object_mapped_ty_links_arena[mapped_ty.links].set_ty_param(t);
        t
    }

    pub(super) fn get_constraint_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let m = ty.kind.expect_object_mapped();
        if let Some(t) = self.object_mapped_ty_links_arena[m.links].get_constraint_ty() {
            return t;
        }
        let ty_param = self.get_ty_param_from_mapped_ty(ty);
        let t = self
            .get_constraint_of_ty_param(ty_param)
            .unwrap_or(self.error_ty);
        self.object_mapped_ty_links_arena[m.links].set_constraint_ty(t);
        t
    }

    pub(super) fn get_name_ty_from_mapped_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let mapped_ty = ty.kind.expect_object_mapped();
        mapped_ty.decl.name_ty.map(|name_ty| {
            if let Some(t) = self.object_mapped_ty_links_arena[mapped_ty.links].get_named_ty() {
                return t;
            }
            let name_ty = self.get_ty_from_type_node(name_ty);
            let name_ty = self.instantiate_ty(name_ty, mapped_ty.mapper);
            let links = &mut self.object_mapped_ty_links_arena[mapped_ty.links];
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
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let mapped_ty = ty.kind.expect_object_mapped();
        if let Some(template_ty) =
            self.object_mapped_ty_links_arena[mapped_ty.links].get_template_ty()
        {
            return template_ty;
        }
        let template_ty = if let Some(decl_ty) = mapped_ty.decl.ty {
            let decl_ty = self.get_ty_from_type_node(decl_ty);
            let is_optional = mapped_ty
                .decl
                .get_modifiers()
                .intersects(bolt_ts_ast::MappedTyModifiers::INCLUDE_OPTIONAL);
            let t = self.add_optionality(decl_ty, true, is_optional);
            self.instantiate_ty(t, mapped_ty.mapper)
        } else {
            self.error_ty
        };
        self.object_mapped_ty_links_arena[mapped_ty.links].set_template_ty(template_ty);
        template_ty
    }
}

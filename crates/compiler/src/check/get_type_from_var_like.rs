use super::TyChecker;
use super::cycle_check::ResolutionKey;
use crate::bind::SymbolID;
use crate::ty::Ty;
use crate::{ir, ty};

impl<'cx> TyChecker<'cx> {
    pub fn get_type_for_var_like(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(id).get_ty() {
            return ty;
        }

        let node_id = id.decl(self.binder);
        let node = self.p.node(node_id);

        if !self.push_ty_resolution(ResolutionKey::Type(id)) {
            // TO: error handle
            return self.any_ty;
        }

        let ty = if let Some(decl) = node.as_var_decl() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_param_decl() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_prop_signature() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_class_prop_ele() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_object_prop_member() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_object_shorthand_member() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_object_method_member() {
            decl.ty
                .map(|ty| self.get_ty_from_type_node(ty))
                .unwrap_or_else(|| self.check_object_method_member(decl))
        } else {
            unreachable!("node: {node:#?}")
        };
        self.get_mut_symbol_links(id).set_ty(ty);

        if self.pop_ty_resolution().has_cycle() {
            // TODO: error handle
            return self.any_ty;
        }
        ty
    }

    pub(super) fn get_widened_ty_for_var_like_decl(
        &mut self,
        decl: &impl ir::VarLike<'cx>,
    ) -> &'cx Ty<'cx> {
        let ty = self.get_ty_for_var_like_decl(decl, true);
        self.widen_ty_for_var_like_decl(ty, decl)
    }

    pub(super) fn get_optional_ty(&mut self, ty: &'cx Ty<'cx>, is_property: bool) -> &'cx Ty<'cx> {
        assert!(*self.config.strict_null_checks());
        let missing_or_undefined = if is_property {
            // self.missing_or_undefined_ty()
            self.undefined_ty
        } else {
            self.undefined_ty
        };
        self.get_union_ty(&[ty, missing_or_undefined], ty::UnionReduction::Lit)
    }

    pub(super) fn add_optionality(
        &mut self,
        declared_ty: &'cx Ty<'cx>,
        is_property: bool,
        is_optional: bool,
    ) -> &'cx Ty<'cx> {
        if *self.config.strict_null_checks() && is_optional {
            self.get_optional_ty(declared_ty, is_property)
        } else {
            declared_ty
        }
    }

    fn get_ty_for_var_like_decl(
        &mut self,
        decl: &impl ir::VarLike<'cx>,
        include_optionality: bool,
    ) -> Option<&'cx Ty<'cx>> {
        let ty = if let Some(decl_ty) = decl.decl_ty() {
            let is_property = self.p.node(decl.id()).is_prop_signature();
            let is_optional = include_optionality && self.p.node(decl.id()).is_optional_decl();
            let ty = self.get_ty_from_type_node(decl_ty);
            Some(self.add_optionality(ty, is_property, is_optional))
        } else if let Some(init) = decl.init() {
            let init_ty = self.check_expr_with_cache(init);
            Some(self.widened_ty_from_init(decl, init_ty))
        } else {
            None
        };
        ty
    }

    fn widen_ty_for_var_like_decl(
        &mut self,
        ty: Option<&'cx Ty<'cx>>,
        decl: &impl ir::VarLike<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = ty {
            return self.get_widened_ty(ty);
        }
        if let Some(decl) = self.p.node(decl.id()).as_param_decl() {
            if decl.dotdotdot.is_some() {
                self.any_array_ty()
            } else {
                self.any_ty
            }
        } else {
            self.any_ty
        }
    }
}

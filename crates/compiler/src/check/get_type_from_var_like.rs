use super::TyChecker;
use crate::ty::Ty;
use crate::{r#trait, ty};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_optional_ty(&mut self, ty: &'cx Ty<'cx>, is_property: bool) -> &'cx Ty<'cx> {
        assert!(self.config.strict_null_checks());
        let missing_or_undefined = if is_property {
            self.undefined_or_missing_ty
        } else {
            self.undefined_ty
        };
        if ty == missing_or_undefined
            || ty
                .kind
                .as_union()
                .is_some_and(|u| u.tys[0] == missing_or_undefined)
        {
            ty
        } else {
            self.get_union_ty(&[ty, missing_or_undefined], ty::UnionReduction::Lit)
        }
    }

    pub(super) fn add_optionality(
        &mut self,
        declared_ty: &'cx Ty<'cx>,
        is_property: bool,
        is_optional: bool,
    ) -> &'cx Ty<'cx> {
        if self.config.strict_null_checks() && is_optional {
            self.get_optional_ty(declared_ty, is_property)
        } else {
            declared_ty
        }
    }

    pub(super) fn get_ty_for_var_like_decl(
        &mut self,
        decl: &impl r#trait::VarLike<'cx>,
        include_optionality: bool,
    ) -> Option<&'cx Ty<'cx>> {
        // TODO: for in stmt
        // TODO: for of stmt
        if let Some(decl_ty) = decl.decl_ty() {
            let is_property = self.p.node(decl.id()).is_prop_signature();
            let is_optional = include_optionality && self.p.node(decl.id()).is_optional_decl();
            let ty = self.get_ty_from_type_node(decl_ty);
            return Some(self.add_optionality(ty, is_property, is_optional));
        }

        if decl.is_param() {
            let parent = self.parent(decl.id()).unwrap();
            let func = self.p.node(parent);
            if let Some(setter) = func.as_setter_decl() {
                // TODO: has bindable name
                let symbol = self.get_symbol_of_decl(setter.id);
                let getter = self
                    .binder
                    .symbol(symbol)
                    .get_declaration_of_kind(|id| self.p.node(id).is_getter_decl());
                if let Some(getter) = getter {
                    let getter_sig = self.get_sig_from_decl(getter);
                    // TODO: this_param
                    return Some(self.get_ret_ty_of_sig(getter_sig));
                }
            }
        }

        if let Some(init) = decl.init() {
            let init_ty = self.check_expr_with_cache(init);
            Some(self.widened_ty_from_init(decl, init_ty))
        } else {
            None
        }
    }

    pub(super) fn widen_ty_for_var_like_decl(
        &mut self,
        ty: Option<&'cx Ty<'cx>>,
        decl: &impl r#trait::VarLike<'cx>,
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

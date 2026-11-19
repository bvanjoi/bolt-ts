use bolt_ts_ast::keyword;
use bolt_ts_ast::{self as ast};
use bolt_ts_checker_errors as errors;
use bolt_ts_ty::TypeFlags;

use super::TyChecker;
use super::ty;

impl<'cx> TyChecker<'cx> {
    fn get_symbol_has_instance_method_of_object_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.all_types_assignable_to_kind::<false>(ty, TypeFlags::NON_PRIMITIVE) {
            let has_instance_property_name = self.get_prop_name_for_known_symbol_name(
                bolt_ts_binder::SymbolName::Atom(keyword::IDENT_HAS_INSTANCE),
            );
            if let Some(has_instance_property) =
                self.get_prop_of_ty::<false, false>(ty, has_instance_property_name)
            {
                let has_instance_property_ty = self.get_type_of_symbol(has_instance_property);
                if !self
                    .get_signatures_of_type(has_instance_property_ty, ty::SigKind::Call)
                    .is_empty()
                {
                    return Some(has_instance_property_ty);
                }
            }
        }
        None
    }

    fn resolve_instanceof_expression(
        &mut self,
        right: &'cx ast::Expr<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Sig<'cx> {
        if self.is_type_any(right_ty) {
            return self.any_sig();
        };
        if let Some(_has_instance_method_type) =
            self.get_symbol_has_instance_method_of_object_ty(right_ty)
        {
            // TODO:
        } else if !(self.ty_has_call_or_ctor_sigs(right_ty)
            || self.is_ty_sub_type_of(right_ty, self.global_fn_ty()))
        {
            let error = errors::TheRightHandSideOfAnInstanceofExpressionMustBeOfTypeAnyAnObjectTypeOrATypeParameter {
                span: right.span(),
            };
            self.push_error(Box::new(error));
            return self.unknown_sig();
        }
        self.any_sig()
    }

    pub(super) fn check_instanceof_expr(
        &mut self,
        left: &'cx ast::Expr<'cx>,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if left_ty == self.silent_never_ty || right_ty == self.silent_never_ty {
            return self.silent_never_ty;
        }
        if !self.is_type_any(left_ty)
            && self.all_types_assignable_to_kind::<false>(left_ty, ty::TypeFlags::PRIMITIVE)
        {
            let error = errors::TheLeftHandSideOfAnInstanceofExpressionMustBeOfTypeAnyAnObjectTypeOrATypeParameter {
                span: left.span(),
            };
            self.push_error(Box::new(error));
        }
        self.resolve_instanceof_expression(right, right_ty);
        self.boolean_ty()
    }
}

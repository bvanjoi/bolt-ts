use super::IterationTypeKind;
use super::TyChecker;
use super::ty;

use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    pub(super) fn unwrap_ret_ty(
        &mut self,
        ret_ty: &'cx ty::Ty<'cx>,
        flags: ast::FnFlags,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let is_generator = flags.contains(ast::FnFlags::GENERATOR);
        let is_async = flags.contains(ast::FnFlags::ASYNC);
        if is_generator {
            let Some(ret_iteration_ty) = self.get_iteration_ty_of_generator_fn_return_ty(
                IterationTypeKind::Return,
                ret_ty,
                is_async,
            ) else {
                return Some(self.error_ty);
            };
            if is_async {
                let ty = self.unwrap_awaited_ty(ret_iteration_ty);
                self.get_awaited_ty_no_alias(ty)
            } else {
                Some(ret_iteration_ty)
            }
        } else if is_async {
            Some(
                self.get_awaited_ty_no_alias(ret_ty)
                    .unwrap_or(self.error_ty),
            )
        } else {
            Some(ret_ty)
        }
    }

    pub(super) fn unwrap_awaited_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(u) = ty.kind.as_union() {
            self.map_union_ty(ty, u, |this, t| Some(this.unwrap_awaited_ty(t)), false)
                .unwrap()
        } else if self.is_awaited_ty_instantiation(ty) {
            ty.alias_ty_arguments().unwrap()[0]
        } else {
            ty
        }
    }
}

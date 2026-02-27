use super::TyChecker;
use super::ty;

use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    pub(super) fn unwrap_ret_ty(
        &mut self,
        ret_ty: &'cx ty::Ty<'cx>,
        flags: ast::FnFlags,
    ) -> &'cx ty::Ty<'cx> {
        let is_generator = flags.contains(ast::FnFlags::GENERATOR);
        let is_async = flags.contains(ast::FnFlags::ASYNC);
        if is_generator {
            todo!()
        } else if is_async {
            self.get_awaited_ty_no_alias(ret_ty)
                .unwrap_or(self.error_ty)
        } else {
            ret_ty
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

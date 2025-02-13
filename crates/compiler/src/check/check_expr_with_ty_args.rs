use crate::{ast, ty};

use super::TyChecker;

pub(super) trait ExprWithTyArgs<'cx> {
    fn id(&self) -> ast::NodeID;
    fn expr_name(&self) -> &'cx ast::EntityName<'cx>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
}

impl<'cx> ExprWithTyArgs<'cx> for ast::TypeofTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn expr_name(&self) -> &'cx ast::EntityName<'cx> {
        self.name
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_expr_with_ty_args(
        &mut self,
        node: &impl ExprWithTyArgs<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(ty_args) = node.ty_args() {
            for ty_arg in ty_args.list {
                self.check_ty(ty_arg);
            }
        }
        use ast::EntityNameKind;
        let expr_ty = match node.expr_name().kind {
            EntityNameKind::Ident(ident) => self.check_ident(ident),
            EntityNameKind::Qualified(_) => {
                // TODO: fix
                self.undefined_ty
            }
        };
        self.get_instantiation_expr_ty(expr_ty, node)
    }

    fn get_instantiation_expr_ty(
        &mut self,
        expr_ty: &'cx ty::Ty<'cx>,
        node: &impl ExprWithTyArgs<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(ty_args) = node.ty_args() else {
            return expr_ty;
        };
        if expr_ty == self.silent_never_ty || self.is_error(expr_ty) {
            return expr_ty;
        }
        // TODO: instantiate
        expr_ty
    }
}

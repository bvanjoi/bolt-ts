use crate::ty;

use super::TyChecker;
use bolt_ts_ast::{self as ast, keyword};

pub(super) trait ExprWithTyArgs<'cx> {
    fn id(&self) -> ast::NodeID;
    fn expr_name(&self) -> Option<&'cx ast::EntityName<'cx>>;
    fn expr(&self) -> Option<&'cx ast::Expr<'cx>>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
}

impl<'cx> ExprWithTyArgs<'cx> for ast::TypeofTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn expr_name(&self) -> Option<&'cx ast::EntityName<'cx>> {
        Some(self.name)
    }
    fn expr(&self) -> Option<&'cx ast::Expr<'cx>> {
        None
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
}

impl<'cx> ExprWithTyArgs<'cx> for ast::ExprWithTyArgs<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn expr_name(&self) -> Option<&'cx ast::EntityName<'cx>> {
        None
    }
    fn expr(&self) -> Option<&'cx ast::Expr<'cx>> {
        Some(self.expr)
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
        let expr_ty = if let Some(expr_name) = node.expr_name() {
            // typeof node
            if let ast::EntityNameKind::Ident(ident) = expr_name.kind {
                if ident.name == keyword::KW_THIS {
                    // TODO: check this expr
                }
            }
            self.check_entity_name(expr_name)
        } else {
            // exprWithTyArgs node
            self.check_expr(node.expr().unwrap())
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

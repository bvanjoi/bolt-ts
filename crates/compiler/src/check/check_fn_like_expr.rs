use super::TyChecker;
use crate::{ast, ty};

pub(super) trait FnLikeExpr<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn body(&self) -> ast::ArrowFnExprBody<'cx>;
}

impl<'cx> FnLikeExpr<'cx> for ast::FnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> ast::ArrowFnExprBody<'cx> {
        ast::ArrowFnExprBody::Block(self.body)
    }
}

impl<'cx> FnLikeExpr<'cx> for ast::ArrowFnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> ast::ArrowFnExprBody<'cx> {
        self.body
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_expr(&mut self, expr: &impl FnLikeExpr<'cx>) -> &'cx ty::Ty<'cx> {
        match expr.body() {
            ast::ArrowFnExprBody::Block(block) => self.check_block(block),
            ast::ArrowFnExprBody::Expr(expr) => {
                self.check_expr(expr);
            }
        };

        let symbol = self.get_symbol_of_decl(expr.id());
        self.get_type_of_symbol(expr.id().module(), symbol)
    }
}

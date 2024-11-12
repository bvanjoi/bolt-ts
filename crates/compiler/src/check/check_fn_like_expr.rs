use super::TyChecker;
use crate::{ast, ty};

pub(super) trait FnLikeExpr<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn body(&self) -> &'cx ast::BlockStmt<'cx>;
}

impl<'cx> FnLikeExpr<'cx> for ast::FnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> &'cx ast::BlockStmt<'cx> {
        &self.body
    }
}

impl<'cx> FnLikeExpr<'cx> for ast::ArrowFnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> &'cx ast::BlockStmt<'cx> {
        &self.body
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_expr(&mut self, expr: &impl FnLikeExpr<'cx>) -> &'cx ty::Ty<'cx> {
        self.check_block(expr.body());
        let symbol = self.get_symbol_of_decl(expr.id());
        self.get_type_of_symbol(symbol)
    }
}

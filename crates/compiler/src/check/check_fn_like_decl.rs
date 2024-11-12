use super::TyChecker;
use crate::ast;

pub(super) trait FnLikeDecl<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn body(&self) -> &'cx ast::BlockStmt<'cx>;
}

impl<'cx> FnLikeDecl<'cx> for ast::FnDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> &'cx ast::BlockStmt<'cx> {
        &self.body
    }
}

impl<'cx> FnLikeDecl<'cx> for ast::ClassMethodEle<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> &'cx ast::BlockStmt<'cx> {
        &self.body
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_decl(&mut self, expr: &impl FnLikeDecl<'cx>) {
        self.check_block(expr.body());
    }
}

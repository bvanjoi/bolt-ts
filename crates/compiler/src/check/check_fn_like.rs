use super::relation::RelationKind;
use super::sig::Sig;
use super::ExpectedArgsCount;
use super::TyChecker;
use crate::bind::SymbolKind;
use crate::{ast, errors, ty};
use rts_span::Span;

pub(super) trait FnLikeExpr<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn is_fn_expr() -> bool;
    fn body(&self) -> &'cx ast::BlockStmt<'cx>;
}

impl<'cx> FnLikeExpr<'cx> for ast::FnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn is_fn_expr() -> bool {
        true
    }
    fn body(&self) -> &'cx ast::BlockStmt<'cx> {
        &self.body
    }
}

impl<'cx> FnLikeExpr<'cx> for ast::ArrowFnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn is_fn_expr() -> bool {
        false
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

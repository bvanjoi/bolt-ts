use super::{CheckMode, TyChecker};
use crate::{ast, ir, ty};

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_expr(
        &mut self,
        expr: &impl ir::FnExprLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(context) = self.get_check_context() {
            if context.mode.intersects(CheckMode::SKIP_CONTEXT_SENSITIVE) {
                return self.any_ty();
            }
        }

        match ir::FnExprLike::body(expr) {
            ast::ArrowFnExprBody::Block(block) => self.check_block(block),
            ast::ArrowFnExprBody::Expr(expr) => {
                self.check_expr(expr);
            }
        };

        let symbol = self.get_symbol_of_decl(expr.id());
        self.get_type_of_symbol(symbol)
    }
}

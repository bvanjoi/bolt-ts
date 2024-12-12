use bolt_ts_span::Span;

use crate::ast;

pub trait CallLike<'cx>: Copy + std::fmt::Debug {
    fn callee(&self) -> &'cx ast::Expr<'cx>;
    fn args(&self) -> ast::Exprs<'cx>;
    fn span(&self) -> Span;
}

impl<'cx> CallLike<'cx> for ast::CallExpr<'cx> {
    fn callee(&self) -> &'cx ast::Expr<'cx> {
        self.expr
    }
    fn args(&self) -> ast::Exprs<'cx> {
        self.args
    }
    fn span(&self) -> Span {
        self.span
    }
}

impl<'cx> CallLike<'cx> for ast::NewExpr<'cx> {
    fn callee(&self) -> &'cx ast::Expr<'cx> {
        self.expr
    }
    fn args(&self) -> ast::Exprs<'cx> {
        self.args.unwrap_or_default()
    }
    fn span(&self) -> Span {
        self.span
    }
}

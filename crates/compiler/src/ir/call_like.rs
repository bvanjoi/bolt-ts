use bolt_ts_ast as ast;
use bolt_ts_span::Span;

pub trait CallLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn callee(&self) -> &'cx ast::Expr<'cx>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
    fn args(&self) -> ast::Exprs<'cx>;
    fn span(&self) -> Span;
}

impl<'cx> CallLike<'cx> for ast::CallExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn callee(&self) -> &'cx ast::Expr<'cx> {
        self.expr
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
    fn args(&self) -> ast::Exprs<'cx> {
        self.args
    }
    fn span(&self) -> Span {
        self.span
    }
}

impl<'cx> CallLike<'cx> for ast::NewExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn callee(&self) -> &'cx ast::Expr<'cx> {
        self.expr
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
    fn args(&self) -> ast::Exprs<'cx> {
        self.args.unwrap_or_default()
    }
    fn span(&self) -> Span {
        self.span
    }
}

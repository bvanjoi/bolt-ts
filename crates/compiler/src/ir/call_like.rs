use bolt_ts_ast as ast;
use bolt_ts_span::Span;

use crate::check::TyChecker;

pub trait CallLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn callee(&self) -> &'cx ast::Expr<'cx>;
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>>;
    fn args(&self) -> ast::Exprs<'cx>;
    fn effective_call_args(&self, checker: &mut TyChecker<'cx>) -> ast::Exprs<'cx>;
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
    fn effective_call_args(&self, _: &mut TyChecker<'cx>) -> bolt_ts_ast::Exprs<'cx> {
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
    fn effective_call_args(&self, _: &mut TyChecker<'cx>) -> bolt_ts_ast::Exprs<'cx> {
        self.args.unwrap_or_default()
    }
    fn span(&self) -> Span {
        self.span
    }
}

impl<'cx> CallLike<'cx> for ast::TaggedTemplateExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn callee(&self) -> &'cx ast::Expr<'cx> {
        self.tag
    }
    fn ty_args(&self) -> Option<&'cx ast::Tys<'cx>> {
        self.ty_args
    }
    fn args(&self) -> ast::Exprs<'cx> {
        let addr = std::ptr::addr_of!(self.tpl);
        let ptr = addr as *const &'cx ast::Expr<'cx>;
        unsafe { std::slice::from_raw_parts(ptr, 1) }
    }
    fn effective_call_args(&self, checker: &mut TyChecker<'cx>) -> bolt_ts_ast::Exprs<'cx> {
        // self.args
        todo!()
    }
    fn span(&self) -> Span {
        self.span
    }
}

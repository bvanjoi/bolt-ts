use bolt_ts_span::Span;

pub trait CallLike<'cx>: std::fmt::Debug {
    fn id(&self) -> crate::NodeID;
    fn callee(&self) -> &'cx crate::Expr<'cx>;
    fn ty_args(&self) -> Option<&'cx crate::Tys<'cx>>;
    fn args(&self) -> crate::Exprs<'cx>;
    fn span(&self) -> Span;
}

impl<'cx> CallLike<'cx> for crate::CallExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn callee(&self) -> &'cx crate::Expr<'cx> {
        self.expr
    }
    fn ty_args(&self) -> Option<&'cx crate::Tys<'cx>> {
        self.ty_args
    }
    fn args(&self) -> crate::Exprs<'cx> {
        self.args
    }
    fn span(&self) -> Span {
        self.span
    }
}

impl<'cx> CallLike<'cx> for crate::NewExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn callee(&self) -> &'cx crate::Expr<'cx> {
        self.expr
    }
    fn ty_args(&self) -> Option<&'cx crate::Tys<'cx>> {
        self.ty_args
    }
    fn args(&self) -> crate::Exprs<'cx> {
        self.args.unwrap_or_default()
    }
    fn span(&self) -> Span {
        self.span
    }
}

impl<'cx> CallLike<'cx> for crate::TaggedTemplateExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn callee(&self) -> &'cx crate::Expr<'cx> {
        self.tag
    }
    fn ty_args(&self) -> Option<&'cx crate::Tys<'cx>> {
        self.ty_args
    }
    fn args(&self) -> crate::Exprs<'cx> {
        let addr = std::ptr::addr_of!(self.tpl);
        unsafe { std::slice::from_raw_parts(addr, 1) }
    }
    fn span(&self) -> Span {
        self.span
    }
}

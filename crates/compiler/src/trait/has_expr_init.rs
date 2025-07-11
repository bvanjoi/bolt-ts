pub trait HasExprInit<'cx>: Copy {
    fn id(&self) -> bolt_ts_ast::NodeID;
    fn init(&self) -> Option<&'cx bolt_ts_ast::Expr<'cx>>;
}

impl<'cx> HasExprInit<'cx> for bolt_ts_ast::ParamDecl<'cx> {
    fn id(&self) -> bolt_ts_ast::NodeID {
        self.id
    }
    fn init(&self) -> Option<&'cx bolt_ts_ast::Expr<'cx>> {
        self.init
    }
}

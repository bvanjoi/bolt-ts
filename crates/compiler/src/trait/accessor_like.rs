use bolt_ts_ast as ast;

pub trait AccessorLike<'cx>: Copy {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>>;
}

impl<'cx> AccessorLike<'cx> for ast::GetterDecl<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> AccessorLike<'cx> for ast::SetterDecl<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

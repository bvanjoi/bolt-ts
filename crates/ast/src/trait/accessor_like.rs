pub trait AccessorLike<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>>;
}

impl<'cx> AccessorLike<'cx> for crate::GetterDecl<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> AccessorLike<'cx> for crate::SetterDecl<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
}

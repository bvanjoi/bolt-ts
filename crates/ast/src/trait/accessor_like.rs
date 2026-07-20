pub trait AccessorLike<'cx>: super::SignatureDeclaration<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>>;
    fn name(&self) -> &'cx crate::PropName<'cx>;
}

impl<'cx> AccessorLike<'cx> for crate::GetterDecl<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
    fn name(&self) -> &'cx crate::PropName<'cx> {
        self.name
    }
}

impl<'cx> AccessorLike<'cx> for crate::SetterDecl<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
    fn name(&self) -> &'cx crate::PropName<'cx> {
        self.name
    }
}

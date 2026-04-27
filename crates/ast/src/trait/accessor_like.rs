pub trait AccessorLike<'cx> {
    fn id(&self) -> crate::NodeID;
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>>;
}

impl<'cx> AccessorLike<'cx> for crate::GetterDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }

    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> AccessorLike<'cx> for crate::SetterDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
}

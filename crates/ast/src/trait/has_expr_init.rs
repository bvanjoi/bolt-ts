pub trait HasExprInit<'cx> {
    fn id(&self) -> crate::NodeID;
    fn init(&self) -> Option<&'cx crate::Expr<'cx>>;
}

impl<'cx> HasExprInit<'cx> for crate::ParamDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        self.init
    }
}

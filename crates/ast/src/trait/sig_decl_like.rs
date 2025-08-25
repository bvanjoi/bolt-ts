pub trait SigDeclLike<'cx> {
    fn id(&self) -> crate::NodeID;
    fn params(&self) -> crate::ParamsDecl<'cx>;
    fn has_rest_param(&self) -> bool;
    fn body(&self) -> Option<crate::NodeID>;
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>>;
}

impl<'cx> SigDeclLike<'cx> for crate::ArrowFnExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn has_rest_param(&self) -> bool {
        crate::has_rest_param(self.params)
    }
    fn body(&self) -> Option<crate::NodeID> {
        use crate::ArrowFnExprBody::*;
        Some(match self.body {
            Block(n) => n.id,
            Expr(n) => n.id(),
        })
    }
    fn ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
}

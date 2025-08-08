pub trait SigDeclLike {
    fn id(&self) -> crate::NodeID;
    fn params(&self) -> crate::ParamsDecl;
    fn has_rest_param(&self) -> bool;
    fn body(&self) -> Option<crate::NodeID>;
}

impl SigDeclLike for crate::ArrowFnExpr<'_> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn params(&self) -> crate::ParamsDecl {
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
}

use bolt_ts_ast as ast;

pub trait SigDeclLike {
    fn id(&self) -> ast::NodeID;
    fn flags(&self, node_flags_map: &bolt_ts_parser::NodeFlagsMap) -> ast::NodeFlags {
        node_flags_map.get(self.id())
    }
    fn params(&self) -> ast::ParamsDecl;
    fn has_rest_param(&self) -> bool;
    fn body(&self) -> Option<ast::NodeID>;
}

impl SigDeclLike for ast::ArrowFnExpr<'_> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> bolt_ts_ast::ParamsDecl {
        self.params
    }
    fn has_rest_param(&self) -> bool {
        ast::has_rest_param(self.params)
    }
    fn body(&self) -> Option<bolt_ts_ast::NodeID> {
        use bolt_ts_ast::ArrowFnExprBody::*;
        Some(match self.body {
            Block(n) => n.id,
            Expr(n) => n.id(),
        })
    }
}

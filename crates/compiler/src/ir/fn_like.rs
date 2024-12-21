use crate::ast;

pub trait FnLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn params(&self) -> ast::ParamsDecl<'cx>;
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>>;
}

impl<'cx> FnLike<'cx> for ast::FnDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        self.body.map(|b| ast::ArrowFnExprBody::Block(b))
    }
}

impl<'cx> FnLike<'cx> for ast::ClassMethodEle<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        self.body.map(|b| ast::ArrowFnExprBody::Block(b))
    }
}

impl<'cx> FnLike<'cx> for ast::ClassCtor<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        self.body.map(|b| ast::ArrowFnExprBody::Block(b))
    }
}

impl<'cx> FnLike<'cx> for ast::CtorSigDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        None
    }
}

impl<'cx> FnLike<'cx> for ast::FnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        Some(ast::ArrowFnExprBody::Block(self.body))
    }
}

impl<'cx> FnLike<'cx> for ast::ArrowFnExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        Some(self.body)
    }
}

pub trait FnDeclLike<'cx>: FnLike<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>>;
}

impl<'cx> FnDeclLike<'cx> for ast::FnDecl<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnDeclLike<'cx> for ast::ClassMethodEle<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnDeclLike<'cx> for ast::ClassCtor<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnDeclLike<'cx> for ast::CtorSigDecl<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        None
    }
}

pub trait FnExprLike<'cx>: FnLike<'cx> {
    fn body(&self) -> ast::ArrowFnExprBody<'cx>;
}
impl<'cx> FnExprLike<'cx> for ast::FnExpr<'cx> {
    fn body(&self) -> ast::ArrowFnExprBody<'cx> {
        ast::ArrowFnExprBody::Block(self.body)
    }
}

impl<'cx> FnExprLike<'cx> for ast::ArrowFnExpr<'cx> {
    fn body(&self) -> ast::ArrowFnExprBody<'cx> {
        self.body
    }
}

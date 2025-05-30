use bolt_ts_atom::AtomId;

use bolt_ts_ast as ast;

pub trait FnLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn ty_params(&self) -> Option<ast::TyParams<'cx>>;
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
        self.body.map(ast::ArrowFnExprBody::Block)
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
}

impl<'cx> FnLike<'cx> for ast::ClassMethodElem<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        self.body.map(ast::ArrowFnExprBody::Block)
    }
}

impl<'cx> FnLike<'cx> for ast::ClassCtor<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        self.body.map(ast::ArrowFnExprBody::Block)
    }
}

impl<'cx> FnLike<'cx> for ast::CtorSigDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
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
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
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
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        Some(self.body)
    }
}

impl<'cx> FnLike<'cx> for ast::MethodSignature<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        None
    }
}

impl<'cx> FnLike<'cx> for ast::ObjectMethodMember<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        Some(ast::ArrowFnExprBody::Block(self.body))
    }
}

impl<'cx> FnLike<'cx> for ast::CallSigDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        None
    }
}

impl<'cx> FnLike<'cx> for ast::CtorTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<bolt_ts_ast::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<ast::ArrowFnExprBody<'cx>> {
        None
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

impl<'cx> FnDeclLike<'cx> for ast::ClassMethodElem<'cx> {
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

impl<'cx> FnDeclLike<'cx> for ast::CtorTy<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        None
    }
}

impl<'cx> FnDeclLike<'cx> for ast::MethodSignature<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        None
    }
}

impl<'cx> FnDeclLike<'cx> for ast::ObjectMethodMember<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        Some(self.body)
    }
}

impl<'cx> FnDeclLike<'cx> for ast::CallSigDecl<'cx> {
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        None
    }
}

pub trait FnExprLike<'cx>: FnLike<'cx> {
    fn name(&self) -> Option<AtomId>;
    fn body(&self) -> ast::ArrowFnExprBody<'cx>;
}
impl<'cx> FnExprLike<'cx> for ast::FnExpr<'cx> {
    fn name(&self) -> Option<AtomId> {
        self.name.map(|name| name.name)
    }
    fn body(&self) -> ast::ArrowFnExprBody<'cx> {
        ast::ArrowFnExprBody::Block(self.body)
    }
}
impl<'cx> FnExprLike<'cx> for ast::ArrowFnExpr<'cx> {
    fn name(&self) -> Option<AtomId> {
        None
    }
    fn body(&self) -> ast::ArrowFnExprBody<'cx> {
        self.body
    }
}

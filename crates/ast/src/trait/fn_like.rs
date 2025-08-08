use bolt_ts_atom::AtomId;

pub trait FnLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> crate::NodeID;
    fn ty_params(&self) -> Option<crate::TyParams<'cx>>;
    fn params(&self) -> crate::ParamsDecl<'cx>;
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>>;
}

impl<'cx> FnLike<'cx> for crate::FnDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        self.body.map(crate::ArrowFnExprBody::Block)
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
}

impl<'cx> FnLike<'cx> for crate::ClassMethodElem<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        self.body.map(crate::ArrowFnExprBody::Block)
    }
}

impl<'cx> FnLike<'cx> for crate::ClassCtor<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        self.body.map(crate::ArrowFnExprBody::Block)
    }
}

impl<'cx> FnLike<'cx> for crate::CtorSigDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        None
    }
}

impl<'cx> FnLike<'cx> for crate::FnExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        Some(crate::ArrowFnExprBody::Block(self.body))
    }
}

impl<'cx> FnLike<'cx> for crate::ArrowFnExpr<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        Some(self.body)
    }
}

impl<'cx> FnLike<'cx> for crate::MethodSignature<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        None
    }
}

impl<'cx> FnLike<'cx> for crate::ObjectMethodMember<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        Some(crate::ArrowFnExprBody::Block(self.body))
    }
}

impl<'cx> FnLike<'cx> for crate::CallSigDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        None
    }
}

impl<'cx> FnLike<'cx> for crate::CtorTy<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn params(&self) -> crate::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<crate::ArrowFnExprBody<'cx>> {
        None
    }
}

pub trait FnDeclLike<'cx>: FnLike<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>>;
}

impl<'cx> FnDeclLike<'cx> for crate::FnDecl<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnDeclLike<'cx> for crate::ClassMethodElem<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnDeclLike<'cx> for crate::ClassCtor<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnDeclLike<'cx> for crate::CtorSigDecl<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        None
    }
}

impl<'cx> FnDeclLike<'cx> for crate::CtorTy<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        None
    }
}

impl<'cx> FnDeclLike<'cx> for crate::MethodSignature<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        None
    }
}

impl<'cx> FnDeclLike<'cx> for crate::ObjectMethodMember<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        Some(self.body)
    }
}

impl<'cx> FnDeclLike<'cx> for crate::CallSigDecl<'cx> {
    fn body(&self) -> Option<&'cx crate::BlockStmt<'cx>> {
        None
    }
}

pub trait FnExprLike<'cx>: FnLike<'cx> {
    fn name(&self) -> Option<AtomId>;
    fn body(&self) -> crate::ArrowFnExprBody<'cx>;
}
impl<'cx> FnExprLike<'cx> for crate::FnExpr<'cx> {
    fn name(&self) -> Option<AtomId> {
        self.name.map(|name| name.name)
    }
    fn body(&self) -> crate::ArrowFnExprBody<'cx> {
        crate::ArrowFnExprBody::Block(self.body)
    }
}
impl<'cx> FnExprLike<'cx> for crate::ArrowFnExpr<'cx> {
    fn name(&self) -> Option<AtomId> {
        None
    }
    fn body(&self) -> crate::ArrowFnExprBody<'cx> {
        self.body
    }
}

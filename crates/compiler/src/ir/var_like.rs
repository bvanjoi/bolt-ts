use crate::ast;

pub trait VarLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn name(&self) -> &'cx ast::Ident;
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>>;
    fn init(&self) -> Option<&'cx ast::Expr<'cx>>;
}

impl<'cx> VarLike<'cx> for ast::VarDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }

    fn name(&self) -> &'cx ast::Ident {
        self.binding
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }
}

impl<'cx> VarLike<'cx> for ast::ParamDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> &'cx ast::Ident {
        self.name
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }
}

impl<'cx> VarLike<'cx> for ast::ClassPropEle<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> &'cx ast::Ident {
        match self.name.kind {
            ast::PropNameKind::Ident(ident) => ident,
            ast::PropNameKind::NumLit(_) => todo!(),
            ast::PropNameKind::StringLit(lit) => todo!(),
        }
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }
}

impl<'cx> VarLike<'cx> for ast::PropSignature<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> &'cx ast::Ident {
        match self.name.kind {
            ast::PropNameKind::Ident(ident) => ident,
            ast::PropNameKind::NumLit(_) => todo!(),
            ast::PropNameKind::StringLit(_) => todo!(),
        }
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        None
    }
}

impl<'cx> VarLike<'cx> for ast::ObjectPropMember<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> &'cx ast::Ident {
        match self.name.kind {
            ast::PropNameKind::Ident(ident) => ident,
            ast::PropNameKind::NumLit(_) => todo!(),
            ast::PropNameKind::StringLit(_) => todo!(),
        }
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        None
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        Some(self.value)
    }
}

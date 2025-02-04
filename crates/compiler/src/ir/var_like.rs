use crate::ast;

pub enum VarLikeName<'cx> {
    Ident(&'cx ast::Ident),
    ObjectPat(&'cx ast::ObjectPat<'cx>),
    NumLit(&'cx ast::NumLit),
    StringLit {
        raw: &'cx ast::StringLit,
        key: bolt_ts_atom::AtomId,
    },
}

impl<'cx> From<&ast::PropName<'cx>> for VarLikeName<'cx> {
    fn from(value: &ast::PropName<'cx>) -> Self {
        match value.kind {
            ast::PropNameKind::Ident(ident) => VarLikeName::Ident(ident),
            ast::PropNameKind::NumLit(num) => VarLikeName::NumLit(num),
            ast::PropNameKind::StringLit { raw, key } => VarLikeName::StringLit { raw, key },
        }
    }
}

pub trait VarLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn name(&self) -> VarLikeName<'cx>;
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>>;
    fn init(&self) -> Option<&'cx ast::Expr<'cx>>;
}

impl<'cx> VarLike<'cx> for ast::VarDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        match self.binding {
            ast::Binding::Ident(n) => VarLikeName::Ident(n),
            ast::Binding::ObjectPat(n) => VarLikeName::ObjectPat(n),
        }
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
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::Ident(self.name)
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }
}

impl<'cx> VarLike<'cx> for ast::ClassPropElem<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::from(self.name)
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
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::from(self.name)
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
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::from(self.name)
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        None
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        Some(self.value)
    }
}

impl<'cx> VarLike<'cx> for ast::ObjectShorthandMember<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::Ident(self.name)
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        None
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        None
    }
}

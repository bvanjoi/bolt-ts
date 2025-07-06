use bolt_ts_ast::{self as ast, NodeFlags};

pub enum VarLikeName<'cx> {
    Ident(&'cx ast::Ident),
    ObjectPat(&'cx ast::ObjectPat<'cx>),
    ArrayPat(&'cx ast::ArrayPat<'cx>),
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
            ast::PropNameKind::Computed(_) => todo!(),
        }
    }
}

pub trait VarLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn name(&self) -> VarLikeName<'cx>;
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>>;
    fn init(&self) -> Option<&'cx ast::Expr<'cx>>;
    fn is_param(&self) -> bool {
        false
    }
    fn is_var_const(&self, _: &'cx bolt_ts_parser::Parser<'cx>) -> bool {
        false
    }
}

impl<'cx> VarLike<'cx> for ast::VarDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        match self.binding.kind {
            ast::BindingKind::Ident(n) => VarLikeName::Ident(n),
            ast::BindingKind::ObjectPat(n) => VarLikeName::ObjectPat(n),
            bolt_ts_ast::BindingKind::ArrayPat(n) => VarLikeName::ArrayPat(n),
        }
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }
    fn is_var_const(&self, p: &'cx bolt_ts_parser::Parser<'cx>) -> bool {
        let block_scope_kind = p
            .get_combined_node_flags(self.id)
            .intersection(NodeFlags::BLOCK_SCOPED);
        block_scope_kind.intersects(
            NodeFlags::CONST
                .union(NodeFlags::USING)
                .union(NodeFlags::AWAIT_USING),
        )
    }
}

impl<'cx> VarLike<'cx> for ast::ParamDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        match self.name.kind {
            bolt_ts_ast::BindingKind::Ident(n) => VarLikeName::Ident(n),
            bolt_ts_ast::BindingKind::ObjectPat(n) => VarLikeName::ObjectPat(n),
            bolt_ts_ast::BindingKind::ArrayPat(n) => VarLikeName::ArrayPat(n),
        }
    }
    fn decl_ty(&self) -> Option<&'cx ast::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx ast::Expr<'cx>> {
        self.init
    }
    fn is_param(&self) -> bool {
        true
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
        Some(self.init)
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

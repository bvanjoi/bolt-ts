pub enum VarLikeName<'cx> {
    Ident(&'cx crate::Ident),
    ObjectPat(&'cx crate::ObjectPat<'cx>),
    ArrayPat(&'cx crate::ArrayPat<'cx>),
    NumLit(&'cx crate::NumLit),
    StringLit {
        raw: &'cx crate::StringLit,
        key: bolt_ts_atom::Atom,
    },
    Computed(&'cx crate::ComputedPropName<'cx>),
}

impl<'cx> From<&crate::PropName<'cx>> for VarLikeName<'cx> {
    fn from(value: &crate::PropName<'cx>) -> Self {
        match value.kind {
            crate::PropNameKind::Ident(ident) => VarLikeName::Ident(ident),
            crate::PropNameKind::NumLit(num) => VarLikeName::NumLit(num),
            crate::PropNameKind::StringLit { raw, key } => VarLikeName::StringLit { raw, key },
            crate::PropNameKind::Computed(computed) => VarLikeName::Computed(computed),
        }
    }
}

pub trait VarLike<'cx>: std::fmt::Debug {
    fn id(&self) -> crate::NodeID;
    fn name(&self) -> VarLikeName<'cx>;
    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>>;
    fn init(&self) -> Option<&'cx crate::Expr<'cx>>;
    fn is_param(&self) -> bool {
        false
    }
}

impl<'cx> VarLike<'cx> for crate::VarDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        binding_kind_to_var_like_name(&self.name.kind)
    }
    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        self.init
    }
}

impl<'cx> VarLike<'cx> for crate::ParamDecl<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        binding_kind_to_var_like_name(&self.name.kind)
    }
    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        self.init
    }
    fn is_param(&self) -> bool {
        true
    }
}

impl<'cx> VarLike<'cx> for crate::ClassPropElem<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::from(self.name)
    }
    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        self.init
    }
}

impl<'cx> VarLike<'cx> for crate::PropSignature<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::from(self.name)
    }
    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        self.ty
    }
    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        None
    }
}

impl<'cx> VarLike<'cx> for crate::ObjectPropAssignment<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::from(self.name)
    }
    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        None
    }
    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        Some(self.init)
    }
}

impl<'cx> VarLike<'cx> for crate::ObjectShorthandMember<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn name(&self) -> VarLikeName<'cx> {
        VarLikeName::Ident(self.name)
    }
    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        None
    }
    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        None
    }
}

impl<'cx> VarLike<'cx> for crate::ArrayBinding<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }

    fn name(&self) -> VarLikeName<'cx> {
        binding_kind_to_var_like_name(&self.name.kind)
    }

    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        None
    }

    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        self.init
    }
}

fn binding_kind_to_var_like_name<'cx>(kind: &crate::BindingKind<'cx>) -> VarLikeName<'cx> {
    match kind {
        crate::BindingKind::Ident(n) => VarLikeName::Ident(n),
        crate::BindingKind::ObjectPat(n) => VarLikeName::ObjectPat(n),
        crate::BindingKind::ArrayPat(n) => VarLikeName::ArrayPat(n),
    }
}

impl<'cx> VarLike<'cx> for crate::ObjectBindingElem<'cx> {
    fn id(&self) -> crate::NodeID {
        self.id
    }

    fn name(&self) -> VarLikeName<'cx> {
        match self.name {
            crate::ObjectBindingName::Shorthand(ident) => VarLikeName::Ident(&ident),
            crate::ObjectBindingName::Prop { name, .. } => {
                binding_kind_to_var_like_name(&name.kind)
            }
        }
    }

    fn decl_ty(&self) -> Option<&'cx crate::Ty<'cx>> {
        None
    }

    fn init(&self) -> Option<&'cx crate::Expr<'cx>> {
        self.init
    }
}

pub trait ClassLike<'cx>: std::fmt::Debug {
    fn id(&self) -> crate::NodeID;
    fn modifiers(&self) -> Option<&'cx crate::Modifiers<'cx>>;
    fn name(&self) -> Option<&'cx crate::Ident>;
    fn ty_params(&self) -> Option<crate::TyParams<'cx>>;
    fn extends(&self) -> Option<&'cx crate::ClassExtendsClause<'cx>>;
    fn implements(&self) -> Option<&'cx crate::ClassImplementsClause<'cx>>;
    fn elems(&self) -> &'cx crate::ClassElems<'cx>;
    fn span(&self) -> crate::Span;
}

impl<'cx> ClassLike<'cx> for crate::ClassDecl<'cx> {
    fn span(&self) -> crate::Span {
        self.span
    }
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn modifiers(&self) -> Option<&'cx crate::Modifiers<'cx>> {
        self.modifiers
    }
    fn name(&self) -> Option<&'cx crate::Ident> {
        self.name
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn extends(&self) -> Option<&'cx crate::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn implements(&self) -> Option<&'cx crate::ClassImplementsClause<'cx>> {
        self.implements
    }
    fn elems(&self) -> &'cx crate::ClassElems<'cx> {
        self.elems
    }
}

impl<'cx> ClassLike<'cx> for crate::ClassExpr<'cx> {
    fn span(&self) -> crate::Span {
        self.span
    }
    fn id(&self) -> crate::NodeID {
        self.id
    }
    fn modifiers(&self) -> Option<&'cx crate::Modifiers<'cx>> {
        None
    }
    fn name(&self) -> Option<&'cx crate::Ident> {
        self.name
    }
    fn ty_params(&self) -> Option<crate::TyParams<'cx>> {
        self.ty_params
    }
    fn extends(&self) -> Option<&'cx crate::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn implements(&self) -> Option<&'cx crate::ClassImplementsClause<'cx>> {
        self.implements
    }
    fn elems(&self) -> &'cx crate::ClassElems<'cx> {
        self.elems
    }
}

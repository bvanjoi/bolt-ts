use crate::ast;

pub trait ClassLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn name(&self) -> Option<&'cx ast::Ident>;
    fn ty_params(&self) -> Option<ast::TyParams<'cx>>;
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>>;
    fn implements(&self) -> Option<&'cx ast::ImplementsClause<'cx>>;
    fn elems(&self) -> &'cx ast::ClassElems<'cx>;
}

impl<'cx> ClassLike<'cx> for ast::ClassDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> Option<&'cx ast::Ident> {
        Some(self.name)
    }
    fn ty_params(&self) -> Option<ast::TyParams<'cx>> {
        self.ty_params
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn implements(&self) -> Option<&'cx ast::ImplementsClause<'cx>> {
        self.implements
    }
    fn elems(&self) -> &'cx ast::ClassElems<'cx> {
        self.elems
    }
}

impl<'cx> ClassLike<'cx> for ast::ClassExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> Option<&'cx ast::Ident> {
        self.name
    }
    fn ty_params(&self) -> Option<ast::TyParams<'cx>> {
        self.ty_params
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn implements(&self) -> Option<&'cx ast::ImplementsClause<'cx>> {
        self.implements
    }
    fn elems(&self) -> &'cx ast::ClassElems<'cx> {
        self.elems
    }
}

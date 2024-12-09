use crate::ast;

pub trait ClassLike<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>>;
    fn implements(&self) -> Option<&'cx ast::ImplementsClause<'cx>>;
    fn elems(&self) -> &'cx ast::ClassElems<'cx>;
}

impl<'cx> ClassLike<'cx> for ast::ClassDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
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

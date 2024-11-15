use crate::ast;

use super::Binder;

pub(super) trait ClassLike<'cx> {
    fn id(&self) -> ast::NodeID;
    fn create_symbol(&'cx self, binder: &mut Binder<'cx>);
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>>;
    fn elems(&self) -> &'cx ast::ClassEles<'cx>;
}

impl<'cx> ClassLike<'cx> for ast::ClassDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn create_symbol(&'cx self, binder: &mut Binder<'cx>) {
        binder.create_class_decl(self);
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn elems(&self) -> &'cx ast::ClassEles<'cx> {
        &self.eles
    }
}

impl<'cx> ClassLike<'cx> for ast::ClassExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn create_symbol(&'cx self, binder: &mut Binder<'cx>) {
        binder.create_class_expr(self);
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn elems(&self) -> &'cx ast::ClassEles<'cx> {
        &self.eles
    }
}

impl<'cx> Binder<'cx> {
    pub(super) fn bind_class_like(&mut self, class: &'cx impl ClassLike<'cx>) {
        self.connect(class.id());
        class.create_symbol(self);

        if let Some(extends) = class.extends() {
            self.bind_expr(extends.expr);
        }

        let old = self.scope_id;
        self.scope_id = self.new_scope();
        for ele in class.elems().eles {
            match ele.kind {
                ast::ClassEleKind::Prop(n) => self.bind_class_prop_ele(n),
                ast::ClassEleKind::Method(n) => self.bind_class_method_ele(n),
                ast::ClassEleKind::IndexSig(_) => {}
            }
        }
        self.scope_id = old;
    }
}

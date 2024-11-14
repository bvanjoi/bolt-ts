use super::TyChecker;
use crate::ast;

pub(super) trait ClassLikeDecl<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn eles(&self) -> ast::ClassEles<'cx>;
    fn implements(&self) -> Option<&'cx ast::ImplementsClause<'cx>>;
}

impl<'cx> ClassLikeDecl<'cx> for ast::ClassDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn eles(&self) -> ast::ClassEles<'cx> {
        self.eles
    }
    fn implements(&self) -> Option<&'cx ast::ImplementsClause<'cx>> {
        self.implements
    }
}

impl<'cx> ClassLikeDecl<'cx> for ast::ClassExpr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn eles(&self) -> ast::ClassEles<'cx> {
        self.eles
    }
    fn implements(&self) -> Option<&'cx ast::ImplementsClause<'cx>> {
        self.implements
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_class_like_decl(&mut self, class: &impl ClassLikeDecl<'cx>) {
        // let symbol = self.get_symbol_of_decl(class.id);
        if let Some(impls) = class.implements() {
            for ty in impls.tys {
                self.check_type_reference_node(ty);
            }
        }

        for ele in class.eles() {
            use ast::ClassEleKind::*;
            match ele.kind {
                Prop(prop) => self.check_class_prop_ele(prop),
                Method(method) => self.check_class_method_ele(method),
                IndexSig(_) => {}
            }
        }
    }
}

use super::Resolver;
use crate::{ast, ir};

impl<'cx> Resolver<'cx, '_> {
    fn resolve_class_prop_ele(&mut self, ele: &'cx ast::ClassPropEle<'cx>) {
        if let Some(ty) = ele.ty {
            self.resolve_ty(ty);
        }
        if let Some(init) = ele.init {
            self.resolve_expr(init);
        }
    }

    fn resolve_class_method_ele(&mut self, ele: &'cx ast::ClassMethodEle<'cx>) {
        self.resolve_params(ele.params);
        if let Some(body) = ele.body {
            self.resolve_block_stmt(body);
        }
        if let Some(ty) = ele.ty {
            self.resolve_ty(ty);
        }
    }

    pub(super) fn resolve_class_like(&mut self, class: &'cx impl ir::ClassLike<'cx>) {
        if let Some(extends) = class.extends() {
            self.resolve_expr(extends.expr);
        }

        if let Some(implements) = class.implements() {
            for ty in implements.list {
                self.resolve_refer_ty(ty);
            }
        }

        for ele in class.elems().elems {
            match ele.kind {
                ast::ClassEleKind::Prop(n) => self.resolve_class_prop_ele(n),
                ast::ClassEleKind::Method(n) => self.resolve_class_method_ele(n),
                ast::ClassEleKind::Ctor(n) => {
                    self.resolve_params(n.params);
                    if let Some(body) = n.body {
                        self.resolve_block_stmt(body);
                    }
                }
                ast::ClassEleKind::IndexSig(_) => {}
                ast::ClassEleKind::Getter(_) => {}
                ast::ClassEleKind::Setter(_) => {}
            }
        }
    }
}

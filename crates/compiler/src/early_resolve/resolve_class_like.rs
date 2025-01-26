use super::Resolver;
use crate::{ast, ir};

impl<'cx> Resolver<'cx, '_> {
    fn resolve_class_prop_ele(&mut self, ele: &'cx ast::ClassPropElem<'cx>) {
        if let Some(ty) = ele.ty {
            self.resolve_ty(ty);
        }
        if let Some(init) = ele.init {
            self.resolve_expr(init);
        }
    }

    fn resolve_class_method_ele(&mut self, ele: &'cx ast::ClassMethodElem<'cx>) {
        self.resolve_params(ele.params);
        if let Some(ty) = ele.ty {
            self.resolve_ty(ty);
        }
        if let Some(body) = ele.body {
            self.resolve_block_stmt(body);
        }
    }

    pub(super) fn resolve_class_like(&mut self, class: &'cx impl ir::ClassLike<'cx>) {
        if let Some(ty_params) = class.ty_params() {
            self.resolve_ty_params(ty_params);
        }

        if let Some(extends) = class.extends() {
            self.resolve_entity_name(extends.name, false);
            if let Some(ty_args) = extends.ty_args {
                self.resolve_tys(ty_args.list);
            }
        }

        if let Some(implements) = class.implements() {
            for ty in implements.list {
                self.resolve_refer_ty(ty);
            }
        }

        for ele in class.elems().elems {
            use ast::ClassEleKind::*;
            match ele.kind {
                Prop(n) => self.resolve_class_prop_ele(n),
                Method(n) => self.resolve_class_method_ele(n),
                Ctor(n) => {
                    self.resolve_params(n.params);
                    if let Some(body) = n.body {
                        self.resolve_block_stmt(body);
                    }
                }
                IndexSig(n) => {
                    self.resolve_index_sig(n);
                }
                Getter(_) => {}
                Setter(_) => {}
            }
        }
    }
}

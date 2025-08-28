use super::Resolver;

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;

impl<'cx> Resolver<'cx, '_, '_> {
    fn resolve_class_prop_ele(&mut self, ele: &'cx ast::ClassPropElem<'cx>) {
        self.resolve_prop_name(ele.name);
        if let Some(ty) = ele.ty {
            self.resolve_ty(ty);
        }
        if let Some(init) = ele.init {
            self.resolve_expr(init);
        }
    }

    fn resolve_class_method_ele(&mut self, ele: &'cx ast::ClassMethodElem<'cx>) {
        self.resolve_prop_name(ele.name);
        if let Some(ty_params) = ele.ty_params {
            self.resolve_ty_params(ty_params);
        }
        self.resolve_params(ele.params);
        if let Some(ty) = ele.ty {
            self.resolve_ty(ty);
        }
        if let Some(body) = ele.body {
            self.resolve_block_stmt(body);
        }
    }

    pub(super) fn resolve_class_like(&mut self, class: &'cx impl r#trait::ClassLike<'cx>) {
        if let Some(ty_params) = class.ty_params() {
            self.resolve_ty_params(ty_params);
        }

        if let Some(extends) = class.extends() {
            self.resolve_expr(extends.expr_with_ty_args.expr);
            if let Some(ty_args) = extends.expr_with_ty_args.ty_args {
                self.resolve_tys(ty_args.list);
            }
        }

        if let Some(implements) = class.implements() {
            for ty in implements.list {
                self.resolve_refer_ty(ty);
            }
        }

        for ele in class.elems().list {
            use bolt_ts_ast::ClassElemKind::*;
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
                Getter(n) => {
                    if let Some(ty) = n.ty {
                        self.resolve_ty(ty);
                    }
                    if let Some(body) = n.body {
                        self.resolve_block_stmt(body);
                    }
                }
                Setter(n) => {
                    assert!(n.params.len() == 1);
                    self.resolve_params(n.params);
                    if let Some(body) = n.body {
                        self.resolve_block_stmt(body);
                    }
                }
                StaticBlock(n) => {
                    self.resolve_block_stmt(n.body);
                }
            }
        }
    }
}

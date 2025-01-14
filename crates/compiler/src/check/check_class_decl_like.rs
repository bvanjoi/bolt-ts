use super::TyChecker;
use crate::ast;
use crate::ir::ClassLike;

impl<'cx> TyChecker<'cx> {
    fn check_ctor(&mut self, ctor: &'cx ast::ClassCtor<'cx>) {
        self.check_fn_like_decl(ctor);
    }

    fn check_class_method_ele(&mut self, method: &'cx ast::ClassMethodEle<'cx>) {
        self.check_fn_like_decl(method);
    }

    fn check_class_prop_ele(&mut self, prop: &'cx ast::ClassPropEle<'cx>) {
        self.check_var_like_decl(prop);
    }

    pub(super) fn check_class_decl_like(&mut self, class: &impl ClassLike<'cx>) {
        let symbol = self.get_symbol_of_decl(class.id());

        if let Some(ty_params) = class.ty_params() {
            self.check_ty_params(ty_params);
        }

        let ty = self.get_declared_ty_of_symbol(symbol);
        let static_ty = self.get_type_of_symbol(symbol);
        self.check_index_constraints(ty, symbol);

        if let Some(impls) = class.implements() {
            for ty in impls.list {
                let _ = self.get_ty_from_ty_reference(*ty);
            }
        }

        for ele in class.elems().elems {
            use ast::ClassEleKind::*;
            match ele.kind {
                Prop(prop) => self.check_class_prop_ele(prop),
                Method(method) => self.check_class_method_ele(method),
                IndexSig(_) => {}
                Ctor(ctor) => self.check_ctor(ctor),
                Getter(_) => {}
                Setter(_) => {}
            }
        }
    }
}

use super::TyChecker;
use crate::ir::ClassLike;
use crate::{ast, bind::SymbolKind};

impl<'cx> TyChecker<'cx> {
    fn check_ctor(&mut self, ctor: &'cx ast::ClassCtor<'cx>) {
        let symbol = self.get_symbol_of_decl(ctor.id);
        let SymbolKind::Function { decls, .. } = &self.binder.symbol(symbol).kind else {
            unreachable!()
        };
        if decls[0] == ctor.id {
            self.check_fn_like_symbol(ctor.id.module(), symbol);
        }

        if let Some(body) = ctor.body {
            self.check_block(body);
        }
    }

    fn check_class_method_ele(&mut self, method: &'cx ast::ClassMethodEle<'cx>) {
        self.check_fn_like_decl(method);
    }

    fn check_class_prop_ele(&mut self, prop: &'cx ast::ClassPropEle<'cx>) {
        self.check_var_like_decl(prop);
    }

    pub(super) fn check_class_like_decl(&mut self, class: &impl ClassLike<'cx>) {
        let symbol = self.get_symbol_of_decl(class.id());
        let ty = self.get_declared_ty_of_symbol(symbol);
        let static_ty = self.get_type_of_symbol(symbol);
        self.check_index_constraints(ty, symbol);

        if let Some(impls) = class.implements() {
            for ty in impls.tys {
                self.check_type_reference_node(ty);
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

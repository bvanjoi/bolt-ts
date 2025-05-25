use super::{TyChecker, ty};

use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_this_ty_of_decl(&mut self, decl: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let sig = self.get_sig_from_decl(decl);
        self.get_this_ty_of_sig(sig)
    }

    pub(super) fn get_this_ty_of_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        sig.this_param
            .map(|this_param| self.get_type_of_symbol(this_param))
    }

    pub(super) fn get_contextual_this_param_ty(
        &mut self,
        id: ast::NodeID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let func = self.p.node(id);

        if func.is_arrow_fn_expr() {
            return None;
        }

        if self.is_context_sensitive_fn_or_object_literal_method(id) {
            todo!()
        }

        None
    }

    pub(super) fn get_this_ty(&mut self, node: &'cx ast::ThisTy) -> &'cx ty::Ty<'cx> {
        let container = self.p.get_this_container(node.id, false, false);
        let c = self.p.node(container);
        if let Some(parent) = self.p.parent(container) {
            let p = self.p.node(parent);
            if (p.is_class_like() || p.is_interface_decl())
                && !c.is_static()
                && c.as_class_ctor().is_none_or(|c| match c.body {
                    Some(body) => self.p.is_descendant_of(node.id, body.id),
                    None => false,
                })
            {
                let s = self.get_symbol_of_decl(parent);
                let ty = self.get_declared_ty_of_class_or_interface(s);
                return if let Some(i) = ty.kind.as_object_interface() {
                    i.this_ty.unwrap()
                } else if let Some(r) = ty.kind.as_object_reference() {
                    let i = r.interface_target().unwrap().kind.expect_object_interface();
                    i.this_ty.unwrap()
                } else {
                    unreachable!()
                };
            }
        }

        self.error_ty
    }
}

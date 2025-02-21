use crate::ty;
use bolt_ts_ast as ast;

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_this_ty_of_decl(&mut self, decl: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let sig = self.get_sig_from_decl(decl);
        self.get_this_ty_of_sig(sig)
    }

    pub(super) fn get_this_ty_of_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        None
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
}

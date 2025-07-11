use super::TyChecker;
use crate::r#trait;
use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    fn check_non_pat_var_like_decl(
        &mut self,
        name_id: ast::NodeID,
        decl_id: ast::NodeID,
        decl: &'cx impl r#trait::VarLike<'cx>,
    ) {
        if !self.p.node(name_id).is_object_binding_elem() {
            if let Some(ty) = decl.decl_ty() {
                self.check_ty(ty);
            }
        }

        let symbol = self.get_symbol_of_decl(decl_id);
        let decl_ty = self.get_type_of_symbol(symbol);
        if let Some(init) = decl.init() {
            let init_ty = self.check_expr_with_cache(init);
            assert!(
                decl.decl_ty().is_none() || self.node_links[&init.id()].get_resolved_ty().is_some(),
            );
            if decl_ty != init_ty {
                self.check_type_assignable_to_and_optionally_elaborate(
                    init_ty,
                    decl_ty,
                    Some(name_id),
                    Some(init.id()),
                );
            }
        }
    }
    pub(super) fn check_var_like_decl(&mut self, decl: &'cx impl r#trait::VarLike<'cx>) {
        use crate::r#trait::VarLikeName::*;
        match decl.name() {
            Ident(name) => self.check_non_pat_var_like_decl(name.id, decl.id(), decl),
            StringLit { raw, .. } => self.check_non_pat_var_like_decl(raw.id, decl.id(), decl),
            NumLit(num) => self.check_non_pat_var_like_decl(num.id, decl.id(), decl),
            ArrayPat(_) | ObjectPat(_) => {
                // todo
            }
        }
    }
}

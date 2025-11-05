use crate::check::errors;
use crate::check::symbol_info::SymbolInfo;

use super::TyChecker;

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;

impl<'cx> TyChecker<'cx> {
    fn check_non_pat_var_like_decl(
        &mut self,
        name_id: ast::NodeID,
        decl_id: ast::NodeID,
        decl: &'cx impl r#trait::VarLike<'cx>,
    ) {
        if !self.p.node(name_id).is_object_binding_elem()
            && let Some(ty) = decl.decl_ty()
        {
            self.check_ty(ty);
        }

        let symbol = self.get_symbol_of_decl(decl_id);
        let ty = self.get_type_of_symbol(symbol);
        if let Some(init) = decl.init() {
            let init_ty = self.check_expr_with_cache(init);
            assert!(
                decl.decl_ty().is_none() || self.node_links[&init.id()].get_resolved_ty().is_some(),
            );
            if ty != init_ty {
                self.check_type_assignable_to_and_optionally_elaborate(
                    init_ty,
                    ty,
                    Some(name_id),
                    Some(init.id()),
                );
            }
        }
    }
    pub(super) fn check_var_like_decl(&mut self, decl: &'cx impl r#trait::VarLike<'cx>) {
        use bolt_ts_ast::r#trait::VarLikeName::*;
        match decl.name() {
            Ident(name) => self.check_non_pat_var_like_decl(name.id, decl.id(), decl),
            StringLit { raw, .. } => self.check_non_pat_var_like_decl(raw.id, decl.id(), decl),
            NumLit(num) => self.check_non_pat_var_like_decl(num.id, decl.id(), decl),
            ArrayPat(_) | ObjectPat(_) | Computed(_) => {
                // todo
            }
        }

        if decl.init().is_some()
            && decl.is_param()
            && let id = decl.id()
            && let Some(f) = self.node_query(id.module()).get_containing_fn(id)
            && self.p.node(f).fn_body().is_none()
        {
            let error =
                errors::AParameterInitializerIsOnlyAllowedInAFunctionOrConstructorImplementation {
                    span: self.p.node(id).span(),
                };
            self.push_error(Box::new(error));
        }
    }
}

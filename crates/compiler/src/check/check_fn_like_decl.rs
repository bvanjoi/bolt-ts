use super::TyChecker;
use crate::{ir, ty};
use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    fn check_param_decl(&mut self, param: &'cx ast::ParamDecl<'cx>) {
        self.check_var_like_decl(param);
        if param.dotdotdot.is_some() {
            if let ast::BindingKind::Ident(ident) = param.name.kind {
                let symbol = self.get_symbol_of_decl(ident.id);
                let ty = self.get_type_of_symbol(symbol);
                let ty = self.get_reduced_ty(ty);
                if !self.is_type_assignable_to(ty, self.any_readonly_array_ty()) {
                    let error =
                        super::errors::ARestParameterMustBeOfAnArrayType { span: param.span };
                    self.push_error(Box::new(error));
                }
            }
        }
    }

    pub(super) fn check_fn_like_decl(&mut self, decl: &impl ir::FnDeclLike<'cx>) {
        let id = decl.id();
        let symbol = self.get_symbol_of_decl(id);
        if self.binder.symbol(symbol).decls[0] == id {
            self.check_fn_like_symbol(symbol);
        }

        for param in decl.params() {
            self.check_param_decl(param)
        }

        if let Some(body) = ir::FnDeclLike::body(decl) {
            self.check_block(body)
        }

        let ret_ty = self.get_ret_ty_from_anno(id);
        self.check_all_code_paths_in_non_void_fn_ret_or_throw(decl, ret_ty);
    }

    fn check_all_code_paths_in_non_void_fn_ret_or_throw(
        &mut self,
        decl: &impl ir::FnDeclLike<'cx>,
        ret_ty: Option<&'cx ty::Ty<'cx>>,
    ) {
        // let fn_flags = self.p.node(decl.id()).fn_flags();
    }
}

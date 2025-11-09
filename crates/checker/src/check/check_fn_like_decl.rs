use super::TyChecker;
use super::symbol_info::SymbolInfo;
use super::ty;

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;

impl<'cx> TyChecker<'cx> {
    fn check_param_decl(&mut self, param: &'cx ast::ParamDecl<'cx>) {
        self.check_var_like_decl(param);
        if param.dotdotdot.is_some()
            && let ast::BindingKind::Ident(_) = param.name.kind
        {
            let symbol = self.get_symbol_of_decl(param.id);
            let ty = self.get_type_of_symbol(symbol);
            let ty = self.get_reduced_ty(ty);
            if !self.is_type_assignable_to(ty, self.any_readonly_array_ty()) {
                let error = super::errors::ARestParameterMustBeOfAnArrayType { span: param.span };
                self.push_error(Box::new(error));
            }
        }
    }

    pub(super) fn check_fn_like_decl(&mut self, decl: &impl r#trait::FnDeclLike<'cx>) {
        let id = decl.id();
        let symbol = self.get_symbol_of_decl(id);
        // TODO: check_sig_decl
        if self
            .binder
            .symbol(symbol)
            .decls
            .as_ref()
            .is_some_and(|decls| decls[0] == id)
        {
            self.check_fn_like_symbol(symbol);
        }

        // TODO: use check_sig
        if let Some(ty_params) = decl.ty_params() {
            self.check_ty_params(ty_params);
        }

        for param in decl.params() {
            self.check_param_decl(param)
        }

        if let Some(body) = r#trait::FnDeclLike::body(decl) {
            self.check_block(body)
        }

        if let Some(ty) = decl.ty() {
            self.check_ty(ty);
        }

        if {
            let n = self.p.node(id);
            !(n.is_ctor_sig_decl() || n.is_ctor_ty() || n.is_class_ctor())
        } {
            let ret_ty = self.get_ret_ty_from_anno(id);
            self.check_all_code_paths_in_non_void_fn_ret_or_throw(decl, ret_ty);
        }
    }

    fn check_all_code_paths_in_non_void_fn_ret_or_throw(
        &mut self,
        func: &impl r#trait::FnDeclLike<'cx>,
        ret_ty: Option<&'cx ty::Ty<'cx>>,
    ) {
        if ret_ty.is_some_and(|ret_ty| {
            ret_ty.maybe_type_of_kind(ty::TypeFlags::VOID)
                || ret_ty
                    .flags
                    .intersects(ty::TypeFlags::ANY.union(ty::TypeFlags::UNDEFINED))
        }) {
            return;
        }

        let Some(body) = r#trait::FnDeclLike::body(func) else {
            return;
        };

        let fn_id = func.id();
        let n = self.p.node(fn_id);
        if n.is_method_signature() || !self.fn_has_implicit_return(fn_id) {
            return;
        }

        // let fn_flags = n.fn_flags();
        let has_explicit_return = self
            .p
            .node_flags(fn_id)
            .contains(ast::NodeFlags::HAS_EXPLICIT_RETURN);
        if let Some(ret_ty) = ret_ty
            && !has_explicit_return
        {
            let name_span = n.name().unwrap().span();
            let error = super::errors::AFunctionWhoseDeclaredTypeIsNeitherUndefinedVoidNorAnyMustReturnAValue { span: name_span };
            self.push_error(Box::new(error));
        }
    }
}

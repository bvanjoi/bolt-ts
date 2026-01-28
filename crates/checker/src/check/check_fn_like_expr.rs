use super::NodeCheckFlags;
use super::SymbolInfo;
use super::ty;
use super::ty::TypeFlags;
use super::{CheckMode, TyChecker};

use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;

impl<'cx> TyChecker<'cx> {
    fn contextually_check_fn_expr_or_object_method_member(&mut self, id: ast::NodeID) {
        let flags = |this: &mut Self| this.get_node_links(id).flags();

        if !flags(self).intersects(NodeCheckFlags::CONTEXT_CHECKED) {
            let contextual_sig = self.get_contextual_sig(id);

            if !flags(self).intersects(NodeCheckFlags::CONTEXT_CHECKED) {
                self.get_mut_node_links(id)
                    .config_flags(|flags| flags | NodeCheckFlags::CONTEXT_CHECKED);
                let symbol = self.get_symbol_of_decl(id);
                let ty = self.get_type_of_symbol(symbol);
                let sigs = self.get_signatures_of_type(ty, ty::SigKind::Call);
                let Some(sig) = sigs.first() else { return };
                if self.is_context_sensitive(id) {
                    if let Some(contextual_sig) = contextual_sig {
                        let inference = self.get_inference_context(id);
                        let mut instantiated_contextual_sig = None;
                        if let Some(check_mode) = self.check_mode
                            && check_mode.intersects(CheckMode::INFERENTIAL)
                        {
                            let inference = inference.unwrap().inference.unwrap();
                            self.infer_from_annotated_params_and_return(
                                sig,
                                contextual_sig,
                                inference,
                            );
                            let rest_ty = contextual_sig.get_rest_ty(self);
                            if let Some(rest_ty) = rest_ty
                                && rest_ty.flags.intersects(TypeFlags::TYPE_PARAMETER)
                            {
                                let mapper = self.inference(inference).non_fixing_mapper;
                                instantiated_contextual_sig =
                                    Some(self.instantiate_sig(contextual_sig, mapper, false));
                            }
                        }
                        let instantiated_contextual_sig =
                            if let Some(sig) = instantiated_contextual_sig {
                                sig
                            } else if let Some(inference) = inference
                                && let Some(i) = inference.inference
                            {
                                let mapper = self.inference(i).mapper;
                                self.instantiate_sig(contextual_sig, mapper, false)
                            } else {
                                contextual_sig
                            };
                        self.assign_contextual_param_tys(sig, instantiated_contextual_sig);
                    } else {
                        self.assign_non_contextual_param_tys(sig);
                    }
                }

                if contextual_sig.is_some()
                    && self.get_ret_ty_from_anno(id).is_none()
                    && self.get_sig_links(sig.id).get_resolved_ret_ty().is_none()
                {
                    let ret_ty = self.get_ret_ty_from_body(id);
                    self.get_mut_sig_links(sig.id).set_resolved_ret_ty(ret_ty);
                }

                self.check_sig_decl(id);
            }
        }
    }

    fn check_sig_decl(&mut self, id: ast::NodeID) {
        let ty_params = self.get_effective_ty_param_decls(id);
        self.check_ty_params(ty_params);
    }

    pub(super) fn check_fn_like_expr_or_object_method_member(
        &mut self,
        node: ast::NodeID,
    ) -> &'cx ty::Ty<'cx> {
        self.check_node_deferred(node);

        if let Some(mode) = self.check_mode
            && mode.contains(CheckMode::SKIP_CONTEXT_SENSITIVE)
            && self.is_context_sensitive(node)
        {
            return if self.get_effective_ret_type_node(node).is_none()
                && !self.has_context_sensitive_params(node)
                && let Some(contextual_sig) = self.get_contextual_sig(node)
                && let ret_ty_of_sig = self.get_ret_ty_of_sig(contextual_sig)
                && self.could_contain_ty_var(ret_ty_of_sig)
            {
                if let Some(context_free_ty) = self.get_node_links(node).get_context_free_ty() {
                    return context_free_ty;
                };
                let ret_ty = self.get_ret_ty_from_body(node);
                let ret_only_sig = self.new_sig(ty::Sig {
                    id: ty::SigID::dummy(),
                    params: self.empty_array(),
                    ret: None,
                    flags: ty::SigFlags::IS_NON_INFERRABLE,
                    this_param: None,
                    target: None,
                    mapper: None,
                    class_decl: None,
                    min_args_count: 0,
                    node_id: None,
                });
                let prev = self.sig_links.insert(
                    ret_only_sig.id,
                    super::SigLinks::default().with_resolved_ret_ty(ret_ty),
                );
                assert!(prev.is_none());
                let symbol = self.get_symbol_of_decl(node);
                let call_sigs = self.alloc(vec![ret_only_sig]);
                let ret_only_ty = self.create_anonymous_ty_with_resolved(
                    Some(symbol),
                    ty::ObjectFlags::NON_INFERRABLE_TYPE,
                    self.alloc(Default::default()),
                    call_sigs,
                    self.empty_array(),
                    self.empty_array(),
                    None,
                );
                self.get_mut_node_links(node)
                    .set_context_free_ty(ret_only_ty);
                ret_only_ty
            } else {
                self.any_fn_ty()
            };
        }

        self.contextually_check_fn_expr_or_object_method_member(node);

        let symbol = self.get_symbol_of_decl(node);
        self.get_type_of_symbol(symbol)
    }

    pub(super) fn check_fn_like_expr(
        &mut self,
        expr: &impl r#trait::FnExprLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.check_fn_like_expr_or_object_method_member(expr.id())
    }

    pub(super) fn check_fn_like_expr_deferred(&mut self, expr: &impl r#trait::FnExprLike<'cx>) {
        self.check_fn_like_expr_or_object_method_member_deferred(
            expr,
            r#trait::FnExprLike::body(expr),
        );
    }

    pub(super) fn check_fn_like_expr_or_object_method_member_deferred(
        &mut self,
        func: &impl r#trait::FnLike<'cx>,
        body: ast::ArrowFnExprBody<'cx>,
    ) {
        let ret_ty = self.get_ret_ty_from_anno(func.id());
        self.check_all_code_paths_in_non_void_fn_ret_or_throw(func, ret_ty);

        use bolt_ts_ast::ArrowFnExprBody::*;
        match body {
            Block(block) => self.check_block(block),
            Expr(expr) => {
                self.check_expr(expr);
            }
        }
    }
}

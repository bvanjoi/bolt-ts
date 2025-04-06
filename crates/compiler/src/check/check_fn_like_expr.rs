use super::NodeCheckFlags;
use super::{CheckMode, TyChecker};
use crate::ty::TypeFlags;
use crate::{ir, ty};
use bolt_ts_ast as ast;

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
                        if let Some(check_mode) = self.check_mode {
                            if check_mode.intersects(CheckMode::INFERENTIAL) {
                                let inference = inference.unwrap().inference.unwrap();
                                self.infer_from_annotated_params(sig, contextual_sig, inference);
                                let rest_ty = contextual_sig.get_rest_ty(self);
                                if let Some(rest_ty) = rest_ty {
                                    if rest_ty.flags.intersects(TypeFlags::TYPE_PARAMETER) {
                                        let mapper = self.inference(inference).non_fixing_mapper;
                                        instantiated_contextual_sig = Some(self.instantiate_sig(
                                            contextual_sig,
                                            mapper,
                                            false,
                                        ));
                                    }
                                }
                            }
                        }
                        let instantiated_contextual_sig =
                            if let Some(sig) = instantiated_contextual_sig {
                                sig
                            } else if let Some(inference) = inference {
                                if let Some(i) = inference.inference {
                                    let mapper = self.inference(i).mapper;
                                    self.instantiate_sig(contextual_sig, mapper, false)
                                } else {
                                    contextual_sig
                                }
                            } else {
                                contextual_sig
                            };
                        self.assign_contextual_param_tys(sig, instantiated_contextual_sig);
                    }
                }

                if contextual_sig.is_some()
                    && self.get_ret_ty_from_anno(id).is_none()
                    && self.get_sig_links(sig.id).get_resolved_ret_ty().is_none()
                {
                    let ret_ty = self.get_ret_ty_from_body(id);
                    self.get_mut_sig_links(sig.id).set_resolved_ret_ty(ret_ty);
                }
            }
        }
    }

    pub(super) fn check_fn_like_expr_or_object_method_member(
        &mut self,
        id: ast::NodeID,
    ) -> &'cx ty::Ty<'cx> {
        self.check_node_deferred(id);
        if let Some(mode) = self.check_mode {
            if mode.intersects(CheckMode::SKIP_CONTEXT_SENSITIVE) {
                return self.any_fn_ty();
            }
        }

        self.contextually_check_fn_expr_or_object_method_member(id);

        let symbol = self.get_symbol_of_decl(id);
        self.get_type_of_symbol(symbol)
    }

    pub(super) fn check_fn_like_expr(
        &mut self,
        expr: &impl ir::FnExprLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.check_fn_like_expr_or_object_method_member(expr.id())
    }

    pub(super) fn check_fn_like_expr_deferred(&mut self, expr: &impl ir::FnExprLike<'cx>) {
        let id = expr.id();
        self.check_fn_like_expr_or_object_method_member_deferred(id, ir::FnExprLike::body(expr));
    }

    pub(super) fn check_fn_like_expr_or_object_method_member_deferred(
        &mut self,
        id: ast::NodeID,
        body: ast::ArrowFnExprBody<'cx>,
    ) {
        let n = self.p.node(id);
        let flags = n.fn_flags();
        let ret_ty = self.get_ret_ty_from_anno(id);

        use bolt_ts_ast::ArrowFnExprBody::*;
        match body {
            Block(block) => self.check_block(block),
            Expr(expr) => {
                self.check_expr(expr);
            }
        }
    }
}

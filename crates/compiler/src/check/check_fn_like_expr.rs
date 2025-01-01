use super::node_links::NodeFlags;
use super::{CheckMode, TyChecker};
use crate::{ast, ir, ty};

impl<'cx> TyChecker<'cx> {
    fn contextually_check_fn_expr(&mut self, expr: &impl ir::FnExprLike<'cx>) -> bool {
        let id = expr.id();
        let flags = |this: &mut Self| this.get_node_links(id).flags();
        let mut body_checked = false;

        if !flags(self).intersects(NodeFlags::CONTEXT_CHECKED) {
            let contextual_sig = self.get_contextual_sig(id);

            if !flags(self).intersects(NodeFlags::CONTEXT_CHECKED) {
                self.get_mut_node_links(id)
                    .config_flags(|flags| flags | NodeFlags::CONTEXT_CHECKED);
                let symbol = self.get_symbol_of_decl(id);
                let ty = self.get_type_of_symbol(symbol);
                let sigs = self.get_sigs_of_ty(ty, ty::SigKind::Call);
                let sig = if sigs.is_empty() {
                    return body_checked;
                } else {
                    sigs[0]
                };
                if self.is_context_sensitive(id) {
                    if let Some(contextual_sig) = contextual_sig {
                        let inference = self.get_inference_context(id);
                        let mut instantiated_contextual_sig = None;
                        if let Some(check_mode) = self.check_mode {
                            if check_mode.intersects(CheckMode::INFERENTIAL) {
                                let inference = inference.unwrap().inference.unwrap();
                                self.infer_from_annotated_params(sig, contextual_sig, inference);
                                // TODO: handle rest
                            }
                        }
                        if instantiated_contextual_sig.is_none() {
                            if let Some(inference) = inference {
                                let mapper = self
                                    .create_inference_fixing_mapper(inference.inference.unwrap());
                                instantiated_contextual_sig =
                                    Some(self.instantiate_sig(contextual_sig, mapper, false));
                            } else {
                                instantiated_contextual_sig = Some(contextual_sig);
                            }
                        }
                        let instantiated_contextual_sig = instantiated_contextual_sig.unwrap();
                        self.assign_contextual_param_tys(sig, instantiated_contextual_sig);
                    }
                }

                if contextual_sig.is_some() && self.get_ret_ty_from_anno(id).is_none() {
                    let ret_ty = self.get_ret_ty_from_body(id);
                    body_checked = true;
                }
            }
        }
        body_checked
    }

    pub(super) fn check_fn_like_expr(
        &mut self,
        expr: &impl ir::FnExprLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(mode) = self.check_mode {
            if mode.intersects(CheckMode::SKIP_CONTEXT_SENSITIVE) {
                return self.any_fn_ty();
            }
        }

        let body_checked = self.contextually_check_fn_expr(expr);

        if !body_checked {
            match ir::FnExprLike::body(expr) {
                ast::ArrowFnExprBody::Block(block) => self.check_block(block),
                ast::ArrowFnExprBody::Expr(expr) => {
                    self.check_expr(expr);
                }
            };
        }

        let symbol = self.get_symbol_of_decl(expr.id());
        self.get_type_of_symbol(symbol)
    }
}

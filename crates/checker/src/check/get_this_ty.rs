use super::errors;
use super::{TyChecker, ty};

use bolt_ts_ast as ast;
use bolt_ts_ty::ObjectFlags;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_this_ty_of_decl(&mut self, decl: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let sig = self.get_sig_from_decl(decl);
        self.get_this_ty_of_sig(sig)
    }

    pub(super) fn get_this_ty_of_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.get_sig_links(sig.id)
            .get_this_param()
            .map(|this_param| self.get_type_of_symbol(this_param))
    }

    fn get_this_ty_of_object_literal_from_contextual_ty(
        &mut self,
        mut n: &'cx ast::ObjectLit<'cx>,
        mut ty: Option<&'cx ty::Ty<'cx>>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let get_this_type_argument = |this: &mut Self, ty: &'cx ty::Ty<'cx>| {
            if ty.get_object_flags().contains(ObjectFlags::REFERENCE) {
                let r = ty.kind.as_object_reference().unwrap();
                if r.target == this.global_this_ty() {
                    return this.get_ty_arguments(ty).first();
                }
            };
            None
        };
        while let Some(t) = ty {
            // get_this_type_from_contextual_type
            let this_ty = self.map_ty(
                t,
                |this, t| {
                    if let Some(i) = t.kind.as_intersection() {
                        i.tys
                            .iter()
                            .find_map(|t| get_this_type_argument(this, t))
                            .copied()
                    } else {
                        get_this_type_argument(this, t).copied()
                    }
                },
                false,
            );
            if let Some(this_ty) = this_ty {
                return Some(this_ty);
            }
            let p = self.parent(n.id).unwrap();
            if !self.p.node(p).is_object_prop_assignment() {
                break;
            }
            let p = self.parent(p).unwrap();
            n = self.p.node(p).expect_object_lit();
            ty = self.get_apparent_ty_of_contextual_ty(p, None);
        }
        None
    }

    pub(super) fn get_contextual_this_parameter_type(
        &mut self,
        id: ast::NodeID,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let func = self.p.node(id);

        if func.is_arrow_fn_expr() {
            return None;
        }

        if self.is_context_sensitive_fn_or_object_literal_method(id)
            && let Some(contextual_sig) = self.get_contextual_sig(id)
            && let Some(this_param) = self.get_sig_links(contextual_sig.id).get_this_param()
        {
            return Some(self.get_type_of_symbol(this_param));
        }

        // TODO: in_js
        if self.config.compiler_options().no_implicit_this() {
            if let Some(containing_literal) = self
                .node_query(id.module())
                .get_containing_object_literal(id)
            {
                let contextual_ty =
                    self.get_apparent_ty_of_contextual_ty(containing_literal.id, None);
                let this_ty = self.get_this_ty_of_object_literal_from_contextual_ty(
                    containing_literal,
                    contextual_ty,
                );
                return Some(if let Some(this_ty) = this_ty {
                    let inference_context = self.get_inference_context(containing_literal.id);
                    let mapper = inference_context
                        .and_then(|i| self.get_mapper_from_context(i.inference.unwrap()));
                    self.instantiate_ty(this_ty, mapper)
                } else if let Some(contextual_ty) = contextual_ty {
                    let ty = self.get_non_nullable_ty(contextual_ty);
                    self.get_widened_ty(ty)
                } else {
                    let ty = self.check_object_literal_cached(containing_literal, None);
                    self.get_widened_ty(ty)
                });
            }

            let func_parent = self.parent(id).unwrap();

            let parent = self
                .node_query(func_parent.module())
                .walk_up_paren_expressions(func_parent);
            if let Some(e) = self.p.node(parent).as_assign_expr() {
                let target = e.left;
                match target.kind {
                    ast::ExprKind::PropAccess(ast::PropAccessExpr { expr, .. })
                    | ast::ExprKind::EleAccess(ast::EleAccessExpr { expr, .. }) => {
                        // TODO: in_js
                        let ty = self.check_expression_cached(expr, None);
                        return Some(self.get_widened_ty(ty));
                    }
                    _ => {}
                }
            }
        }

        None
    }

    pub(super) fn get_this_ty(&mut self, node: &'cx ast::ThisTy) -> &'cx ty::Ty<'cx> {
        let container = self
            .node_query(node.id.module())
            .get_this_container(node.id, false, false);
        let c = self.p.node(container);
        if let Some(parent) = self.parent(container)
            && let p = self.p.node(parent)
            && (p.is_class_like() || p.is_interface_decl())
            && !c.is_static()
            && c.as_class_ctor().is_none_or(|c| match c.body {
                Some(body) => self
                    .node_query(node.id.module())
                    .is_descendant_of(node.id, body.id),
                None => false,
            })
        {
            let s = self.get_symbol_of_declaration(parent);
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

        let error = errors::AThisTypeIsAvailableOnlyInANonStaticMemberOfAClassOrInterface {
            span: node.span,
        };
        self.push_error(Box::new(error));
        self.error_ty
    }
}

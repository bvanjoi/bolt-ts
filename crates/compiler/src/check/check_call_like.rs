use super::infer::InferenceFlags;
use super::relation::RelationKind;
use super::ty::ElementFlags;
use super::ty::{Sig, SigFlags, Sigs};
use super::ExpectedArgsCount;
use super::TyChecker;
use super::{errors, CheckMode};
use crate::ir;
use crate::{ast, ty};
use bolt_ts_span::Span;

pub(super) trait CallLikeExpr<'cx>: ir::CallLike<'cx> {
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx>;
}

impl<'cx> CallLikeExpr<'cx> for ast::CallExpr<'cx> {
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx> {
        checker.resolve_call_expr(self)
    }
}

impl<'cx> CallLikeExpr<'cx> for ast::NewExpr<'cx> {
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx> {
        checker.resolve_new_expr(self)
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_call_like_expr(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let sig = if let Some(resolved_sig) = self.get_node_links(expr.id()).get_resolved_sig() {
            resolved_sig
        } else {
            let sig = expr.resolve_sig(self);
            self.get_mut_node_links(expr.id()).set_resolved_sig(sig);
            sig
        };
        let ty = self.get_ret_ty_of_sig(sig);
        ty
    }

    pub(super) fn get_ret_ty_of_sig(&mut self, sig: &'cx Sig<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_sig_links(sig.id).get_resolved_ret_ty() {
            return ty;
        }
        let ty = if let Some(target) = sig.target {
            let ret_ty = self.get_ret_ty_of_sig(target);
            self.instantiate_ty(ret_ty, sig.mapper)
        } else if let Some(node_id) = sig.node_id {
            if let Some(ty) = self.get_ret_ty_from_anno(node_id) {
                ty
            } else {
                self.get_ret_ty_from_body(node_id)
            }
        } else {
            self.any_ty()
        };
        self.get_mut_sig_links(sig.id).set_resolved_ret_ty(ty);
        ty
    }

    pub(super) fn get_ty_at_pos(&mut self, sig: &Sig<'cx>, pos: usize) -> &'cx ty::Ty<'cx> {
        self.try_get_ty_at_pos(sig, pos).unwrap_or(self.any_ty())
    }

    pub(super) fn try_get_ty_at_pos(
        &mut self,
        sig: &Sig<'cx>,
        pos: usize,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let param_count = if sig.has_rest_param() {
            sig.params.len() - 1
        } else {
            sig.params.len()
        };
        if pos < param_count {
            Some(self.get_type_of_symbol(sig.params[pos]))
        } else if sig.has_rest_param() {
            let rest_ty = self.get_type_of_symbol(sig.params[param_count]);
            // let index = pos - param_count;
            (!rest_ty.kind.is_tuple()).then(|| {
                rest_ty.kind.expect_object_reference().resolved_ty_args[0]
                // TODO:
                // let index_ty = self.get_number_literal_type(index as f64);
                // self.get_indexed_access_ty(rest_ty, index_ty, None)
            })
        } else {
            None
        }
    }

    fn resolve_new_expr(&mut self, expr: &impl CallLikeExpr<'cx>) -> &'cx Sig<'cx> {
        let ty = self.check_expr(expr.callee());
        let sigs = self.signatures_of_type(ty, ty::SigKind::Constructor);

        if !sigs.is_empty() {
            let abstract_sigs = sigs
                .iter()
                .filter(|sig| sig.flags.contains(SigFlags::HAS_ABSTRACT))
                .collect::<thin_vec::ThinVec<_>>();
            if !abstract_sigs.is_empty() {
                let abstract_class_list = abstract_sigs
                    .iter()
                    .map(|sig| {
                        let node = self.p.node(sig.class_decl.unwrap()).expect_class_decl();
                        let name = self.atoms.get(node.name.name).to_string();
                        let span = node.name.span;
                        errors::ClassNameHasAbstractModifier { span, name }
                    })
                    .collect::<Vec<_>>();
                assert!(!abstract_class_list.is_empty());
                let error = errors::CannotCreateAnInstanceOfAnAbstractClass {
                    span: expr.callee().span(),
                    abstract_class_list,
                };
                self.push_error(Box::new(error));
            }
        }
        self.resolve_call(ty, expr, sigs)
    }

    fn resolve_call_expr(&mut self, expr: &impl CallLikeExpr<'cx>) -> &'cx Sig<'cx> {
        let ty = self.check_expr(expr.callee());
        let sigs = self.signatures_of_type(ty, ty::SigKind::Call);
        let class_sigs = self.signatures_of_type(ty, ty::SigKind::Constructor);

        if sigs.is_empty() {
            if let Some(sig) = class_sigs.first() {
                assert_eq!(class_sigs.len(), 1);
                let ast::Node::ClassDecl(decl) = self.p.node(sig.class_decl.unwrap()) else {
                    unreachable!()
                };
                let error = errors::ValueOfType0IsNotCallable {
                    span: expr.callee().span(),
                    ty: format!("typeof {}", self.atoms.get(decl.name.name)),
                };
                self.push_error(Box::new(error));
            }
            // TODO: use unreachable
            return self.unknown_sig();
        }

        self.resolve_call(ty, expr, sigs)
    }

    pub(super) fn get_min_arg_count(&mut self, sig: &'cx Sig<'cx>) -> usize {
        let mut min_arg_count = None;
        if sig.has_rest_param() {
            let rest_ty = self.get_type_of_symbol(sig.params[sig.params.len() - 1]);
            if rest_ty.kind.is_tuple() {
                let tuple = rest_ty
                    .kind
                    .expect_object_reference()
                    .target
                    .kind
                    .expect_object_tuple();
                let required_count = if let Some(first_optional_index) = tuple
                    .element_flags
                    .iter()
                    .position(|ele| !ele.intersects(ElementFlags::REQUIRED))
                {
                    first_optional_index
                } else {
                    tuple.shape.fixed_length
                };
                if required_count > 0 {
                    min_arg_count = Some(sig.params.len() - 1 + required_count);
                }
            }
        }
        if min_arg_count.is_none() {
            min_arg_count = Some(sig.min_args_count)
        }
        let mut min_arg_count = min_arg_count.unwrap();

        for i in (0..min_arg_count).rev() {
            let ty = self.get_ty_at_pos(sig, i);
            min_arg_count = i;
        }
        min_arg_count
    }

    fn get_spread_arg_index(&self, args: &[&'cx ast::Expr<'cx>]) -> Option<usize> {
        // TODO: support args
        None
    }

    pub(super) fn has_effective_rest_param(&mut self, sig: &'cx Sig<'cx>) -> bool {
        if sig.has_rest_param() {
            let rest_ty = self.get_type_of_symbol(sig.params[sig.params.len() - 1]);
            if !rest_ty.kind.is_tuple() {
                return true;
            }
            let tuple = rest_ty
                .kind
                .expect_object_reference()
                .target
                .kind
                .expect_object_tuple();
            tuple.combined_flags.intersects(ElementFlags::VARIABLE)
        } else {
            false
        }
    }

    fn has_correct_arity(&mut self, expr: &impl CallLikeExpr<'cx>, sig: &'cx Sig<'cx>) -> bool {
        let param_count = sig.get_param_count(self);
        let min_args = self.get_min_arg_count(sig);
        let args = expr.args();
        let arg_count = args.len();
        if args.is_empty() {
            return min_args == 0;
        } else if let Some(spread_arg_index) = self.get_spread_arg_index(args) {
            return spread_arg_index >= min_args && spread_arg_index < param_count;
        }

        if arg_count > param_count && !self.has_effective_rest_param(sig) {
            return false;
        }

        if arg_count >= min_args {
            return true;
        }

        for i in arg_count..min_args {
            let ty = self.get_ty_at_pos(sig, i);
        }

        true
    }

    fn get_signature_applicability_error(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        sig: &ty::Sig<'cx>,
        relation: RelationKind,
        check_mode: CheckMode,
        report_error: bool,
    ) -> bool {
        let args = expr.args();
        let rest_type = sig.get_non_array_rest_ty(self);
        let arg_count = args.len();
        let arg_count = if rest_type.is_some() {
            usize::min(arg_count, sig.get_param_count(self) - 1)
        } else {
            arg_count
        };
        let mut has_error = false;
        for (i, arg) in args.iter().enumerate().take(arg_count) {
            let param_ty = self.get_ty_at_pos(sig, i);
            let arg_ty = self.check_expr_with_contextual_ty(arg, param_ty, None, check_mode);
            let error_node = report_error.then(|| arg.id());
            if !self.check_type_related_to_and_optionally_elaborate(
                arg_ty,
                param_ty,
                relation,
                error_node,
                Some(expr.id()),
                |this, span, source, target| {
                    let source = this.get_base_ty_of_literal_ty(source);
                    Box::new(errors::ArgumentOfTyIsNotAssignableToParameterOfTy {
                        span,
                        arg_ty: this.print_ty(source).to_string(),
                        param_ty: this.print_ty(target).to_string(),
                    })
                },
            ) {
                has_error = true
            }
        }
        has_error
    }

    fn choose_overload(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        candidates: Sigs<'cx>,
        relation: RelationKind,
        is_single_non_generic_candidate: bool,
        mut argument_check_mode: CheckMode,
    ) -> Option<&'cx Sig<'cx>> {
        if is_single_non_generic_candidate {
            let candidate = candidates[0];
            if !self.has_correct_arity(expr, candidate)
                || self.get_signature_applicability_error(
                    expr,
                    candidate,
                    relation,
                    CheckMode::empty(),
                    true,
                )
            {
                None
            } else {
                Some(candidate)
            }
        } else {
            for candidate in candidates {
                if !self.has_correct_arity(expr, candidate) {
                    continue;
                }
                let mut check_candidate = *candidate;
                let mut infer_ctx = None;

                if let Some(ty_params) = candidate.ty_params {
                    let infer = self.create_inference_context(
                        ty_params,
                        Some(candidate),
                        InferenceFlags::empty(),
                    );
                    infer_ctx = Some(infer);
                    // TODO: ty_arg_tys = instantiate_tys(infer_ty_args(), infer.non_fixing_mapper)

                    let ty_arg_tys = Some({
                        let tys = self.infer_ty_args(
                            expr,
                            candidate,
                            expr.args(),
                            argument_check_mode | CheckMode::SKIP_GENERIC_FUNCTIONS,
                            infer,
                        );
                        let mapper = self.create_inference_non_fixing_mapper(infer);
                        // self.instantiate_tys(tys, mapper)
                        tys
                    });
                    if self.inferences[infer.as_usize()]
                        .flags
                        .intersects(InferenceFlags::SKIPPED_GENERIC_FUNCTION)
                    {
                        argument_check_mode |= CheckMode::SKIP_GENERIC_FUNCTIONS;
                    } else {
                        argument_check_mode |= CheckMode::empty();
                    }

                    check_candidate =
                        self.get_sig_instantiation(candidate, ty_arg_tys, false, None);
                }

                if !argument_check_mode.is_empty() {
                    argument_check_mode = CheckMode::empty();
                    if let Some(infer) = infer_ctx {
                        let ty_arg_tys = {
                            let tys = self.infer_ty_args(
                                expr,
                                candidate,
                                expr.args(),
                                argument_check_mode,
                                infer,
                            );
                            let mapper = self.create_inference_fixing_mapper(infer);
                            self.instantiate_tys(tys, mapper)
                        };
                        check_candidate =
                            self.get_sig_instantiation(candidate, Some(ty_arg_tys), false, None);
                    };
                }

                return Some(check_candidate);
            }
            None
        }
    }

    fn resolve_call(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        expr: &impl CallLikeExpr<'cx>,
        candidates: Sigs<'cx>,
    ) -> &'cx Sig<'cx> {
        if candidates.is_empty() {
            return self.unknown_sig();
        }

        let mut min_required_params = usize::MAX;
        let mut max_required_params = usize::MIN;

        let args = expr.args();

        let is_single = candidates.len() == 1;
        let is_single_non_generic_candidate = is_single && candidates[0].ty_params.is_none();

        let mut check_mode = CheckMode::empty();
        if !is_single_non_generic_candidate
            && args.iter().any(|arg| self.is_context_sensitive(arg.id()))
        {
            check_mode |= CheckMode::SKIP_CONTEXT_SENSITIVE;
        }

        let mut res = None;

        if candidates.len() > 1 {
            if let Some(sig) = self.choose_overload(
                expr,
                candidates,
                RelationKind::Subtype,
                is_single_non_generic_candidate,
                check_mode,
            ) {
                return sig;
            }
        } else {
            res = self.choose_overload(
                expr,
                candidates,
                RelationKind::Assignable,
                is_single_non_generic_candidate,
                check_mode,
            );
        }

        for sig in candidates {
            if sig.min_args_count < min_required_params {
                min_required_params = sig.min_args_count;
            }
            max_required_params = if sig.has_rest_param() {
                usize::MAX
            } else {
                usize::max(sig.params.len(), max_required_params)
            }
        }

        // FIXME: overload
        let candidate = candidates[0];

        if min_required_params <= args.len() && args.len() <= max_required_params {
            // arguments had been check in `choose_overload`
        } else if min_required_params == max_required_params {
            let x = min_required_params;
            let y = args.len();
            let span = if x < y {
                let lo = args[y - x].span().lo;
                let hi = args.last().unwrap().span().hi;
                Span::new(lo, hi, expr.span().module)
            } else {
                expr.callee().span()
            };
            let error = errors::ExpectedXArgsButGotY {
                span,
                x: super::ExpectedArgsCount::Count(x),
                y,
            };
            self.push_error(Box::new(error));
        } else if args.len() > max_required_params {
            let lo = args[max_required_params].span().lo;
            let hi = args.last().unwrap().span().hi;
            let span = Span::new(lo, hi, expr.span().module);
            let error = errors::ExpectedXArgsButGotY {
                span,
                x: ExpectedArgsCount::Range {
                    lo: min_required_params,
                    hi: max_required_params,
                },
                y: args.len(),
            };
            self.push_error(Box::new(error));
        } else if args.len() < min_required_params {
            let span = expr.span();
            let error: crate::Diag = if max_required_params == usize::MAX {
                Box::new(errors::ExpectedAtLeastXArgsButGotY {
                    span,
                    x: min_required_params,
                    y: args.len(),
                })
            } else {
                Box::new(errors::ExpectedXArgsButGotY {
                    span,
                    x: ExpectedArgsCount::Range {
                        lo: min_required_params,
                        hi: max_required_params,
                    },
                    y: args.len(),
                })
            };
            self.push_error(error);
        }

        res.unwrap_or(candidate)
    }
}

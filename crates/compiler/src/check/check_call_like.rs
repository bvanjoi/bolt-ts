use super::errors;
use super::relation::RelationKind;
use super::ty::ElementFlags;
use super::ty::{Sig, SigFlags, Sigs};
use super::ExpectedArgsCount;
use super::TyChecker;
use crate::ir;
use crate::{ast, ty};
use bolt_ts_span::Span;

pub(super) trait CallLikeExpr<'cx>: ir::CallLike<'cx> {
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx>;
    fn sigs(checker: &TyChecker<'cx>, ty: &'cx ty::Ty<'cx>) -> Sigs<'cx>;
}

impl<'cx> CallLikeExpr<'cx> for ast::CallExpr<'cx> {
    fn sigs(checker: &TyChecker<'cx>, ty: &'cx ty::Ty<'cx>) -> Sigs<'cx> {
        checker.signatures_of_type(ty, ty::SigKind::Call)
    }
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx> {
        checker.resolve_call_expr(self)
    }
}

impl<'cx> CallLikeExpr<'cx> for ast::NewExpr<'cx> {
    fn sigs(checker: &TyChecker<'cx>, ty: &'cx ty::Ty<'cx>) -> Sigs<'cx> {
        if ty.kind.as_object_interface().is_some() {
            checker.ty_declared_members[&ty.id].ctor_sigs
        } else {
            let sigs = checker.signatures_of_type(ty, ty::SigKind::Constructor);
            // unreachable!("{ty:#?}");
            sigs
        }
    }
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx> {
        checker.resolve_new_expr(self)
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_call_like_expr(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let sig = expr.resolve_sig(self);
        self.get_ret_ty_of_sig(expr, sig)
    }

    fn get_ret_ty_of_sig(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        sig: &'cx Sig<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        sig.ret.map_or(self.any_ty(), |ret| {
            let ty = self.p.node(ret);
            if ty.is_class_decl() {
                let s = self.get_symbol_of_decl(ret);
                self.get_type_of_symbol(s)
            } else {
                let ret_ty = if let Some(target) = sig.target {
                    let ret_ty = self.get_ret_ty_of_sig(expr, target);
                    self.instantiate_ty(ret_ty, sig.mapper)
                } else {
                    self.get_ty_from_type_node(&ty.as_ty().unwrap())
                };

                if let Some(ty_params) = sig.ty_params {
                    let sources = ty_params
                        .iter()
                        .map(|id| self.get_declared_ty_of_symbol(*id))
                        .collect::<Vec<_>>();
                    let sources = self.alloc(sources);
                    let targets = if let Some(ty_args) = expr.ty_args() {
                        // callee<ty_args>()
                        let ty_args = ty_args
                            .list
                            .iter()
                            .map(|arg| self.get_ty_from_type_node(arg))
                            .collect::<Vec<_>>();
                        self.alloc(ty_args)
                    } else {
                        // callee()
                        self.infer_ty_args(sig, expr.args())
                    };
                    let mapper = self.alloc(ty::TyMapper::create(sources, targets));
                    self.instantiate_ty_with_alias(ret_ty, mapper)
                } else {
                    ret_ty
                }
            }
        })
    }

    pub(super) fn get_ty_at_pos(&mut self, sig: &Sig<'cx>, pos: usize) -> Option<&'cx ty::Ty<'cx>> {
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
        self.resolve_call(ty, expr, sigs)
    }

    fn resolve_call_expr(&mut self, expr: &impl CallLikeExpr<'cx>) -> &'cx Sig<'cx> {
        let ty = self.check_expr(expr.callee());
        let sigs = ast::CallExpr::sigs(self, ty);
        let class_sigs = ast::NewExpr::sigs(self, ty);

        if sigs.is_empty() {
            if let Some(sig) = class_sigs.first() {
                assert_eq!(class_sigs.len(), 1);
                let ast::Node::ClassDecl(decl) = self.p.node(sig.node_id) else {
                    unreachable!()
                };
                let error = errors::ValueOfType0IsNotCallable {
                    span: expr.callee().span(),
                    ty: format!("typeof {}", self.atoms.get(decl.name.name)),
                    callee_is_class: Some(errors::DidYouMeanToIncludeNew),
                };
                self.push_error(expr.span().module, Box::new(error));
            }
            // TODO: use unreachable
            return self.unknown_sig();
        }

        self.resolve_call(ty, expr, sigs)
    }

    fn get_min_arg_count(&mut self, sig: &'cx Sig<'cx>) -> usize {
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

    fn has_effective_rest_params(&mut self, sig: &'cx Sig<'cx>) -> bool {
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

        if arg_count > param_count && !self.has_effective_rest_params(sig) {
            return false;
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
            let param_ty = self.get_ty_at_pos(sig, i).unwrap_or(self.any_ty());
            let arg_ty = self.check_expr_with_contextual_ty(arg, param_ty);
            if !self.check_type_related_to_and_optionally_elaborate(
                arg.span(),
                arg_ty,
                param_ty,
                relation,
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
        sigs: Sigs<'cx>,
        relation: RelationKind,
        is_single_non_generic_candidate: bool,
    ) -> Option<&'cx Sig<'cx>> {
        if is_single_non_generic_candidate {
            let sig = sigs[0];
            if !self.has_correct_arity(expr, sig)
                || self.get_signature_applicability_error(expr, sig, relation)
            {
                None
            } else {
                Some(sig)
            }
        } else {
            for sig in sigs {
                if !self.has_correct_arity(expr, sig) {
                    continue;
                }
                // if let Some(ty_params) = sig.ty_params {
                //     let location = self.p.parent(ty_params[0].decl(self.binder)).unwrap();
                // }
                return Some(sig);
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
        if candidates.len() > 1 {
            if let Some(sig) = self.choose_overload(
                expr,
                candidates,
                RelationKind::Subtype,
                is_single_non_generic_candidate,
            ) {
                return sig;
            }
        } else {
            self.choose_overload(
                expr,
                candidates,
                RelationKind::Assignable,
                is_single_non_generic_candidate,
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
        if candidate.flags.intersects(SigFlags::HAS_ABSTRACT) {
            let error = errors::CannotCreateAnInstanceOfAnAbstractClass {
                span: expr.callee().span(),
            };
            self.push_error(expr.span().module, Box::new(error));
        }

        if min_required_params <= args.len() && args.len() <= max_required_params {
            // arguments had been check in `check_overload`
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
            self.push_error(span.module, Box::new(error));
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
            self.push_error(span.module, Box::new(error));
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
            self.push_error(span.module, error);
        }

        candidate
    }
}

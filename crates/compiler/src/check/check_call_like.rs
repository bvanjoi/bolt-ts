use super::cycle_check::ResolutionKey;
use super::infer::InferenceFlags;
use super::relation::RelationKind;
use super::symbol_info::SymbolInfo;
use super::ty::ElementFlags;
use super::ty::{Sig, SigFlags, Sigs};
use super::{CheckMode, errors};
use super::{ExpectedArgsCount, Ternary};
use super::{InferenceContextId, TyChecker};
use crate::bind::SymbolID;
use crate::ir::{self, CallLike};
use crate::ty;
use crate::ty::TypeFlags;

use bolt_ts_ast as ast;
use bolt_ts_span::Span;
use bolt_ts_utils::no_hashset_with_capacity;

pub(super) trait CallLikeExpr<'cx>: ir::CallLike<'cx> {
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx>;
    fn is_super_call(&self) -> bool;
}

impl<'cx> CallLikeExpr<'cx> for ast::CallExpr<'cx> {
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx> {
        checker.resolve_call_expr(self)
    }
    fn is_super_call(&self) -> bool {
        matches!(self.callee().kind, ast::ExprKind::Super(..))
    }
}

impl<'cx> CallLikeExpr<'cx> for ast::NewExpr<'cx> {
    fn resolve_sig(&self, checker: &mut TyChecker<'cx>) -> &'cx Sig<'cx> {
        checker.resolve_new_expr(self)
    }
    fn is_super_call(&self) -> bool {
        false
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
        if !self.push_ty_resolution(ResolutionKey::ResolvedReturnType(sig.id)) {
            return self.error_ty;
        }
        let mut ty = if let Some(target) = sig.target {
            let ret_ty = self.get_ret_ty_of_sig(target);
            self.instantiate_ty(ret_ty, sig.mapper)
        } else if let Some(node_id) = sig.node_id {
            if let Some(ty) = self.get_ret_ty_from_anno(node_id) {
                ty
            } else {
                self.get_ret_ty_from_body(node_id)
            }
        } else {
            self.any_ty
        };

        if self.pop_ty_resolution().has_cycle() {
            if let Some(node_id) = sig.node_id {
                if let Some(ret_ty) = self.get_effective_ret_type_node(node_id) {}
            }
            ty = self.any_ty;
        }
        self.get_mut_sig_links(sig.id).set_resolved_ret_ty(ty);
        ty
    }

    pub(super) fn get_rest_ty_of_sig(&mut self, sig: &'cx Sig<'cx>) -> &'cx ty::Ty<'cx> {
        self.try_get_rest_ty_of_sig(sig).unwrap_or(self.any_ty)
    }

    pub(super) fn try_get_rest_ty_of_sig(
        &mut self,
        sig: &'cx Sig<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if sig.has_rest_param() {
            let sig_rest_ty = self.get_type_of_symbol(sig.params[sig.params.len() - 1]);
            let rest_ty = if sig_rest_ty.is_tuple() {
                todo!()
            } else {
                sig_rest_ty
            };
            Some(rest_ty)
        } else {
            None
        }
    }

    pub(super) fn get_rest_ty_at_pos(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        pos: usize,
        readonly: bool,
    ) -> &'cx ty::Ty<'cx> {
        let param_count = source.get_param_count(self);
        let min_arg_count = self.get_min_arg_count(source);
        let rest_ty = source.get_rest_ty(self);
        if let Some(rest_ty) = rest_ty {
            if pos >= param_count - 1 {
                return if pos == param_count - 1 {
                    rest_ty
                } else {
                    let ty = self.get_indexed_access_ty(rest_ty, self.number_ty, None, None);
                    self.create_array_ty(ty, false)
                };
            }
        }
        let mut tys = Vec::with_capacity(param_count);
        let mut flags = Vec::with_capacity(param_count);
        // let mut names = Vec::with_capacity(param_count);
        for i in pos..param_count {
            if rest_ty.is_none() || i < param_count - 1 {
                tys.push(self.get_ty_at_pos(source, i));
                flags.push(if i < min_arg_count {
                    ElementFlags::REQUIRED
                } else {
                    ElementFlags::OPTIONAL
                });
            } else {
                tys.push(rest_ty.unwrap());
                flags.push(ElementFlags::VARIADIC);
            }
            // names.push(self.getnam);
        }
        self.create_tuple_ty(self.alloc(tys), Some(self.alloc(flags)), readonly)
    }

    pub(super) fn get_ty_at_pos(&mut self, sig: &Sig<'cx>, pos: usize) -> &'cx ty::Ty<'cx> {
        self.try_get_ty_at_pos(sig, pos).unwrap_or(self.any_ty)
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
            // TODO: use `get_type_of_param`
            Some(self.get_type_of_symbol(sig.params[pos]))
        } else if sig.has_rest_param() {
            let rest_ty = self.get_type_of_symbol(sig.params[param_count]);
            let index = pos - param_count;
            let use_indexed_access = if let Some(t) = rest_ty.as_tuple() {
                t.combined_flags.intersects(ElementFlags::VARIABLE) || index < t.fixed_length
            } else {
                true
            };
            use_indexed_access.then(|| {
                let index_ty = self.get_number_literal_type(index as f64);
                self.get_indexed_access_ty(rest_ty, index_ty, None, None)
            })
        } else {
            None
        }
    }

    pub(super) fn get_rest_or_any_ty_at_pos(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        pos: usize,
    ) -> &'cx ty::Ty<'cx> {
        let rest_ty = self.get_rest_ty_at_pos(source, pos, false);
        let element_ty = self.get_element_ty_of_array_ty(rest_ty);
        if self.is_type_any(element_ty) {
            self.any_ty
        } else {
            rest_ty
        }
    }

    fn resolve_new_expr(&mut self, expr: &impl CallLikeExpr<'cx>) -> &'cx Sig<'cx> {
        let mut ty = self.check_expr(expr.callee());

        ty = self.get_apparent_ty(ty);
        let ctor_sigs = self.get_signatures_of_type(ty, ty::SigKind::Constructor);
        if !ctor_sigs.is_empty() {
            let abstract_sigs = ctor_sigs
                .iter()
                .filter(|sig| sig.flags.contains(SigFlags::ABSTRACT))
                .collect::<thin_vec::ThinVec<_>>();
            if !abstract_sigs.is_empty() {
                let abstract_class_list = abstract_sigs
                    .iter()
                    .map(|sig| {
                        let node = self.p.node(sig.class_decl.unwrap()).expect_class_decl();
                        if let Some(name) = node.name {
                            let span = name.span;
                            let name = self.atoms.get(name.name).to_string();
                            errors::ClassNameHasAbstractModifier {
                                span,
                                name: Some(name),
                            }
                        } else {
                            let abstract_modifier_span = node
                                .modifiers
                                .unwrap()
                                .list
                                .iter()
                                .find_map(|m| {
                                    if m.kind == ast::ModifierKind::Abstract {
                                        Some(m.span)
                                    } else {
                                        None
                                    }
                                })
                                .unwrap();
                            errors::ClassNameHasAbstractModifier {
                                span: abstract_modifier_span,
                                name: None,
                            }
                        }
                    })
                    .collect::<Vec<_>>();
                assert!(!abstract_class_list.is_empty());
                let error = errors::CannotCreateAnInstanceOfAnAbstractClass {
                    span: expr.callee().span(),
                    abstract_class_list,
                };
                self.push_error(Box::new(error));
            }

            return self.resolve_call(ty, expr, ctor_sigs);
        }

        let call_sigs = self.get_signatures_of_type(ty, ty::SigKind::Call);
        if !call_sigs.is_empty() {
            let sig = self.resolve_call(ty, expr, call_sigs);
            return sig;
        }

        if ty != self.error_ty {
            self.invocation_error(expr, ty, ty::SigKind::Constructor);
        }
        self.unknown_sig()
    }

    fn invocation_error(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        apparent_ty: &'cx ty::Ty<'cx>,
        kind: ty::SigKind,
    ) {
        let error = errors::ThisExpressionIsNotConstructable {
            span: expr.span(),
            is_call: kind == ty::SigKind::Call,
        };
        self.push_error(Box::new(error));
    }

    fn resolve_call_expr(&mut self, expr: &impl CallLikeExpr<'cx>) -> &'cx Sig<'cx> {
        let ty = self.check_expr(expr.callee());
        let apparent_ty = self.get_apparent_ty(ty);
        if self.is_error(apparent_ty) {
            return self.unknown_sig();
        }

        let call_sigs = self.get_signatures_of_type(apparent_ty, ty::SigKind::Call);

        if call_sigs.is_empty() {
            let ctor_sigs = self.get_signatures_of_type(apparent_ty, ty::SigKind::Constructor);
            if let Some(sig) = ctor_sigs.first() {
                assert_eq!(ctor_sigs.len(), 1);
                let ast::Node::ClassDecl(decl) = self.p.node(sig.class_decl.unwrap()) else {
                    unreachable!()
                };
                let error = errors::ValueOfType0IsNotCallable {
                    span: expr.callee().span(),
                    ty: format!("typeof {}", self.atoms.get(decl.name.unwrap().name)),
                };
                self.push_error(Box::new(error));
            }
            return self.unknown_sig();
        }

        self.resolve_call(ty, expr, call_sigs)
    }

    pub(super) fn get_min_arg_count(&mut self, sig: &'cx Sig<'cx>) -> usize {
        // TODO: cache
        let mut min_arg_count = None;
        if sig.has_rest_param() {
            let rest_ty = self.get_type_of_symbol(sig.params[sig.params.len() - 1]);
            if let Some(tuple) = rest_ty.as_tuple() {
                let required_count = if let Some(first_optional_index) = tuple
                    .element_flags
                    .iter()
                    .position(|ele| !ele.intersects(ElementFlags::REQUIRED))
                {
                    first_optional_index
                } else {
                    tuple.fixed_length
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
            if self
                .filter_type(ty, |_, ty| ty.flags.intersects(TypeFlags::VOID))
                .flags
                .intersects(TypeFlags::NEVER)
            {
                break;
            }
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
            let Some(tuple) = rest_ty.as_tuple() else {
                return true;
            };
            tuple.combined_flags.intersects(ElementFlags::VARIABLE)
        } else {
            false
        }
    }

    fn has_correct_ty_arg_arity(
        &mut self,
        candidate: &'cx Sig<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
    ) -> bool {
        let num_ty_params = candidate
            .ty_params
            .map(|ty_params| ty_params.len())
            .unwrap_or_default();
        let min_ty_args = self.get_min_ty_arg_count(candidate.ty_params);
        if let Some(ty_args) = ty_args {
            let ty_args = ty_args.list;
            if ty_args.is_empty() {
                true
            } else {
                let ty_args_len = ty_args.len();
                ty_args_len >= min_ty_args && ty_args_len <= num_ty_params
            }
        } else {
            true
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
            if self
                .filter_type(ty, |_, ty| ty.flags.intersects(TypeFlags::VOID))
                .flags
                .intersects(TypeFlags::NEVER)
            {
                return false;
            }
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
        inference_context: Option<InferenceContextId>,
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
            if !matches!(arg.kind, ast::ExprKind::Omit(_)) {
                let param_ty = self.get_ty_at_pos(sig, i);
                let arg_ty = self.check_expr_with_contextual_ty(arg, param_ty, None, check_mode);
                let error_node = report_error.then(|| arg.id());
                let regular_arg_ty = if check_mode.intersects(CheckMode::SKIP_CONTEXT_SENSITIVE) {
                    self.get_regular_ty_of_object_literal(arg_ty)
                } else {
                    arg_ty
                };
                let check_arg_ty = if let Some(infer) = inference_context {
                    let mapper = self.inference(infer).non_fixing_mapper;
                    self.instantiate_ty(regular_arg_ty, Some(mapper))
                } else {
                    regular_arg_ty
                };
                if self.check_type_related_to_and_optionally_elaborate(
                    check_arg_ty,
                    param_ty,
                    relation,
                    error_node,
                    Some(arg.id()),
                    |this, span, source, target| {
                        let source = this.get_base_ty_of_literal_ty(source);
                        Box::new(errors::ArgumentOfTyIsNotAssignableToParameterOfTy {
                            span,
                            arg_ty: this.print_ty(source).to_string(),
                            param_ty: this.print_ty(target).to_string(),
                        })
                    },
                ) == Ternary::FALSE
                {
                    has_error = true
                }
            }
        }
        has_error
    }

    fn check_ty_args(
        &mut self,
        sig: &'cx Sig<'cx>,
        ty_args: &'cx ast::Tys<'cx>,
        report_error: bool,
    ) -> Option<ty::Tys<'cx>> {
        let ty_params = sig.ty_params.unwrap();
        let ty_arg_tys = {
            let ty_arg_tys = ty_args
                .list
                .iter()
                .map(|ty_arg| self.get_ty_from_type_node(ty_arg))
                .collect::<Vec<_>>();
            let min_ty_arg_count = self.get_min_ty_arg_count(Some(ty_params));
            self.fill_missing_ty_args(
                Some(self.alloc(ty_arg_tys)),
                Some(ty_params),
                min_ty_arg_count,
            )
        };
        let mapper = self.create_ty_mapper(ty_params, ty_arg_tys.unwrap_or_default());
        for (i, ty_param) in ty_params.iter().enumerate() {
            let Some(constraint) = self.get_constraint_of_ty_param(ty_param) else {
                continue;
            };
            let ty_arg = ty_arg_tys.unwrap()[i];
            let target = {
                let ty = self.instantiate_ty(constraint, Some(mapper));
                self.get_ty_with_this_arg(ty, Some(ty_arg), false)
            };
            if self.check_type_assignable_to(
                ty_arg,
                target,
                report_error.then_some(ty_args.list[i].id()),
            ) == Ternary::FALSE
            {
                return None;
            }
        }

        ty_arg_tys
    }

    fn choose_overload(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        candidates: Sigs<'cx>,
        relation: RelationKind,
        is_single_non_generic_candidate: bool,
        mut argument_check_mode: CheckMode,
        candidates_for_arg_error: &mut nohash_hasher::IntSet<ty::SigID>,
    ) -> Option<&'cx Sig<'cx>> {
        let ty_args = if !expr.is_super_call() {
            expr.ty_args()
        } else {
            None
        };

        if is_single_non_generic_candidate {
            let candidate = candidates[0];
            if !self.has_correct_arity(expr, candidate) {
                None
            } else if self.get_signature_applicability_error(
                expr,
                candidate,
                relation,
                CheckMode::empty(),
                true,
                None,
            ) {
                // TODO: candidates_for_argument_error
                None
            } else {
                Some(candidate)
            }
        } else {
            for candidate in candidates {
                if !self.has_correct_ty_arg_arity(candidate, expr.ty_args())
                    || !self.has_correct_arity(expr, candidate)
                {
                    continue;
                }
                let mut check_candidate = *candidate;
                let mut infer_ctx = None;

                if let Some(ty_params) = candidate.ty_params {
                    let ty_arg_tys;
                    if let Some(ty_args) = ty_args {
                        ty_arg_tys = self.check_ty_args(candidate, ty_args, false);
                    } else {
                        let infer = self.create_inference_context(
                            ty_params,
                            Some(candidate),
                            InferenceFlags::empty(),
                        );
                        infer_ctx = Some(infer);
                        ty_arg_tys = Some({
                            let tys = self.infer_ty_args(
                                expr,
                                candidate,
                                expr.args(),
                                argument_check_mode | CheckMode::SKIP_GENERIC_FUNCTIONS,
                                infer,
                            );
                            let mapper = self.inference(infer).non_fixing_mapper;
                            self.instantiate_tys(tys, mapper)
                        });

                        if self.inferences[infer.as_usize()]
                            .flags
                            .intersects(InferenceFlags::SKIPPED_GENERIC_FUNCTION)
                        {
                            argument_check_mode |= CheckMode::SKIP_GENERIC_FUNCTIONS;
                        }
                    }

                    check_candidate =
                        self.get_sig_instantiation(candidate, ty_arg_tys, false, None);
                }

                if self.get_signature_applicability_error(
                    expr,
                    check_candidate,
                    relation,
                    argument_check_mode,
                    false,
                    infer_ctx,
                ) {
                    candidates_for_arg_error.insert(check_candidate.id);
                    continue;
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
                            let mapper = self.inference(infer).mapper;
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

        let mut candidates_for_arg_error = no_hashset_with_capacity(candidates.len());

        let args = expr.args();

        let is_single = candidates.len() == 1;
        let is_single_non_generic_candidate = is_single && candidates[0].ty_params.is_none();

        let mut argument_check_mode = CheckMode::empty();
        if !is_single_non_generic_candidate
            && args.iter().any(|arg| self.is_context_sensitive(arg.id()))
        {
            argument_check_mode = CheckMode::SKIP_CONTEXT_SENSITIVE;
        }

        let args = self.get_effective_call_args(expr);

        let mut res = None;

        if candidates.len() > 1 {
            res = self.choose_overload(
                expr,
                candidates,
                RelationKind::Subtype,
                is_single_non_generic_candidate,
                argument_check_mode,
                &mut candidates_for_arg_error,
            );
        }

        if res.is_none() {
            res = self.choose_overload(
                expr,
                candidates,
                RelationKind::Assignable,
                is_single_non_generic_candidate,
                argument_check_mode,
                &mut candidates_for_arg_error,
            );
        }

        if let Some(cached) = self.get_node_links(expr.id()).get_resolved_sig() {
            if cached != self.resolving_sig() {
                return cached;
            }
        }
        if let Some(result) = res {
            return result;
        }

        let (best_match_idx, candidate) =
            self.get_candidate_for_overload_failure(expr, candidates, args);

        for sig in candidates {
            if sig.min_args_count < min_required_params {
                min_required_params = sig.min_args_count;
            }
            // max_required_params = usize::max(max, sig.get_param_count(self));
            max_required_params = if sig.has_rest_param() {
                usize::MAX
            } else {
                usize::max(sig.params.len(), max_required_params)
            }
        }

        let prev_errors = self.diags.len();
        if min_required_params <= args.len() && args.len() <= max_required_params {
            // arguments had been check in `choose_overload`
        } else if min_required_params == max_required_params {
            let x = min_required_params;
            let y = args.len();
            let span = if x < y && y - x < args.len() {
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

        if prev_errors < self.diags.len() {
            return candidate;
        }

        if !candidates_for_arg_error.is_empty() {
            if candidates_for_arg_error.len() == 1 {
                let last = candidates_for_arg_error.drain().last().unwrap();
                let last = self.sigs[last.as_usize()];
                self.get_signature_applicability_error(
                    expr,
                    last,
                    RelationKind::Assignable,
                    CheckMode::empty(),
                    true,
                    None,
                );
            } else {
                let error = errors::NoOverloadMatchesThisCall {
                    span: expr.span(),
                    unmatched_calls: candidates_for_arg_error
                        .iter()
                        .flat_map(|c| {
                            let c = self.sigs[c.as_usize()];
                            self.p.node(c.def_id()).ident_name().map(|i| i.span)
                        })
                        .collect(),
                };
                self.push_error(Box::new(error));
                // TODO: more detail
                // let mut max = 0;
                // let mut min = usize::MAX;
                // let mut min_index = 0;
                // let mut i = 0;
                // for c in candidates_for_arg_error {
                //     let diags = self.get_signature_applicability_error(
                //         expr,
                //         c,
                //         RelationKind::Assignable,
                //         CheckMode::empty(),
                //         true,
                //         None,
                //     );
                //     if diags {
                //         // TODO: chain
                //         break;
                //     }
                // }
            }
        } else {
            let sigs_with_correct_ty_arg_arity = candidates
                .iter()
                .filter(|sig| self.has_correct_ty_arg_arity(sig, expr.ty_args()))
                .cloned()
                .collect::<thin_vec::ThinVec<_>>();
            if sigs_with_correct_ty_arg_arity.is_empty() {
                self.get_ty_arg_arity_error(expr, candidates, expr.ty_args());
            } else {
                self.get_arg_arity_error(expr, &sigs_with_correct_ty_arg_arity, args);
            }
        }

        candidate
    }

    fn get_ty_arg_arity_error(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        sigs: &[&'cx Sig<'cx>],
        ty_args: Option<&'cx ast::Tys<'cx>>,
    ) {
        let ty_arg_count = ty_args
            .map(|ty_args| ty_args.list.len())
            .unwrap_or_default();
        if sigs.len() == 1 {
            let sig = sigs[0];
            let min = self.get_min_ty_arg_count(sig.ty_params);
            let max = sig
                .ty_params
                .map(|ty_params| ty_params.len())
                .unwrap_or_default();
            let x = if min == max {
                super::ExpectedArgsCount::Count(min)
            } else {
                super::ExpectedArgsCount::Range { lo: min, hi: max }
            };
            let span = ty_args.map(|ty_args| ty_args.span).unwrap_or_else(|| {
                let hi = expr.callee().span().hi;
                Span::new(hi, hi, expr.span().module)
            });
            let error = errors::ExpectedXTyArgsButGotY {
                span,
                x,
                y: ty_arg_count,
            };
            self.push_error(Box::new(error));
        }
    }

    fn get_arg_arity_error(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        sigs: &[&'cx Sig<'cx>],
        args: ast::Exprs<'cx>,
    ) {
        let mut min = usize::MAX;
        let mut max = usize::MIN;
        let mut max_below = usize::MIN;
        let mut min_above = usize::MAX;
        let mut closest_sig = None;
        for sig in sigs {
            let min_param = self.get_min_arg_count(sig);
            let max_param = sig.get_param_count(self);
            if min_param < min {
                min = min_param;
                closest_sig = Some(sig);
            }
            max = usize::max(max, max_param);
            if min_param < args.len() && min_param > max_below {
                max_below = min_param;
            }
            if args.len() < max_param && max_param < min_above {
                min_above = max_param;
            }
        }

        let has_rest_param = sigs.iter().any(|sig| self.has_effective_rest_param(sig));
        let param_range = if has_rest_param {
            (min, min)
        } else if min < max {
            (min, max)
        } else {
            (min, min)
        };

        if min < args.len() && args.len() < max {
        } else if args.len() < min {
            let (min, max) = param_range;
            if min == max {
                let error = errors::ExpectedXArgsButGotY {
                    span: expr.span(),
                    x: super::ExpectedArgsCount::Count(min),
                    y: args.len(),
                };
                self.push_error(Box::new(error));
            }
        }
    }

    fn get_candidate_for_overload_failure(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        candidates: Sigs<'cx>,
        args: ast::Exprs<'cx>,
    ) -> (usize, &'cx ty::Sig<'cx>) {
        assert!(!candidates.is_empty());
        // self.check_node_deferred(expr.id());
        if candidates.len() == 1
            || candidates
                .iter()
                .any(|c| c.ty_params.is_some_and(|ty_params| !ty_params.is_empty()))
        {
            self.pick_longest_candidate_sig(expr, candidates, args)
        } else {
            (
                usize::MAX,
                self.create_union_of_sigs_for_overload_failure(candidates),
            )
        }
    }

    fn get_num_non_rest_params(sig: &'cx Sig<'cx>) -> usize {
        let num_params = sig.params.len();
        if sig.has_rest_param() {
            num_params - 1
        } else {
            num_params
        }
    }

    fn create_combined_symbol_for_overload_failure(
        &mut self,
        symbols: Vec<SymbolID>,
        ty: &'cx ty::Ty<'cx>,
    ) -> SymbolID {
        let symbol = symbols[0];
        self.create_transient_symbol_with_ty(symbol, ty)
    }

    fn create_combined_symbol_from_tys(
        &mut self,
        symbols: Vec<SymbolID>,
        tys: &[&'cx ty::Ty<'cx>],
    ) -> SymbolID {
        let union = self.get_union_ty(tys, ty::UnionReduction::Subtype);
        self.create_combined_symbol_for_overload_failure(symbols, union)
    }

    fn create_union_of_sigs_for_overload_failure(
        &mut self,
        candidates: Sigs<'cx>,
    ) -> &'cx ty::Sig<'cx> {
        assert!(!candidates.is_empty());
        let (min_args_count, max_non_rest_param_count) = {
            let mut min = usize::MAX;
            let mut max = usize::MIN;
            for candidate in candidates {
                let v = Self::get_num_non_rest_params(candidate);
                if v < min {
                    min = v;
                }
                if v > max {
                    max = v;
                }
            }
            (min, max)
        };

        let mut params = Vec::with_capacity(16);
        for i in 0..max_non_rest_param_count {
            let symbols = candidates
                .iter()
                .flat_map(|s| {
                    if s.has_rest_param() {
                        if i < s.params.len() - 1 {
                            s.params.get(i)
                        } else {
                            s.params.last()
                        }
                    } else if i < s.params.len() {
                        s.params.get(i)
                    } else {
                        None
                    }
                })
                .copied()
                .collect::<Vec<_>>();
            assert!(!symbols.is_empty());
            let tys = candidates
                .iter()
                .flat_map(|candidate| self.try_get_ty_at_pos(candidate, i))
                .collect::<Vec<_>>();
            let symbol = self.create_combined_symbol_from_tys(symbols, &tys);
            params.push(symbol);
        }
        let rest_param_symbols = candidates
            .iter()
            .filter_map(|s| {
                if s.has_rest_param() {
                    s.params.last()
                } else {
                    None
                }
            })
            .copied()
            .collect::<Vec<_>>();
        let mut flags = ty::SigFlags::IS_SIGNATURE_CANDIDATE_FOR_OVERLOAD_FAILURE;
        if !rest_param_symbols.is_empty() {
            let tys = candidates
                .iter()
                .flat_map(|c| self.try_get_rest_ty_of_sig(c))
                .collect::<Vec<_>>();
            let ty = self.get_union_ty(&tys, ty::UnionReduction::Subtype);
            let ty = self.create_array_ty(ty, false);
            params.push(self.create_combined_symbol_for_overload_failure(rest_param_symbols, ty));
            flags |= SigFlags::HAS_REST_PARAMETER;
        }

        if candidates.iter().any(|c| c.has_literal_tys()) {
            flags |= SigFlags::HAS_LITERAL_TYPES;
        }

        self.new_sig(ty::Sig {
            ty_params: None,
            flags,
            id: ty::SigID::dummy(),
            node_id: candidates[0].node_id,
            params: self.alloc(params),
            min_args_count,
            ret: None, // TODO:
            target: None,
            mapper: None,
            class_decl: None,
        })
    }

    fn get_longest_candidate_index(&mut self, candidates: Sigs<'cx>, args_count: usize) -> usize {
        let mut max_params_index = usize::MAX;
        let mut max_params = usize::MAX;

        for (i, candidate) in candidates.iter().enumerate() {
            let param_count = candidate.get_param_count(self);
            if param_count >= args_count || self.has_effective_rest_param(candidate) {
                return i;
            }
            if max_params == usize::MAX || param_count > max_params {
                max_params = param_count;
                max_params_index = i;
            }
        }

        max_params_index
    }

    fn get_ty_args_from_nodes(
        &mut self,
        ty_args: &'cx ast::Tys<'cx>,
        ty_params: ty::Tys<'cx>,
    ) -> ty::Tys<'cx> {
        let mut ty_args = ty_args
            .list
            .iter()
            .map(|ty| self.get_ty_from_type_node(ty))
            .collect::<Vec<_>>();
        while ty_args.len() > ty_params.len() {
            ty_args.pop();
        }
        while ty_args.len() < ty_params.len() {
            let ty_param = ty_params[ty_args.len()];
            let default_ty = self
                .get_default_ty_from_ty_param(ty_param)
                .or_else(|| self.get_constraint_of_ty_param(ty_param))
                .unwrap_or(self.any_ty);
            ty_args.push(default_ty);
        }

        self.alloc(ty_args)
    }

    fn pick_longest_candidate_sig(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
        candidates: Sigs<'cx>,
        args: ast::Exprs<'cx>,
    ) -> (usize, &'cx Sig<'cx>) {
        let best_index = self.get_longest_candidate_index(candidates, args.len());
        let candidate = candidates[best_index];
        let Some(ty_params) = candidate.ty_params else {
            return (best_index, candidate);
        };
        if ty_params.is_empty() {
            return (best_index, candidate);
        }

        let instantiated = if let Some(ty_args) = expr.ty_args() {
            let ty_args = self.get_ty_args_from_nodes(ty_args, ty_params);
            self.create_sig_instantiation(candidate, Some(ty_args))
        } else {
            // TODO: infer
            candidate
        };
        (best_index, instantiated)
    }
}

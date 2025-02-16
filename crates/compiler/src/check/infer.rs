use crate::ir;
use crate::ty::{self, SigFlags, SigKind, TypeFlags};
use crate::{ast, ty::Sig};

use super::create_ty::IntersectionFlags;
use super::get_contextual::ContextFlags;
use super::utils::append_if_unique;
use super::{fn_mapper, CheckMode, InferenceContextId, TyChecker};

use thin_vec::{thin_vec, ThinVec};

#[derive(Debug, Clone)]
pub struct InferenceInfo<'cx> {
    pub ty_param: &'cx ty::Ty<'cx>,
    pub candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub contra_candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub is_fixed: bool,
    pub top_level: bool,
    pub priority: Option<InferencePriority>,
    pub inferred_ty: Option<&'cx ty::Ty<'cx>>,
}

impl<'cx> InferenceInfo<'cx> {
    fn create(ty_param: &'cx ty::Ty<'cx>) -> Self {
        Self {
            ty_param,
            candidates: None,
            contra_candidates: None,
            is_fixed: false,
            top_level: true,
            priority: None,
            inferred_ty: None,
        }
    }

    pub(super) fn has_inference_candidates(&self) -> bool {
        self.candidates.is_some() || self.contra_candidates.is_some()
    }
}

bitflags::bitflags! {
  #[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
  pub struct InferencePriority: u16 {
        const NAKED_TYPE_VARIABLE               = 1 << 0;
        const SPECULATIVE_TUPLE                 = 1 << 1;
        const SUBSTITUTE_SOURCE                 = 1 << 2;
        const HOMOMORPHIC_MAPPED_TYPE           = 1 << 3;
        const PARTIAL_HOMOMORPHIC_MAPPED_TYPE   = 1 << 4;
        const MAPPED_TYPE_CONSTRAINT            = 1 << 5;
        const CONTRAVARIANT_CONDITIONAL         = 1 << 6;
        const RETURN_TYPE                       = 1 << 7;
        const LITERAL_KEYOF                     = 1 << 8;
        const NO_CONSTRAINTS                    = 1 << 9;
        const ALWAYS_STRICT                     = 1 << 10;
        const MAX_VALUE                         = 1 << 11;

        const PRIORITY_IMPLIES_COMBINATION  = Self::RETURN_TYPE.bits() | Self::MAPPED_TYPE_CONSTRAINT.bits() | Self::LITERAL_KEYOF.bits();
        const CIRCULARITY                   = !0;
    }
}

bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
  pub struct InferenceFlags: u8 {
    const NO_DEFAULT                = 1 << 0;
    const ANY_DEFAULT               = 1 << 1;
    const SKIPPED_GENERIC_FUNCTION  = 1 << 2;
  }
}

pub(super) struct InferenceContext<'cx> {
    pub inferences: thin_vec::ThinVec<InferenceInfo<'cx>>,
    pub sig: Option<&'cx ty::Sig<'cx>>,
    pub flags: InferenceFlags,
    pub mapper: &'cx fn_mapper::FixingMapper<'cx>,
    pub non_fixing_mapper: &'cx dyn ty::TyMap<'cx>,
    pub ret_mapper: Option<&'cx dyn ty::TyMap<'cx>>,
}

impl<'cx> InferenceContext<'cx> {
    fn create(
        checker: &TyChecker<'cx>,
        id: InferenceContextId,
        ty_params: ty::Tys<'cx>,
        sig: Option<&'cx ty::Sig<'cx>>,
        flags: InferenceFlags,
    ) -> Self {
        let inferences = ty_params
            .iter()
            .map(|ty_param| InferenceInfo::create(ty_param))
            .collect::<thin_vec::ThinVec<_>>();
        let mapper = checker.making_inference_fixing_mapper(id, &inferences);
        let non_fixing_mapper = checker.making_inference_non_fixing_mapper(id, &inferences);
        Self {
            inferences,
            sig,
            flags,
            ret_mapper: None,
            mapper,
            non_fixing_mapper,
        }
    }
}

impl<'cx> TyChecker<'cx> {
    pub fn create_inference_context(
        &mut self,
        ty_params: ty::Tys<'cx>,
        sig: Option<&'cx ty::Sig<'cx>>,
        flags: InferenceFlags,
    ) -> InferenceContextId {
        let id = InferenceContextId(self.inferences.len() as u32);
        let inference = InferenceContext::create(self, id, ty_params, sig, flags);
        self.inferences.push(inference);
        id
    }

    fn making_inference_fixing_mapper(
        &self,
        id: InferenceContextId,
        inferences: &[InferenceInfo<'cx>],
    ) -> &'cx fn_mapper::FixingMapper<'cx> {
        let sources = inferences.iter().map(|i| i.ty_param).collect::<Vec<_>>();
        let sources = self.alloc(sources);
        self.alloc(fn_mapper::FixingMapper {
            inference: id,
            sources,
        })
    }

    fn making_inference_non_fixing_mapper(
        &self,
        id: InferenceContextId,
        inferences: &[InferenceInfo<'cx>],
    ) -> &'cx fn_mapper::NonFixingMapper<'cx> {
        let sources = inferences.iter().map(|i| i.ty_param).collect::<Vec<_>>();
        let sources = self.alloc(sources);
        self.alloc(fn_mapper::NonFixingMapper {
            inference: id,
            sources,
        })
    }

    fn set_inference_ret_mapper(
        &mut self,
        inference: InferenceContextId,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
    ) {
        self.inferences[inference.as_usize()].ret_mapper = mapper;
    }

    pub(super) fn inference(&self, id: InferenceContextId) -> &InferenceContext<'cx> {
        &self.inferences[id.as_usize()]
    }

    pub(super) fn inference_info(
        &self,
        inference: InferenceContextId,
        idx: usize,
    ) -> &InferenceInfo<'cx> {
        &self.inference_infos(inference)[idx]
    }

    pub(crate) fn set_inferred_ty_of_inference_info(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
        ty: &'cx ty::Ty<'cx>,
    ) {
        let cache = &mut self.inferences[inference.as_usize()].inferences[idx].inferred_ty;
        assert!(cache.is_none());
        *cache = Some(ty)
    }

    pub(super) fn inference_infos(&self, inference: InferenceContextId) -> &[InferenceInfo<'cx>] {
        &self.inference(inference).inferences
    }

    pub(crate) fn config_inference_flags(
        &mut self,
        inference: InferenceContextId,
        f: impl FnOnce(&mut InferenceFlags),
    ) {
        f(&mut self.inferences[inference.as_usize()].flags);
    }

    pub(super) fn get_inferred_tys(&mut self, inference: InferenceContextId) -> ty::Tys<'cx> {
        let tys = (0..self.inference(inference).inferences.len())
            .map(|idx| self.get_inferred_ty(inference, idx))
            .collect::<Vec<_>>();
        self.alloc(tys)
    }

    fn is_ty_param_at_top_level_in_ret_top(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        ty_param: &'cx ty::Ty<'cx>,
    ) -> bool {
        let ret_ty = self.get_ret_ty_of_sig(sig);
        self.is_ty_param_at_top_level(ret_ty, ty_param, 0)
    }

    fn is_ty_param_at_top_level(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        ty_param: &'cx ty::Ty<'cx>,
        depth: u8,
    ) -> bool {
        if ty == ty_param {
            true
        } else if let Some(union) = ty.kind.as_union() {
            union
                .tys
                .iter()
                .any(|ty| self.is_ty_param_at_top_level(ty, ty_param, depth))
        } else if depth >= 3 {
            false
        } else if let Some(cond) = ty.kind.as_cond_ty() {
            // TODO:
            false
        } else {
            false
        }
    }

    fn get_covariant_inference(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
        sig: &'cx ty::Sig<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let candidates = self
            .inference_info(inference, idx)
            .candidates
            .as_ref()
            .unwrap();
        let candidates = if candidates.len() > 1 {
            let (object_literals, mut non_object_literals): (ThinVec<_>, ThinVec<_>) = candidates
                .into_iter()
                .copied()
                .partition(|c| c.is_object_or_array_literal());
            if object_literals.is_empty() {
                candidates.to_vec()
            } else {
                let lits = self.get_union_ty(&object_literals, ty::UnionReduction::Subtype);
                non_object_literals.push(lits);
                non_object_literals.into()
            }
        } else {
            candidates.to_vec()
        };

        let has_primitive_constraint = {
            let ty_param = self.inference_info(inference, idx).ty_param;
            self.get_constraint_of_ty_param(ty_param)
                .is_some_and(|constraint| {
                    let t = if constraint.flags.intersects(TypeFlags::CONDITIONAL) {
                        self.get_default_constraint_of_cond_ty(constraint)
                    } else {
                        constraint
                    };
                    t.maybe_type_of_kind(
                        TypeFlags::PRIMITIVE
                            | TypeFlags::INDEX
                            | TypeFlags::TEMPLATE_LITERAL
                            | TypeFlags::STRING_MAPPING,
                    )
                })
        };

        let primitive_constraint = has_primitive_constraint;

        let i = self.inference_info(inference, idx);

        // for case:
        // `function foo<T>(a: T, b: T): void`
        // the call `foo('a', 'b')` should widen `'a'` and `'b'` into
        // string type rather than string literal type.
        let widen_literal_tys = !primitive_constraint
            && i.top_level
            && (i.is_fixed || !self.is_ty_param_at_top_level_in_ret_top(sig, i.ty_param));

        let base_candidates = if primitive_constraint {
            candidates
        } else if widen_literal_tys {
            candidates
                .iter()
                .map(|c| self.get_widened_literal_ty(c))
                .collect()
        } else {
            candidates
        };

        if base_candidates.len() == 1 {
            self.get_widened_ty(base_candidates[0])
        } else {
            let ty = self.get_union_ty(&base_candidates, ty::UnionReduction::Lit);
            self.get_widened_ty(ty)
        }
    }

    fn get_contravariant_inference(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
    ) -> &'cx ty::Ty<'cx> {
        let info = self.inference_info(inference, idx);
        // TODO: remove clone
        let cs = info.candidates.as_ref().unwrap().clone();
        if info
            .priority
            .unwrap()
            .intersects(InferencePriority::PRIORITY_IMPLIES_COMBINATION)
        {
            self.get_intersection_ty(&cs, IntersectionFlags::None, None, None)
        } else {
            self.get_common_sub_ty(&cs)
        }
    }

    pub(super) fn get_ty_with_this_arg(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        this_arg: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(_) = ty.kind.as_object_reference() {
            ty
            // let target = r.target.kind.expect_object_interface();
            // let ty_args = self.get_ty_arguments(ty);
            // if target
            //     .ty_params
            //     .map(|ty_params| ty_params.len())
            //     .unwrap_or_default()
            //     == ty_args.len()
            // {
            //     let mut ty_args = ty_args.to_vec();
            //     ty_args.push(this_arg.unwrap_or(target.this_ty.unwrap()));
            //     let ty_args = self.alloc(ty_args);
            //     self.create_reference_ty(r.target, Some(ty_args), ObjectFlags::empty())
            // } else {
            //     ty
            // }
        } else if let Some(i) = ty.kind.as_intersection() {
            let tys = self
                .same_map_tys(Some(i.tys), |this, ty, _| {
                    this.get_ty_with_this_arg(ty, this_arg)
                })
                .unwrap();
            self.get_intersection_ty(tys, IntersectionFlags::None, None, None)
        } else {
            ty
        }
    }

    pub(crate) fn get_inferred_ty(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
    ) -> &'cx ty::Ty<'cx> {
        let i = self.inference_info(inference, idx);
        if let Some(inferred_ty) = i.inferred_ty {
            return inferred_ty;
        }
        let mut inferred_ty = None;
        let mut fallback_ty = None;
        if let Some(sig) = self.get_inference_sig(inference) {
            let candidates_is_some = i.candidates.is_some();
            let contra_candidates_is_some = i.contra_candidates.is_some();
            let inferred_covariant_ty = if candidates_is_some {
                Some(self.get_covariant_inference(inference, idx, sig))
            } else {
                None
            };
            let inferred_contravariant_ty = if contra_candidates_is_some {
                Some(self.get_contravariant_inference(inference, idx))
            } else {
                None
            };
            if inferred_covariant_ty.is_some() || inferred_contravariant_ty.is_some() {
                let prefer_covariant_ty =
                    inferred_covariant_ty.is_some_and(|inferred_covariant_ty| {
                        inferred_contravariant_ty.map_or(true, |inferred_contravariant_ty| {
                            !inferred_contravariant_ty
                                .flags
                                .intersects(TypeFlags::NEVER | TypeFlags::ANY)
                                && self
                                    .inference_info(inference, idx)
                                    .contra_candidates
                                    .as_ref()
                                    .cloned()
                                    .is_some_and(|cs| {
                                        cs.iter().any(|t| {
                                            self.is_type_assignable_to(inferred_covariant_ty, t)
                                        })
                                    })
                                && (0..self.inference(inference).inferences.len()).all(|other| {
                                    other == idx
                                        && self
                                            .get_constraint_of_ty_param(
                                                self.inference_info(inference, other).ty_param,
                                            )
                                            .is_some_and(|t| {
                                                t != self.inference_info(inference, idx).ty_param
                                            })
                                        || self
                                            .inference_info(inference, other)
                                            .candidates
                                            .as_ref()
                                            .cloned()
                                            .map_or(true, |cs| {
                                                cs.iter().all(|t| {
                                                    self.is_type_assignable_to(
                                                        t,
                                                        inferred_covariant_ty,
                                                    )
                                                })
                                            })
                                })
                        })
                    });
                inferred_ty = if prefer_covariant_ty {
                    inferred_covariant_ty
                } else {
                    inferred_contravariant_ty
                };
                fallback_ty = if prefer_covariant_ty {
                    inferred_contravariant_ty
                } else {
                    inferred_covariant_ty
                };
            } else if self
                .inference(inference)
                .flags
                .intersects(InferenceFlags::NO_DEFAULT)
            {
                inferred_ty = Some(self.silent_never_ty);
            } else {
                let ty_param = self.inference_info(inference, idx).ty_param;
                if let Some(default_ty) = self.get_default_ty_from_ty_param(ty_param) {
                    todo!("back reference mapper")
                    // let back_reference_mapper =
                    // inferred_ty =
                }
            }
        } else {
            inferred_ty = self.get_ty_from_inference(inference, idx)
        };

        let i = self.inference_info(inference, idx);
        let ty = if let Some(constraint) = self.get_constraint_of_ty_param(i.ty_param) {
            let instantiated_constraint = self.instantiate_ty(constraint, None);
            if inferred_ty.map_or(true, |inferred_ty| {
                let ty = self.get_ty_with_this_arg(instantiated_constraint, fallback_ty);
                // TODO: more flexible compare types
                !self.is_type_related_to(inferred_ty, ty, super::relation::RelationKind::Assignable)
            }) {
                instantiated_constraint
            } else {
                inferred_ty.unwrap_or(self.any_ty)
            }
        } else {
            inferred_ty.unwrap_or(self.any_ty)
        };

        self.set_inferred_ty_of_inference_info(inference, idx, ty);
        ty
    }

    pub(super) fn get_inference_sig(
        &self,
        inference: InferenceContextId,
    ) -> Option<&'cx ty::Sig<'cx>> {
        self.inference(inference).sig
    }

    fn get_inference_info_for_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        inference: InferenceContextId,
    ) -> Option<usize> {
        if ty.kind.is_type_variable() {
            self.inferences[inference.as_usize()]
                .inferences
                .iter()
                .position(|i| i.ty_param == ty)
        } else {
            None
        }
    }

    fn tys_definitely_unrelated(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        if source.is_tuple() && target.is_tuple() {
            todo!()
        } else {
            self.get_unmatched_prop(source, target, false).is_some()
                && self.get_unmatched_prop(target, source, false).is_some()
        }
    }

    pub(super) fn infer_ty_args(
        &mut self,
        node: &impl ir::CallLike<'cx>,
        sig: &'cx Sig<'cx>,
        args: &'cx [&'cx ast::Expr<'cx>],
        check_mode: CheckMode,
        inference: InferenceContextId,
    ) -> ty::Tys<'cx> {
        let Some(sig_ty_params) = sig.ty_params else {
            unreachable!()
        };

        let node_id = node.id();
        if !self.p.node(node_id).is_bin_expr() {
            if let Some(contextual_ty) = self.get_contextual_ty(node_id, Some(ContextFlags::None)) {
                let inference_target_ty = self.get_ret_ty_of_sig(sig);
                if self.could_contain_ty_var(inference_target_ty) {
                    let outer_context = self.get_inference_context(node_id);
                    let is_from_binding_pattern = false;
                    if !is_from_binding_pattern {
                        let outer_mapper =
                            outer_context
                                .and_then(|ctx| ctx.inference)
                                .map(|inference| {
                                    self.inference(inference).mapper as &'cx dyn ty::TyMap<'cx>
                                });
                        let instantiated_ty = self.instantiate_ty(contextual_ty, outer_mapper);
                        let inference_source_ty = if let Some(contextual_sig) =
                            self.get_single_call_sig(instantiated_ty)
                        {
                            if let Some(ty_param) = contextual_sig.ty_params {
                                // TODO: `get_or_create_ty_from_sig`
                                instantiated_ty
                            } else {
                                instantiated_ty
                            }
                        } else {
                            instantiated_ty
                        };
                        self.infer_tys(
                            inference,
                            inference_source_ty,
                            inference_target_ty,
                            Some(InferencePriority::RETURN_TYPE),
                            false,
                        );
                    }

                    let ret_ctx = self.create_inference_context(
                        sig_ty_params,
                        Some(sig),
                        self.inferences[inference.as_usize()].flags,
                    );
                    let ret_mapper = outer_context
                        .and_then(|ctx| ctx.inference)
                        .and_then(|ctx| self.inference(ctx).ret_mapper);
                    let ret_source_ty = self.instantiate_ty(contextual_ty, ret_mapper);
                    self.infer_tys(ret_ctx, ret_source_ty, inference_target_ty, None, false);
                    let ret_inference = self.inference(ret_ctx);
                    let ret_inferences = ret_inference
                        .inferences
                        .iter()
                        .filter(|i| i.has_inference_candidates())
                        .cloned()
                        .collect::<thin_vec::ThinVec<_>>();
                    if ret_inferences.is_empty() {
                        drop(ret_inferences);
                        self.set_inference_ret_mapper(ret_ctx, None);
                    } else {
                        let id = InferenceContextId(self.inferences.len() as u32);
                        let mapper = self.making_inference_fixing_mapper(id, &ret_inferences);
                        let non_fixing_mapper =
                            self.making_inference_non_fixing_mapper(id, &ret_inferences);
                        let inference = InferenceContext {
                            inferences: ret_inferences,
                            sig: ret_inference.sig,
                            flags: ret_inference.flags,
                            ret_mapper: None,
                            mapper,
                            non_fixing_mapper,
                        };
                        self.set_inference_ret_mapper(
                            ret_ctx,
                            Some(inference.mapper as &'cx dyn ty::TyMap<'cx>),
                        );
                        self.inferences.push(inference);
                    };
                }
            }
        }

        for (idx, arg) in args.iter().enumerate() {
            let param_ty = self.get_ty_at_pos(sig, idx);
            if self.could_contain_ty_var(param_ty) {
                let arg_ty =
                    self.check_expr_with_contextual_ty(arg, param_ty, Some(inference), check_mode);
                self.infer_tys(inference, arg_ty, param_ty, None, false);
            }
        }

        self.get_inferred_tys(inference)
    }

    pub(super) fn infer_from_annotated_params(
        &mut self,
        sig: &'cx Sig<'cx>,
        contextual_sig: &'cx Sig<'cx>,
        inference: InferenceContextId,
    ) {
        let len = sig.params.len() - (if sig.has_rest_param() { 1 } else { 0 });
        for i in 0..len {
            let decl = sig.params[i].decl(self.binder);
            if let Some(ty_node) = self.p.node(decl).as_param_decl().and_then(|decl| decl.ty) {
                let source = self.get_ty_from_type_node(ty_node);
                let target = self.get_ty_at_pos(contextual_sig, i);
                self.infer_tys(inference, source, target, None, false);
            }
        }
    }

    pub(super) fn infer_state<'checker>(
        &'checker mut self,
        inference: InferenceContextId,
        priority: Option<InferencePriority>,
        contravariant: bool,
    ) -> InferenceState<'cx, 'checker> {
        let priority = priority.unwrap_or(InferencePriority::empty());
        InferenceState {
            priority,
            inference_priority: InferencePriority::MAX_VALUE,
            c: self,
            inference,
            contravariant,
            bivariant: false,
            propagation_ty: None,
        }
    }

    pub(super) fn infer_tys(
        &mut self,
        inference: InferenceContextId,
        original_source: &'cx ty::Ty<'cx>,
        original_target: &'cx ty::Ty<'cx>,
        priority: Option<InferencePriority>,
        contravariant: bool,
    ) {
        let mut state = self.infer_state(inference, priority, contravariant);
        state.infer_from_tys(original_source, original_target);
    }

    pub(super) fn apply_to_ret_ty(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        f: impl FnOnce(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        // TODO: handle ty_pred
        let target_ret_ty = self.get_ret_ty_of_sig(target);
        if self.could_contain_ty_var(target_ret_ty) {
            let source_ret_ty = self.get_ret_ty_of_sig(source);
            f(self, source_ret_ty, target_ret_ty);
        }
    }

    pub(super) fn clear_cached_inferences(&mut self, id: InferenceContextId) {
        let list = &mut self.inferences[id.as_usize()].inferences;
        for i in list {
            if !i.is_fixed {
                i.inferred_ty = None;
            }
        }
    }
}

pub(super) struct InferenceState<'cx, 'checker> {
    priority: InferencePriority,
    inference_priority: InferencePriority,
    c: &'checker mut TyChecker<'cx>,
    inference: InferenceContextId,
    contravariant: bool,
    bivariant: bool,
    propagation_ty: Option<&'cx ty::Ty<'cx>>,
}

impl<'cx> InferenceState<'cx, '_> {
    fn get_mut_inference(&mut self) -> &mut InferenceContext<'cx> {
        &mut self.c.inferences[self.inference.as_usize()]
    }

    fn get_mut_inference_info(&mut self, idx: usize) -> &mut InferenceInfo<'cx> {
        &mut self.get_mut_inference().inferences[idx]
    }

    fn reset_info_candidate(&mut self, idx: usize) {
        let info = self.get_mut_inference_info(idx);
        info.candidates = None;
    }

    fn reset_info_contra_candidate(&mut self, idx: usize) {
        let info = self.get_mut_inference_info(idx);
        info.contra_candidates = None;
    }

    fn set_info_priority(&mut self, idx: usize, priority: InferencePriority) {
        let info = self.get_mut_inference_info(idx);
        info.priority = Some(priority);
    }

    fn set_info_toplevel(&mut self, idx: usize) {
        let info = self.get_mut_inference_info(idx);
        info.top_level = true;
    }

    fn set_info_toplevel_false(&mut self, idx: usize) {
        let info = self.get_mut_inference_info(idx);
        info.top_level = false;
    }

    fn append_candidate(&mut self, idx: usize, candidate: &'cx ty::Ty<'cx>) {
        let info = self.get_mut_inference_info(idx);
        if let Some(candidates) = &mut info.candidates {
            candidates.push(candidate);
        } else {
            info.candidates = Some(thin_vec::thin_vec![candidate]);
        }
    }

    fn append_contra_candidate(&mut self, idx: usize, candidate: &'cx ty::Ty<'cx>) {
        let info = self.get_mut_inference_info(idx);
        if let Some(candidates) = &mut info.contra_candidates {
            candidates.push(candidate);
        } else {
            info.contra_candidates = Some(thin_vec::thin_vec![candidate]);
        }
    }

    fn infer_to_cond_ty(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        let Some(target_cond) = target.kind.as_cond_ty() else {
            unreachable!()
        };
        if let Some(source_cond) = source.kind.as_cond_ty() {
            self.infer_from_tys(source_cond.check_ty, target_cond.check_ty);
            self.infer_from_tys(source_cond.extends_ty, target_cond.extends_ty);
            // TODO:
        } else {
            // TODO:
        }
    }

    pub(super) fn infer_from_tys(
        &mut self,
        mut source: &'cx ty::Ty<'cx>,
        mut target: &'cx ty::Ty<'cx>,
    ) {
        let original_target = target;
        if !self.c.could_contain_ty_var(target) || target.is_no_infer_ty() {
            return;
        }

        if source == target && source.kind.is_union_or_intersection() {
            let tys = source.kind.tys_of_union_or_intersection().unwrap();
            for t in tys {
                self.infer_from_tys(t, t);
            }
            return;
        }

        if let Some(target_union) = target.kind.as_union() {
            let source_tys = if let Some(source_union) = source.kind.as_union() {
                source_union.tys
            } else {
                self.c.alloc([source])
            };
            let target_tys = target_union.tys;
            let (temp_sources, temp_targets) =
                self.infer_from_matching_tys(source_tys, target_tys, |this, s, t| {
                    this.c.is_ty_or_base_identical_to(s, t)
                });
            let (sources, targets) =
                self.infer_from_matching_tys(temp_sources, temp_targets, |_, s, t| {
                    TyChecker::is_ty_closely_matched_by(s, t)
                });
            if targets.is_empty() {
                return;
            }
            target = self.c.get_union_ty(targets, ty::UnionReduction::Lit);
            if sources.is_empty() {
                self.infer_with_priority(source, target, InferencePriority::NAKED_TYPE_VARIABLE);
                return;
            }
            source = self.c.get_union_ty(sources, ty::UnionReduction::Lit);
        };

        if target.kind.is_type_variable() {
            if let Some(idx) = self.c.get_inference_info_for_ty(target, self.inference) {
                let info = self.c.inference_info(self.inference, idx);
                if !info.is_fixed {
                    let candidate = self.propagation_ty.unwrap_or(source);
                    if info.priority.is_none() || self.priority < info.priority.unwrap() {
                        self.reset_info_candidate(idx);
                        self.reset_info_contra_candidate(idx);
                        self.set_info_priority(idx, self.priority);
                        self.set_info_toplevel(idx);
                    }
                    let info = self.c.inference_info(self.inference, idx);
                    if Some(self.priority) == info.priority {
                        if self.contravariant && !self.bivariant {
                            if info
                                .contra_candidates
                                .as_ref()
                                .map_or(true, |cs| !cs.contains(&candidate))
                            {
                                self.append_contra_candidate(idx, candidate);
                                self.c.clear_cached_inferences(self.inference);
                            }
                        } else if info
                            .candidates
                            .as_ref()
                            .map_or(true, |cs| !cs.contains(&candidate))
                        {
                            self.append_candidate(idx, candidate);
                            self.c.clear_cached_inferences(self.inference);
                        }
                    }

                    if !self.priority.intersects(InferencePriority::RETURN_TYPE)
                        && target.flags.intersects(TypeFlags::TYPE_PARAMETER)
                        && self.c.inference_info(self.inference, idx).top_level
                        && !self.c.is_ty_param_at_top_level(original_target, target, 0)
                    {
                        self.set_info_toplevel_false(idx);
                        self.c.clear_cached_inferences(self.inference);
                    }
                }
                self.inference_priority = if self.inference_priority < self.priority {
                    self.inference_priority
                } else {
                    self.priority
                };
                return;
            }
        }

        if target.kind.is_cond_ty() {
            self.invoke_once(source, target, |this, source, target| {
                this.infer_to_cond_ty(source, target);
            });
        } else if let Some(u) = source.kind.as_union() {
            for ty in u.tys {
                self.infer_from_tys(ty, target);
            }
        } else {
            source = self.c.get_reduced_ty(source);
            if !(self.priority.intersects(InferencePriority::NO_CONSTRAINTS)
                && source
                    .flags
                    .intersects(TypeFlags::INTERSECTION | TypeFlags::INSTANTIABLE))
            {
                let apparent_source = self.c.get_apparent_ty(source);
                if apparent_source != source
                    && !(apparent_source
                        .flags
                        .intersects(TypeFlags::OBJECT | TypeFlags::INTERSECTION))
                {
                    return self.infer_from_tys(apparent_source, target);
                }
                source = apparent_source;
            }
            if source.kind.is_object_or_intersection() {
                self.invoke_once(source, target, |this, source, target| {
                    this.infer_from_object_tys(source, target);
                });
            }
        }
    }

    fn invoke_once(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        action: impl FnOnce(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        let saved_inference_priority = self.inference_priority;
        self.inference_priority = InferencePriority::MAX_VALUE;
        action(self, source, target);
        self.inference_priority = if self.inference_priority < saved_inference_priority {
            self.inference_priority
        } else {
            saved_inference_priority
        };
    }

    fn infer_with_priority(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        new_priority: InferencePriority,
    ) {
        let saved_priority = self.priority;
        self.priority |= new_priority;
        self.infer_from_tys(source, target);
        self.priority = saved_priority
    }

    fn infer_from_object_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        if !self.c.tys_definitely_unrelated(source, target) {
            if source.is_tuple() || source.kind.is_array(self.c) {
                if target.is_tuple() {
                    // TODO:
                }
                if target.kind.is_array(self.c) {
                    self.infer_from_index_tys(source, target);
                    return;
                }
            }
            self.infer_from_props(source, target);
            self.infer_from_sigs(source, target, SigKind::Call);
            self.infer_from_sigs(source, target, SigKind::Constructor);
        }
    }

    fn infer_from_index_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        let new_priority = InferencePriority::empty();
        let target_index_infos = self.c.get_index_infos_of_ty(target);
        for target_index_info in target_index_infos {
            if let Some(source_info) = self
                .c
                .get_applicable_index_info(source, target_index_info.key_ty)
            {
                self.infer_with_priority(
                    source_info.val_ty,
                    target_index_info.val_ty,
                    new_priority,
                );
            }
        }
    }

    fn infer_from_matching_tys(
        &mut self,
        sources: ty::Tys<'cx>,
        targets: ty::Tys<'cx>,
        matches: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> bool,
    ) -> (ty::Tys<'cx>, ty::Tys<'cx>) {
        let mut matched_sources = Vec::with_capacity(sources.len());
        let mut matched_targets = Vec::with_capacity(targets.len());
        for t in targets {
            for s in sources {
                if matches(self, s, t) {
                    self.infer_from_tys(s, t);
                    append_if_unique(&mut matched_sources, *s);
                    append_if_unique(&mut matched_targets, *t);
                }
            }
        }

        let matched = |matched: &[&'cx ty::Ty<'cx>], tys: ty::Tys<'cx>| {
            if matched.is_empty() {
                tys
            } else {
                let tys = tys
                    .iter()
                    .filter(|t| !matched.contains(t))
                    .copied()
                    .collect::<Vec<_>>();
                self.c.alloc(tys)
            }
        };

        (
            matched(&matched_sources, sources),
            matched(&matched_targets, targets),
        )
    }

    pub(super) fn apply_to_param_tys(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        callback: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        let source_count = source.get_param_count(self.c);
        let target_count = target.get_param_count(self.c);
        let source_rest_ty = source.get_rest_ty(self.c);
        let target_rest_ty = target.get_rest_ty(self.c);
        let target_non_rest_count = if target_rest_ty.is_some() {
            target_count - 1
        } else {
            target_count
        };
        let param_count = if source_rest_ty.is_some() {
            target_count
        } else {
            usize::min(source_count, target_non_rest_count)
        };
        // TODO: `source_this_ty`
        for i in 0..param_count {
            let source_ty = self.c.get_ty_at_pos(source, i);
            let target_ty = self.c.get_ty_at_pos(target, i);
            callback(self, source_ty, target_ty);
        }
        if let Some(target_rest_ty) = target_rest_ty {
            let readonly = false;
            let rest_ty = self.c.get_rest_ty_at_pos(source, param_count, readonly);
            callback(self, rest_ty, target_rest_ty);
        }
    }

    fn infer_from_contravariant_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        self.contravariant = !self.contravariant;
        self.infer_from_tys(source, target);
        self.contravariant = !self.contravariant;
    }

    fn infer_from_sig(&mut self, source: &'cx ty::Sig<'cx>, target: &'cx ty::Sig<'cx>) {
        if !source.flags.intersects(SigFlags::IS_NON_INFERRABLE) {
            let save_bivariant = self.bivariant;
            let n = self.c.p.node(target.def_id());
            self.bivariant = self.bivariant
                || n.is_class_method_ele()
                || n.is_object_method_member()
                || n.is_method_signature()
                || n.is_class_ctor();

            self.apply_to_param_tys(source, target, |this, source, target| {
                let strict_fn_tys = false; // TODO: config;
                if strict_fn_tys || this.priority.intersects(InferencePriority::ALWAYS_STRICT) {
                    this.infer_from_contravariant_tys(source, target);
                } else {
                    this.infer_from_tys(source, target);
                }
            });

            self.bivariant = save_bivariant;
        }
        self.apply_to_ret_ty(source, target, |this, source, target| {
            this.infer_from_tys(source, target);
        });
    }

    fn infer_from_sigs(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        kind: SigKind,
    ) {
        let source_sigs = self.c.get_signatures_of_type(source, kind);
        let source_len = source_sigs.len();
        if source_len > 0 {
            let target_sigs = self.c.get_signatures_of_type(target, kind);
            let target_len = target_sigs.len();
            for i in 0..target_len {
                let source_index = if source_len + i > target_len {
                    source_len + i - target_len
                } else {
                    0
                };
                let source = self.c.get_base_sig(source_sigs[source_index]);
                let target = self.c.get_erased_sig(target_sigs[i]);
                self.infer_from_sig(source, target);
            }
        }
    }

    fn apply_to_ret_ty(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        f: impl FnOnce(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        // TODO: handle ty_pred
        let target_ret_ty = self.c.get_ret_ty_of_sig(target);
        if self.c.could_contain_ty_var(target_ret_ty) {
            let source_ret_ty = self.c.get_ret_ty_of_sig(source);
            f(self, source_ret_ty, target_ret_ty);
        }
    }

    fn infer_from_props(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        for target_prop in self.c.get_props_of_ty(target) {
            if let Some(source_prop) = self
                .c
                .get_prop_of_ty(source, self.c.symbol(*target_prop).name())
            {
                // TODO:
            }
        }
    }
}

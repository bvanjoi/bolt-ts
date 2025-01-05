use crate::ir;
use crate::ty::{self, SigKind, TyMapper};
use crate::{ast, ty::Sig};

use super::get_contextual::ContextFlags;
use super::utils::append_if_unique;
use super::{CheckMode, InferenceContextId, TyChecker};

use thin_vec::thin_vec;

#[derive(Debug, Clone)]
pub struct InferenceInfo<'cx> {
    pub ty_params: &'cx ty::Ty<'cx>,
    pub candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub contra_candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub is_fixed: bool,
    pub top_level: bool,
    pub priority: Option<InferencePriority>,
    pub inferred_ty: Option<&'cx ty::Ty<'cx>>,
}

impl<'cx> InferenceInfo<'cx> {
    fn create(ty_params: &'cx ty::Ty<'cx>) -> Self {
        Self {
            ty_params,
            candidates: None,
            contra_candidates: None,
            is_fixed: false,
            top_level: true,
            priority: None,
            inferred_ty: None,
        }
    }

    fn has_inference_candidates(&self) -> bool {
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

#[derive(Debug, Clone)]
pub struct InferenceContext<'cx> {
    pub inferences: thin_vec::ThinVec<InferenceInfo<'cx>>,
    pub sig: Option<&'cx ty::Sig<'cx>>,
    pub flags: InferenceFlags,
    pub ret_mapper: Option<InferenceContextId>,
}

impl<'cx> InferenceContext<'cx> {
    fn create(
        ty_params: ty::Tys<'cx>,
        sig: Option<&'cx ty::Sig<'cx>>,
        flags: InferenceFlags,
    ) -> Self {
        let inferences = ty_params
            .iter()
            .map(|ty_param| InferenceInfo::create(ty_param))
            .collect();
        Self {
            inferences,
            sig,
            flags,
            ret_mapper: None,
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
        self.create_inference_context_worker(InferenceContext::create(ty_params, sig, flags))
    }

    fn create_inference_context_worker(
        &mut self,
        inference: InferenceContext<'cx>,
    ) -> InferenceContextId {
        let id = InferenceContextId(self.inferences.len() as u32);
        self.inferences.push(inference);
        id
    }

    pub(crate) fn create_inference_fixing_mapper(
        &mut self,
        id: InferenceContextId,
    ) -> &'cx TyMapper<'cx> {
        let sources = self
            .inference(id)
            .inferences
            .iter()
            .map(|i| i.ty_params)
            .collect::<Vec<_>>();
        let sources = self.alloc(sources);
        let target = (0..self.inference(id).inferences.len())
            .map(|idx| {
                // TODO: handle `!is_fixed`
                self.get_inferred_ty(id, idx)
            })
            .collect::<Vec<_>>();
        let target = self.alloc(target);
        self.alloc(TyMapper::create(sources, target))
    }

    pub(crate) fn create_inference_non_fixing_mapper(
        &mut self,
        id: InferenceContextId,
    ) -> &'cx TyMapper<'cx> {
        let sources = self
            .inference(id)
            .inferences
            .iter()
            .map(|i| i.ty_params)
            .collect::<Vec<_>>();
        let sources = self.alloc(sources);
        let target = (0..self.inference(id).inferences.len())
            .map(|idx| self.get_inferred_ty(id, idx))
            .collect::<Vec<_>>();
        let target = self.alloc(target);
        self.alloc(TyMapper::create(sources, target))
    }

    pub(crate) fn create_inference_ret_mapper(
        &mut self,
        inference: InferenceContextId,
    ) -> Option<&'cx TyMapper<'cx>> {
        self.inference(inference)
            .ret_mapper
            .map(|ret_mapper| self.create_inference_fixing_mapper(ret_mapper))
    }
    pub(crate) fn set_inference_ret_mapper(
        &mut self,
        inference: InferenceContextId,
        mapper_ctx: Option<InferenceContextId>,
    ) {
        self.inferences[inference.as_usize()].ret_mapper = mapper_ctx;
    }

    pub(crate) fn inference(&self, id: InferenceContextId) -> &InferenceContext<'cx> {
        &self.inferences[id.as_usize()]
    }

    pub(crate) fn inference_info(
        &self,
        inference: InferenceContextId,
        idx: usize,
    ) -> &InferenceInfo<'cx> {
        &self.inference(inference).inferences[idx]
    }

    fn get_inferred_ty(&mut self, inference: InferenceContextId, idx: usize) -> &'cx ty::Ty<'cx> {
        let ctx = &self.inferences[inference.as_usize()];
        let i = &ctx.inferences[idx];
        // TODO: cache
        if let Some(sig) = ctx.sig {
            if let Some(candidates) = &i.candidates {
                // TODO: use `get_covariant_inference`
                self.create_union_type(candidates.to_vec(), ty::UnionReduction::Subtype)
            } else if let Some(tys) = &i.contra_candidates {
                todo!()
            } else {
                self.any_ty()
            }
        } else {
            self.get_ty_from_inference(inference, idx)
                .unwrap_or(self.undefined_ty())
        }
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
                .position(|i| i.ty_params == ty)
        } else {
            None
        }
    }

    fn tys_definitely_unrelated(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        if source.kind.is_tuple() && target.kind.is_tuple() {
            todo!()
        } else {
            self.get_unmatched_prop(source, target).is_some()
                && self.get_unmatched_prop(target, source).is_some()
        }
    }

    fn get_inferred_tys(&mut self, inference: InferenceContextId) -> ty::Tys<'cx> {
        let tys = (0..self.inference(inference).inferences.len())
            .map(|idx| self.get_inferred_ty(inference, idx))
            .collect::<Vec<_>>();
        self.alloc(tys)
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
                        let outer_mapper = outer_context
                            .and_then(|ctx| ctx.inference)
                            .map(|inference| self.create_inference_fixing_mapper(inference));
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
                        .and_then(|ctx| self.create_inference_ret_mapper(ctx));
                    let ret_source_ty = self.instantiate_ty(contextual_ty, ret_mapper);
                    self.infer_tys(ret_ctx, ret_source_ty, inference_target_ty, None, false);
                    let ret_inference = self.inference(ret_ctx);
                    let ret_inferences = ret_inference
                        .inferences
                        .iter()
                        .filter(|i| i.has_inference_candidates())
                        .collect::<thin_vec::ThinVec<_>>();
                    if ret_inferences.is_empty() {
                        drop(ret_inferences);
                        self.set_inference_ret_mapper(ret_ctx, None);
                    } else {
                        let inferences = ret_inferences
                            .into_iter()
                            .cloned()
                            .collect::<thin_vec::ThinVec<_>>();
                        let inference = InferenceContext {
                            inferences,
                            sig: ret_inference.sig,
                            flags: ret_inference.flags,
                            ret_mapper: None,
                        };
                        let id = self.create_inference_context_worker(inference);
                        self.set_inference_ret_mapper(ret_ctx, Some(id));
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

    fn infer_tys(
        &mut self,
        inference: InferenceContextId,
        original_source: &'cx ty::Ty<'cx>,
        original_target: &'cx ty::Ty<'cx>,
        priority: Option<InferencePriority>,
        contravariant: bool,
    ) {
        let priority = priority.unwrap_or(InferencePriority::empty());
        let mut state = InferenceState {
            priority,
            inference_priority: InferencePriority::MAX_VALUE,
            checker: self,
            inference,
            contravariant,
            bivariant: false,
            propagation_ty: None,
        };
        state.infer_from_tys(original_source, original_target);
    }
}

struct InferenceState<'cx, 'checker> {
    priority: InferencePriority,
    inference_priority: InferencePriority,
    checker: &'checker mut TyChecker<'cx>,
    inference: InferenceContextId,
    contravariant: bool,
    bivariant: bool,
    propagation_ty: Option<&'cx ty::Ty<'cx>>,
}

impl<'cx> InferenceState<'cx, '_> {
    fn get_mut_inference(&mut self) -> &mut InferenceContext<'cx> {
        &mut self.checker.inferences[self.inference.as_usize()]
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

    fn clear_cached_inferences(&mut self) {
        let list = &mut self.get_mut_inference().inferences;
        for i in list {
            if !i.is_fixed {
                i.inferred_ty = None;
            }
        }
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

    fn infer_from_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        if !self.checker.could_contain_ty_var(target) {
            return;
        }

        if source == target && source.kind.is_union_or_intersection() {
            if let Some(union) = source.kind.as_union() {
                // TODO: as intersection
                for t in union.tys {
                    self.infer_from_tys(t, t);
                }
                return;
            }
        }

        if let Some(target_union) = target.kind.as_union() {
            if let Some(source_union) = source.kind.as_union() {
                // source_union.tys
            } else {
                // [source]
            }
        };

        if target.kind.is_type_variable() {
            if let Some(idx) = self
                .checker
                .get_inference_info_for_ty(target, self.inference)
            {
                let info = self.checker.inference_info(self.inference, idx);
                if !info.is_fixed {
                    let candidate = self.propagation_ty.unwrap_or(source);
                    if info.priority.is_none() || self.priority < info.priority.unwrap() {
                        self.reset_info_candidate(idx);
                        self.reset_info_contra_candidate(idx);
                        self.set_info_priority(idx, self.priority);
                        self.set_info_toplevel(idx);
                    }
                    let info = self.checker.inference_info(self.inference, idx);
                    if Some(self.priority) == info.priority {
                        if self.contravariant && !self.bivariant {
                            if info
                                .contra_candidates
                                .as_ref()
                                .map_or(true, |cs| !cs.contains(&candidate))
                            {
                                self.append_candidate(idx, candidate);
                                self.clear_cached_inferences();
                            }
                        } else if info
                            .candidates
                            .as_ref()
                            .map_or(true, |cs| !cs.contains(&candidate))
                        {
                            self.append_candidate(idx, candidate);
                            self.clear_cached_inferences();
                        }
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

        if let Some(target_cond) = target.kind.as_cond_ty() {
        } else if source.kind.is_object() {
            self.invoke_once(source, target, |this, source, target| {
                this.infer_from_object_tys(source, target);
            });
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
        if !self.checker.tys_definitely_unrelated(source, target) {
            if source.kind.is_tuple() || source.kind.is_array(self.checker) {
                target.kind.is_tuple();
                if target.kind.is_array(self.checker) {
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
        let target_index_infos = self.checker.get_index_infos_of_ty(target);
        for target_index_info in target_index_infos {
            if let Some(source_info) = self
                .checker
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
        matches: impl Fn(&'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> bool,
        priority: InferencePriority,
        inference: InferenceContextId,
    ) -> (ty::Tys<'cx>, ty::Tys<'cx>) {
        let mut matched_sources = Vec::with_capacity(sources.len());
        let mut matched_targets = Vec::with_capacity(targets.len());
        for t in targets {
            for s in sources {
                if matches(s, t) {
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
                self.checker.alloc(tys)
            }
        };

        (
            matched(&matched_sources, sources),
            matched(&matched_targets, targets),
        )
    }

    fn infer_from_sig(&mut self, source: &'cx ty::Sig<'cx>, target: &'cx ty::Sig<'cx>) {
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
        let source_sigs = self.checker.get_sigs_of_ty(source, kind);
        let source_len = source_sigs.len();
        if source_len > 0 {
            let target_sigs = self.checker.get_sigs_of_ty(target, kind);
            let target_len = target_sigs.len();
            for i in 0..target_len {
                let source_index = if source_len + i > target_len {
                    source_len + i - target_len
                } else {
                    0
                };
                let source = self.checker.get_base_sig(source_sigs[source_index]);
                let target = self.checker.get_erased_sig(target_sigs[i]);
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
        let target_ret_ty = self.checker.get_ret_ty_of_sig(target);
        if self.checker.could_contain_ty_var(target_ret_ty) {
            let source_ret_ty = self.checker.get_ret_ty_of_sig(source);
            f(self, source_ret_ty, target_ret_ty);
        }
    }

    fn infer_from_props(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        for target_prop in self.checker.get_props_of_ty(target) {
            if let Some(source_prop) = self
                .checker
                .get_prop_of_ty(source, self.checker.binder.symbol(*target_prop).name)
            {
                // TODO:
            }
        }
    }
}

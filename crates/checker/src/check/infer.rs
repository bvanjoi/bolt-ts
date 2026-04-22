use super::check_expr::IterationUse;
use super::check_type_related_to::RecursionFlags;
use super::create_ty::IntersectionFlags;
use super::get_contextual::ContextFlags;

use super::ty::{self, SigFlags, SigKind, TyID, TypeFlags};
use super::ty::{ObjectFlags, Sig};
use super::utils::append_if_unique;
use super::{CheckMode, InferenceContextId, TyChecker, fn_mapper};

use bolt_ts_ast::r#trait;
use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_binder::SymbolFlags;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use thin_vec::{ThinVec, thin_vec};

#[derive(Debug, Clone)]
pub struct InferenceInfo<'cx> {
    pub ty_param: &'cx ty::Ty<'cx>,
    pub candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub contra_candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub is_fixed: bool,
    pub top_level: bool,
    pub priority: Option<InferencePriority>,
    pub inferred_ty: Option<&'cx ty::Ty<'cx>>,
    pub implied_arity: Option<usize>,
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
            implied_arity: None,
        }
    }

    pub(super) fn has_inference_candidates(&self) -> bool {
        self.candidates.is_some() || self.contra_candidates.is_some()
    }

    pub(super) fn has_inference_candidates_or_default(&self, checker: &TyChecker<'cx>) -> bool {
        self.candidates.is_some()
            || self.contra_candidates.is_some()
            || checker.has_ty_param_default(self.ty_param.kind.expect_param())
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
    pub outer_ret_mapper: Option<&'cx dyn ty::TyMap<'cx>>,
}

impl<'cx> InferenceContext<'cx> {
    fn create(
        checker: &TyChecker<'cx>,
        id: InferenceContextId,
        inferences: ThinVec<InferenceInfo<'cx>>,
        sig: Option<&'cx ty::Sig<'cx>>,
        flags: InferenceFlags,
    ) -> Self {
        let mapper = checker.making_inference_fixing_mapper(id, &inferences);
        let non_fixing_mapper = checker.making_inference_non_fixing_mapper(id, &inferences);
        Self {
            inferences,
            sig,
            flags,
            ret_mapper: None,
            outer_ret_mapper: None,
            mapper,
            non_fixing_mapper,
        }
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn create_inference_context(
        &mut self,
        ty_params: &[&'cx ty::Ty<'cx>],
        sig: Option<&'cx ty::Sig<'cx>>,
        flags: InferenceFlags,
    ) -> InferenceContextId {
        let id = InferenceContextId(self.inferences.len() as u32);
        let inferences = ty_params
            .iter()
            .map(|ty_param| InferenceInfo::create(ty_param))
            .collect::<thin_vec::ThinVec<_>>();
        let inference = InferenceContext::create(self, id, inferences, sig, flags);
        self.inferences.push(inference);
        id
    }

    pub(super) fn clone_inference_context(
        &mut self,
        context_id: InferenceContextId,
        extra_flags: InferenceFlags,
    ) -> InferenceContextId {
        let inference_context = self.inference(context_id);
        let inferences = inference_context.inferences.clone();
        let id = InferenceContextId(self.inferences.len() as u32);
        let inference = InferenceContext::create(
            self,
            id,
            inferences,
            inference_context.sig,
            inference_context.flags | extra_flags,
        );
        self.inferences.push(inference);
        id
    }

    pub(super) fn get_mapper_from_context(
        &self,
        inference: InferenceContextId,
    ) -> Option<&'cx dyn ty::TyMap<'cx>> {
        Some(self.inference(inference).mapper as &'cx dyn ty::TyMap<'cx>)
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

    fn set_inference_outer_ret_mapper(
        &mut self,
        inference: InferenceContextId,
        mapper: &'cx dyn ty::TyMap<'cx>,
    ) {
        self.inferences[inference.as_usize()].outer_ret_mapper = Some(mapper);
    }

    pub(super) fn inference(&self, id: InferenceContextId) -> &InferenceContext<'cx> {
        let id = id.as_usize();
        debug_assert!(id < self.inferences.len());
        unsafe { self.inferences.get_unchecked(id) }
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

    pub(crate) fn override_inferred_ty_of_inference_info(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
        ty: &'cx ty::Ty<'cx>,
    ) {
        let cache = &mut self.inferences[inference.as_usize()].inferences[idx].inferred_ty;
        assert!(cache.is_some());
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
        // TODO: get_ty_predicate_of_sig
        let ret_ty = self.get_ret_ty_of_sig(sig);
        self.is_ty_param_at_top_level(ret_ty, ty_param, 0)
    }

    fn is_ty_param_at_top_level(
        &self,
        ty: &'cx ty::Ty<'cx>,
        ty_param: &'cx ty::Ty<'cx>,
        depth: u8,
    ) -> bool {
        if ty == ty_param {
            true
        } else if let Some(tys) = ty.kind.tys_of_union_or_intersection() {
            tys.iter()
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
                let lits = self.get_union_ty::<false>(
                    &object_literals,
                    ty::UnionReduction::Subtype,
                    None,
                    None,
                    None,
                );
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
                    let t = if let Some(cond_ty) = constraint.kind.as_cond_ty() {
                        self.get_default_constraint_of_cond_ty(cond_ty)
                    } else {
                        constraint
                    };
                    t.maybe_type_of_kind(
                        TypeFlags::PRIMITIVE
                            .union(TypeFlags::INDEX)
                            .union(TypeFlags::TEMPLATE_LITERAL)
                            .union(TypeFlags::STRING_MAPPING),
                    )
                })
        };

        let primitive_constraint = has_primitive_constraint;

        // for case:
        // `function foo<T>(a: T, b: T): void`
        // the call `foo('a', 'b')` should widen `'a'` and `'b'` into
        // string type rather than string literal type.
        let widen_literal_tys = |this: &mut Self| {
            let i = this.inference_info(inference, idx);
            !primitive_constraint
                && i.top_level
                && (i.is_fixed || !this.is_ty_param_at_top_level_in_ret_top(sig, i.ty_param))
        };

        let base_candidates = if primitive_constraint {
            candidates
        } else if widen_literal_tys(self) {
            candidates
                .iter()
                .map(|c| self.get_widened_literal_ty(c))
                .collect()
        } else {
            candidates
        };

        let i = self.inference_info(inference, idx);

        let base_candidates = self.alloc(base_candidates);
        let unwidened_ty = if i
            .priority
            .is_some_and(|p| p.intersects(InferencePriority::PRIORITY_IMPLIES_COMBINATION))
        {
            self.get_union_ty::<false>(
                base_candidates,
                ty::UnionReduction::Subtype,
                None,
                None,
                None,
            )
        } else {
            self.get_common_super_ty(base_candidates)
        };

        self.get_widened_ty(unwidened_ty)
    }

    fn get_contravariant_inference(
        &mut self,
        inference: InferenceContextId,
        idx: usize,
    ) -> &'cx ty::Ty<'cx> {
        let info = self.inference_info(inference, idx);
        // TODO: remove clone
        let cs = info.contra_candidates.as_ref().unwrap().clone();
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
        need_apparent_ty: bool,
    ) -> &'cx ty::Ty<'cx> {
        if ty.get_object_flags().contains(ObjectFlags::REFERENCE) {
            let ty_args = self.get_ty_arguments(ty);
            let i = if let Some(t) = ty.as_tuple() {
                t.ty.kind.expect_object_interface()
            } else {
                ty.kind
                    .expect_object_reference()
                    .interface_target()
                    .unwrap()
                    .kind
                    .expect_object_interface()
            };
            if i.ty_params
                .map(|ty_params| ty_params.len())
                .unwrap_or_default()
                == ty_args.len()
            {
                let mut ty_args = ty_args.to_vec();
                ty_args.push(this_arg.unwrap_or(i.this_ty.unwrap()));
                let ty_args = self.alloc(ty_args);

                // TODO: remove this into `create_reference_ty`
                let target = if let Some(refer) = ty.kind.as_object_reference() {
                    if refer.target.kind.is_object_interface() {
                        ty
                    } else {
                        refer.target
                    }
                } else if ty.kind.is_object_tuple() {
                    ty
                } else {
                    unreachable!("{:#?}", ty)
                };
                self.create_type_reference(target, Some(ty_args), ObjectFlags::empty())
            } else {
                ty
            }
        } else if let Some(i) = ty.kind.as_intersection() {
            let tys = self
                .same_map_tys(Some(i.tys), |this, ty, _| {
                    this.get_ty_with_this_arg(ty, this_arg, need_apparent_ty)
                })
                .unwrap();
            if !std::ptr::eq(tys, i.tys) {
                self.get_intersection_ty(tys, IntersectionFlags::None, None, None)
            } else {
                ty
            }
        } else if need_apparent_ty {
            self.get_apparent_ty(ty)
        } else {
            ty
        }
    }

    fn get_default_ty_argument_ty(&self, is_in_javascript_file: bool) -> &'cx ty::Ty<'cx> {
        if is_in_javascript_file {
            self.any_ty
        } else {
            self.unknown_ty
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
                        inferred_contravariant_ty.is_none_or(|inferred_contravariant_ty| {
                            !inferred_contravariant_ty
                                .flags
                                .intersects(TypeFlags::NEVER.union(TypeFlags::ANY))
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
                                            .is_none_or(|cs| {
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
                    let forward_inferences = &self.inference(inference).inferences[idx..];
                    let sources = forward_inferences
                        .iter()
                        .map(|i| i.ty_param)
                        .collect::<Vec<_>>();
                    let sources = self.alloc(sources);
                    let targets = forward_inferences
                        .iter()
                        .map(|_| self.unknown_ty)
                        .collect::<Vec<_>>();
                    let targets = self.alloc(targets);
                    let mapper = self.create_ty_mapper(sources, targets);
                    let mapper = self.merge_ty_mappers(
                        Some(mapper),
                        self.inference(inference).non_fixing_mapper,
                    );
                    inferred_ty = Some(self.instantiate_ty_worker(default_ty, mapper));
                }
            }
        } else {
            inferred_ty = self.get_ty_from_inference(inference, idx);
        };

        let is_in_javascript_file = self
            .inference(inference)
            .flags
            .contains(InferenceFlags::ANY_DEFAULT);
        self.set_inferred_ty_of_inference_info(
            inference,
            idx,
            inferred_ty.unwrap_or(self.get_default_ty_argument_ty(is_in_javascript_file)),
        );

        let i = self.inference_info(inference, idx);
        if let Some(constraint) = self.get_constraint_of_ty_param(i.ty_param) {
            let mapper = self.inference(inference).non_fixing_mapper;
            let instantiated_constraint = self.instantiate_ty_worker(constraint, mapper);
            if let Some(ty) = inferred_ty {
                let constraint_with_this =
                    self.get_ty_with_this_arg(instantiated_constraint, Some(ty), false);
                // TODO: `ctx.compare_types`
                if !self.is_type_related_to(
                    ty,
                    constraint_with_this,
                    super::relation::RelationKind::Assignable,
                ) {
                    let filtered_by_constraint = if self
                        .inference_info(inference, idx)
                        .priority
                        .is_some_and(|p| p == InferencePriority::RETURN_TYPE)
                    {
                        // TODO: filter_ty
                        ty
                    } else {
                        self.never_ty
                    };
                    if filtered_by_constraint.flags.contains(TypeFlags::NEVER) {
                        inferred_ty = None;
                    }
                }
            }
            if inferred_ty.is_none() {
                inferred_ty = Some(
                    if let Some(fallback_ty) = fallback_ty
                    && let target =
                        self.get_ty_with_this_arg(instantiated_constraint, Some(fallback_ty), false)
                        // TODO: `ctx.compare_types`
                    && self.is_type_related_to(
                        fallback_ty,
                        target,
                        super::relation::RelationKind::Assignable,
                    ) {
                        fallback_ty
                    } else {
                        instantiated_constraint
                    },
                );
            }
            self.override_inferred_ty_of_inference_info(inference, idx, inferred_ty.unwrap());
        }

        // clear_active_mapper_cache
        for mapper_cache in &mut self.activity_ty_mapper_caches {
            mapper_cache.clear();
        }

        self.inference_info(inference, idx).inferred_ty.unwrap()
    }

    pub(super) fn get_inference_sig(
        &self,
        inference: InferenceContextId,
    ) -> Option<&'cx ty::Sig<'cx>> {
        self.inference(inference).sig
    }

    fn tuple_tys_definitely_unrelated(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        let s = source.as_tuple().unwrap();
        let t = target.as_tuple().unwrap();
        !t.combined_flags.intersects(ty::ElementFlags::VARIADIC) && t.min_length > s.min_length
            || !t.combined_flags.intersects(ty::ElementFlags::VARIABLE)
                && (s.combined_flags.intersects(ty::ElementFlags::VARIABLE)
                    || t.fixed_length < s.fixed_length)
    }

    fn tys_definitely_unrelated(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        if source.is_tuple() && target.is_tuple() {
            self.tuple_tys_definitely_unrelated(source, target)
        } else {
            self.get_unmatched_prop(source, target, false, true)
                .is_some()
                && self
                    .get_unmatched_prop(target, source, false, false)
                    .is_some()
        }
    }

    pub(super) fn infer_ty_arguments(
        &mut self,
        node: &impl r#trait::CallLike<'cx>,
        sig: &'cx Sig<'cx>,
        args: &'cx [&'cx ast::Expr<'cx>],
        check_mode: CheckMode,
        inference: InferenceContextId,
    ) -> ty::Tys<'cx> {
        let Some(sig_ty_params) = self.get_sig_links(sig.id).get_ty_params() else {
            unreachable!()
        };

        let node_id = node.id();
        if !self.p.node(node_id).is_bin_expr()
            && let skip_binding_patterns = sig_ty_params
                .iter()
                .all(|tp| self.get_default_ty_from_ty_param(tp).is_some())
            && let Some(contextual_ty) = self.get_contextual_ty(
                node_id,
                Some(if skip_binding_patterns {
                    ContextFlags::SKIP_BINDING_PATTERNS
                } else {
                    ContextFlags::empty()
                }),
            )
        {
            let inference_target_ty = self.get_ret_ty_of_sig(sig);
            if self.could_contain_ty_var(inference_target_ty) {
                let outer_context = self.get_inference_context(node_id);
                let is_from_binding_pattern = !skip_binding_patterns
                    && self.get_contextual_ty(node_id, Some(ContextFlags::SKIP_BINDING_PATTERNS))
                        != Some(contextual_ty);
                if !is_from_binding_pattern {
                    let outer_mapper = outer_context
                        .and_then(|ctx| ctx.inference)
                        .map(|outer_context| {
                            self.clone_inference_context(outer_context, InferenceFlags::NO_DEFAULT)
                        })
                        .and_then(|id| self.get_mapper_from_context(id));
                    let instantiated_ty = self.instantiate_ty(contextual_ty, outer_mapper);
                    let inference_source_ty =
                        if let Some(contextual_sig) = self.get_single_call_sig(instantiated_ty) {
                            if let Some(ty_param) =
                                self.get_sig_links(contextual_sig.id).get_ty_params()
                            {
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
                    .and_then(|outer_context| outer_context.inference)
                    .and_then(|outer_context| {
                        // create_outer_return_mapper
                        if let Some(outer_ret_mapper) =
                            self.inference(outer_context).outer_ret_mapper
                        {
                            Some(outer_ret_mapper)
                        } else {
                            let id = self
                                .clone_inference_context(outer_context, InferenceFlags::empty());
                            let mapper = self.inference(id).mapper;
                            let ret = self
                                .merge_ty_mappers(self.inference(outer_context).ret_mapper, mapper);
                            self.set_inference_outer_ret_mapper(outer_context, ret);
                            Some(ret)
                        }
                    });
                let ret_source_ty = self.instantiate_ty(contextual_ty, ret_mapper);
                self.infer_tys(ret_ctx, ret_source_ty, inference_target_ty, None, false);
                let ret_inference = self.inference(ret_ctx);
                // clone_inferred_part_of_context
                let ret_inferences = ret_inference
                    .inferences
                    .iter()
                    .filter(|i| i.has_inference_candidates())
                    .cloned()
                    .collect::<thin_vec::ThinVec<_>>();
                if ret_inferences.is_empty() {
                    debug_assert!(self.inferences[inference.as_usize()].ret_mapper.is_none());
                } else {
                    let id = InferenceContextId(self.inferences.len() as u32);
                    let mapper = self.making_inference_fixing_mapper(id, &ret_inferences);
                    let non_fixing_mapper =
                        self.making_inference_non_fixing_mapper(id, &ret_inferences);
                    let inference_context = InferenceContext {
                        inferences: ret_inferences,
                        sig: ret_inference.sig,
                        flags: ret_inference.flags,
                        ret_mapper: None,
                        outer_ret_mapper: None,
                        mapper,
                        non_fixing_mapper,
                    };
                    self.set_inference_ret_mapper(
                        inference,
                        Some(inference_context.mapper as &'cx dyn ty::TyMap<'cx>),
                    );
                    self.inferences.push(inference_context);
                };
            }
        }

        let arg_len = args.len();
        let rest_ty = sig.get_non_array_rest_ty(self);
        let arg_count = match rest_ty {
            Some(_) => usize::min(sig.get_param_count(self) - 1, arg_len),
            None => arg_len,
        };
        if let Some(rest_ty) = rest_ty
            && rest_ty.flags.contains(TypeFlags::TYPE_PARAMETER)
        {
            let info = self.inferences[inference.as_usize()]
                .inferences
                .iter_mut()
                .find(|i| i.ty_param == rest_ty);
            if let Some(info) = info {
                info.implied_arity = args
                    .iter()
                    .skip(arg_count)
                    .position(|arg| matches!(arg.kind, ast::ExprKind::SpreadElement(_)))
                    .map_or(Some(arg_len - arg_count), |_| None)
            }
        }

        if let Some(this_ty) = self.get_this_ty_of_sig(sig)
            && self.could_contain_ty_var(this_ty)
        {
            let this_argument_node = self.get_this_argument_of_call(node);
            let ty = self.get_this_argument_ty(this_argument_node);
            self.infer_tys(inference, ty, this_ty, None, false);
        }

        for (idx, arg) in args.iter().enumerate() {
            if !matches!(arg.kind, ast::ExprKind::Omit(_)) {
                let param_ty = self.get_ty_at_pos(sig, idx);
                if self.could_contain_ty_var(param_ty) {
                    let arg_ty = self.check_expr_with_contextual_ty(
                        arg,
                        param_ty,
                        Some(inference),
                        check_mode,
                    );
                    self.infer_tys(inference, arg_ty, param_ty, None, false);
                }
            }
        }

        if let Some(rest_ty) = rest_ty
            && self.could_contain_ty_var(rest_ty)
        {
            let spared_ty = self.get_spared_argument_ty(
                args,
                arg_count,
                arg_len,
                rest_ty,
                Some(inference),
                check_mode,
            );
            self.infer_tys(inference, spared_ty, rest_ty, None, false);
        }

        self.get_inferred_tys(inference)
    }

    fn is_mutable_array_like_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.is_mutable_array_or_tuple_ty(ty)
            || ty
                .flags
                .intersects(TypeFlags::ANY.union(TypeFlags::NULLABLE))
                && self.is_type_assignable_to(ty, self.any_array_ty())
    }

    fn is_mutable_array_or_tuple_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.is_array(self) && !ty.is_readonly_array(self)
            || ty.as_tuple().is_some_and(|t| !t.readonly)
    }

    fn get_mutable_array_or_tuple_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if let ty::TyKind::Union(u) = ty.kind {
            self.map_union_ty(
                ty,
                u,
                |this, t| Some(this.get_mutable_array_or_tuple_ty(t)),
                false,
            )
            .unwrap()
        } else if ty.flags.contains(TypeFlags::ANY) || {
            let t = self.get_base_constraint_of_ty(ty).unwrap_or(ty);
            self.is_mutable_array_or_tuple_ty(t)
        } {
            ty
        } else if let Some(t) = ty.as_tuple() {
            let element_types = self.get_element_tys(ty);
            self.create_tuple_ty(element_types, Some(t.element_flags), t.readonly)
        } else {
            let element_types = self.alloc([ty]);
            let element_flags = self.alloc([ty::ElementFlags::VARIADIC]);
            self.create_tuple_ty(element_types, Some(element_flags), false)
        }
    }

    pub(super) fn get_spared_argument_ty(
        &mut self,
        args: &[&'cx ast::Expr<'cx>],
        index: usize,
        arg_count: usize,
        rest_ty: &'cx ty::Ty<'cx>,
        context: Option<InferenceContextId>,
        check_mode: CheckMode,
    ) -> &'cx ty::Ty<'cx> {
        let is_const_context = self.is_const_ty_variable(rest_ty);
        if arg_count > 0
            && index >= arg_count - 1
            && let Some(arg) = args.get(arg_count - 1)
            && let ast::ExprKind::SpreadElement(spread) = arg.kind
        {
            // TODO: synthetic node
            let spread_ty =
                self.check_expr_with_contextual_ty(spread.expr, rest_ty, context, check_mode);
            return if self.is_array_like_ty(spread_ty) {
                self.get_mutable_array_or_tuple_ty(spread_ty)
            } else {
                self.check_iterated_ty_or_element_ty(
                    IterationUse::SPREAD,
                    spread_ty,
                    self.undefined_ty,
                    Some(spread.expr.id()),
                )
            };
        }

        let mut tys = vec![];
        let mut flags = vec![];
        // let mut names = vec![];
        for i in index..arg_count {
            let arg = args[i];
            if let ast::ExprKind::SpreadElement(spread) = arg.kind {
                // TODO: synthetic node
                let spared_ty = self.check_expr(spread.expr);
                if self.is_array_like_ty(spared_ty) {
                    tys.push(spared_ty);
                    flags.push(ty::ElementFlags::VARIADIC);
                } else {
                    let iterated_ty = self.check_iterated_ty_or_element_ty(
                        IterationUse::SPREAD,
                        spared_ty,
                        self.undefined_ty,
                        Some(spread.expr.id()),
                    );
                    tys.push(iterated_ty);
                    flags.push(ty::ElementFlags::REST);
                }
            } else {
                let element_index = i - index;
                let contextual_ty = if rest_ty.is_tuple() {
                    self.get_contextual_ty_for_element_expr(
                        Some(rest_ty),
                        element_index,
                        Some(arg_count - index),
                        None,
                        None,
                    )
                    .unwrap_or(self.unknown_ty)
                } else {
                    let index_ty =
                        self.get_number_literal_type::<false>(element_index.into(), None);
                    self.get_indexed_access_ty(
                        rest_ty,
                        index_ty,
                        Some(ty::AccessFlags::Contextual),
                        None,
                        None,
                        None,
                    )
                };
                let arg_ty =
                    self.check_expr_with_contextual_ty(arg, contextual_ty, context, check_mode);
                let has_primitive_contextual_ty = is_const_context
                    || contextual_ty.maybe_type_of_kind(
                        TypeFlags::PRIMITIVE
                            .union(TypeFlags::INDEX)
                            .union(TypeFlags::TEMPLATE_LITERAL)
                            .union(TypeFlags::STRING_MAPPING),
                    );
                tys.push(if has_primitive_contextual_ty {
                    self.get_regular_ty_of_literal_ty(arg_ty)
                } else {
                    self.get_widened_literal_ty(arg_ty)
                });
                flags.push(ty::ElementFlags::REQUIRED);
            }
            // TODO: synthetic node
            // names.push(None);
        }

        let element_types = self.alloc(tys);
        let element_flags = self.alloc(flags);
        let readonly = is_const_context
            && !self.some_type(rest_ty, |this, t| this.is_mutable_array_like_ty(t));
        self.create_tuple_ty(element_types, Some(element_flags), readonly)
    }

    pub(super) fn get_this_argument_of_call(
        &mut self,
        node: &impl r#trait::CallLike<'cx>,
    ) -> Option<&'cx ast::Expr<'cx>> {
        // TODO: binary
        let n = self.p.node(node.id());
        let expr = match n {
            ast::Node::CallExpr(n) => n.expr,
            ast::Node::TaggedTemplateExpr(n) => n.tag,
            ast::Node::NewExpr(_) => return None,
            _ => unreachable!(),
        };
        const FLAGS: u8 = ast::SKIP_OUTER_EXPRESSION_ALL_FLAGS;
        let callee = ast::Expr::skip_outer_expr::<FLAGS>(expr);
        match callee.kind {
            ast::ExprKind::PropAccess(n) => Some(n.expr),
            ast::ExprKind::EleAccess(n) => Some(n.expr),
            _ => None,
        }
    }

    pub(super) fn infer_from_annotated_params_and_return(
        &mut self,
        sig: &'cx Sig<'cx>,
        contextual_sig: &'cx Sig<'cx>,
        inference: InferenceContextId,
    ) {
        let len = sig.params.len() - (if sig.has_rest_param() { 1 } else { 0 });
        for i in 0..len {
            let decl = sig.params[i].decl(&self.binder);
            if let Some(ty_node) = self.p.node(decl).as_param_decl().and_then(|decl| decl.ty) {
                let source = self.get_ty_from_type_node(ty_node);
                let target = self.get_ty_at_pos(contextual_sig, i);
                self.infer_tys(inference, source, target, None, false);
            }
        }
        if let Some(ty_node) = self.get_effective_ret_type_node(sig.def_id()) {
            let source = self.get_ty_from_type_node(ty_node);
            let target = self.get_ret_ty_of_sig(contextual_sig);
            self.infer_tys(inference, source, target, None, false);
        }
    }

    pub(super) fn infer_state<'checker>(
        &'checker mut self,
        inference: InferenceContextId,
        priority: Option<InferencePriority>,
        contravariant: bool,
        original_target: &'cx ty::Ty<'cx>,
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
            expanding_flags: RecursionFlags::empty(),
            source_stack: Vec::with_capacity(4),
            target_stack: Vec::with_capacity(4),
            visited: fx_hashmap_with_capacity(4),
            original_target,
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
        let mut state = self.infer_state(inference, priority, contravariant, original_target);
        state.infer_from_tys(original_source, original_target);
    }

    pub(super) fn apply_to_ret_ty(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        f: impl FnOnce(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        if let Some(target_ty_pred) = self.get_ty_predicate_of_sig(target)
            && let Some(source_ty_pred) = self.get_ty_predicate_of_sig(source)
        {
            todo!("type_predicate_kind_match");
        }
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

    fn infer_from_lit_parts_to_template_lit(
        &mut self,
        source_texts: &[bolt_ts_atom::Atom],
        source_tys: &[&'cx ty::Ty<'cx>],
        t: &'cx ty::TemplateLitTy<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        let last_source_index = source_texts.len() - 1;
        let source_start_text = source_texts[0];
        let source_start_text_str = self.atoms.get(source_start_text);
        let source_end_text = source_texts[last_source_index];
        let source_end_text_str = self.atoms.get(source_end_text);

        let target_texts = t.texts;
        let last_target_index = target_texts.len() - 1;
        let target_start_text = target_texts[0];
        let target_start_text_str = self.atoms.get(target_start_text);
        let target_end_text = target_texts[last_target_index];
        let target_end_text_str = self.atoms.get(target_end_text);
        let target_end_text_len = target_end_text_str.len();

        if (last_source_index == 0
            && source_start_text_str.len() < target_start_text_str.len() + target_end_text_len)
            || !source_start_text_str.starts_with(target_start_text_str)
            || !source_end_text_str.ends_with(target_end_text_str)
        {
            return None;
        }

        let mut matches = Vec::with_capacity(last_target_index);
        let mut seg: usize = 0;
        let mut pos: usize = target_start_text_str.len();

        fn get_source_text(
            atoms: &mut bolt_ts_atom::AtomIntern,
            index: usize,
            last_source_index: usize,
            source_end_text: bolt_ts_atom::Atom,
            target_end_text_len: usize,
            source_texts: &[bolt_ts_atom::Atom],
        ) -> &'static str {
            if index < last_source_index {
                atoms.get(source_texts[index])
            } else {
                let source_end_text_str = atoms.get(source_end_text);
                let len = source_end_text_str.len();
                &source_end_text_str[..len - target_end_text_len]
            }
        }

        let add_match = |this: &mut Self,
                         s: usize,
                         p: usize,
                         seg: &mut usize,
                         pos: &mut usize,
                         matches: &mut Vec<&'cx ty::Ty<'cx>>| {
            if s == *seg {
                let sub = &get_source_text(
                    &mut this.atoms,
                    s,
                    last_source_index,
                    source_end_text,
                    target_end_text_len,
                    source_texts,
                )[*pos..p];
                let atom = this.atoms.atom(sub);
                let match_type = this.get_string_literal_type_from_string(atom);
                matches.push(match_type);
            } else {
                let mut parts = Vec::with_capacity(source_texts.len());
                let a = &this.atoms.get(source_texts[*seg])[*pos..];
                let a_atom = this.atoms.atom(a);
                parts.push(a_atom);
                parts.extend(source_texts[*seg + 1..s].iter());
                let b = &get_source_text(
                    &mut this.atoms,
                    s,
                    last_source_index,
                    source_end_text,
                    target_end_text_len,
                    source_texts,
                )[0..p];
                let b_atom = this.atoms.atom(b);
                parts.push(b_atom);
                let match_type = this.get_template_lit_ty(&parts, &source_tys[*seg..s]);
                matches.push(match_type);
            }
            *seg = s;
            *pos = p;
        };

        for atom in target_texts.iter().take(last_target_index).skip(1) {
            let delim = self.atoms.get(*atom);
            if !delim.is_empty() {
                let mut s = seg;
                let mut p = pos;
                loop {
                    let src_text = get_source_text(
                        &mut self.atoms,
                        s,
                        last_source_index,
                        source_end_text,
                        target_end_text_len,
                        source_texts,
                    );
                    if let Some(found) = src_text[p..].find(delim) {
                        p += found;
                        break;
                    }
                    s += 1;
                    if s == source_texts.len() {
                        return None;
                    }
                    p = 0;
                }
                let delim_len = delim.len();
                add_match(self, s, p, &mut seg, &mut pos, &mut matches);
                pos += delim_len;
            } else if pos
                < get_source_text(
                    &mut self.atoms,
                    seg,
                    last_source_index,
                    source_end_text,
                    target_end_text_len,
                    source_texts,
                )
                .len()
            {
                add_match(self, seg, pos + 1, &mut seg, &mut pos, &mut matches);
            } else if seg < last_source_index {
                add_match(self, seg + 1, 0, &mut seg, &mut pos, &mut matches);
            } else {
                return None;
            }
        }
        let p = get_source_text(
            &mut self.atoms,
            last_source_index,
            last_source_index,
            source_end_text,
            target_end_text_len,
            source_texts,
        )
        .len();
        add_match(self, last_source_index, p, &mut seg, &mut pos, &mut matches);
        Some(self.alloc(matches))
    }

    fn infer_tys_from_template_lit_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target_template_lit_ty: &'cx ty::TemplateLitTy<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        match source.kind {
            ty::TyKind::StringLit(s) => self.infer_from_lit_parts_to_template_lit(
                &[s.val],
                self.empty_array(),
                target_template_lit_ty,
            ),
            ty::TyKind::TemplateLit(s) => {
                if self.array_is_equal(Some(s.texts), Some(target_template_lit_ty.texts)) {
                    let tys = s
                        .tys
                        .iter()
                        .enumerate()
                        .map(|(i, s)| {
                            let s_base = self.get_base_constraint_or_ty(s);
                            let t_base =
                                self.get_base_constraint_or_ty(target_template_lit_ty.tys[i]);
                            if self.is_type_assignable_to(s_base, t_base) {
                                s
                            } else {
                                self.get_string_like_ty_for_ty(s)
                            }
                        })
                        .collect::<Vec<_>>();
                    Some(self.alloc(tys))
                } else {
                    self.infer_from_lit_parts_to_template_lit(
                        s.texts,
                        s.tys,
                        target_template_lit_ty,
                    )
                }
            }
            _ => None,
        }
    }

    pub(super) fn is_ty_matched_by_template_lit_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::TemplateLitTy<'cx>,
    ) -> bool {
        let Some(inferences) = self.infer_tys_from_template_lit_ty(source, target) else {
            return false;
        };
        inferences
            .iter()
            .enumerate()
            .all(|(i, r)| self.is_valid_ty_for_template_lit_placeholder(r, target.tys[i]))
    }
}

pub(super) struct InferenceState<'cx, 'checker> {
    priority: InferencePriority,
    inference_priority: InferencePriority,
    expanding_flags: RecursionFlags,
    source_stack: Vec<&'cx ty::Ty<'cx>>,
    target_stack: Vec<&'cx ty::Ty<'cx>>,
    visited: FxHashMap<(TyID, TyID), InferencePriority>,
    c: &'checker mut TyChecker<'cx>,
    inference: InferenceContextId,
    contravariant: bool,
    bivariant: bool,
    propagation_ty: Option<&'cx ty::Ty<'cx>>,
    original_target: &'cx ty::Ty<'cx>,
}

impl<'cx> InferenceState<'cx, '_> {
    fn get_inference_info_for_ty(&self, ty: &'cx ty::Ty<'cx>) -> Option<usize> {
        if ty.kind.is_type_variable() {
            self.c.inferences[self.inference.as_usize()]
                .inferences
                .iter()
                .position(|i| i.ty_param == ty)
        } else {
            None
        }
    }
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
            let source_t = self.c.get_true_ty_from_cond_ty(source_cond);
            let target_t = self.c.get_true_ty_from_cond_ty(target_cond);
            self.infer_from_tys(source_t, target_t);
            let source_f = self.c.get_false_ty_from_cond_ty(source_cond);
            let target_f = self.c.get_false_ty_from_cond_ty(target_cond);
            self.infer_from_tys(source_f, target_f);
        } else {
            let t = self.c.get_true_ty_from_cond_ty(target_cond);
            let f = self.c.get_false_ty_from_cond_ty(target_cond);
            let target_tys = self.c.alloc([t, f]);
            self.infer_to_multiple_tys_with_priority(
                source,
                target_tys,
                target.flags,
                if self.contravariant {
                    InferencePriority::CONTRAVARIANT_CONDITIONAL
                } else {
                    InferencePriority::empty()
                },
            );
        }
    }

    fn infer_to_multiple_tys_with_priority(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        targets: ty::Tys<'cx>,
        target_flags: TypeFlags,
        new_priority: InferencePriority,
    ) {
        let save_priority = self.priority;
        self.priority |= new_priority;
        self.infer_to_multiple_tys(source, targets, target_flags);
        self.priority = save_priority;
    }

    fn infer_from_ty_arguments(
        &mut self,
        source_tys: ty::Tys<'cx>,
        target_tys: ty::Tys<'cx>,
        variances: &[super::VarianceFlags],
    ) {
        let count = if source_tys.len() < target_tys.len() {
            source_tys.len()
        } else {
            target_tys.len()
        };
        for i in 0..count {
            if i < variances.len()
                && (variances[i] & super::VarianceFlags::VARIANCE_MASK
                    == super::VarianceFlags::CONTRAVARIANT)
            {
                self.infer_from_contravariant_tys(source_tys[i], target_tys[i]);
            } else {
                self.infer_from_tys(source_tys[i], target_tys[i]);
            }
        }
    }

    pub(super) fn infer_from_tys(
        &mut self,
        mut source: &'cx ty::Ty<'cx>,
        mut target: &'cx ty::Ty<'cx>,
    ) {
        if !self.c.could_contain_ty_var(target) || target.is_no_infer_ty() {
            return;
        }

        if source == self.c.wildcard_ty || source == self.c.blocked_string_ty {
            let saved_propagation_ty = self.propagation_ty;
            self.propagation_ty = Some(source);
            self.infer_from_tys(target, target);
            self.propagation_ty = saved_propagation_ty;
            return;
        }

        if let Some(alias_symbol) = source.alias_symbol()
            && let Some(target_alias_symbol) = target.alias_symbol()
            && alias_symbol == target_alias_symbol
        {
            if let Some(source_alias_ty_arguments) = source.alias_ty_arguments() {
                let target_alias_ty_arguments = target.alias_ty_arguments().unwrap();
                let params = self
                    .c
                    .get_symbol_links(alias_symbol)
                    .get_ty_params()
                    .unwrap();
                let min_params = self.c.get_min_ty_arg_count_of_ty_params(params);
                let source_tys = self
                    .c
                    .fill_missing_ty_args(Some(source_alias_ty_arguments), Some(params), min_params)
                    .unwrap();
                let target_tys = self
                    .c
                    .fill_missing_ty_args(Some(target_alias_ty_arguments), Some(params), min_params)
                    .unwrap();
                let variances = self.c.get_alias_variances(alias_symbol);
                self.infer_from_ty_arguments(source_tys, target_tys, variances);
            }
            return;
        }

        if source == target
            && let Some(tys) = source.kind.tys_of_union_or_intersection()
        {
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
            target =
                self.c
                    .get_union_ty::<false>(targets, ty::UnionReduction::Lit, None, None, None);
            if sources.is_empty() {
                self.infer_with_priority(source, target, InferencePriority::NAKED_TYPE_VARIABLE);
                return;
            }
            source =
                self.c
                    .get_union_ty::<false>(&sources, ty::UnionReduction::Lit, None, None, None);
        } else if let Some(i) = target.kind.as_intersection()
            && !i.tys.iter().all(|t| self.c.is_non_generic_object_ty(t))
        {
            if !source.kind.is_union() {
                let sources = if let Some(s) = source.kind.as_intersection() {
                    s.tys
                } else {
                    self.c.alloc([source])
                };
                let (sources, targets) =
                    self.infer_from_matching_tys(sources, i.tys, |this, s, t| {
                        this.c.is_type_identical_to(s, t)
                    });
                if sources.is_empty() || targets.is_empty() {
                    return;
                };
                source = self
                    .c
                    .get_intersection_ty(sources, IntersectionFlags::None, None, None);
                target = self
                    .c
                    .get_intersection_ty(targets, IntersectionFlags::None, None, None);
            }
        }

        if target
            .flags
            .intersects(TypeFlags::INDEXED_ACCESS.union(TypeFlags::SUBSTITUTION))
        {
            if target.is_no_infer_ty() {
                return;
            }
            target = self.c.get_actual_ty_variable(target);
        }

        if target.kind.is_type_variable() {
            // TODO: is_from_inference_block_source

            if let Some(idx) = self.get_inference_info_for_ty(target) {
                if source
                    .get_object_flags()
                    .contains(ObjectFlags::NON_INFERRABLE_TYPE)
                    || source == self.c.non_inferrable_any_ty
                {
                    return;
                }
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
                                .is_none_or(|cs| !cs.contains(&candidate))
                            {
                                self.append_contra_candidate(idx, candidate);
                                self.c.clear_cached_inferences(self.inference);
                            }
                        } else if info
                            .candidates
                            .as_ref()
                            .is_none_or(|cs| !cs.contains(&candidate))
                        {
                            self.append_candidate(idx, candidate);
                            self.c.clear_cached_inferences(self.inference);
                        }
                    }

                    if !self.priority.intersects(InferencePriority::RETURN_TYPE)
                        && target.flags.intersects(TypeFlags::TYPE_PARAMETER)
                        && self.c.inference_info(self.inference, idx).top_level
                        && !self
                            .c
                            .is_ty_param_at_top_level(self.original_target, target, 0)
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

            let simplified = self
                .c
                .get_simplified_ty(target, super::SimplifiedKind::Reading);
            if simplified != target {
                self.infer_from_tys(source, simplified);
            } else if let Some(target_index_ty) = target.kind.as_indexed_access() {
                let index_ty = self
                    .c
                    .get_simplified_ty(target_index_ty.index_ty, super::SimplifiedKind::Reading);
                if index_ty.flags.contains(TypeFlags::INSTANTIABLE) {
                    let object_ty = self.c.get_simplified_ty(
                        target_index_ty.object_ty,
                        super::SimplifiedKind::Reading,
                    );
                    let simplified = self.c.distribute_object_over_object_ty(
                        object_ty,
                        index_ty,
                        super::SimplifiedKind::Reading,
                    );
                    if let Some(simplified) = simplified
                        && simplified != target
                    {
                        self.infer_from_tys(source, simplified);
                    }
                }
            }
        }

        if let Some(source_refer) = source.kind.as_object_reference()
            && let Some(target_refer) = target.kind.as_object_reference()
            && (source_refer.target == target_refer.target
                || source.kind.is_array(self.c) && target.kind.is_array(self.c))
            && !(source_refer.node.is_some() && target_refer.node.is_some())
        {
            let source_tys = self.c.get_ty_arguments(source);
            let target_tys = self.c.get_ty_arguments(target);
            let variances = self.c.get_variances(source_refer.target);
            self.infer_from_ty_arguments(source_tys, target_tys, variances);
        } else if let Some(source) = source.kind.as_index_ty()
            && let Some(target) = target.kind.as_index_ty()
        {
            self.infer_from_contravariant_tys(source.ty, target.ty);
        } else if (source.is_literal_ty() || source.flags.contains(TypeFlags::STRING))
            && target.flags.contains(TypeFlags::INDEX)
        {
            let Some(target_index_ty) = target.kind.as_index_ty() else {
                unreachable!()
            };
            let empty = self.c.create_empty_object_ty_from_string_literal(source);
            self.infer_from_contravariant_tys_with_priority(
                empty,
                target_index_ty.ty,
                InferencePriority::LITERAL_KEYOF,
            );
        } else if let Some(source) = source.kind.as_indexed_access()
            && let Some(target) = target.kind.as_indexed_access()
        {
            self.infer_from_tys(source.object_ty, target.object_ty);
            self.infer_from_tys(source.index_ty, target.index_ty);
            // TODO: (source.flags & TypeFlags.StringMapping && target.flags & TypeFlags.StringMapping)
            // TODO: (source.flags & TypeFlags.Substitution)
        } else if target.kind.is_cond_ty() {
            self.invoke_once(source, target, |this, source, target| {
                this.infer_to_cond_ty(source, target);
            });
        } else if let Some(tys) = target.kind.tys_of_union_or_intersection() {
            self.infer_to_multiple_tys(source, tys, target.flags);
        } else if let Some(u) = source.kind.as_union() {
            for ty in u.tys {
                self.infer_from_tys(ty, target);
            }
        } else if let Some(t) = target.kind.as_template_lit_ty() {
            self.infer_to_template_lit_ty(source, t)
        } else {
            source = self.c.get_reduced_ty(source);
            if self.c.is_generic_mapped_ty(source) && self.c.is_generic_mapped_ty(target) {
                self.invoke_once(source, target, |this, source, target| {
                    this.infer_from_generic_mapped_tys(source, target);
                });
            }
            if !(self.priority.contains(InferencePriority::NO_CONSTRAINTS)
                && source
                    .flags
                    .intersects(TypeFlags::INTERSECTION.union(TypeFlags::INSTANTIABLE)))
            {
                let apparent_source = self.c.get_apparent_ty(source);
                if apparent_source != source
                    && !apparent_source
                        .flags
                        .intersects(TypeFlags::OBJECT.union(TypeFlags::INTERSECTION))
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

    fn infer_from_generic_mapped_tys(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) {
        let source_mapped_ty = source.kind.expect_object_mapped();
        let target_mapped_ty = target.kind.expect_object_mapped();
        let s = self.c.get_constraint_ty_from_mapped_ty(source_mapped_ty);
        let t = self.c.get_constraint_ty_from_mapped_ty(target_mapped_ty);
        self.infer_from_tys(s, t);

        let s = self.c.get_template_ty_from_mapped_ty(source_mapped_ty);
        let t = self.c.get_template_ty_from_mapped_ty(target_mapped_ty);
        self.infer_from_tys(s, t);

        if let Some(source_name_ty) = self.c.get_name_ty_from_mapped_ty(source_mapped_ty)
            && let Some(target_name_ty) = self.c.get_name_ty_from_mapped_ty(target_mapped_ty)
        {
            self.infer_from_tys(source_name_ty, target_name_ty);
        }
    }

    fn get_single_type_variable_from_intersection_types(
        &self,
        tys: ty::Tys<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let mut type_variable = None;
        for ty in tys {
            let i = ty.kind.as_intersection()?;
            let t = i
                .tys
                .iter()
                .find(|t| self.get_inference_info_for_ty(t).is_some())
                .copied();
            if type_variable.is_some() && t != type_variable {
                return None;
            }
            type_variable = t;
        }
        type_variable
    }

    fn infer_to_multiple_tys(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        targets: ty::Tys<'cx>,
        target_flags: TypeFlags,
    ) {
        let mut type_variable_count = 0;
        if target_flags.intersects(TypeFlags::UNION) {
            let mut naked_type_variable = None;
            let sources = if let Some(source_union) = source.kind.as_union() {
                source_union.tys
            } else {
                self.c.alloc([source])
            };
            let mut matched = vec![false; sources.len()];
            let mut inference_circularity = false;
            for t in targets {
                if self.get_inference_info_for_ty(t).is_some() {
                    naked_type_variable = Some(t);
                    type_variable_count += 1;
                } else {
                    for i in 0..sources.len() {
                        let save_inference_priority = self.inference_priority;
                        self.inference_priority = InferencePriority::MAX_VALUE;
                        self.infer_from_tys(sources[i], t);
                        if self.inference_priority == self.priority {
                            matched[i] = true
                        }
                        inference_circularity = inference_circularity
                            || self.inference_priority == InferencePriority::CIRCULARITY;
                        self.inference_priority =
                            if self.inference_priority < save_inference_priority {
                                self.inference_priority
                            } else {
                                save_inference_priority
                            };
                    }
                }
            }
            if type_variable_count == 0 {
                let intersection_type_variable =
                    self.get_single_type_variable_from_intersection_types(targets);
                if let Some(intersection_type_variable) = intersection_type_variable {
                    self.infer_with_priority(
                        source,
                        intersection_type_variable,
                        InferencePriority::NAKED_TYPE_VARIABLE,
                    );
                }
                return;
            }
            if type_variable_count == 1 && !inference_circularity {
                let unmatched = sources
                    .iter()
                    .enumerate()
                    .flat_map(|(i, s)| if matched[i] { None } else { Some(s) })
                    .copied()
                    .collect::<Vec<_>>();
                if !unmatched.is_empty() {
                    let source = self.c.get_union_ty::<false>(
                        &unmatched,
                        ty::UnionReduction::Lit,
                        None,
                        None,
                        None,
                    );
                    self.infer_from_tys(source, naked_type_variable.unwrap());
                    return;
                }
            }
        } else {
            for t in targets {
                if self.get_inference_info_for_ty(t).is_some() {
                    type_variable_count += 1;
                } else {
                    self.infer_from_tys(source, t);
                }
            }
        }
        if if target_flags.intersects(TypeFlags::INTERSECTION) {
            type_variable_count == 1
        } else {
            type_variable_count > 0
        } {
            for t in targets {
                if self.get_inference_info_for_ty(t).is_some() {
                    self.infer_with_priority(source, t, InferencePriority::NAKED_TYPE_VARIABLE);
                }
            }
        }
    }

    fn invoke_once(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        action: impl FnOnce(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        let key = (source.id, target.id);
        if let Some(status) = self.visited.get(&key) {
            self.inference_priority = if self.inference_priority < *status {
                self.inference_priority
            } else {
                *status
            };
            return;
        }
        let saved_inference_priority = self.inference_priority;
        self.inference_priority = InferencePriority::MAX_VALUE;
        let saved_expanding_flags = self.expanding_flags;
        self.source_stack.push(source);
        self.target_stack.push(target);

        if self.c.is_deeply_nested_type(source, &self.source_stack, 2) {
            self.expanding_flags |= RecursionFlags::SOURCE;
        }
        if self.c.is_deeply_nested_type(target, &self.target_stack, 2) {
            self.expanding_flags |= RecursionFlags::TARGET;
        }
        if self.expanding_flags != RecursionFlags::BOTH {
            action(self, source, target);
        } else {
            self.inference_priority = InferencePriority::CIRCULARITY;
        }

        self.target_stack.pop();
        self.source_stack.pop();
        self.expanding_flags = saved_expanding_flags;
        self.visited.insert(key, self.inference_priority);
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

    fn infer_to_template_lit_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::TemplateLitTy<'cx>,
    ) {
        let matches = self.c.infer_tys_from_template_lit_ty(source, target);
        let tys = target.tys;
        if matches.is_some() || target.texts.iter().all(|t| *t == keyword::IDENT_EMPTY) {
            for i in 0..tys.len() {
                let source = matches.map_or(self.c.never_ty, |m| m[i]);
                let target = tys[i];
                if source.flags.contains(TypeFlags::STRING_LITERAL)
                    && target.flags.intersects(TypeFlags::TYPE_VARIABLE)
                {
                    let inference_context = self.get_inference_info_for_ty(target);
                    let constraint = if let Some(inference_context) = inference_context {
                        let ty_param = self
                            .c
                            .inference_info(self.inference, inference_context)
                            .ty_param;
                        self.c.get_base_constraint_of_ty(ty_param)
                    } else {
                        None
                    };
                    if let Some(constraint) = constraint
                        && !self.c.is_type_any(constraint)
                    {
                        let constraint_tys = if let Some(u) = constraint.kind.as_union() {
                            u.tys
                        } else {
                            &[constraint]
                        };
                        let mut all_ty_flags = self
                            .c
                            .reduced_left(
                                constraint_tys,
                                |_, flags, t, _| flags | t.flags,
                                |_, _| unreachable!(),
                                Some(TypeFlags::empty()),
                                None,
                                None,
                            )
                            .unwrap();
                        if !all_ty_flags.contains(TypeFlags::STRING) {
                            let str = source.kind.expect_string_lit();
                            if all_ty_flags.intersects(TypeFlags::NUMBER_LIKE)
                                && !self.c.is_valid_number_string(str.val, true)
                            {
                                all_ty_flags &= TypeFlags::NUMBER_LIKE.complement();
                            }
                            if all_ty_flags.intersects(TypeFlags::BIG_INT_LIKE) && false
                            /* !is_valid_bit_int_str*/
                            {
                                all_ty_flags &= TypeFlags::BIG_INT_LIKE.complement();
                            }
                            let matching_ty = self
                                .c
                                .reduced_left(
                                    constraint_tys,
                                    |this, left, right, _| {
                                        if !right.flags.intersects(all_ty_flags) {
                                            left
                                        } else if left.flags.contains(TypeFlags::STRING) {
                                            left
                                        } else if right.flags.contains(TypeFlags::STRING) {
                                            source
                                        } else if left.flags.contains(TypeFlags::TEMPLATE_LITERAL) {
                                            left
                                        } else if let Some(r) = right.kind.as_template_lit_ty()
                                            && this.is_ty_matched_by_template_lit_ty(source, r)
                                        {
                                            source
                                        } else if left.flags.contains(TypeFlags::STRING_MAPPING) {
                                            left
                                        } else if right.flags.contains(TypeFlags::STRING_MAPPING)
                                            && str.val
                                                == this.apply_string_mapping(
                                                    right.symbol().unwrap(),
                                                    str.val,
                                                )
                                        {
                                            source
                                        } else if left.flags.intersects(TypeFlags::STRING_LITERAL) {
                                            left
                                        } else if right
                                            .kind
                                            .as_string_lit()
                                            .is_some_and(|r| r.val == str.val)
                                        {
                                            right
                                        } else if left.flags.contains(TypeFlags::NUMBER) {
                                            left
                                        } else if right.flags.contains(TypeFlags::NUMBER) {
                                            let val =
                                                this.atoms.get(str.val).parse::<f64>().unwrap();
                                            this.get_number_literal_type_from_number(val)
                                        } else if left.flags.contains(TypeFlags::ENUM) {
                                            left
                                        } else if right.flags.contains(TypeFlags::ENUM) {
                                            // getNumberLiteralType(+str)
                                            todo!()
                                        } else if left.flags.contains(TypeFlags::NUMBER_LITERAL) {
                                            left
                                        } else if right
                                            .kind
                                            .as_number_lit()
                                            .is_some_and(|r| /*r.val == str*/ todo!())
                                        {
                                            right
                                        } else if left.flags.contains(TypeFlags::BIG_INT) {
                                            left
                                        } else if right.flags.contains(TypeFlags::BIG_INT) {
                                            this.parse_bigint_literal_ty(str)
                                        } else if left.flags.contains(TypeFlags::BIG_INT_LITERAL) {
                                            left
                                        } else if right.flags.contains(TypeFlags::BIG_INT_LITERAL)
                                            && this.is_bigint_equal_to_str(
                                                right.kind.expect_bigint_lit(),
                                                str,
                                            )
                                        {
                                            right
                                        } else if left.flags.contains(TypeFlags::BOOLEAN) {
                                            left
                                        } else if right.flags.contains(TypeFlags::BOOLEAN) {
                                            if str.val == keyword::KW_TRUE {
                                                this.true_ty
                                            } else if str.val == keyword::KW_FALSE {
                                                this.false_ty
                                            } else {
                                                this.boolean_ty()
                                            }
                                        } else if left.flags.contains(TypeFlags::BOOLEAN_LITERAL) {
                                            left
                                        } else if right.flags.contains(TypeFlags::BOOLEAN_LITERAL)
                                            && right
                                                .kind
                                                .as_intrinsic()
                                                .is_some_and(|r| r.name == str.val)
                                        {
                                            right
                                        } else if left.flags.contains(TypeFlags::UNDEFINED) {
                                            left
                                        } else if right.flags.contains(TypeFlags::UNDEFINED)
                                            && right
                                                .kind
                                                .as_intrinsic()
                                                .is_some_and(|r| r.name == str.val)
                                        {
                                            right
                                        } else if left.flags.contains(TypeFlags::NULL) {
                                            left
                                        } else if right.flags.contains(TypeFlags::NULL)
                                            && right
                                                .kind
                                                .as_intrinsic()
                                                .is_some_and(|r| r.name == str.val)
                                        {
                                            right
                                        } else {
                                            left
                                        }
                                    },
                                    |_, _| unreachable!(),
                                    Some(self.c.never_ty),
                                    None,
                                    None,
                                )
                                .unwrap();

                            if !matching_ty.flags.intersects(TypeFlags::NEVER) {
                                self.infer_from_tys(matching_ty, target);
                                continue;
                            }
                        }
                    }
                }
                self.infer_from_tys(source, target);
            }
        }
    }

    fn infer_to_mapped_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        target_mapped_ty: &'cx ty::MappedTy<'cx>,
        constraint_ty: &'cx ty::Ty<'cx>,
    ) -> bool {
        assert!(std::ptr::eq(
            target.kind.expect_object_mapped(),
            target_mapped_ty
        ));
        if let Some(tys) = constraint_ty.kind.tys_of_union_or_intersection() {
            let mut result = false;
            for ty in tys {
                result = self.infer_to_mapped_ty(source, target, target_mapped_ty, ty);
            }
            result
        } else if let Some(index_ty) = constraint_ty.kind.as_index_ty() {
            let inference_info = self.get_inference_info_for_ty(index_ty.ty);
            if let Some(inference_info) = inference_info
                && let info = self.c.inference_info(self.inference, inference_info)
                && !info.is_fixed
                && !self.c.is_from_inference_block_source(source)
                && let info_target = info.ty_param
                && let Some(inferred_ty) =
                    self.c
                        .infer_ty_for_homomorphic_map_ty(source, target, constraint_ty)
            {
                let priority = if source
                    .get_object_flags()
                    .intersects(ObjectFlags::NON_INFERRABLE_TYPE)
                {
                    InferencePriority::PARTIAL_HOMOMORPHIC_MAPPED_TYPE
                } else {
                    InferencePriority::HOMOMORPHIC_MAPPED_TYPE
                };
                self.infer_with_priority(inferred_ty, info_target, priority);
            }
            true
        } else if constraint_ty.kind.is_param() {
            // TODO: source.pattern
            let index_ty = self.c.get_index_ty(source, ty::IndexFlags::empty());
            self.infer_with_priority(
                index_ty,
                constraint_ty,
                InferencePriority::MAPPED_TYPE_CONSTRAINT,
            );
            if let Some(extended_constraint) = self.c.get_constraint_of_ty(constraint_ty)
                && self.infer_to_mapped_ty(source, target, target_mapped_ty, extended_constraint)
            {
                return true;
            }
            let prop_tys = self
                .c
                .get_props_of_ty(source)
                .into_iter()
                .map(|&s| self.c.get_type_of_symbol(s))
                .collect::<Vec<_>>();
            let index_tys = self
                .c
                .get_index_infos_of_ty(source)
                .into_iter()
                .map(|info| {
                    if !std::ptr::eq(info, &self.c.enum_number_index_info()) {
                        info.val_ty
                    } else {
                        self.c.never_ty
                    }
                });
            let tys = prop_tys.into_iter().chain(index_tys).collect::<Vec<_>>();
            let source =
                self.c
                    .get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None);
            let target = self.c.get_template_ty_from_mapped_ty(target_mapped_ty);
            self.infer_from_tys(source, target);
            true
        } else {
            false
        }
    }

    fn infer_from_object_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        if let Some(source_reference) = source.kind.as_object_reference()
            && let Some(target_reference) = target.kind.as_object_reference()
            && (source_reference.target == target_reference.target
                || source.kind.is_array(self.c) && target.kind.is_array(self.c))
        {
            let source_type_arguments = self.c.get_ty_arguments(source);
            let target_type_arguments = self.c.get_ty_arguments(target);
            let variances = self.c.get_variances(source_reference.target);
            self.infer_from_ty_arguments(source_type_arguments, target_type_arguments, variances);
            return;
        }

        if self.c.is_generic_mapped_ty(source) && self.c.is_generic_mapped_ty(target) {
            self.infer_from_generic_mapped_tys(source, target);
        }

        if let Some(target_mapped_ty) = target.kind.as_object_mapped()
            && target_mapped_ty.decl.name_ty.is_none()
        {
            let constraint_ty = self.c.get_constraint_ty_from_mapped_ty(target_mapped_ty);
            if self.infer_to_mapped_ty(source, target, target_mapped_ty, constraint_ty) {
                return;
            }
        }

        if !self.c.tys_definitely_unrelated(source, target) {
            if source.is_tuple() || source.kind.is_array(self.c) {
                if let Some(target_tuple) = target.as_tuple() {
                    let source_arity = TyChecker::get_ty_reference_arity(source);
                    let target_arity = TyChecker::get_ty_reference_arity(target);
                    let is_tuple_ty_structure_matching = |t1: &ty::TupleTy, t2: &ty::TupleTy| {
                        source_arity == target_arity
                            && t1.element_flags.iter().enumerate().all(|(i, f)| {
                                f.intersection(ty::ElementFlags::VARIABLE)
                                    == t2.element_flags[i].intersection(ty::ElementFlags::VARIABLE)
                            })
                    };
                    let element_tys = self.c.get_ty_arguments(target);
                    let element_flags = target_tuple.element_flags;
                    if let Some(source_tuple) = source.as_tuple()
                        && is_tuple_ty_structure_matching(source_tuple, target_tuple)
                    {
                        for i in 0..target_arity {
                            let s = self.c.get_ty_arguments(source);
                            self.infer_from_tys(s[i], element_tys[i]);
                        }
                        return;
                    }
                    let start_len = if let Some(s) = source.as_tuple() {
                        usize::min(s.fixed_length, target_tuple.fixed_length)
                    } else {
                        0
                    };

                    let end_len = usize::min(
                        if let Some(s) = source.as_tuple() {
                            s.get_end_elem_count(ty::ElementFlags::FIXED)
                        } else {
                            0
                        },
                        if target_tuple
                            .combined_flags
                            .intersects(ty::ElementFlags::VARIABLE)
                        {
                            target_tuple.get_end_elem_count(ty::ElementFlags::FIXED)
                        } else {
                            0
                        },
                    );

                    for i in 0..start_len {
                        let s = self.c.get_ty_arguments(source);
                        self.infer_from_tys(s[i], element_tys[i]);
                    }
                    if source.as_tuple().is_none_or(|s| {
                        source_arity - start_len - end_len == 1
                            && s.element_flags[start_len].intersects(ty::ElementFlags::REST)
                    }) {
                        let rest_ty = self.c.get_ty_arguments(source)[start_len];
                        for i in start_len..target_arity - end_len {
                            let s = if element_flags[i].intersects(ty::ElementFlags::VARIADIC) {
                                self.c.create_array_ty(rest_ty, false)
                            } else {
                                rest_ty
                            };
                            self.infer_from_tys(s, element_tys[i]);
                        }
                    } else {
                        let middle_length = target_arity - start_len - end_len;
                        if middle_length == 2 {
                            if element_flags[start_len]
                                .intersection(element_flags[start_len + 1])
                                .intersects(ty::ElementFlags::VARIADIC)
                            {
                                if let Some(target_info) =
                                    self.get_inference_info_for_ty(element_tys[start_len])
                                    && let Some(implied_arity) = self
                                        .c
                                        .inference_info(self.inference, target_info)
                                        .implied_arity
                                {
                                    let s = self.c.slice_tuple_ty(
                                        source,
                                        start_len,
                                        end_len + source_arity - implied_arity,
                                    );
                                    self.infer_from_tys(s, element_tys[start_len]);
                                    let s = self.c.slice_tuple_ty(
                                        source,
                                        start_len + implied_arity,
                                        end_len,
                                    );
                                    self.infer_from_tys(s, element_tys[start_len + 1]);
                                }
                            } else if element_flags[start_len]
                                .intersects(ty::ElementFlags::VARIADIC)
                                && element_flags[start_len + 1].intersects(ty::ElementFlags::REST)
                            {
                                let info_idx =
                                    self.get_inference_info_for_ty(element_tys[start_len]);
                                let param = info_idx.map(|info_idx| {
                                    self.c.inference_info(self.inference, info_idx).ty_param
                                });
                                let constraint =
                                    param.and_then(|param| self.c.get_base_constraint_of_ty(param));
                                if let Some(constraint) = constraint
                                    && let Some(t) = constraint.as_tuple()
                                    && !t.combined_flags.intersects(ty::ElementFlags::VARIABLE)
                                {
                                    let implied_arity = t.fixed_length;
                                    let s = self.c.slice_tuple_ty(
                                        source,
                                        start_len,
                                        source_arity - (start_len + implied_arity),
                                    );
                                    self.infer_from_tys(s, element_tys[start_len]);
                                    let s = self
                                        .c
                                        .get_element_ty_of_slice_of_tuple_ty(
                                            source,
                                            start_len + implied_arity,
                                            Some(end_len),
                                            None,
                                            None,
                                        )
                                        .unwrap();
                                    self.infer_from_tys(s, element_tys[start_len + 1]);
                                }
                            } else if element_flags[start_len].intersects(ty::ElementFlags::REST)
                                && element_flags[start_len + 1]
                                    .intersects(ty::ElementFlags::VARIADIC)
                            {
                                let info_idx =
                                    self.get_inference_info_for_ty(element_tys[start_len]);
                                let param = info_idx.map(|info_idx| {
                                    self.c.inference_info(self.inference, info_idx).ty_param
                                });
                                let constraint =
                                    param.and_then(|param| self.c.get_base_constraint_of_ty(param));
                                if let Some(constraint) = constraint
                                    && let Some(t) = constraint.as_tuple()
                                    && !t.combined_flags.intersects(ty::ElementFlags::VARIABLE)
                                {
                                    let implied_arity = t.fixed_length;
                                    let end_index = source_arity
                                        - t.get_end_elem_count(ty::ElementFlags::FIXED);
                                    let start_index = end_index - implied_arity;
                                    let trailing_slice = {
                                        let tys = &self.c.get_ty_arguments(source)
                                            [start_index..end_index];
                                        let flags = &source.as_tuple().unwrap().element_flags
                                            [start_index..end_index];
                                        self.c.create_tuple_ty(tys, Some(flags), false)
                                    };
                                    let s = self
                                        .c
                                        .get_element_ty_of_slice_of_tuple_ty(
                                            source,
                                            start_index,
                                            Some(end_len + implied_arity),
                                            None,
                                            None,
                                        )
                                        .unwrap();
                                    self.infer_from_tys(s, element_tys[start_len]);
                                    self.infer_from_tys(trailing_slice, element_tys[start_len + 1]);
                                }
                            }
                        } else if middle_length == 1
                            && element_flags[start_len].intersects(ty::ElementFlags::VARIADIC)
                        {
                            let ends_in_optionals = target_tuple.element_flags[target_arity - 1]
                                .contains(ty::ElementFlags::OPTIONAL);
                            let source_slice = self.c.slice_tuple_ty(source, start_len, end_len);
                            self.infer_with_priority(
                                source_slice,
                                element_tys[start_len],
                                if ends_in_optionals {
                                    InferencePriority::SPECULATIVE_TUPLE
                                } else {
                                    InferencePriority::empty()
                                },
                            );
                        } else if middle_length == 1
                            && element_flags[start_len].contains(ty::ElementFlags::REST)
                            && let Some(rest_ty) = self.c.get_element_ty_of_slice_of_tuple_ty(
                                source,
                                start_len,
                                Some(end_len),
                                None,
                                None,
                            )
                        {
                            self.infer_from_tys(rest_ty, element_tys[start_len]);
                        }
                    }
                    for i in 0..end_len {
                        let s = self.c.get_ty_arguments(source)[source_arity - i - 1];
                        self.infer_from_tys(s, element_tys[target_arity - i - 1]);
                    }
                    return;
                } else if target.kind.is_array(self.c) {
                    self.infer_from_index_tys(source, target);
                    return;
                }
            }
            self.infer_from_props(source, target);
            self.infer_from_sigs(source, target, SigKind::Call);
            self.infer_from_sigs(source, target, SigKind::Constructor);
            self.infer_from_index_tys(source, target);
        }
    }

    fn infer_from_index_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        let new_priority =
            if !(source.get_object_flags() & target.get_object_flags() & ObjectFlags::MAPPED)
                .is_empty()
            {
                InferencePriority::HOMOMORPHIC_MAPPED_TYPE
            } else {
                InferencePriority::empty()
            };
        let target_index_infos = self.c.get_index_infos_of_ty(target);
        if self.c.is_object_ty_with_inferable_index(source) {
            for target_info in target_index_infos {
                let source_props = self.c.get_props_of_ty(source);
                let mut props_tys = Vec::with_capacity(source_props.len());
                for prop in source_props {
                    let lit = self.c.get_literal_ty_from_prop(
                        *prop,
                        TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE,
                        false,
                    );
                    if self.c.is_applicable_index_ty(lit, target_info.key_ty) {
                        let prop_ty = self.c.get_type_of_symbol(*prop);
                        props_tys.push(
                            if self.c.symbol(*prop).flags.intersects(SymbolFlags::OPTIONAL) {
                                // TODO: remove missing
                                prop_ty
                            } else {
                                prop_ty
                            },
                        );
                    }
                }

                for info in self.c.get_index_infos_of_ty(source) {
                    if self
                        .c
                        .is_applicable_index_ty(info.key_ty, target_info.key_ty)
                    {
                        props_tys.push(info.key_ty);
                    }
                }

                if !props_tys.is_empty() {
                    let u = self.c.get_union_ty::<false>(
                        &props_tys,
                        ty::UnionReduction::Lit,
                        None,
                        None,
                        None,
                    );
                    self.infer_with_priority(u, target_info.val_ty, new_priority);
                }
            }
        }
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

    // TODO: remove duplicate
    fn apply_to_param_tys(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        callback: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        let source_count = source.get_param_count(self.c);
        let target_count = target.get_param_count(self.c);
        let source_rest_ty = source.get_effective_rest_ty(self.c);
        let target_rest_ty = target.get_effective_rest_ty(self.c);
        let target_non_rest_count = if target_rest_ty.is_some() {
            target_count - 1
        } else {
            target_count
        };
        let param_count = if source_rest_ty.is_some() {
            target_non_rest_count
        } else {
            usize::min(source_count, target_non_rest_count)
        };
        if let Some(source_this_ty) = self.c.get_this_ty_of_sig(source)
            && let Some(target_this_ty) = self.c.get_this_ty_of_sig(target)
        {
            callback(self, source_this_ty, target_this_ty);
        }
        for i in 0..param_count {
            let source_ty = self.c.get_ty_at_pos(source, i);
            let target_ty = self.c.get_ty_at_pos(target, i);
            callback(self, source_ty, target_ty);
        }
        if let Some(target_rest_ty) = target_rest_ty {
            let readonly = self.c.is_const_ty_variable(target_rest_ty)
                && !self
                    .c
                    .some_type(target_rest_ty, |this, t| this.is_mutable_array_like_ty(t));
            let rest_ty = self.c.get_rest_ty_at_pos(source, param_count, readonly);
            callback(self, rest_ty, target_rest_ty);
        }
    }

    fn infer_from_contravariant_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        self.contravariant = !self.contravariant;
        self.infer_from_tys(source, target);
        self.contravariant = !self.contravariant;
    }

    fn infer_from_contravariant_tys_with_priority(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        new_priority: InferencePriority,
    ) {
        let save_priority = self.priority;
        self.priority |= new_priority;
        self.infer_from_contravariant_tys(source, target);
        self.priority = save_priority;
    }

    fn infer_from_sig(&mut self, source: &'cx ty::Sig<'cx>, target: &'cx ty::Sig<'cx>) {
        if !source.flags.contains(SigFlags::IS_NON_INFERRABLE) {
            let save_bivariant = self.bivariant;
            let n = self.c.p.node(target.def_id());
            self.bivariant = self.bivariant
                || n.is_class_method_elem()
                || n.is_object_method_member()
                || n.is_method_signature()
                || n.is_class_ctor();

            self.apply_to_param_tys(source, target, |this, source, target| {
                if this.c.config.compiler_options().strict_function_types()
                    || this.priority.contains(InferencePriority::ALWAYS_STRICT)
                {
                    this.infer_from_contravariant_tys(source, target);
                } else {
                    this.infer_from_tys(source, target);
                }
            });

            self.bivariant = save_bivariant;
        }
        self.apply_to_return_ty(source, target, |this, source, target| {
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
            for (i, target_sig) in target_sigs.iter().enumerate().take(target_len) {
                let source_index = (source_len + i).saturating_sub(target_len);
                let source = self.c.get_base_sig(source_sigs[source_index]);
                let target = self.c.get_erased_sig(target_sig);
                self.infer_from_sig(source, target);
            }
        }
    }

    fn apply_to_return_ty(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        f: impl FnOnce(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        if let Some(target_ty_pred) = self.c.get_ty_predicate_of_sig(target)
            && let Some(source_ty_pred) = self.c.get_ty_predicate_of_sig(source)
            && target_ty_pred.kind_match(source_ty_pred)
            && let Some(s) = source_ty_pred.ty()
            && let Some(t) = target_ty_pred.ty()
        {
            f(self, s, t);
            return;
        }

        let target_ret_ty = self.c.get_ret_ty_of_sig(target);
        if self.c.could_contain_ty_var(target_ret_ty) {
            let source_ret_ty = self.c.get_ret_ty_of_sig(source);
            f(self, source_ret_ty, target_ret_ty);
        }
    }

    fn infer_from_props(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        let remove_missing_ty = |this: &mut Self, s: bolt_ts_binder::SymbolID| {
            let is_optional = this.c.symbol(s).flags.contains(SymbolFlags::OPTIONAL);
            let t = this.c.get_type_of_symbol(s);
            this.c.remove_missing_ty(t, is_optional)
        };
        for &target_prop in self.c.get_props_of_ty(target) {
            if let Some(source_prop) = self
                .c
                .get_prop_of_ty::<false>(source, self.c.symbol(target_prop).name)
            {
                let s = remove_missing_ty(self, source_prop);
                let t = remove_missing_ty(self, target_prop);
                self.infer_from_tys(s, t);
            }
        }
    }
}

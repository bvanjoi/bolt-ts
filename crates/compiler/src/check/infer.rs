use std::borrow::Cow;

use crate::bind::SymbolFlags;
use crate::ir;
use crate::ty::{self, SigFlags, SigKind, TyID, TypeFlags};
use crate::ty::{ObjectFlags, Sig};
use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use super::check_type_related_to::RecursionFlags;
use super::create_ty::IntersectionFlags;
use super::get_contextual::ContextFlags;
use super::utils::append_if_unique;
use super::{CheckMode, InferenceContextId, TyChecker, fn_mapper};

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
        if ty.get_object_flags().intersects(ObjectFlags::REFERENCE) {
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
                self.create_reference_ty(target, Some(ty_args), ObjectFlags::empty())
            } else {
                ty
            }
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
                        inferred_contravariant_ty.is_none_or(|inferred_contravariant_ty| {
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
                    inferred_ty = Some(self.instantiate_ty(default_ty, Some(mapper)));
                }
            }
        } else {
            inferred_ty = self.get_ty_from_inference(inference, idx);
        };

        let is_in_javascript_file = self
            .inference(inference)
            .flags
            .intersects(InferenceFlags::ANY_DEFAULT);
        self.set_inferred_ty_of_inference_info(
            inference,
            idx,
            inferred_ty.unwrap_or(self.get_default_ty_argument_ty(is_in_javascript_file)),
        );

        let i = self.inference_info(inference, idx);
        if let Some(constraint) = self.get_constraint_of_ty_param(i.ty_param) {
            let instantiated_constraint = self.instantiate_ty(
                constraint,
                Some(self.inference(inference).non_fixing_mapper),
            );
            if inferred_ty.is_none_or(|inferred_ty| {
                let ty = self.get_ty_with_this_arg(instantiated_constraint, Some(inferred_ty));
                // TODO: more flexible compare types
                !self.is_type_related_to(inferred_ty, ty, super::relation::RelationKind::Assignable)
            }) {
                // TODO: fallback
                self.override_inferred_ty_of_inference_info(
                    inference,
                    idx,
                    instantiated_constraint,
                );
            }
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
            if let Some(contextual_ty) =
                self.get_contextual_ty(node_id, Some(ContextFlags::empty()))
            {
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
            expanding_flags: RecursionFlags::empty(),
            source_stack: Vec::with_capacity(32),
            target_stack: Vec::with_capacity(32),
            visited: fx_hashmap_with_capacity(32),
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

    fn infer_from_lit_parts_to_template_lit(
        &mut self,
        source_texts: &[bolt_ts_atom::AtomId],
        source_tys: &[&'cx ty::Ty<'cx>],
        target: &'cx ty::Ty<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        let last_source_index = source_texts.len() - 1;
        let source_start_text = source_texts[0];
        let source_start_text_str = self.atoms.get(source_start_text);
        let source_end_text = source_texts[last_source_index];
        let source_end_text_str = self.atoms.get(source_end_text);

        let t = target.kind.expect_template_lit_ty();
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

        fn get_source_text<'a>(
            atoms: &'a bolt_ts_atom::AtomMap,
            index: usize,
            last_source_index: usize,
            source_end_text: bolt_ts_atom::AtomId,
            target_end_text_len: usize,
            source_texts: &[bolt_ts_atom::AtomId],
        ) -> &'a str {
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
                    this.atoms,
                    s,
                    last_source_index,
                    source_end_text,
                    target_end_text_len,
                    source_texts,
                )[*pos..p];
                let atom = bolt_ts_atom::AtomId::from_str(sub);
                if !this.atoms.contains(atom) {
                    let sub = sub.to_string();
                    this.atoms.insert(atom, Cow::Owned(sub));
                }
                let match_type = this.get_string_literal_type(atom);
                matches.push(match_type);
            } else {
                let mut parts = Vec::with_capacity(source_texts.len());
                let a = &this.atoms.get(source_texts[*seg])[*pos..];
                let a_atom = bolt_ts_atom::AtomId::from_str(a);
                if !this.atoms.contains(a_atom) {
                    let a = a.to_string();
                    this.atoms.insert(a_atom, Cow::Owned(a));
                }
                parts.push(a_atom);
                parts.extend(source_texts[*seg + 1..s].iter());
                let b = &get_source_text(
                    this.atoms,
                    s,
                    last_source_index,
                    source_end_text,
                    target_end_text_len,
                    source_texts,
                )[0..p];
                let b_atom = bolt_ts_atom::AtomId::from_str(b);
                if !this.atoms.contains(b_atom) {
                    let b = b.to_string();
                    this.atoms.insert(b_atom, Cow::Owned(b));
                }
                parts.push(b_atom);
                let match_type = this.get_template_lit_ty(&parts, &source_tys[*seg..s]);
                matches.push(match_type);
            }
            *seg = s;
            *pos = p;
        };

        for i in 1..last_target_index {
            let delim = self.atoms.get(target_texts[i]);
            if !delim.is_empty() {
                let mut s = seg;
                let mut p = pos;
                loop {
                    let src_text = get_source_text(
                        self.atoms,
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
                    self.atoms,
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
            self.atoms,
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
        target: &'cx ty::Ty<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        let t = target.kind.expect_template_lit_ty();
        if let Some(s) = source.kind.as_string_lit() {
            self.infer_from_lit_parts_to_template_lit(&[s.val], self.empty_array(), target)
        } else if let Some(s) = source.kind.as_template_lit_ty() {
            if self.array_is_equal(Some(s.texts), Some(t.texts)) {
                let tys = s
                    .tys
                    .iter()
                    .enumerate()
                    .map(|(i, s)| {
                        let s_base = self.get_base_constraint_or_ty(s);
                        let t_base = self.get_base_constraint_or_ty(t.tys[i]);
                        if self.is_type_assignable_to(s_base, t_base) {
                            s
                        } else {
                            self.get_string_like_ty_for_ty(s)
                        }
                    })
                    .collect::<Vec<_>>();
                Some(self.alloc(tys))
            } else {
                self.infer_from_lit_parts_to_template_lit(s.texts, s.tys, target)
            }
        } else {
            None
        }
    }

    pub(super) fn is_ty_matched_by_template_lit_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        let Some(inferences) = self.infer_tys_from_template_lit_ty(source, target) else {
            return false;
        };
        let t = target.kind.expect_template_lit_ty();
        inferences
            .iter()
            .enumerate()
            .all(|(i, r)| self.is_valid_ty_for_template_lit_placeholder(r, t.tys[i]))
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
        } else if target
            .kind
            .as_intersection()
            .is_some_and(|i| !i.tys.iter().all(|t| self.c.is_non_generic_object_ty(t)))
        {
            let t = target.kind.expect_intersection();
            if !source.kind.is_union() {
                let sources = if let Some(s) = source.kind.as_intersection() {
                    s.tys
                } else {
                    self.c.alloc([source])
                };
                let (sources, targets) =
                    self.infer_from_matching_tys(sources, t.tys, |this, s, t| {
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

        if target.kind.is_type_variable() {
            if let Some(idx) = self.get_inference_info_for_ty(target) {
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
        } else if let Some(tys) = target.kind.tys_of_union_or_intersection() {
            self.infer_to_multiple_tys(source, tys, target.flags);
        } else if let Some(u) = source.kind.as_union() {
            for ty in u.tys {
                self.infer_from_tys(ty, target);
            }
        } else if target.kind.is_template_lit_ty() {
            self.infer_to_template_lit_ty(source, target)
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
                    let source = self.c.get_union_ty(&unmatched, ty::UnionReduction::Lit);
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

    fn infer_to_template_lit_ty(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        let t = target.kind.expect_template_lit_ty();
        let matches = self.c.infer_tys_from_template_lit_ty(source, target);
        let tys = t.tys;
        if matches.is_some() || t.texts.iter().all(|t| *t == keyword::IDENT_EMPTY) {
            for i in 0..tys.len() {
                let source = matches.map_or(self.c.never_ty, |m| m[i]);
                let target = tys[i];
                if source.flags.intersects(TypeFlags::STRING_LITERAL)
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
                    if let Some(constraint) = constraint {
                        if !self.c.is_type_any(Some(constraint)) {
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
                                    Some(TypeFlags::empty()),
                                    None,
                                    None,
                                )
                                .unwrap();
                            if !all_ty_flags.intersects(TypeFlags::STRING) {
                                let str = source.kind.expect_string_lit();
                                if all_ty_flags.intersects(TypeFlags::NUMBER_LIKE)
                                    && !self.c.is_valid_number_string(str.val, true)
                                {
                                    all_ty_flags &= !TypeFlags::NUMBER_LIKE;
                                }
                                if all_ty_flags.intersects(TypeFlags::BIG_INT_LIKE) && false
                                /* !is_valid_big_int_string */
                                {
                                    all_ty_flags &= !TypeFlags::BIG_INT_LIKE;
                                }
                                let matching_ty = self
                                    .c
                                    .reduced_left(
                                        constraint_tys,
                                        |this, left, right, _| {
                                            if !right.flags.intersects(all_ty_flags) {
                                                left
                                            } else if left.flags.intersects(TypeFlags::STRING) {
                                                left
                                            } else if right.flags.intersects(TypeFlags::STRING) {
                                                source
                                            } else if left
                                                .flags
                                                .intersects(TypeFlags::TEMPLATE_LITERAL)
                                            {
                                                left
                                            } else if right
                                                .flags
                                                .intersects(TypeFlags::TEMPLATE_LITERAL)
                                                && this
                                                    .is_ty_matched_by_template_lit_ty(source, right)
                                            {
                                                source
                                            } else if left
                                                .flags
                                                .intersects(TypeFlags::STRING_MAPPING)
                                            {
                                                left
                                            } else if right
                                                .flags
                                                .intersects(TypeFlags::STRING_MAPPING)
                                                && str.val
                                                    == this.apply_string_mapping(
                                                        right.symbol().unwrap(),
                                                        str.val,
                                                    )
                                            {
                                                source
                                            } else if left
                                                .flags
                                                .intersects(TypeFlags::STRING_LITERAL)
                                            {
                                                left
                                            } else if right
                                                .kind
                                                .as_string_lit()
                                                .is_some_and(|r| r.val == str.val)
                                            {
                                                right
                                            } else if left.flags.intersects(TypeFlags::NUMBER) {
                                                left
                                            } else if right.flags.intersects(TypeFlags::NUMBER) {
                                                let val =
                                                    this.atoms.get(str.val).parse::<f64>().unwrap();
                                                this.get_number_literal_type(val)
                                            } else if left.flags.intersects(TypeFlags::ENUM) {
                                                left
                                            } else if right.flags.intersects(TypeFlags::ENUM) {
                                                // getNumberLiteralType(+str)
                                                todo!()
                                            } else if left
                                                .flags
                                                .intersects(TypeFlags::NUMBER_LITERAL)
                                            {
                                                left
                                            } else if right
                                                .kind
                                                .as_number_lit()
                                                .is_some_and(|r| /*r.val == str*/ todo!())
                                            {
                                                right
                                            } else if left.flags.intersects(TypeFlags::BIG_INT) {
                                                left
                                                // TODO: else if right.kind flags.intersects(TypeFlags::BigInt){parseBigIntLiteralType}(str)
                                                // TODO: else if left.flags.intersects(TypeFlags::BigIntLiteral){left} else if right.flags.intersects(TypeFlags::BigIntLiteral) && pseudoBigIntToString((right as BigIntLiteralType).value) === str{right}
                                            } else if left.flags.intersects(TypeFlags::BOOLEAN) {
                                                left
                                            } else if right.flags.intersects(TypeFlags::BOOLEAN) {
                                                if str.val == keyword::KW_TRUE {
                                                    this.true_ty
                                                } else if str.val == keyword::KW_FALSE {
                                                    this.false_ty
                                                } else {
                                                    this.boolean_ty()
                                                }
                                            } else if left
                                                .flags
                                                .intersects(TypeFlags::BOOLEAN_LITERAL)
                                            {
                                                left
                                                // TODO: else if right.flags.intersects(TypeFlags::BOOLEAN_LITERAL) && (right as IntrinsicType).intrinsicName === str{right}
                                            } else if left.flags.intersects(TypeFlags::UNDEFINED) {
                                                left
                                                // TODO: else if right.flags.intersects(TypeFlags::UNDEFINED) && (right as IntrinsicType).intrinsicName === str{right}
                                            } else if left.flags.intersects(TypeFlags::NULL) {
                                                left
                                                // TODO: else if right.flags.intersects(TypeFlags::NULL) && (right as IntrinsicType).intrinsicName === str{right}
                                            } else {
                                                left
                                            }
                                        },
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
                }
                self.infer_from_tys(source, target);
            }
        }
    }

    fn infer_from_object_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
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
                    if let Some(source_tuple) = source.as_tuple() {
                        if is_tuple_ty_structure_matching(source_tuple, target_tuple) {
                            for i in 0..target_arity {
                                let s = self.c.get_ty_arguments(source);
                                self.infer_from_tys(s[i], element_tys[i]);
                            }
                            return;
                        }
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
                                {
                                    if let Some(implied_arity) = self
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
                                if let Some(constraint) = constraint {
                                    if let Some(t) = constraint.as_tuple() {
                                        if !t.combined_flags.intersects(ty::ElementFlags::VARIABLE)
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
                                    }
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
                                if let Some(constraint) = constraint {
                                    if let Some(t) = constraint.as_tuple() {
                                        if !t.combined_flags.intersects(ty::ElementFlags::VARIABLE)
                                        {
                                            let implied_arity = t.fixed_length;
                                            let end_index = source_arity
                                                - t.get_end_elem_count(ty::ElementFlags::FIXED);
                                            let start_index = end_index - implied_arity;
                                            let trailing_slice = {
                                                let tys = &self.c.get_ty_arguments(source)
                                                    [start_index..end_index];
                                                let flags =
                                                    &source.as_tuple().unwrap().element_flags
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
                                            self.infer_from_tys(
                                                trailing_slice,
                                                element_tys[start_len + 1],
                                            );
                                        }
                                    }
                                }
                            }
                        } else if middle_length == 1
                            && element_flags[start_len].intersects(ty::ElementFlags::VARIADIC)
                        {
                            let ends_in_optionals = target_tuple.element_flags[target_arity - 1]
                                .intersects(ty::ElementFlags::OPTIONAL);
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
                            && element_flags[start_len].intersects(ty::ElementFlags::REST)
                        {
                            if let Some(rest_ty) = self.c.get_element_ty_of_slice_of_tuple_ty(
                                source,
                                start_len,
                                Some(end_len),
                                None,
                                None,
                            ) {
                                self.infer_from_tys(rest_ty, element_tys[start_len]);
                            }
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
                    let lit = self.c.get_lit_ty_from_prop(*prop);
                    if self.c.is_applicable_index_ty(lit, target_info.key_ty) {
                        let prop_ty = self.c.get_type_of_symbol(*prop);
                        props_tys.push(
                            if self
                                .c
                                .symbol(*prop)
                                .flags()
                                .intersects(SymbolFlags::OPTIONAL)
                            {
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
                    let u = self.c.get_union_ty(&props_tys, ty::UnionReduction::Lit);
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
                let s = {
                    let s = self.c.get_type_of_symbol(source_prop);
                    // TODO: remove_missing
                    s
                };
                let t = {
                    let t = self.c.get_type_of_symbol(*target_prop);
                    // TODO: remove_missing
                    t
                };
                self.infer_from_tys(s, t);
            }
        }
    }
}

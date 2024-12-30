use crate::ir;
use crate::ty::{self, TyMapper};
use crate::{ast, ty::Sig};

use super::utils::append_if_unique;
use super::{CheckMode, InferenceContextId, TyChecker};

#[derive(Debug, Clone)]
pub struct InferenceInfo<'cx> {
    pub ty_params: &'cx ty::Ty<'cx>,
    pub candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub contra_candidates: Option<thin_vec::ThinVec<&'cx ty::Ty<'cx>>>,
    pub is_fixed: bool,
    pub top_level: bool,
}

impl<'cx> InferenceInfo<'cx> {
    fn create(ty_params: &'cx ty::Ty<'cx>) -> Self {
        Self {
            ty_params,
            candidates: None,
            contra_candidates: None,
            is_fixed: false,
            top_level: true,
        }
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
        let context = InferenceContext::create(ty_params, sig, flags);
        let id = self.next_inference_context_id;
        assert_eq!(id.as_usize(), self.inference_contexts.len());
        self.next_inference_context_id = self.next_inference_context_id.next();
        self.inference_contexts.push(context);
        id
    }

    pub(crate) fn create_inference_fixing_mapper(
        &mut self,
        id: InferenceContextId,
    ) -> &'cx TyMapper<'cx> {
        let sources = self.inference_contexts[id.as_usize()]
            .inferences
            .iter()
            .map(|i| i.ty_params)
            .collect::<Vec<_>>();
        let sources = self.alloc(sources);
        let target = (0..self.inference_contexts[id.as_usize()].inferences.len())
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
        let sources = self.inference_contexts[id.as_usize()]
            .inferences
            .iter()
            .map(|i| i.ty_params)
            .collect::<Vec<_>>();
        let sources = self.alloc(sources);
        let target = (0..self.inference_contexts[id.as_usize()].inferences.len())
            .map(|idx| self.get_inferred_ty(id, idx))
            .collect::<Vec<_>>();
        let target = self.alloc(target);
        self.alloc(TyMapper::create(sources, target))
    }

    fn get_inferred_ty(&mut self, inference: InferenceContextId, idx: usize) -> &'cx ty::Ty<'cx> {
        let ctx = &self.inference_contexts[inference.as_usize()];
        let i = &ctx.inferences[idx];
        // TODO: cache
        if let Some(sig) = ctx.sig {
            if let Some(tys) = &i.candidates {
                // TODO: use `get_covariant_inference`
                self.create_union_type(tys.to_vec(), ty::UnionReduction::Subtype)
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

    fn infer_from_matching_tys(
        &mut self,
        sources: ty::Tys<'cx>,
        targets: ty::Tys<'cx>,
        matches: impl Fn(&'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> bool,
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
                    .map(|t| *t)
                    .collect::<Vec<_>>();
                self.alloc(tys)
            }
        };

        (
            matched(&matched_sources, sources),
            matched(&matched_targets, targets),
        )
    }
    fn infer_from_tys(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) {
        if !self.could_contain_ty_var(target) {
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
    }

    fn infer_tys(
        &mut self,
        inference: InferenceContextId,
        arg_ty: &'cx ty::Ty<'cx>,
        param_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let inferences = &mut self.inference_contexts[inference.as_usize()].inferences;
        self.undefined_ty()
    }

    fn get_inferred_tys(&mut self, inference: InferenceContextId) -> ty::Tys<'cx> {
        let tys = (0..self.inference_contexts[inference.as_usize()]
            .inferences
            .len())
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
        let Some(ty_params) = sig.ty_params else {
            unreachable!()
        };

        for (idx, arg) in args.iter().enumerate() {
            let param_ty = self.get_ty_at_pos(sig, idx);
            if self.could_contain_ty_var(param_ty) {
                let arg_ty =
                    self.check_expr_with_contextual_ty(arg, param_ty, Some(inference), check_mode);
                self.infer_tys(inference, arg_ty, param_ty);
            }
        }

        self.get_inferred_tys(inference)
    }
}

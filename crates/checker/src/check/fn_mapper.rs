use super::ty;
use super::{InferenceContextId, TyChecker, links};

#[derive(Debug)]
pub(super) struct PermissiveMapper;

impl<'cx> ty::TyMap<'cx> for PermissiveMapper {
    fn get_mapped_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        checker: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_param() {
            checker.wildcard_ty
        } else {
            ty
        }
    }
}

#[derive(Debug)]
pub(super) struct RestrictiveMapper;

impl<'cx> ty::TyMap<'cx> for RestrictiveMapper {
    fn get_mapped_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        checker: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(param) = ty.kind.as_param() else {
            return ty;
        };
        let use_self = if let Some(c) = checker.param_ty_constraint(ty) {
            c == checker.no_constraint_ty()
        } else {
            checker.get_constraint_decl(ty).is_none()
        };
        if use_self {
            ty
        } else if let Some(restrictive_instantiation) =
            checker.common_ty_links_arena[ty.links].get_restrictive_instantiation()
        {
            restrictive_instantiation
        } else {
            let restrictive_instantiation = checker.create_param_ty(param.symbol, false);
            let no_constraint_ty = checker.no_constraint_ty();
            checker.ty_links.insert(
                restrictive_instantiation.id,
                links::TyLinks::default().with_param_ty_constraint(no_constraint_ty),
            );
            checker.common_ty_links_arena[ty.links]
                .set_restrictive_instantiation(restrictive_instantiation);
            restrictive_instantiation
        }
    }
}

#[derive(Debug)]
pub(super) struct NonFixingMapper<'cx> {
    pub(super) inference: InferenceContextId,
    pub(super) sources: ty::Tys<'cx>,
}

impl<'cx> ty::TyMap<'cx> for NonFixingMapper<'cx> {
    fn get_mapped_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        checker: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        for idx in 0..self.sources.len() {
            if self.sources[idx].id == ty.id {
                return checker.get_inferred_ty(self.inference, idx);
            }
        }
        ty
    }
}

#[derive(Debug)]
pub(super) struct FixingMapper<'cx> {
    pub(super) inference: InferenceContextId,
    pub(super) sources: ty::Tys<'cx>,
}

impl<'cx> ty::TyMap<'cx> for FixingMapper<'cx> {
    fn get_mapped_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        checker: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        for idx in 0..self.sources.len() {
            if self.sources[idx].id == ty.id {
                if !checker.inference_info(self.inference, idx).is_fixed {
                    // TODO: `inferFromIntraExpressionSites`
                    checker.clear_cached_inferences(self.inference);
                    checker.inferences[self.inference.as_usize()].inferences[idx].is_fixed = true;
                }
                return checker.get_inferred_ty(self.inference, idx);
            }
        }
        ty
    }
}

#[derive(Debug)]
pub(super) struct LazyInferredTyParamMapper<'cx> {
    pub(super) ty_reference: &'cx bolt_ts_ast::ReferTy<'cx>,
    pub(super) ty_params: ty::Tys<'cx>,
}

impl<'cx> ty::TyMap<'cx> for LazyInferredTyParamMapper<'cx> {
    fn get_mapped_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        checker: &mut TyChecker<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        for (idx, ty_param) in self.ty_params.iter().enumerate() {
            if ty.eq(ty_param) {
                // get_effective_ty_argument_at_index
                return if let Some(n) = self
                    .ty_reference
                    .ty_args
                    .and_then(|ty_args| ty_args.list.get(idx))
                {
                    checker.get_ty_from_type_node(n)
                } else {
                    checker
                        .get_effective_ty_args(self.ty_reference.id, self.ty_params)
                        .unwrap()[idx]
                };
            }
        }
        ty
    }
}

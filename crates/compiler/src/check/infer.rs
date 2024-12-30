use crate::ty::{self, TyMapper};
use crate::{ast, ty::Sig};

use super::TyChecker;

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
    pub fn create(
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

    pub fn create_fixing_mapper(&self, checker: &mut TyChecker<'cx>) -> &'cx TyMapper<'cx> {
        let sources = self
            .inferences
            .iter()
            .map(|i| i.ty_params)
            .collect::<Vec<_>>();
        let sources = checker.alloc(sources);
        let target = self
            .inferences
            .iter()
            .enumerate()
            .map(|(idx, _)| {
                // TODO: handle `!is_fixed`
                self.get_inferred_ty(idx, checker)
            })
            .collect::<Vec<_>>();
        let target = checker.alloc(target);
        checker.alloc(TyMapper::create(sources, target))
    }

    pub fn create_non_fixing_mapper(&self, checker: &mut TyChecker<'cx>) -> &'cx TyMapper<'cx> {
        let sources = self
            .inferences
            .iter()
            .map(|i| i.ty_params)
            .collect::<Vec<_>>();
        let sources = checker.alloc(sources);
        let target = self
            .inferences
            .iter()
            .enumerate()
            .map(|(idx, _)| self.get_inferred_ty(idx, checker))
            .collect::<Vec<_>>();
        let target = checker.alloc(target);
        checker.alloc(TyMapper::create(sources, target))
    }

    pub fn get_inferred_ty(&self, idx: usize, checker: &mut TyChecker<'cx>) -> &'cx ty::Ty<'cx> {
        let i = &self.inferences[idx];
        // TODO: cache
        if let Some(sig) = self.sig {
            if let Some(tys) = &i.candidates {
                // TODO: use `get_covariant_inference`
                checker.create_union_type(tys.to_vec(), ty::UnionReduction::Subtype)
            } else if let Some(tys) = &i.contra_candidates {
                todo!()
            } else {
                checker.undefined_ty()
            }
        } else {
            checker
                .get_ty_from_inference(i)
                .unwrap_or(checker.undefined_ty())
        }
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn infer_ty_args(
        &mut self,
        sig: &'cx Sig<'cx>,
        args: &'cx [&'cx ast::Expr<'cx>],
    ) -> ty::Tys<'cx> {
        let Some(ty_params) = sig.ty_params else {
            unreachable!()
        };
        let args = args
            .iter()
            .map(|arg| self.check_expr(arg))
            .collect::<Vec<_>>();

        let mut tys = vec![self.any_ty(); ty_params.len()];
        for (i, arg) in args.iter().enumerate() {
            let Some(param_ty) = self.get_ty_at_pos(sig, i) else {
                todo!("handle rest param")
            };
            let Some(param_ty) = param_ty.kind.as_param() else {
                // todo!("nested or non-param ty")
                continue;
            };
            let idx = param_ty.offset;
            if tys[idx] != self.any_ty() {
                tys[idx] = arg;
            } else {
                //todo!("error handle")
            }
        }
        self.alloc(tys)
    }
}

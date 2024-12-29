use crate::ty;
use crate::{ast, ty::Sig};

use super::TyChecker;

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
                todo!("nested or non-param ty")
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

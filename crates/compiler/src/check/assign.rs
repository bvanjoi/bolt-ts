use super::TyChecker;
use crate::bind::SymbolID;
use crate::ty;

impl<'cx> TyChecker<'cx> {
    fn assign_param_ty(&mut self, param: SymbolID, ctx: Option<&'cx ty::Ty<'cx>>) {
        if let Some(ty) = self.get_symbol_links(param).get_ty() {
            if let Some(ctx) = ctx {
                // assert_eq!(ctx, ty, "Parameter symbol already has a cached type which differs from newly assigned type")
            }
            return;
        }
        let decl = param.decl(self.binder);
        let ty = if let Some(ctx) = ctx {
            ctx
        } else {
            self.get_widened_ty_for_var_like_decl(self.p.node(decl).expect_param_decl())
        };
        self.get_mut_symbol_links(param).set_ty(ty);
    }

    pub fn assign_contextual_param_tys(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        context: &'cx ty::Sig<'cx>,
    ) {
        let resolved_sig_ty_params = if context.ty_params.is_some() {
            if sig.ty_params.is_none() {
                context.ty_params
            } else {
                return;
            }
        } else {
            sig.ty_params
        };

        let len = sig.params.len() - (if sig.has_rest_param() { 1 } else { 0 });
        for i in 0..len {
            let param = sig.params[i];
            let decl = param.decl(self.binder);
            let decl = self.p.node(decl).expect_param_decl();
            if decl.ty.is_none() {
                let ty = self.try_get_ty_at_pos(context, i);
                if let Some(ty) = ty {
                    if decl.init.is_some() {
                        todo!()
                    }
                }
                self.assign_param_ty(param, ty);
            }
        }

        if sig.has_rest_param() {
            todo!()
        }
    }
}

use super::TyChecker;
use crate::bind::SymbolID;
use crate::ty;

impl<'cx> TyChecker<'cx> {
    fn assign_param_ty(&mut self, param: SymbolID, ctx: Option<&'cx ty::Ty<'cx>>) {
        if let Some(ty) = self.get_symbol_links(param).get_ty() {
            if let Some(ctx) = ctx {
                assert_eq!(
                    ctx, ty,
                    "Parameter symbol already has a cached type which differs from newly assigned type"
                )
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

    pub(super) fn assign_contextual_param_tys(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        context: &'cx ty::Sig<'cx>,
    ) {
        let ty_params = if context.ty_params.is_some() {
            if sig.ty_params.is_none() {
                // TODO: store context ty_params into sig.
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
                let mut ty = self.try_get_ty_at_pos(context, i);
                if let Some(t) = ty {
                    if decl.init.is_some() {
                        let init_ty = self.check_decl_init(decl, None);
                        if !self.is_type_assignable_to(init_ty, t) && {
                            let target = self.widened_ty_from_init(decl, init_ty);
                            self.is_type_assignable_to(t, target)
                        } {
                            ty = Some(init_ty);
                        }
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

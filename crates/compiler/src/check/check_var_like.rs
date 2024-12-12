use super::TyChecker;
use crate::ir;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_var_like_decl(&mut self, decl: &'cx impl ir::VarLike<'cx>) {
        let symbol = self.get_symbol_of_decl(decl.id());
        let decl_ty = self.get_type_of_symbol(symbol);
        if let Some(init) = decl.init() {
            let init_ty = self.check_expr_with_cache(init);
            if decl_ty != init_ty {
                self.check_type_assignable_to_and_optionally_elaborate(
                    decl.name().span,
                    init_ty,
                    decl_ty,
                );
            }
        }
    }
}

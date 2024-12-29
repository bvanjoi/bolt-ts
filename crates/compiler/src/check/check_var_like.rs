use super::TyChecker;
use crate::ir;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_var_like_decl(&mut self, decl: &'cx impl ir::VarLike<'cx>) {
        let symbol = self.get_symbol_of_decl(decl.id());
        let decl_ty = self.get_type_of_symbol(symbol);
        if let Some(init) = decl.init() {
            // if self.atoms.get(decl.name().name) == "x" {
            //     dbg!(123);
            // }
            let init_ty = self.check_expr_with_cache(init);
            assert!(
                decl.decl_ty().is_none()
                    || self
                        .node_links
                        .get(&init.id())
                        .unwrap()
                        .get_resolved_ty()
                        .is_some(),
            );
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

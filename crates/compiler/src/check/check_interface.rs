use crate::ast;

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_interface_decl(&mut self, interface: &'cx ast::InterfaceDecl<'cx>) {
        let symbol = self.get_symbol_of_decl(interface.id);

        let i = self.binder.symbol(symbol).expect_interface();
        if i.decl == interface.id {
            let ty = self.get_declared_ty_of_symbol(symbol);
            for base_ty in self.base_types(ty) {
                self.check_type_assignable_to(ty, base_ty, Some(true));
            }
            self.check_index_constraints(ty, symbol);
        }
    }
}

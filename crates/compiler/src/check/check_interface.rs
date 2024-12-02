use crate::ast;

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_interface_decl(&mut self, interface: &'cx ast::InterfaceDecl<'cx>) {
        let symbol = self.get_symbol_of_decl(interface.id);
        let ty = self.get_declared_ty_of_symbol(interface.id.module(), symbol);
        self.check_index_constraints(ty, interface.id.module(), symbol);
    }
}

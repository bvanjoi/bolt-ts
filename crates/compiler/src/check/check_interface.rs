use bolt_ts_ast as ast;

use crate::ty;

use super::{Ternary, TyChecker, errors};

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_interface_decl(&mut self, interface: &'cx ast::InterfaceDecl<'cx>) {
        let symbol = self.get_symbol_of_decl(interface.id);

        let i = self.binder.symbol(symbol).expect_interface();
        let decl_id = i.decls[0];
        if decl_id == interface.id {
            let ty = self.get_declared_ty_of_symbol(symbol);
            self.resolve_structured_type_members(ty);
            let ty_with_this = self.get_ty_with_this_arg(ty, None);
            if self.check_inherited_props_are_identical(ty) {
                for base_ty in self.base_types(ty) {
                    let target = {
                        let this_ty = if let Some(r) = ty.kind.as_object_reference() {
                            r.target.kind.expect_object_interface().this_ty
                        } else {
                            ty.kind.expect_object_interface().this_ty
                        };
                        self.get_ty_with_this_arg(base_ty, this_ty)
                    };
                    let res = self.check_type_assignable_to(ty_with_this, target, Some(decl_id));
                    if res == Ternary::FALSE {
                        let error = errors::Interface0IncorrectlyExtendsInterface1 {
                            span: interface.name.span,
                            base: base_ty.to_string(self),
                            derived: ty.to_string(self),
                        };
                        self.push_error(Box::new(error));
                    }
                }
                self.check_index_constraints(ty, symbol);
            }
        }
    }

    fn check_inherited_props_are_identical(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        let base_tys = self.base_types(ty);
        if base_tys.len() < 2 {
            return true;
        }
        // TODO:
        true
    }
}

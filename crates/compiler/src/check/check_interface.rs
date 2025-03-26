use bolt_ts_ast as ast;
use bolt_ts_utils::fx_hashmap_with_capacity;

use crate::{bind, ty};

use super::symbol_info::SymbolInfo;
use super::{Ternary, TyChecker, errors};

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_interface_decl(&mut self, interface: &'cx ast::InterfaceDecl<'cx>) {
        let symbol = self.get_symbol_of_decl(interface.id);

        let first_interface_decl = self
            .binder
            .symbol(symbol)
            .get_declaration_of_kind(|n| self.p.node(n).is_interface_decl())
            .unwrap();
        if first_interface_decl == interface.id {
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
                    let res = self.check_type_assignable_to(
                        ty_with_this,
                        target,
                        Some(first_interface_decl),
                    );
                    if res == Ternary::FALSE {
                        let error = errors::Interface0IncorrectlyExtendsInterface1 {
                            span: interface.name.span,
                            base: base_ty.to_string(self),
                            derived: ty.to_string(self),
                        };
                        self.push_error(Box::new(error));
                    }
                }
                self.check_index_constraints(ty, false);
            }
        }

        self.check_object_ty_for_duplicate_decls(interface.members);
    }

    pub(super) fn check_object_ty_for_duplicate_decls(
        &mut self,
        members: ast::ObjectTyMembers<'cx>,
    ) {
        let mut names = fx_hashmap_with_capacity(members.len());
        for member in members {
            match member.kind {
                ast::ObjectTyMemberKind::Prop(p) => {
                    let name = p.name;
                    let member_name = match name.kind {
                        ast::PropNameKind::Ident(ident) => ident.name,
                        ast::PropNameKind::StringLit { key, .. } => key,
                        _ => continue,
                    };
                    if let Some(old) = names.get(&member_name).copied() {
                        let error = bind::errors::DuplicateIdentifier {
                            span: p.span,
                            name: self.atoms.get(member_name).to_string(),
                            original_span: old,
                        };
                        self.push_error(Box::new(error));
                    } else {
                        names.insert(member_name, p.span);
                    }
                }
                _ => {}
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

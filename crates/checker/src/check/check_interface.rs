use bolt_ts_ast as ast;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use crate::ty;

use super::symbol_info::SymbolInfo;
use super::{TyChecker, errors};

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_interface_decl(&mut self, interface: &'cx ast::InterfaceDecl<'cx>) {
        if let Some(ty_params) = interface.ty_params {
            self.check_ty_params(ty_params);
        }

        let symbol = self.get_symbol_of_decl(interface.id);

        let first_interface_decl = self
            .binder
            .symbol(symbol)
            .get_declaration_of_kind(|n| self.p.node(n).is_interface_decl())
            .unwrap();

        if first_interface_decl == interface.id {
            let ty = self.get_declared_ty_of_symbol(symbol);
            self.resolve_structured_type_members(ty);
            let ty_with_this = self.get_ty_with_this_arg(ty, None, false);
            if self.check_inherited_props_are_identical(ty, interface.name) {
                for base_ty in self.get_base_tys(ty) {
                    let target = {
                        let this_ty = if let Some(r) = ty.kind.as_object_reference() {
                            r.target.kind.expect_object_interface().this_ty
                        } else {
                            ty.kind.expect_object_interface().this_ty
                        };
                        self.get_ty_with_this_arg(base_ty, this_ty, false)
                    };
                    let res = self.check_type_assignable_to(
                        ty_with_this,
                        target,
                        Some(first_interface_decl),
                    );
                    if !res {
                        let error = errors::InterfaceDerivedIncorrectlyExtendsInterfaceBase {
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

        for member in interface.members {
            self.check_object_ty_member(member);
        }
    }

    fn check_object_ty_member(&mut self, member: &'cx ast::ObjectTyMember<'cx>) {
        match member.kind {
            ast::ObjectTyMemberKind::Prop(n) => self.check_var_like_decl(n),
            _ => {
                // TODO:
            }
        }
    }

    pub(super) fn check_object_ty_for_duplicate_decls(
        &mut self,
        members: ast::ObjectTyMembers<'cx>,
    ) {
        let mut names = fx_hashmap_with_capacity(members.len());
        for member in members {
            if let ast::ObjectTyMemberKind::Prop(p) = member.kind {
                let name = p.name;
                let member_name = match name.kind {
                    ast::PropNameKind::Ident(ident) => ident.name,
                    ast::PropNameKind::StringLit { key, .. } => key,
                    _ => continue,
                };
                if let Some(old) = names.get(&member_name).copied() {
                    let error = bolt_ts_binder::errors::DuplicateIdentifier {
                        span: p.span,
                        name: self.atoms.get(member_name).to_string(),
                        original_span: old,
                    };
                    self.push_error(Box::new(error));
                } else {
                    names.insert(member_name, p.span);
                }
            }
        }
    }

    fn check_inherited_props_are_identical(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        ty_node: &'cx ast::Ident,
    ) -> bool {
        let base_tys = self.get_base_tys(ty);
        if base_tys.len() < 2 {
            return true;
        }

        self.resolve_structured_type_members(ty);
        let i = if let Some(r) = ty.kind.as_object_reference() {
            r.target.kind.expect_object_interface()
        } else {
            ty.kind.expect_object_interface()
        };
        let mut seen: FxHashMap<_, _> = self.interface_ty_links_arena[i.links]
            .expect_declared_members()
            .props
            .iter()
            .map(|&symbol| {
                let p = self.symbol(symbol);
                (p.name, (symbol, ty))
            })
            .collect();
        let mut ok = true;
        for base in base_tys {
            let props = {
                let ty = self.get_ty_with_this_arg(base, i.this_ty, false);
                self.get_props_of_ty(ty)
            };
            for prop in props {
                let s = self.symbol(*prop);
                let name = s.name;
                if let Some(existing) = seen.get(&name) {
                    let is_inherited_prop = existing.1 != ty;
                    if is_inherited_prop && !self.is_prop_identical_to(existing.0, *prop) {
                        ok = false;
                        let error = errors::InterfaceCannotSimultaneouslyExtendTypes1And2 {
                            span: ty_node.span,
                            interface: ty.to_string(self),
                            ty1: existing.1.to_string(self),
                            ty2: base.to_string(self),
                        };
                        self.push_error(Box::new(error));
                    }
                } else {
                    seen.insert(name, (*prop, base));
                }
            }
        }
        ok
    }
}

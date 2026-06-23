use bolt_ts_ast as ast;
use bolt_ts_binder::SymbolID;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use super::ty;
use super::{TyChecker, errors};

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_interface_declaration(&mut self, interface: &'cx ast::InterfaceDecl<'cx>) {
        self.check_type_parameters(interface.ty_params);

        self.check_exports_on_merged_decls(interface.id);

        let symbol = self.get_symbol_of_declaration(interface.id);
        self.check_ty_param_lists_identical(symbol);

        let first_interface_decl = self
            .binder
            .symbol(symbol)
            .get_declaration_of_kind(|n| self.p.node(n).is_interface_decl())
            .unwrap();

        if first_interface_decl == interface.id {
            let ty = self.get_declared_ty_of_symbol(symbol);
            self.resolve_structured_type_members(ty);
            let ty_with_this = self.get_type_with_this_argument::<false>(ty, None);
            if self.check_inherited_props_are_identical(ty, interface.name) {
                for base_ty in self.get_base_tys(ty) {
                    let target = {
                        let this_ty = if let Some(r) = ty.kind.as_object_reference() {
                            r.target.kind.expect_object_interface().this_ty
                        } else {
                            ty.kind.expect_object_interface().this_ty
                        };
                        self.get_type_with_this_argument::<false>(base_ty, this_ty)
                    };
                    self.check_type_assignable_to(
                        ty_with_this,
                        target,
                        Some(first_interface_decl),
                        Some(|this: &mut Self| {
                            let error = errors::InterfaceDerivedIncorrectlyExtendsInterfaceBase {
                                span: interface.name.span,
                                base: this.print_ty(base_ty, None).to_string(),
                                derived: this.print_ty(ty, None).to_string(),
                            };
                            this.push_error(Box::new(error));
                        }),
                    );
                }
                self.check_index_constraints::<false>(ty, symbol);
            }
        }

        self.check_object_ty_for_duplicate_decls(interface.members);

        for member in interface.members {
            self.check_object_ty_member(member);
        }

        self.check_ty_for_duplicate_index_sigs_of_interface_declaration(interface);
    }

    fn check_ty_for_duplicate_index_sigs_of_interface_declaration(
        &mut self,
        node: &'cx ast::InterfaceDecl<'cx>,
    ) {
        let node_symbol = self.get_symbol_of_declaration(node.id);
        let s = self.binder.symbol(node_symbol);
        if let Some(decls) = s.decls.as_ref()
            && !decls.is_empty()
            && decls[0] != node.id
        {
            return;
        }
        if let Some(index_symbol) = self.get_index_symbol(node_symbol) {
            self.check_ty_for_duplicate_index_sigs_worker(index_symbol);
        }
    }

    pub(super) fn check_ty_for_duplicate_index_sigs_worker(&mut self, index_symbol: SymbolID) {
        let s = self.symbol(index_symbol);
        let Some(decls) = s.decls.as_ref() else {
            return;
        };
        let mut index_signature_map: FxHashMap<&'cx ty::Ty<'cx>, Vec<ast::NodeID>> =
            fx_hashmap_with_capacity(decls.len());
        for declaration in decls.clone() {
            let n = self.p.node(declaration);
            if let Some(n) = n.as_index_sig_decl() {
                let ty = self.get_ty_from_type_node(n.key_ty);
                self.for_each_ty(ty, |_, ty| match index_signature_map.get_mut(ty) {
                    Some(n) => n.push(declaration),
                    None => {
                        index_signature_map.insert(ty, vec![declaration]);
                    }
                });
            }
        }
        for item in index_signature_map {
            if item.1.len() > 1 {
                for declaration in item.1 {
                    let error = errors::DuplicateIndexSignatureForTypeX {
                        span: self.p.node(declaration).span(),
                        ty: self.print_ty(item.0, None).to_string(),
                    };
                    self.push_error(Box::new(error));
                }
            }
        }
    }

    fn check_object_ty_member(&mut self, member: &'cx ast::ObjectTyMember<'cx>) {
        match member.kind {
            ast::ObjectTyMemberKind::Prop(n) => {
                let decl_name = ast::DeclarationName::from_prop_name(n.name);
                self.check_invalid_dynamic_name(decl_name, |this| {
                    let error = errors::AComputedPropertyNameInAnInterfaceMustReferToAnExpressionWhoseTypeIsALiteralTypeOrAUniqueSymbolType {
                        span: n.name.span(),
                    };
                    this.push_error(Box::new(error));
                });
                self.check_var_like_decl(n)
            }
            ast::ObjectTyMemberKind::Method(n) => {
                // check_method_declaration
                self.check_fn_like_decl(n);
            }
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
                let ty = self.get_type_with_this_argument::<false>(base, i.this_ty);
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
                            interface: self.print_ty(ty, None).to_string(),
                            ty1: self.print_ty(existing.1, None).to_string(),
                            ty2: self.print_ty(base, None).to_string(),
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

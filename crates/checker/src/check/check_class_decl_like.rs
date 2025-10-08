use super::symbol_info::SymbolInfo;
use super::{TyChecker, errors};
use crate::ty;
use crate::ty::TypeFlags;

use bolt_ts_ast::r#trait::ClassLike;
use bolt_ts_ast::{self as ast, pprint_ident};
use bolt_ts_atom::Atom;
use bolt_ts_binder::SymbolID;
use bolt_ts_utils::{fx_hashmap_with_capacity, no_hashmap_with_capacity};

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct DeclarationMeaning: u8 {
        const GET_ACCESSOR          = 1 << 0;
        const SET_ACCESSOR          = 1 << 1;
        const PROPERTY_ASSIGNMENT   = 1 << 2;
        const METHOD                = 1 << 3;
        const PRIVATE_STATIC        = 1 << 4;
        const GET_OR_SET_ACCESSOR   = Self::GET_ACCESSOR.bits() | Self::SET_ACCESSOR.bits();
        const PROPERTY_ASSIGNMENT_OR_METHOD = Self::PROPERTY_ASSIGNMENT.bits() | Self::METHOD.bits();
    }
}

impl<'cx> TyChecker<'cx> {
    fn class_decl_extends_null(&mut self, class: ast::NodeID) -> bool {
        let class_symbol = self.get_symbol_of_decl(class);
        let class_instance_ty = self.get_declared_ty_of_symbol(class_symbol);
        let base_ctor_ty = self.get_base_constructor_type_of_class(class_instance_ty);
        base_ctor_ty == self.null_widening_ty
    }

    fn find_first_super_call_in_ctor_body(
        &self,
        ctor: &'cx ast::ClassCtor<'cx>,
    ) -> Option<&'cx ast::CallExpr<'cx>> {
        let body = ctor.body?;

        struct FindFirstSuperCall<'cx> {
            ret: Option<&'cx ast::CallExpr<'cx>>,
        }

        use ast::visitor::Visitor;
        impl<'cx> Visitor<'cx> for FindFirstSuperCall<'cx> {
            fn visit_fn_decl(&mut self, _: &'cx bolt_ts_ast::FnDecl<'cx>) {}
            fn visit_class_decl(&mut self, _: &'cx bolt_ts_ast::ClassDecl<'cx>) {}
            fn visit_call_expr(&mut self, node: &'cx bolt_ts_ast::CallExpr<'cx>) {
                if let ast::ExprKind::Super(_) = node.expr.kind {
                    debug_assert!(self.ret.is_none());
                    self.ret = Some(node);
                    return;
                }
                ast::visitor::visit_call_expr(self, node);
            }
            fn visit_block_stmt(&mut self, node: &'cx bolt_ts_ast::BlockStmt<'cx>) {
                for stmt in node.stmts {
                    self.visit_stmt(stmt);
                    if self.ret.is_some() {
                        break;
                    }
                }
            }
        }

        let mut v = FindFirstSuperCall { ret: None };
        v.visit_block_stmt(body);

        v.ret
    }

    fn check_ctor(&mut self, ctor: &'cx ast::ClassCtor<'cx>) {
        self.check_fn_like_decl(ctor);

        let containing_class_decl = self.parent(ctor.id).unwrap();
        let extends = match self.p.node(containing_class_decl) {
            ast::Node::ClassExpr(c) => c.extends,
            ast::Node::ClassDecl(c) => c.extends,
            _ => unreachable!(),
        };
        if let Some(extends) = extends {
            let extends_null = self.class_decl_extends_null(containing_class_decl);
            if let Some(first_super_call) = self.find_first_super_call_in_ctor_body(ctor) {
                // TODO:
            } else if !extends_null {
                let error = errors::ConstructorsForDerivedClassesMustContainASuperCall {
                    span: ctor.name_span,
                };
                self.push_error(Box::new(error));
            }
        }
    }

    fn check_class_method_elem(&mut self, method: &'cx ast::ClassMethodElem<'cx>) {
        self.check_fn_like_decl(method);
    }

    fn check_class_prop_ele(&mut self, prop: &'cx ast::ClassPropElem<'cx>) {
        self.check_var_like_decl(prop);
    }

    fn issue_member_spec_error(
        &mut self,
        class: &impl ClassLike<'cx>,
        ty_with_this: &'cx ty::Ty<'cx>,
        base_with_this: &'cx ty::Ty<'cx>,
    ) {
        let mut issued_member_error = false;
        for member in class.elems().list {
            if member.kind.is_static() {
                continue;
            }
            use bolt_ts_ast::ClassElemKind::*;
            let member_name = match member.kind {
                Ctor(_) => None,
                Prop(n) => Some(n.name.id()),
                Method(n) => Some(n.name.id()),
                IndexSig(_) => None,
                Getter(n) => Some(n.name.id()),
                Setter(n) => Some(n.name.id()),
                StaticBlockDecl(_) => None,
            };

            let declared_prop = member_name
                .and_then(|name| self.get_symbol_at_loc(name))
                .or_else(|| self.get_symbol_at_loc(member.kind.id()));

            if let Some(declared_prop) = declared_prop
                && let name = self.binder.symbol(declared_prop).name
                && let Some(prop) = self.get_prop_of_ty(ty_with_this, name)
                && let Some(base_prop) = self.get_prop_of_ty(base_with_this, name)
                && let prop_ty = self.get_type_of_symbol(prop)
                && let base_prop_ty = self.get_type_of_symbol(base_prop)
                && !self.check_type_assignable_to(prop_ty, base_prop_ty, member_name)
            {
                let span = self.p.node(member_name.unwrap()).span();
                let error = errors::TypeIsNotAssignableToType {
                    span,
                    ty1: self.print_ty(prop_ty).to_string(),
                    ty2: self.print_ty(base_prop_ty).to_string(),
                };
                self.push_error(Box::new(error));
                issued_member_error = true;
            }
        }

        if !issued_member_error {
            self.check_type_assignable_to(ty_with_this, base_with_this, Some(class.id()));
        }
    }

    fn check_class_for_duplicate_decls(&mut self, class: &impl ClassLike<'cx>) {
        let mut instance_names = no_hashmap_with_capacity::<Atom, DeclarationMeaning>(8);
        let mut static_names = no_hashmap_with_capacity::<Atom, DeclarationMeaning>(8);
        let mut private_names = no_hashmap_with_capacity::<Atom, DeclarationMeaning>(8);

        let add_name = |this: &mut Self,
                        names: &mut nohash_hasher::IntMap<Atom, DeclarationMeaning>,
                        prop_name: &ast::PropName,
                        prop_name_atom: Atom,
                        meaning: DeclarationMeaning| {
            match names.get(&prop_name_atom) {
                Some(prev) => {
                    if prev.contains(DeclarationMeaning::PRIVATE_STATIC)
                        != meaning.contains(DeclarationMeaning::PRIVATE_STATIC)
                    {
                        todo!("error handle")
                    } else {
                        let prev_is_method = prev.contains(DeclarationMeaning::METHOD);
                        let is_method = meaning.contains(DeclarationMeaning::METHOD);
                        if prev_is_method || is_method {
                            if prev_is_method != is_method {
                                // todo!("error handle")
                            }
                        } else if prev
                            .intersection(meaning)
                            .intersects(DeclarationMeaning::PRIVATE_STATIC.complement())
                        {
                            let error = errors::DuplicateIdentifierX {
                                span: prop_name.span(),
                                ident: this.atoms.get(prop_name_atom).to_string(),
                            };
                            this.push_error(Box::new(error));
                        } else {
                            names.insert(prop_name_atom, *prev | meaning);
                        }
                    }
                }
                None => {
                    names.insert(prop_name_atom, meaning);
                }
            }
        };

        for elem in class.elems().list {
            if let ast::ClassElemKind::Ctor(ctor) = elem.kind {
                for param in ctor.params {
                    // TODO:
                }
            } else if let Some(name) = elem.kind.name()
                && let Some(member_name) = name.kind.get_name(&mut self.atoms)
            {
                let is_static = elem.kind.is_static();
                let is_private = false; // TODO:
                let names = if is_private {
                    &mut private_names
                } else if is_static {
                    &mut static_names
                } else {
                    &mut instance_names
                };
                let meaning = match elem.kind {
                    ast::ClassElemKind::Getter(_) => DeclarationMeaning::GET_ACCESSOR,
                    ast::ClassElemKind::Setter(_) => DeclarationMeaning::SET_ACCESSOR,
                    ast::ClassElemKind::Prop(_) => DeclarationMeaning::GET_OR_SET_ACCESSOR,
                    ast::ClassElemKind::Method(_) => DeclarationMeaning::METHOD,
                    _ => unreachable!(),
                };
                add_name(self, names, name, member_name, meaning)
            }
        }
    }

    pub(super) fn check_class_like_decl(&mut self, class: &impl ClassLike<'cx>) {
        let symbol = self.get_symbol_of_decl(class.id());

        if let Some(ty_params) = class.ty_params() {
            self.check_ty_params(ty_params);
        }

        let ty = self.get_declared_ty_of_symbol(symbol);
        let ty_with_this = self.get_ty_with_this_arg(ty, None, false);
        let static_ty = self.get_type_of_symbol(symbol);
        self.check_class_for_duplicate_decls(class);
        self.check_index_constraints(ty, false);

        if let Some(base_ty_node) = self.get_effective_base_type_node(class.id()) {
            let base_ctor_ty = self.get_base_constructor_type_of_class(ty);
            let static_base_ty = self.get_apparent_ty(base_ctor_ty);
            // check base type accessibility
            let sigs = self.get_signatures_of_type(static_base_ty, ty::SigKind::Constructor);
            if !sigs.is_empty()
                && let Some(decl) = sigs[0].node_id
                && self
                    .p
                    .node(decl)
                    .has_effective_modifier(bolt_ts_ast::ModifierKind::Private)
                && let Some(class_decl) =
                    self.get_class_like_decl_of_symbol(static_base_ty.symbol().unwrap())
                && !self
                    .node_query(base_ty_node.id.module())
                    .is_node_within_class(base_ty_node.id, class_decl)
            {
                let error = errors::CannotExtendAClass0ClassConstructorIsMarkedAsPrivate {
                    span: base_ty_node.expr_with_ty_args.span,
                    class: pprint_ident(self.p.node(class_decl).ident_name().unwrap(), &self.atoms),
                };
                self.push_error(Box::new(error));
            }

            let base_tys = self.get_base_tys(ty);
            if !base_tys.is_empty() {
                let base_ty = base_tys[0];
                // check_kinds_of_property_member_overrides
                let base_properties = self.get_props_of_ty(base_ty);
                struct MemberInfo<'cx> {
                    missed_props: Vec<SymbolID>,
                    base_ty: &'cx ty::Ty<'cx>,
                    ty: &'cx ty::Ty<'cx>,
                }
                let mut not_implemented_info = fx_hashmap_with_capacity(0);
                'base_prop_check: for base_prop in base_properties {
                    let base = self.get_target_symbol(*base_prop);
                    let base_s = self.symbol(base);
                    if base_s
                        .flags
                        .contains(bolt_ts_binder::SymbolFlags::PROTOTYPE)
                    {
                        continue;
                    }
                    let base_s_name = base_s.name;
                    let Some(base_s_in_type) = self.get_prop_of_object_ty(ty, base_s_name) else {
                        continue;
                    };
                    let derived = self.get_target_symbol(base_s_in_type);
                    let base_declaration_flags =
                        self.get_declaration_modifier_flags_from_symbol(base, None);
                    if derived == base {
                        let derived_class_decl =
                            self.get_class_like_decl_of_symbol(ty.symbol().unwrap());
                        if base_declaration_flags.contains(ast::ModifierKind::Abstract)
                            && derived_class_decl.is_none_or(|derived_class_decl| {
                                !self
                                    .p
                                    .node(derived_class_decl)
                                    .has_syntactic_modifier(ast::ModifierKind::Abstract.into())
                            })
                        {
                            for other_base_ty in self.get_base_tys(ty) {
                                if base_ty.eq(other_base_ty) {
                                    continue;
                                }
                                let base_s_in_type =
                                    self.get_prop_of_object_ty(other_base_ty, base_s_name);
                                let derived_elsewhere =
                                    base_s_in_type.map(|s| self.get_target_symbol(s));
                                if derived_elsewhere.is_some_and(|d| d != base) {
                                    continue 'base_prop_check;
                                }
                            }
                            not_implemented_info.insert(
                                derived_class_decl,
                                MemberInfo {
                                    missed_props: vec![*base_prop],
                                    base_ty,
                                    ty,
                                },
                            );
                        }
                    } else {
                        // TODO:
                    }
                }

                for (error_node, member_info) in not_implemented_info {
                    if member_info.missed_props.len() == 1 {
                        if let Some(error_node) = error_node
                            && let decl = self.p.node(error_node)
                            && decl.is_class_expr()
                        {
                            let error = errors::NonAbstractClassExpressionDoesNotImplementInheritedAbstractMember0FromClass1 {
                                span: class.span(),
                                member:  self.symbol(member_info.missed_props[0]).name.to_string(&self.atoms),
                                class: base_ty.to_string(self),
                            };
                            self.push_error(Box::new(error));
                        }
                    }
                }
                // ====
            }
        }

        if let Some(impls) = class.implements() {
            for ty_ref_node in impls.list {
                self.check_ty_refer_ty(ty_ref_node);
                let t = {
                    let t = self.get_ty_from_ty_reference(*ty_ref_node);
                    self.get_reduced_ty(t)
                };
                if t == self.error_ty {
                    continue;
                } else if t.flags.intersects(TypeFlags::PRIMITIVE) {
                    let error = errors::AClassCannotImplementAPrimTy {
                        span: ty_ref_node.span,
                        ty: self.print_ty(t).to_string(),
                    };
                    self.push_error(Box::new(error));
                } else if self.is_valid_base_ty(t) {
                    let this_arg = ty
                        .kind
                        .expect_object_reference()
                        .target
                        .kind
                        .expect_object_interface()
                        .this_ty;
                    let base_with_this = self.get_ty_with_this_arg(t, this_arg, false);
                    if !self.check_type_assignable_to(ty_with_this, base_with_this, None) {
                        self.issue_member_spec_error(class, ty_with_this, base_with_this);
                    }
                } else {
                    let error = errors::AClassCanOnlyImplementAnObjectTypeOrIntersectionOfObjectTypesWithStaticallyKnownMembers {
                        span: ty_ref_node.span,
                    };
                    self.push_error(Box::new(error));
                }
            }
        }

        for ele in class.elems().list {
            use bolt_ts_ast::ClassElemKind::*;
            match ele.kind {
                Prop(n) => self.check_class_prop_ele(n),
                Method(n) => self.check_class_method_elem(n),
                Ctor(n) => self.check_ctor(n),
                IndexSig(_) => {}
                Getter(n) => self.check_getter_decl(n),
                Setter(n) => self.check_accessor_decl(n),
                StaticBlockDecl(n) => {
                    self.check_block(n.body);
                }
            }
        }
    }
}

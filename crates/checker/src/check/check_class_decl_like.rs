use super::symbol_info::SymbolInfo;
use super::{TyChecker, errors};
use crate::ty;
use crate::ty::TypeFlags;

use bolt_ts_ast::r#trait::ClassLike;
use bolt_ts_ast::{self as ast, pprint_ident};
use bolt_ts_atom::Atom;
use bolt_ts_utils::no_hashmap_with_capacity;

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
    fn check_ctor(&mut self, ctor: &'cx ast::ClassCtor<'cx>) {
        self.check_fn_like_decl(ctor);
    }

    fn check_class_method_ele(&mut self, method: &'cx ast::ClassMethodElem<'cx>) {
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
                StaticBlock(_) => None,
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
                Method(n) => self.check_class_method_ele(n),
                Ctor(n) => self.check_ctor(n),
                IndexSig(_) => {}
                Getter(n) => self.check_getter_decl(n),
                Setter(n) => self.check_accessor_decl(n),
                StaticBlock(n) => {
                    self.check_block(n.body);
                }
            }
        }
    }
}

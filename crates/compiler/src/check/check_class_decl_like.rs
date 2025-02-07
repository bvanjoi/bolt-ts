use super::{errors, Ternary, TyChecker};
use crate::ir::ClassLike;
use crate::ty::TypeFlags;
use crate::{ast, ty};

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

    fn is_valid_base_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if let Some(param_ty) = ty.kind.as_param() {
            if let Some(constraint) = self.get_base_constraint_of_ty(ty) {
                return self.is_valid_base_ty(constraint);
            }
        }

        if ty.kind.is_object()
            || ty
                .flags
                .intersects(TypeFlags::NON_PRIMITIVE | TypeFlags::ANY)
        {
            true
            // TODO: !is_generic_mapped_ty
        } else {
            ty.kind.is_intersection()
        }
    }

    fn issue_member_spec_error(
        &mut self,
        class: &impl ClassLike<'cx>,
        ty_with_this: &'cx ty::Ty<'cx>,
        base_with_this: &'cx ty::Ty<'cx>,
    ) {
        let mut issued_member_error = false;
        for member in class.elems().elems {
            if member.kind.is_static() {
                continue;
            }
            use ast::ClassEleKind::*;
            let member_name = match member.kind {
                Ctor(_) => None,
                Prop(n) => Some(n.name.id()),
                Method(n) => Some(n.name.id()),
                IndexSig(_) => None,
                Getter(n) => Some(n.name.id()),
                Setter(n) => Some(n.name.id()),
            };

            let declared_prop = member_name
                .and_then(|name| self.get_symbol_at_loc(name))
                .or_else(|| self.get_symbol_at_loc(member.kind.id()));

            if let Some(declared_prop) = declared_prop {
                let name = self.binder.symbol(declared_prop).name;
                let prop = self.get_prop_of_ty(ty_with_this, name);
                let base_prop = self.get_prop_of_ty(base_with_this, name);
                if let Some(prop) = prop {
                    if let Some(base_prop) = base_prop {
                        let prop_ty = self.get_type_of_symbol(prop);
                        let base_prop_ty = self.get_type_of_symbol(base_prop);
                        if self.check_type_assignable_to(prop_ty, base_prop_ty, member_name)
                            == Ternary::FALSE
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
                }
            }
        }

        if !issued_member_error {
            self.check_type_assignable_to(ty_with_this, base_with_this, Some(class.id()));
        }
    }

    pub(super) fn check_class_decl_like(&mut self, class: &impl ClassLike<'cx>) {
        let symbol = self.get_symbol_of_decl(class.id());

        if let Some(ty_params) = class.ty_params() {
            self.check_ty_params(ty_params);
        }

        let ty = self.get_declared_ty_of_symbol(symbol);
        let ty_with_this = self.get_ty_with_this_arg(ty, None);
        let static_ty = self.get_type_of_symbol(symbol);
        self.check_index_constraints(ty, symbol);

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
                    let base_with_this = self.get_ty_with_this_arg(t, this_arg);
                    if self.check_type_assignable_to(ty_with_this, base_with_this, None)
                        == Ternary::FALSE
                    {
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

        for ele in class.elems().elems {
            use ast::ClassEleKind::*;
            match ele.kind {
                Prop(prop) => self.check_class_prop_ele(prop),
                Method(method) => self.check_class_method_ele(method),
                IndexSig(_) => {}
                Ctor(ctor) => self.check_ctor(ctor),
                Getter(_) => {}
                Setter(_) => {}
            }
        }
    }
}

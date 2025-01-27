use super::{errors, TyChecker};
use crate::ir::ClassLike;
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

    fn is_valid_base_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        if let Some(param_ty) = ty.kind.as_param() {
            if let Some(constraint) = self.get_base_constraint_of_ty(ty) {
                return self.is_valid_base_ty(constraint);
            }
        }

        if ty.kind.is_object() || ty == self.non_primitive_ty() || ty.kind.is_any() {
            true
            // TODO: !is_generic_mapped_ty
        } else if ty.kind.is_intersection() {
            true
        } else {
            false
        }
    }

    pub(super) fn check_class_decl_like(&mut self, class: &impl ClassLike<'cx>) {
        let symbol = self.get_symbol_of_decl(class.id());

        if let Some(ty_params) = class.ty_params() {
            self.check_ty_params(ty_params);
        }

        let ty = self.get_declared_ty_of_symbol(symbol);
        let static_ty = self.get_type_of_symbol(symbol);
        self.check_index_constraints(ty, symbol);

        if let Some(impls) = class.implements() {
            for ty in impls.list {
                self.check_ty_refer_ty(ty);
                let t = self.get_ty_from_ty_reference(*ty);
                if t == self.error_ty() {
                    continue;
                } else if t.kind.is_primitive() || t == self.boolean_ty() {
                    let error = errors::AClassCannotImplementAPrimTy {
                        span: ty.span,
                        ty: self.print_ty(t).to_string(),
                    };
                    self.push_error(Box::new(error));
                } else if self.is_valid_base_ty(t) {
                    // TODO:
                } else {
                    let error = errors::AClassCanOnlyImplementAnObjectTypeOrIntersectionOfObjectTypesWithStaticallyKnownMembers {
                        span: ty.span,
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

use super::TyChecker;
use super::ast;
use super::errors;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::ty::ElementFlags;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_ty(&mut self, ty: &'cx ast::Ty<'cx>) {
        use bolt_ts_ast::TyKind::*;
        let saved_current_node = self.current_node;
        self.current_node = Some(ty.id());
        match ty.kind {
            Refer(n) => self.check_ty_refer_ty(n),
            IndexedAccess(n) => self.check_indexed_access_ty(n),
            Cond(n) => {
                self.check_ty(n.check_ty);
                self.check_ty(n.extends_ty);
                self.check_ty(n.true_ty);
                self.check_ty(n.false_ty);
            }
            ObjectLit(n) => self.check_object_lit_ty(n),
            TyOp(n) => self.check_ty_op(n),
            Tuple(n) => self.check_tuple_ty(n),
            Fn(n) => {
                // TODO: check_signature_decl
                if let Some(ty_params) = n.ty_params {
                    self.check_ty_params(ty_params);
                }
                self.check_ty(n.ty);
            }
            Pred(n) => self.check_pred_ty(n),
            Mapped(n) => self.check_mapped_ty(n),
            Array(n) => (),
            Ctor(n) => (),
            Lit(n) => (),
            NamedTuple(n) => (),
            Rest(n) => (),
            Union(n) => (),
            Intersection(n) => (),
            Typeof(n) => self.check_type_query(n),
            Paren(n) => (),
            Infer(n) => (),
            Intrinsic(n) => (),
            Nullable(n) => (),
            TemplateLit(n) => (),
            This(n) => (),
        };
        self.current_node = saved_current_node;
    }

    fn check_type_query(&mut self, n: &'cx ast::TypeofTy<'cx>) {
        self.get_ty_from_type_query(n);
    }

    fn check_mapped_ty(&mut self, n: &'cx ast::MappedTy<'cx>) {
        if n.ty.is_none() && self.config.no_implicit_any() {
            // TODO: skip when check js file
            let error = errors::MappedObjectTypeImplicitlyHasAnAnyTemplateType { span: n.span };
            self.push_error(Box::new(error));
        }
    }

    fn check_pred_ty(&mut self, n: &'cx ast::PredTy<'cx>) {
        if let Some(ty) = n.ty {
            self.check_ty(ty);
        }
    }

    fn check_tuple_ty(&mut self, n: &'cx ast::TupleTy<'cx>) {
        let mut seen_optional_element = false;
        let mut seen_rest_element = false;
        for e in n.tys {
            let mut flags = Self::get_tuple_element_flags(e);
            if flags.intersects(ElementFlags::VARIADIC) {
                let ty = match e.kind {
                    ast::TyKind::Rest(node) => self.get_ty_from_type_node(node.ty),
                    ast::TyKind::NamedTuple(node) => self.get_ty_from_type_node(node.ty),
                    _ => unreachable!(),
                };
                if !self.is_array_like_ty(ty) {
                    // todo!("error handle");
                }
                if ty.kind.is_array(self)
                    || ty
                        .as_tuple()
                        .is_some_and(|tup| tup.combined_flags.intersects(ElementFlags::REST))
                {
                    flags |= ElementFlags::REST;
                }
                if flags.intersects(ElementFlags::REST) {
                    if seen_rest_element {
                        todo!("error handle");
                    }
                    seen_rest_element = true;
                } else if flags.intersects(ElementFlags::OPTIONAL) {
                    if seen_rest_element {
                        todo!("error handle");
                    }
                    seen_optional_element = true;
                } else if flags.intersects(ElementFlags::REQUIRED) && seen_optional_element {
                    todo!("error handle");
                }
            }
            self.check_ty(e);
        }

        self.get_ty_from_tuple_node(n);
    }

    fn check_ty_op(&mut self, n: &'cx ast::TyOp<'cx>) {
        self.check_ty(n.ty);
    }

    pub(super) fn check_ty_refer_ty(&mut self, n: &'cx ast::ReferTy<'cx>) {
        if let Some(ty_args) = n.ty_args {
            for ty_arg in ty_args.list {
                self.check_ty(ty_arg);
            }
        }
        self.check_ty_refer_ty_or_import(n);
    }

    fn check_object_lit_ty(&mut self, n: &'cx ast::ObjectLitTy<'cx>) {
        self.check_object_ty_for_duplicate_decls(n.members);
        for prop in n.members.iter() {
            use bolt_ts_ast::ObjectTyMemberKind::*;
            match &prop.kind {
                IndexSig(n) => self.check_index_sig_decl(n),
                Prop(_) => (),
                Method(_) => (),
                CallSig(_) => (),
                CtorSig(_) => (),
                Setter(_) => (),
                Getter(_) => (),
            }
        }

        let ty = self.get_ty_from_object_lit_or_fn_or_ctor_ty_node(n.id);
        self.check_index_constraints(ty, false);
        // TODO: duplicate index signatures check
    }

    fn check_index_sig_decl(&mut self, n: &'cx ast::IndexSigDecl<'cx>) {
        let ty = self.get_ty_from_type_node(n.key_ty);
        if self.some_type(ty, |this, t| {
            t.flags
                .intersects(ty::TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE)
                || this.is_generic_ty(ty)
        }) {
            let error = errors::AnIndexSignatureParameterTypeCannotBeALiteralTypeOrGenericTypeConsiderUsingAMappedObjectTypeInstead {
                span: n.span,
            };
            self.push_error(Box::new(error));
        } else if !self.every_type(ty, |this, ty| this.is_valid_index_key_ty(ty)) {
            let error = errors::AnIndexSignatureParameterTypeMustBeStringNumberSymbolOrATemplateLiteralType {
                span: n.span,
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_indexed_access_ty(&mut self, n: &'cx ast::IndexedAccessTy<'cx>) {
        self.check_ty(n.ty);
        self.check_ty(n.index_ty);
        let ty = self.get_ty_from_indexed_access_node(n);
        self.check_indexed_access_index_ty(ty, n);
    }
}

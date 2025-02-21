use crate::ty::TypeFlags;

use super::TyChecker;
use super::ast;
use super::errors;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_ty(&mut self, ty: &'cx ast::Ty<'cx>) {
        use bolt_ts_ast::TyKind::*;
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
            _ => (),
        }
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
        for prop in n.members.iter() {
            use bolt_ts_ast::ObjectTyMemberKind::*;
            match &prop.kind {
                IndexSig(n) => self.check_index_sig_decl(n),
                Prop(_) => (),
                Method(_) => (),
                CallSig(_) => (),
                CtorSig(_) => (),
            }
        }
    }

    fn check_index_sig_decl(&mut self, n: &'cx ast::IndexSigDecl<'cx>) {
        let ty = self.get_ty_from_type_node(n.params[0].ty.unwrap());
        if !self.every_type(ty, |_, ty| {
            // is valid index key type
            ty.flags.intersects(TypeFlags::STRING | TypeFlags::NUMBER)
        }) {
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

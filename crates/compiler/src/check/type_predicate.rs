use bolt_ts_ast as ast;
use bolt_ts_atom::AtomId;

use crate::{keyword, ty};

#[derive(Debug, Clone, Copy)]
pub struct TyPred<'cx> {
    pub kind: TyPredKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum TyPredKind<'cx> {
    Ident(IdentTyPred<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct IdentTyPred<'cx> {
    pub param_name: AtomId,
    pub param_index: u32,
    pub ty: Option<&'cx ty::Ty<'cx>>,
}

impl<'cx> super::TyChecker<'cx> {
    pub fn create_ident_ty_pred(
        &self,
        param_name: AtomId,
        param_index: u32,
        ty: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx TyPred<'cx> {
        self.alloc(TyPred {
            kind: TyPredKind::Ident(IdentTyPred {
                param_name,
                param_index,
                ty,
            }),
        })
    }

    pub fn create_ty_pred_from_node(
        &mut self,
        node: &'cx ast::PredTy<'cx>,
        sig: &'cx ty::Sig<'cx>,
    ) -> &'cx TyPred<'cx> {
        let name = node.name.name;
        let ty = self.get_ty_from_type_node(node.ty);
        if name == keyword::KW_THIS {
            todo!()
        } else {
            let index = sig
                .params
                .iter()
                .position(|&param| self.binder.symbol(param).name.expect_atom() == name)
                .unwrap();
            self.create_ident_ty_pred(name, index as u32, Some(ty))
        }
    }
}

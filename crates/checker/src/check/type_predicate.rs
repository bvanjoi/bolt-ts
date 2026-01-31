use bolt_ts_ast as ast;
use bolt_ts_atom::Atom;

use super::symbol_info::SymbolInfo;
use super::ty;

#[derive(Debug, Clone, Copy)]
pub struct TyPred<'cx> {
    pub kind: TyPredKind<'cx>,
}

impl<'cx> TyPred<'cx> {
    pub fn ty(&self) -> Option<&'cx ty::Ty<'cx>> {
        match &self.kind {
            TyPredKind::Ident(pred) => Some(pred.ty),
            TyPredKind::This(pred) => Some(pred.ty),
            TyPredKind::AssertsThis(pred) => pred.ty,
            TyPredKind::AssertsIdent(pred) => pred.ty,
        }
    }

    pub fn kind_match(&self, other: &TyPred<'cx>) -> bool {
        match (&self.kind, &other.kind) {
            (TyPredKind::Ident(a), TyPredKind::Ident(b)) => a.param_index == b.param_index,
            (TyPredKind::AssertsIdent(a), TyPredKind::AssertsIdent(b)) => {
                a.param_index == b.param_index
            }
            (TyPredKind::This(_), TyPredKind::This(_))
            | (TyPredKind::AssertsThis(_), TyPredKind::AssertsThis(_)) => true,

            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyPredKind<'cx> {
    Ident(IdentTyPred<'cx>),
    This(ThisTyPred<'cx>),
    AssertsThis(AssertsThisTyPred<'cx>),
    AssertsIdent(AssertsIdentTyPred<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct AssertsThisTyPred<'cx> {
    pub ty: Option<&'cx ty::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ThisTyPred<'cx> {
    pub ty: &'cx ty::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct AssertsIdentTyPred<'cx> {
    pub param_name: Atom,
    pub param_index: u32,
    pub ty: Option<&'cx ty::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct IdentTyPred<'cx> {
    pub param_name: Atom,
    pub param_index: u32,
    pub ty: &'cx ty::Ty<'cx>,
}

impl<'cx> super::TyChecker<'cx> {
    pub fn create_ident_ty_pred(
        &self,
        param_name: Atom,
        param_index: u32,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx TyPred<'cx> {
        let kind = TyPredKind::Ident(IdentTyPred {
            param_name,
            param_index,
            ty,
        });
        self.alloc(TyPred { kind })
    }

    pub fn create_ty_pred_from_ty_pred_node(
        &mut self,
        node: &'cx ast::PredTy<'cx>,
        sig: &'cx ty::Sig<'cx>,
    ) -> &'cx TyPred<'cx> {
        use bolt_ts_ast::PredTyName;
        // TODO: asserts_modifier
        let ty = node.ty.map(|ty| self.get_ty_from_type_node(ty));
        match node.name {
            PredTyName::Ident(ident) => {
                let name = ident.name;
                let index = sig
                    .params
                    .iter()
                    .position(|&param| self.binder.symbol(param).name.expect_atom() == name)
                    .unwrap();
                if node.asserts.is_some() {
                    let kind = TyPredKind::AssertsIdent(AssertsIdentTyPred {
                        param_name: name,
                        param_index: index as u32,
                        ty,
                    });
                    self.alloc(TyPred { kind })
                } else {
                    self.create_ident_ty_pred(name, index as u32, ty.unwrap())
                }
            }
            PredTyName::This(_) => {
                let kind = if node.asserts.is_some() {
                    TyPredKind::AssertsThis(AssertsThisTyPred { ty })
                } else {
                    TyPredKind::This(ThisTyPred { ty: ty.unwrap() })
                };
                self.alloc(TyPred { kind })
            }
        }
    }
}

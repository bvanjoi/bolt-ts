use super::ty;
use super::TyChecker;
use crate::ty::ObjectShape;
use crate::{ast, bind};

impl<'cx> TyChecker<'cx> {
    #[inline(always)]
    pub(super) fn ty_of_symbol(&self, symbol: bind::SymbolID) -> &'cx ty::Ty<'cx> {
        self.symbol_links[&symbol].get_ty().unwrap()
    }

    #[inline(always)]
    pub(super) fn declared_ty_of_symbol(&self, symbol: bind::SymbolID) -> &'cx ty::Ty<'cx> {
        self.symbol_links[&symbol].get_declared_ty().unwrap()
    }

    #[inline(always)]
    pub(super) fn ty_of_node(&self, node: ast::NodeID) -> &'cx ty::Ty<'cx> {
        self.node_links[&node].get_ty().unwrap()
    }

    pub(super) fn base_tys(&self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        if let Some(i) = ty.kind.as_object_interface() {
            ObjectShape::get_base_tys(i)
        } else {
            &[]
        }
    }

    pub(super) fn index_infos(&self, ty: &'cx ty::Ty<'cx>) -> ty::IndexInfos<'cx> {
        if let Some(i) = ty.kind.as_object_interface() {
            ObjectShape::get_index_infos(i)
        } else {
            &[]
        }
    }
}

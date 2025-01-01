use super::{SymbolLinks, TyChecker};
use crate::bind::{SymbolID, SymbolName};
use crate::ty;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_index_symbol(&self, symbol: SymbolID) -> Option<SymbolID> {
        self.members(symbol).get(&SymbolName::Index).copied()
    }

    pub(super) fn get_index_infos_of_symbol(
        &mut self,
        symbol: SymbolID,
    ) -> &'cx [&'cx ty::IndexInfo<'cx>] {
        let index_infos = self
            .get_index_symbol(symbol)
            .map(|index_symbol| {
                let decl = self.binder.symbol(index_symbol).expect_index().decl;
                let decl = self.p.node(decl).expect_index_sig_decl();
                let val_ty = self.get_ty_from_type_node(decl.ty);
                decl.params
                    .iter()
                    .map(|param| {
                        let Some(ty) = param.ty else { unreachable!() };
                        let key_ty = self.get_ty_from_type_node(ty);
                        self.alloc(ty::IndexInfo {
                            key_ty,
                            val_ty,
                            symbol: index_symbol,
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        self.alloc(index_infos)
    }

    pub(super) fn get_index_infos_of_structured_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> ty::IndexInfos<'cx> {
        if ty.kind.is_structured() {
            self.resolve_structured_type_members(ty);
            self.index_infos_of_ty(ty)
        } else {
            &[]
        }
    }

    pub(super) fn get_index_infos_of_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::IndexInfos<'cx> {
        self.get_index_infos_of_structured_ty(ty)
    }

    pub(super) fn get_applicable_index_info(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop_name_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        self.index_infos_of_ty(ty)
            .iter()
            .find(|info| self.is_applicable_index_ty(prop_name_ty, info.key_ty))
            .copied()
    }
}

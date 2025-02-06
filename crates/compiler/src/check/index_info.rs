use super::TyChecker;
use crate::bind::{SymbolID, SymbolName};
use crate::ty;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_index_symbol(&self, symbol: SymbolID) -> Option<SymbolID> {
        self.members(symbol).get(&SymbolName::Index).copied()
    }

    pub(super) fn get_index_infos_of_symbol(&mut self, symbol: SymbolID) -> ty::IndexInfos<'cx> {
        self.get_index_symbol(symbol)
            .map(|index_symbol| self.get_index_infos_of_index_symbol(index_symbol))
            .unwrap_or_default()
    }

    pub(super) fn get_index_infos_of_index_symbol(
        &mut self,
        symbol: SymbolID,
    ) -> ty::IndexInfos<'cx> {
        let index_symbol = self.binder.symbol(symbol).expect_index();
        let decl = self.p.node(index_symbol.decl).expect_index_sig_decl();
        let val_ty = self.get_ty_from_type_node(decl.ty);
        let index_infos = decl
            .params
            .iter()
            .map(|param| {
                let Some(ty) = param.ty else { unreachable!() };
                let key_ty = self.get_ty_from_type_node(ty);
                self.alloc(ty::IndexInfo {
                    key_ty,
                    val_ty,
                    symbol,
                })
            })
            .collect::<Vec<_>>();
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

    pub(super) fn find_index_info(
        &mut self,
        index_infos: &[&'cx ty::IndexInfo<'cx>],
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&ty::IndexInfo<'cx>> {
        index_infos
            .iter()
            .find(|info| info.key_ty == key_ty)
            .copied()
    }

    pub(super) fn get_index_info_of_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&ty::IndexInfo<'cx>> {
        let index_infos = self.get_index_infos_of_ty(ty);
        self.find_index_info(index_infos, key_ty)
    }

    pub(super) fn get_index_infos_of_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> ty::IndexInfos<'cx> {
        let ty = self.get_reduced_apparent_ty(ty);
        self.get_index_infos_of_structured_ty(ty)
    }

    pub(super) fn find_applicable_index_info(
        &mut self,
        index_infos: &'cx [&'cx ty::IndexInfo<'cx>],
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        let mut string_index_info: Option<&ty::IndexInfo<'_>> = None;
        let mut applicable_info: Option<&ty::IndexInfo<'_>> = None;
        let mut applicable_infos: Option<Vec<&ty::IndexInfo<'_>>> = None;

        for info in index_infos {
            if info.key_ty == self.string_ty {
                string_index_info = Some(info)
            } else if self.is_applicable_index_ty(key_ty, info.key_ty) {
                if applicable_info.is_none() {
                    applicable_info = Some(info)
                } else if let Some(applicable_infos) = applicable_infos.as_mut() {
                    applicable_infos.push(info)
                } else {
                    applicable_infos = Some(vec![applicable_info.unwrap(), info])
                }
            }
        }

        if let Some(applicable_infos) = applicable_infos {
            // TODO: create index info
            applicable_info
        } else if let Some(applicable_info) = applicable_info {
            Some(applicable_info)
        } else if string_index_info.is_some() && self.is_applicable_index_ty(key_ty, self.string_ty)
        {
            string_index_info
        } else {
            None
        }
    }

    pub(super) fn get_applicable_index_info(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        let index_infos = self.get_index_infos_of_ty(ty);
        self.find_applicable_index_info(index_infos, key_ty)
    }
}

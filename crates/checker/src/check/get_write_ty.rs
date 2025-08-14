use super::ResolutionKey;
use super::TyChecker;
use super::symbol_info::SymbolInfo;
use super::ty;
use crate::ty::CheckFlags;
use bolt_ts_binder::{SymbolFlags, SymbolID};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_write_type_of_symbol(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if self.symbol(symbol).flags.intersects(SymbolFlags::ACCESSOR) {
            let check_flags = self.get_check_flags(symbol);
            if check_flags.intersects(CheckFlags::INSTANTIATED) {
                // TODO:
                todo!()
            } else {
                self.get_write_type_of_accessor(symbol)
            }
        } else {
            self.get_type_of_symbol(symbol)
        }
    }

    fn get_write_type_of_accessor(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(symbol).get_write_ty() {
            return ty;
        }
        if !self.push_ty_resolution(ResolutionKey::WriteType(symbol)) {
            return self.any_ty;
        }

        let setter = self
            .binder
            .symbol(symbol)
            .get_declaration_of_kind(|id| self.p.node(id).is_setter_decl());
        let write_ty = setter
            .and_then(|setter| {
                let setter = self.p.node(setter).expect_setter_decl();
                setter.params[0].ty
            })
            .map(|setter_ty| self.get_ty_from_type_node(setter_ty));

        if self.pop_ty_resolution().has_cycle() {
            todo!("cycle");
        }

        let ty = write_ty.unwrap_or_else(|| self.get_ty_of_accessor(symbol));
        self.get_mut_symbol_links(symbol).set_write_ty(ty);
        ty
    }
}

use super::ty;
use super::ResolutionKey;
use super::TyChecker;
use crate::bind::{SymbolFlags, SymbolID};
use crate::ty::CheckFlags;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_write_type_of_symbol(&mut self, symbol: SymbolID) -> &'cx ty::Ty<'cx> {
        let check_flags = self.check_flags(symbol);
        if self
            .binder
            .symbol(symbol)
            .flags
            .intersects(SymbolFlags::ACCESSOR)
        {
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
            return self.any_ty();
        }

        let s = self.binder.symbol(symbol).expect_getter_setter();
        let setter = s.setter_decl;
        let write_ty = if let Some(setter_ty) = setter
            .and_then(|setter| {
                let setter = self.p.node(setter).expect_setter_decl();
                setter.params[0].ty
            })
            .map(|setter_ty| self.get_ty_from_type_node(setter_ty))
        {
            Some(setter_ty)
        } else {
            None
        };

        if self.pop_ty_resolution().has_cycle() {
            todo!("cycle");
        }

        let ty = write_ty.unwrap_or_else(|| self.get_ty_of_accessor(symbol));
        self.get_mut_symbol_links(symbol).set_write_ty(ty);
        ty
    }
}

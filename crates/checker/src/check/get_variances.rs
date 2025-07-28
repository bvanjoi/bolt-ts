use crate::ty::ObjectFlags;
use crate::ty::TyMapper;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;

use super::TyChecker;
use super::symbol_info::SymbolInfo;
use super::ty;

bitflags::bitflags! {
  #[derive(Clone, Copy, Debug, PartialEq)]
  pub struct VarianceFlags: u8 {
      const INVARIANT     = 0;
      const COVARIANT     = 1 << 0;
      const CONTRAVARIANT = 1 << 1;
      const INDEPENDENT   = 1 << 2;
      const UNMEASURABLE  = 1 << 3;
      const UNRELIABLE    = 1 << 4;
      const BIVARIANT     = Self::COVARIANT.bits() | Self::CONTRAVARIANT.bits();
      const VARIANCE_MASK = Self::INVARIANT.bits() | Self::COVARIANT.bits() | Self::CONTRAVARIANT.bits() | Self::INDEPENDENT.bits();
      const ALLOWS_STRUCTURAL_FALLBACK = Self::UNMEASURABLE.bits() | Self::UNRELIABLE.bits();
  }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_variances(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx [VarianceFlags] {
        assert!(ty.kind.is_object_reference());
        if ty == self.global_array_ty() || ty == self.global_readonly_array_ty() || ty.is_tuple() {
            return self.array_variances();
        }
        let r = ty.kind.expect_object_reference();
        let i = r.target.kind.expect_object_interface();
        self._get_variances(i.symbol, i.ty_params)
    }

    fn create_marker_ty(
        &mut self,
        symbol: SymbolID,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        assert!(source.kind.is_param());
        let ty = self.get_declared_ty_of_symbol(symbol);
        if ty == self.error_ty {
            return ty;
        }
        let mapper = self.alloc(TyMapper::make_unary(source, target));
        let result = if self
            .binder
            .symbol(symbol)
            .flags
            .intersects(SymbolFlags::TYPE_ALIAS)
        {
            let ty_params = self.get_symbol_links(symbol).get_ty_params().unwrap();
            let tys = self.instantiate_tys(ty_params, mapper);
            self.get_type_alias_instantiation(symbol, tys, None, None)
        } else {
            let ty_params = if let Some(i) = ty.kind.as_object_interface() {
                i.ty_params
            } else if let Some(t) = ty
                .kind
                .as_object_reference()
                .and_then(|r| r.interface_target())
            {
                let i = t.kind.expect_object_interface();
                i.ty_params
            } else {
                unreachable!("target: {:#?}", target)
            };
            let ty_args = self.instantiate_tys(ty_params.unwrap_or_default(), mapper);
            self.create_reference_ty(ty, Some(ty_args), ObjectFlags::empty())
        };
        self.mark_tys.insert(result.id);
        result
    }

    pub(super) fn is_marker_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.mark_tys.contains(&ty.id)
    }

    fn _get_variances(
        &mut self,
        symbol: SymbolID,
        ty_params: Option<ty::Tys<'cx>>,
    ) -> &'cx [VarianceFlags] {
        if let Some(variances) = self.get_symbol_links(symbol).get_variances() {
            return variances;
        }
        // TODO: in/out
        let ty_params = ty_params.unwrap_or_default();
        let empty_array = self.empty_array();
        self.get_mut_symbol_links(symbol).set_variances(empty_array);
        let mut variances = Vec::with_capacity(ty_params.len());
        for tp in ty_params {
            let ty_with_super = self.create_marker_ty(symbol, tp, self.mark_super_ty());
            let ty_with_sub = self.create_marker_ty(symbol, tp, self.mark_sub_ty());
            let mut variance = if self.is_type_assignable_to(ty_with_sub, ty_with_super) {
                VarianceFlags::COVARIANT
            } else {
                VarianceFlags::INVARIANT
            } | if self.is_type_assignable_to(ty_with_sub, ty_with_super) {
                VarianceFlags::CONTRAVARIANT
            } else {
                VarianceFlags::INVARIANT
            };
            if variance == VarianceFlags::BIVARIANT && {
                let ty = self.create_marker_ty(symbol, tp, self.mark_other_ty());
                self.is_type_assignable_to(ty, ty_with_super)
            } {
                variance = VarianceFlags::INDEPENDENT;
            }

            variances.push(variance);
        }

        let variances = self.alloc(variances);
        self.get_mut_symbol_links(symbol)
            .override_variances(variances);
        variances
    }
}

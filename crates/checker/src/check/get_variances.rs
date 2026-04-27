use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolID;

use super::TyChecker;
use super::ty;
use super::ty::ObjectFlags;
use super::ty::TyMapper;

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
        if ty == self.global_array_ty()
            || ty == self.global_readonly_array_ty()
            || ty.get_object_flags().contains(ObjectFlags::TUPLE)
        {
            return self.array_variances();
        }
        let object_ty = ty.kind.expect_object();
        match object_ty.kind {
            ty::ObjectTyKind::Interface(i) => {
                let symbol = i.symbol.unwrap();
                self.get_variances_worker(symbol, i.ty_params)
            }
            ty::ObjectTyKind::Reference(r) => {
                let i = r.target.kind.expect_object_interface();
                let symbol = i.symbol.unwrap();
                self.get_variances_worker(symbol, i.ty_params)
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn get_alias_variances(&mut self, symbol: SymbolID) -> &'cx [VarianceFlags] {
        let ty_params = self.get_symbol_links(symbol).get_ty_params();
        self.get_variances_worker(symbol, ty_params)
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
            self.create_type_reference(ty, Some(ty_args), ObjectFlags::empty())
        };
        self.mark_tys.insert(result.id);
        result
    }

    pub(super) fn is_marker_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.mark_tys.contains(&ty.id)
    }

    fn get_variances_worker(
        &mut self,
        symbol: SymbolID,
        ty_params: Option<ty::Tys<'cx>>,
    ) -> &'cx [VarianceFlags] {
        if let Some(variances) = self.get_symbol_links(symbol).get_variances() {
            return variances;
        }
        // TODO: in/out
        let old_variance_computation = self.in_variance_computation;
        let save_resolution_start = self.resolution_start;
        if !self.in_variance_computation {
            self.in_variance_computation = true;
            self.resolution_start = self.resolution_tys.len() as i32;
        };
        let ty_params = ty_params.unwrap_or_default();
        let empty_array = self.empty_array();
        self.get_mut_symbol_links(symbol).set_variances(empty_array);
        let mut variances = Vec::with_capacity(ty_params.len());
        for tp in ty_params {
            let old_reliability_flags = self.reliability_flags;
            let old_enable_out_of_band_variance_marker_handler =
                self.enable_out_of_band_variance_marker_handler;
            self.enable_out_of_band_variance_marker_handler = true;
            let ty_with_super = self.create_marker_ty(symbol, tp, self.mark_super_ty());
            let ty_with_sub = self.create_marker_ty(symbol, tp, self.mark_sub_ty());
            let mut variance = if self.is_type_assignable_to(ty_with_sub, ty_with_super) {
                VarianceFlags::COVARIANT
            } else {
                VarianceFlags::INVARIANT
            } | if self.is_type_assignable_to(ty_with_super, ty_with_sub) {
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

            self.enable_out_of_band_variance_marker_handler =
                old_enable_out_of_band_variance_marker_handler;
            if self.reliability_flags.contains(VarianceFlags::UNMEASURABLE) {
                variance |= VarianceFlags::UNMEASURABLE;
            }

            if self.reliability_flags.contains(VarianceFlags::UNRELIABLE) {
                variance |= VarianceFlags::UNRELIABLE;
            }

            self.reliability_flags = old_reliability_flags;

            variances.push(variance);
        }

        if !old_variance_computation {
            self.in_variance_computation = false;
            self.resolution_start = save_resolution_start;
        }

        let variances = self.alloc(variances);
        self.get_mut_symbol_links(symbol)
            .override_variances(variances);
        variances
    }
}

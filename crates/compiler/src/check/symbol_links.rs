use crate::bind::SymbolID;

use super::ty;

macro_rules! prop {
    ( $( ($x: ident, $ty: ty, $with_x:ident, $set_x: ident, $get_x: ident) ),* $( , )? ) => {
        #[derive(Debug, Default, Clone, Copy)]
        pub struct SymbolLinks<'cx> {
            $(
                $x: Option<$ty>,
            )*
        }

        impl<'cx> SymbolLinks<'cx> {
            $(
                pub fn $with_x(mut self, ty: $ty) -> Self {
                    self.$set_x(ty);
                    self
                }
                pub fn $set_x(&mut self, ty: $ty) {
                    assert!(self.$x.is_none());
                    self.$x = Some(ty);
                }
                pub fn $get_x(&self) -> Option<$ty> {
                    self.$x
                }
            )*
        }
    };
}

prop!(
    (ty, &'cx ty::Ty<'cx>, with_ty, set_ty, get_ty),
    (
        declared_ty,
        &'cx ty::Ty<'cx>,
        with_declared_ty,
        set_declared_ty,
        get_declared_ty
    ),
    (
        ty_params,
        ty::Tys<'cx>,
        with_ty_params,
        set_ty_params,
        get_ty_params
    ),
    (
        check_flags,
        ty::CheckFlags,
        with_check_flags,
        set_check_flags,
        get_check_flags
    ),
    (target, SymbolID, with_target, set_target, get_target),
    (
        mapper,
        &'cx ty::TyMapper<'cx>,
        with_ty_mapper,
        set_ty_mapper,
        get_ty_mapper
    ),
);

use super::ty;

impl<'cx> SymbolLinks<'cx> {
    pub fn new() -> Self {
        Default::default()
    }
}

macro_rules! prop {
    ( $( ($x: ident, $ty: ty, $with_x:ident, $set_x: ident, $get_x: ident) ),* $( , )? ) => {
        #[derive(Debug, Default)]
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
    )
);

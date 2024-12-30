use super::ty;

macro_rules! prop {
    ( $( ($x: ident, $ty: ty, $with_x:ident, $set_x: ident, $get_x: ident) ),* $( , )? ) => {
        #[derive(Debug, Default)]
        pub struct NodeLinks<'cx> {
            $(
                $x: Option<$ty>,
            )*
        }

        impl<'cx> NodeLinks<'cx> {
            $(
                pub fn $with_x(mut self, $x: $ty) -> Self {
                    self.$set_x($x);
                    self
                }
                pub fn $set_x(&mut self, $x: $ty) {
                    assert!(self.$x.is_none());
                    self.$x = Some($x);
                }
                pub fn $get_x(&self) -> Option<$ty> {
                    self.$x
                }
            )*
        }
    };
}

prop!(
    (
        ty,
        &'cx ty::Ty<'cx>,
        with_resolved_ty,
        set_resolved_ty,
        get_resolved_ty
    ),
    (
        sig,
        &'cx ty::Sig<'cx>,
        with_resolved_sig,
        set_resolved_sig,
        get_resolved_sig
    )
);

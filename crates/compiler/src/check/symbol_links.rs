use crate::bind::SymbolID;

use super::ty;

macro_rules! l {
    ($s: ident, $(($x: ident, $ty: ty)),* $( , )? ) => {
        #[derive(Debug, Default, Clone, Copy)]
        pub struct $s<'cx> {
            $(
                $x: Option<$ty>,
            )*
        }

        impl<'cx> $s<'cx> {
            paste::paste! {
                $(
                    pub fn [<with_ $x>](mut self, $x: $ty) -> Self {
                        self.[<set_ $x>]($x);
                        self
                    }
                    pub fn [<set_ $x>](&mut self, $x: $ty) {
                        assert!(self.$x.is_none());
                        self.$x = Some($x);
                    }
                    pub fn [<get_ $x>](&self) -> Option<$ty> {
                        self.$x
                    }
                    pub fn [<config_ $x>](&mut self, f: impl FnOnce($ty) -> $ty) {
                        self.$x = match self.$x {
                            Some(c) => Some(f(c)),
                            None => unreachable!("`{}` is not defined", stringify!($x)),
                        };
                    }
                )*
            }
        }
    };
}

pub(super) use l as links;

links!(
    SymbolLinks,
    (ty, &'cx ty::Ty<'cx>),
    (declared_ty, &'cx ty::Ty<'cx>),
    (ty_params, ty::Tys<'cx>),
    (check_flags, ty::CheckFlags),
    (target, SymbolID),
    (ty_mapper, &'cx ty::TyMapper<'cx>),
);

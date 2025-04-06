macro_rules! links {
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
                    #[allow(unused)]
                    pub fn [<with_ $x>](mut self, $x: $ty) -> Self {
                        self.[<set_ $x>]($x);
                        self
                    }
                    #[allow(unused)]
                    pub fn [<set_ $x>](&mut self, $x: $ty) {
                        assert!(self.$x.is_none());
                        self.$x = Some($x);
                    }
                    #[allow(unused)]
                    pub fn [<override_ $x>](&mut self, $x: $ty) {
                        assert!(self.$x.is_some());
                        self.$x = Some($x);
                    }
                    #[allow(unused)]
                    pub fn [<get_ $x>](&self) -> Option<$ty> {
                        self.$x
                    }
                    #[allow(unused)]
                    pub fn [<expect_ $x>](&self) -> $ty {
                        self.$x.unwrap()
                    }
                    #[allow(unused)]
                    pub fn [<config_ $x>](&mut self, f: impl FnOnce($ty) -> $ty) {
                        self.$x = match self.$x {
                            Some(c) => Some(f(c)),
                            None => unreachable!("`{}` is not defined", stringify!($x)),
                        };
                    }
                )*
            }
        }

        paste::paste! {
            pub type [<$s Arena>]<'cx> = id_arena::Arena<$s<'cx>>;
            pub type [<$s ID>]<'cx> = id_arena::Id<$s<'cx>>;
        }
    };
}

links!(
    CommonTyLinks,
    (permissive_instantiation, &'cx super::Ty<'cx>),
    (restrictive_instantiation, &'cx super::Ty<'cx>),
    (immediate_base_constraint, &'cx super::Ty<'cx>),
    // object flags cache
    (contain_ty_variables, bool),
);

links!(
    FreshTyLinks,
    (fresh_ty, &'cx super::Ty<'cx>),
    (regular_ty, &'cx super::Ty<'cx>)
);

links!(
    InterfaceTyLinks,
    (declared_members, &'cx super::DeclaredMembers<'cx>),
);

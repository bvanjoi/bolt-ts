#[macro_export]
macro_rules! new_index {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $name(u32);
        impl $name {
            pub(crate) fn root() -> $name {
                $name(0)
            }
            pub(crate) fn next(&self) -> $name {
                $name(self.0 + 1)
            }
            pub fn as_u32(&self) -> u32 {
                self.0
            }
        }
    };
}

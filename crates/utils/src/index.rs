#[macro_export]
macro_rules! index_with_module {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $name {
            module: bolt_ts_span::ModuleID,
            index: u32,
        }
        impl $name {
            pub const fn root(module: bolt_ts_span::ModuleID) -> $name {
                $name { module, index: 0 }
            }
            pub const fn next(&self) -> $name {
                $name {
                    module: self.module,
                    index: self.index + 1,
                }
            }
            pub const fn index_as_u32(&self) -> u32 {
                self.index
            }
            pub const fn index_as_usize(&self) -> usize {
                self.index as usize
            }
            pub const fn module(&self) -> bolt_ts_span::ModuleID {
                self.module
            }
        }
    };
}

#[macro_export]
macro_rules! index {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $name(u32);
        impl $name {
            pub const fn root() -> $name {
                $name(0)
            }
            pub const fn next(&self) -> $name {
                $name(self.0 + 1)
            }
            pub const fn as_u32(&self) -> u32 {
                self.0
            }
            pub const fn as_usize(&self) -> usize {
                self.0 as usize
            }
        }
    };
}

use bolt_ts_ast as ast;

mod check_flags;
mod object_flags;
mod type_facts;
mod type_flags;

pub use check_flags::CheckFlags;
pub use object_flags::ObjectFlags;
pub use type_facts::TypeFacts;
pub use type_flags::TypeFlags;

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct IndexFlags: u8 {
        const STRINGS_ONLY          = 1 << 0;
        const NO_INDEX_SIGNATURES   = 1 << 1;
        const NO_REDUCIBLE_CHECK    = 1 << 2;
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct ElementFlags: u8 {
        /// `T`
        const REQUIRED  = 1 << 0;
        /// `T?`
        const OPTIONAL  = 1 << 1;
        /// `...T[]`
        const REST      = 1 << 2;
        /// `...T`
        const VARIADIC  = 1 << 3;

        const FIXED         = Self::REQUIRED.bits() | Self::OPTIONAL.bits();
        const VARIABLE      = Self::REST.bits() | Self::VARIADIC.bits();
        const NON_REQUIRED  = Self::OPTIONAL.bits() | Self::REST.bits() | Self::VARIADIC.bits();
        const NON_REST      = Self::REQUIRED.bits() | Self::OPTIONAL.bits() | Self::VARIADIC.bits();
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Pattern<'cx> {
    ObjectPattern(&'cx ast::ObjectPat<'cx>),
    ArrayPattern(&'cx ast::ArrayPat<'cx>),
}

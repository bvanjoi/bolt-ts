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
}

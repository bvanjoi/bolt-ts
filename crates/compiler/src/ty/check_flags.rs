use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Default, Clone, Copy, PartialEq)]
    pub struct CheckFlags: u32 {
        /// Instantiated symbol
        const INSTANTIATED          = 1 << 0;
        /// Property in union or intersection type
        const SYNTHETIC_PROPERTY    = 1 << 1;
        /// Method in union or intersection type
        const SYNTHETIC_METHOD      = 1 << 2;
        /// Readonly transient symbol
        const READONLY              = 1 << 3;
        /// Synthetic property present in some but not all constituents
        const READ_PARTIAL          = 1 << 4;
        /// Synthetic property present in some but only satisfied by an index signature in others
        const WRITE_PARTIAL         = 1 << 5;
        /// Synthetic property with non-uniform type in constituents
        const HAS_NON_UNIFORM_TYPE  = 1 << 6;
        /// Synthetic property with at least one literal type in constituents
        const HAS_LITERAL_TYPE      = 1 << 7;
        /// Synthetic property with public constituent(s)
        const CONTAINS_PUBLIC       = 1 << 8;
        /// Synthetic property with protected constituent(s)
        const CONTAINS_PROTECTED    = 1 << 9;
        /// Synthetic property with private constituent(s)
        const CONTAINS_PRIVATE      = 1 << 10;
        /// Synthetic property with static constituent(s)
        const CONTAINS_STATIC       = 1 << 11;
        /// Late-bound symbol for a computed property with a dynamic name
        const LATE                  = 1 << 12;
        /// Property of reverse-inferred homomorphic mapped type
        const REVERSE_MAPPED        = 1 << 13;
        /// Optional parameter
        const OPTIONAL_PARAMETER    = 1 << 14;
        /// Rest parameter
        const REST_PARAMETER        = 1 << 15;
        /// Calculation of the type of this symbol is deferred due to processing costs, should be fetched with `getTypeOfSymbolWithDeferredType`
        const DEFERRED_TYPE         = 1 << 16;
        /// Synthetic property with at least one never type in constituents
        const HAS_NEVER_TYPE        = 1 << 17;
        /// Property of mapped type
        const MAPPED                = 1 << 18;
        /// Strip optionality in mapped property
        const STRIP_OPTIONAL        = 1 << 19;
        /// Unresolved type alias symbol
        const UNRESOLVED            = 1 << 20;

        const SYNTHETIC = Self::SYNTHETIC_PROPERTY.bits() | Self::SYNTHETIC_METHOD.bits();
        const DISCRIMINANT = Self::HAS_NON_UNIFORM_TYPE.bits() | Self::HAS_LITERAL_TYPE.bits();
        const PARTIAL = Self::READ_PARTIAL.bits() | Self::WRITE_PARTIAL.bits();
    }
}

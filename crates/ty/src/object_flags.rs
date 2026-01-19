bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct ObjectFlags: u32 {
        const CLASS = 1 << 0;
        const INTERFACE = 1 << 1;
        const REFERENCE = 1 << 2;
        const TUPLE = 1 << 3;
        const ANONYMOUS = 1 << 4;
        const MAPPED = 1 << 5;
        const INSTANTIATED = 1 << 6;
        const OBJECT_LITERAL = 1 << 7;
        const EVOLVING_ARRAY = 1 << 8;
        const OBJECT_LITERAL_PATTERN_WITH_COMPUTED_PROPERTIES = 1 << 9;
        const REVERSE_MAPPED = 1 << 10;
        const JSX_ATTRIBUTES = 1 << 11;
        const JS_LITERAL = 1 << 12;
        const FRESH_LITERAL = 1 << 13;
        const ARRAY_LITERAL = 1 << 14;
        const PRIMITIVE_UNION = 1 << 15;
        const CONTAINS_WIDENING_TYPE = 1 << 16;
        const CONTAINS_OBJECT_OR_ARRAY_LITERAL = 1 << 17;
        const NON_INFERRABLE_TYPE = 1 << 18;
        const COULD_CONTAIN_TYPE_VARIABLES_COMPUTED = 1 << 19;
        const COULD_CONTAIN_TYPE_VARIABLES = 1 << 20;
        const CONTAINS_SPREAD = 1 << 21;
        const OBJECT_REST_TYPE = 1 << 22;
        const INSTANTIATION_EXPRESSION_TYPE = 1 << 23;
        const SINGLE_SIGNATURE_TYPE = 1 << 27;
        const IS_CLASS_INSTANCE_CLONE = 1 << 24;
        const IDENTICAL_BASE_TYPE_CALCULATED = 1 << 25;
        const IDENTICAL_BASE_TYPE_EXISTS = 1 << 26;
        const IS_GENERIC_TYPE_COMPUTED = 1 << 21;
        const IS_GENERIC_OBJECT_TYPE = 1 << 22;
        const IS_GENERIC_INDEX_TYPE = 1 << 23;
        const IS_NARROWING_TYPE = 1 << 24;
        const CONTAINS_INTERSECTIONS = 1 << 24;
        const IS_UNKNOWN_LIKE_UNION_COMPUTED = 1 << 25;
        const IS_UNKNOWN_LIKE_UNION = 1 << 26;
        const IS_NEVER_INTERSECTION_COMPUTED = 1 << 24;
        const IS_NEVER_INTERSECTION = 1 << 25;
        const IS_CONSTRAINED_TYPE_VARIABLE = 1 << 26;

        const IS_GENERIC_TYPE = Self::IS_GENERIC_OBJECT_TYPE.bits() | Self::IS_GENERIC_INDEX_TYPE.bits();
        const CLASS_OR_INTERFACE = Self::CLASS.bits() | Self::INTERFACE.bits();
        const REQUIRES_WIDENING = Self::CONTAINS_WIDENING_TYPE.bits() | Self::CONTAINS_OBJECT_OR_ARRAY_LITERAL.bits();
        const PROPAGATING_FLAGS = Self::CONTAINS_WIDENING_TYPE.bits() | Self::CONTAINS_OBJECT_OR_ARRAY_LITERAL.bits() | Self::NON_INFERRABLE_TYPE.bits();
        const INSTANTIATED_MAPPED = Self::MAPPED.bits() | Self::INSTANTIATED.bits();
        const OBJECT_TYPE_KIND_MASK = Self::CLASS_OR_INTERFACE.bits() | Self::REFERENCE.bits() | Self::TUPLE.bits() | Self::ANONYMOUS.bits() | Self::MAPPED.bits() | Self::REVERSE_MAPPED.bits() | Self::EVOLVING_ARRAY.bits();
    }
}

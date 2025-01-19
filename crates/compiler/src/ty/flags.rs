bitflags::bitflags! {
  #[derive(Clone, Copy, Debug)]
    pub struct TypeFlags: u32 {
        const ANY = 1 << 0;
        const UNKNOWN = 1 << 1;
        const STRING = 1 << 2;
        const NUMBER = 1 << 3;
        const BOOLEAN = 1 << 4;
        const ENUM = 1 << 5;
        const BIG_INT = 1 << 6;
        const STRING_LITERAL = 1 << 7;
        const NUMBER_LITERAL = 1 << 8;
        const BOOLEAN_LITERAL = 1 << 9;
        const ENUM_LITERAL = 1 << 10;
        const BIG_INT_LITERAL = 1 << 11;
        const ES_SYMBOL = 1 << 12;
        const UNIQUE_ES_SYMBOL = 1 << 13;
        const VOID = 1 << 14;
        const UNDEFINED = 1 << 15;
        const NULL = 1 << 16;
        const NEVER = 1 << 17;
        const TYPE_PARAMETER = 1 << 18;
        const OBJECT = 1 << 19;
        const UNION = 1 << 20;
        const INTERSECTION = 1 << 21;
        const INDEX = 1 << 22;
        const INDEXED_ACCESS = 1 << 23;
        const CONDITIONAL = 1 << 24;
        const SUBSTITUTION = 1 << 25;
        const NON_PRIMITIVE = 1 << 26;
        const TEMPLATE_LITERAL = 1 << 27;
        const STRING_MAPPING = 1 << 28;
        const RESERVED1 = 1 << 29;
        const RESERVED2 = 1 << 30;

        const ANY_OR_UNKNOWN = Self::ANY.bits() | Self::UNKNOWN.bits();
        const NULLABLE = Self::UNDEFINED.bits() | Self::NULL.bits();
        const LITERAL = Self::STRING_LITERAL.bits() | Self::NUMBER_LITERAL.bits() | Self::BIG_INT_LITERAL.bits() | Self::BOOLEAN_LITERAL.bits();
        const UNIT = Self::ENUM.bits() | Self::LITERAL.bits() | Self::UNIQUE_ES_SYMBOL.bits() | Self::NULLABLE.bits();
        const FRESHABLE = Self::ENUM.bits() | Self::LITERAL.bits();
        const STRING_OR_NUMBER_LITERAL = Self::STRING_LITERAL.bits() | Self::NUMBER_LITERAL.bits();
        const STRING_OR_NUMBER_LITERAL_OR_UNIQUE = Self::STRING_LITERAL.bits() | Self::NUMBER_LITERAL.bits() | Self::UNIQUE_ES_SYMBOL.bits();
        const DEFINITELY_FALSY = Self::STRING_LITERAL.bits() | Self::NUMBER_LITERAL.bits() | Self::BIG_INT_LITERAL.bits() | Self::BOOLEAN_LITERAL.bits() | Self::VOID.bits() | Self::UNDEFINED.bits() | Self::NULL.bits();
        const POSSIBLY_FALSY = Self::DEFINITELY_FALSY.bits() | Self::STRING.bits() | Self::NUMBER.bits() | Self::BIG_INT.bits() | Self::BOOLEAN.bits();
        const INTRINSIC = Self::ANY.bits() | Self::UNKNOWN.bits() | Self::STRING.bits() | Self::NUMBER.bits() | Self::BIG_INT.bits() | Self::BOOLEAN.bits() | Self::BOOLEAN_LITERAL.bits() | Self::ES_SYMBOL.bits() | Self::VOID.bits() | Self::UNDEFINED.bits() | Self::NULL.bits() | Self::NEVER.bits() | Self::NON_PRIMITIVE.bits();
        const STRING_LIKE = Self::STRING.bits() | Self::STRING_LITERAL.bits() | Self::TEMPLATE_LITERAL.bits() | Self::STRING_MAPPING.bits();
        const NUMBER_LIKE = Self::NUMBER.bits() | Self::NUMBER_LITERAL.bits() | Self::ENUM.bits();
        const BIG_INT_LIKE = Self::BIG_INT.bits() | Self::BIG_INT_LITERAL.bits();
        const BOOLEAN_LIKE = Self::BOOLEAN.bits() | Self::BOOLEAN_LITERAL.bits();
        const ENUM_LIKE = Self::ENUM.bits() | Self::ENUM_LITERAL.bits();
        const ES_SYMBOL_LIKE = Self::ES_SYMBOL.bits() | Self::UNIQUE_ES_SYMBOL.bits();
        const VOID_LIKE = Self::VOID.bits() | Self::UNDEFINED.bits();
        const PRIMITIVE = Self::STRING_LIKE.bits() | Self::NUMBER_LIKE.bits() | Self::BIG_INT_LIKE.bits() | Self::BOOLEAN_LIKE.bits() | Self::ENUM_LIKE.bits() | Self::ES_SYMBOL_LIKE.bits() | Self::VOID_LIKE.bits() | Self::NULL.bits();
        const DEFINITELY_NON_NULLABLE = Self::STRING_LIKE.bits() | Self::NUMBER_LIKE.bits() | Self::BIG_INT_LIKE.bits() | Self::BOOLEAN_LIKE.bits() | Self::ENUM_LIKE.bits() | Self::ES_SYMBOL_LIKE.bits() | Self::OBJECT.bits() | Self::NON_PRIMITIVE.bits();
        const DISJOINT_DOMAINS = Self::NON_PRIMITIVE.bits() | Self::STRING_LIKE.bits() | Self::NUMBER_LIKE.bits() | Self::BIG_INT_LIKE.bits() | Self::BOOLEAN_LIKE.bits() | Self::ES_SYMBOL_LIKE.bits() | Self::VOID_LIKE.bits() | Self::NULL.bits();
        const UNION_OR_INTERSECTION = Self::UNION.bits() | Self::INTERSECTION.bits();
        const STRUCTURED_TYPE = Self::OBJECT.bits() | Self::UNION.bits() | Self::INTERSECTION.bits();
        const TYPE_VARIABLE = Self::TYPE_PARAMETER.bits() | Self::INDEXED_ACCESS.bits();
        const INSTANTIABLE_NON_PRIMITIVE = Self::TYPE_VARIABLE.bits() | Self::CONDITIONAL.bits() | Self::SUBSTITUTION.bits();
        const INSTANTIABLE_PRIMITIVE = Self::INDEX.bits() | Self::TEMPLATE_LITERAL.bits() | Self::STRING_MAPPING.bits();
        const INSTANTIABLE = Self::INSTANTIABLE_NON_PRIMITIVE.bits() | Self::INSTANTIABLE_PRIMITIVE.bits();
        const STRUCTURED_OR_INSTANTIABLE = Self::STRUCTURED_TYPE.bits() | Self::INSTANTIABLE.bits();
        const OBJECT_FLAGS_TYPE = Self::ANY.bits() | Self::NULLABLE.bits() | Self::NEVER.bits() | Self::OBJECT.bits() | Self::UNION.bits() | Self::INTERSECTION.bits();
        const SIMPLIFIABLE = Self::INDEXED_ACCESS.bits() | Self::CONDITIONAL.bits();
        const SINGLETON = Self::ANY.bits() | Self::UNKNOWN.bits() | Self::STRING.bits() | Self::NUMBER.bits() | Self::BOOLEAN.bits() | Self::BIG_INT.bits() | Self::ES_SYMBOL.bits() | Self::VOID.bits() | Self::UNDEFINED.bits() | Self::NULL.bits() | Self::NEVER.bits() | Self::NON_PRIMITIVE.bits();
        const NARROWABLE = Self::ANY.bits() | Self::UNKNOWN.bits() | Self::STRUCTURED_OR_INSTANTIABLE.bits() | Self::STRING_LIKE.bits() | Self::NUMBER_LIKE.bits() | Self::BIG_INT_LIKE.bits() | Self::BOOLEAN_LIKE.bits() | Self::ES_SYMBOL.bits() | Self::UNIQUE_ES_SYMBOL.bits() | Self::NON_PRIMITIVE.bits();
        const INCLUDES_MASK = Self::ANY.bits() | Self::UNKNOWN.bits() | Self::PRIMITIVE.bits() | Self::NEVER.bits() | Self::OBJECT.bits() | Self::UNION.bits() | Self::INTERSECTION.bits() | Self::NON_PRIMITIVE.bits() | Self::TEMPLATE_LITERAL.bits() | Self::STRING_MAPPING.bits();
        const INCLUDES_MISSING_TYPE = Self::TYPE_PARAMETER.bits();
        const INCLUDES_NON_WIDENING_TYPE = Self::INDEX.bits();
        const INCLUDES_WILDCARD = Self::INDEXED_ACCESS.bits();
        const INCLUDES_EMPTY_OBJECT = Self::CONDITIONAL.bits();
        const INCLUDES_INSTANTIABLE = Self::SUBSTITUTION.bits();
        const INCLUDES_CONSTRAINED_TYPE_VARIABLE = Self::RESERVED1.bits();
        const INCLUDES_ERROR = Self::RESERVED2.bits();
        const NOT_PRIMITIVE_UNION = Self::ANY.bits() | Self::UNKNOWN.bits() | Self::VOID.bits() | Self::NEVER.bits() | Self::OBJECT.bits() | Self::INTERSECTION.bits() | Self::INCLUDES_INSTANTIABLE.bits();
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
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

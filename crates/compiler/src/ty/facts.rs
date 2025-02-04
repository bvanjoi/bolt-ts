use crate::keyword;
use bolt_ts_atom::AtomId;

use super::{Ty, TyKind};

bitflags::bitflags! {
  #[derive(Debug, Clone, Copy)]
  pub struct TypeFacts: u32 {
    /// `typeof x === "string"`
    const TYPEOF_EQ_STRING = 1 << 0;
    /// `typeof x === "number"`
    const TYPEOF_EQ_NUMBER = 1 << 1;
    /// `typeof x === "bigint"`
    const TYPEOF_EQ_BIGINT = 1 << 2;
    /// `typeof x === "boolean"`
    const TYPEOF_EQ_BOOLEAN = 1 << 3;
    /// `typeof x === "symbol"`
    const TYPEOF_EQ_SYMBOL = 1 << 4;
    /// `typeof x === "object"`
    const TYPEOF_EQ_OBJECT = 1 << 5;
    /// `typeof x === "function"`
    const TYPEOF_EQ_FUNCTION = 1 << 6;
    /// `typeof x === "xxx"`
    const TYPEOF_EQ_HOST_OBJECT = 1 << 7;
    /// `typeof x !== "string"`
    const TYPEOF_NE_STRING = 1 << 8;
    /// `typeof x !== "number"`
    const TYPEOF_NE_NUMBER = 1 << 9;
    /// `typeof x !== "bigint"`
    const TYPEOF_NE_BIGINT = 1 << 10;
    /// `typeof x !== "boolean"`
    const TYPEOF_NE_BOOLEAN = 1 << 11;
    /// `typeof x !== "symbol"`
    const TYPEOF_NE_SYMBOL = 1 << 12;
    /// `typeof x !== "object"`
    const TYPEOF_NE_OBJECT = 1 << 13;
    /// `typeof x !== "function"`
    const TYPEOF_NE_FUNCTION = 1 << 14;
    /// `typeof x !== "xxx"`
    const TYPEOF_NE_HOST_OBJECT = 1 << 15;
    /// `x === undefined`
    const EQ_UNDEFINED = 1 << 16;
    /// `x === null`
    const EQ_NULL = 1 << 17;
    /// `x === undefined / x === null`
    const EQ_UNDEFINED_OR_NULL = 1 << 18;
    /// `x !== undefined`
    const NE_UNDEFINED = 1 << 19;
    /// `x !== null`
    const NE_NULL = 1 << 20;
    /// `x != undefined / x != null`
    const NE_UNDEFINED_OR_NULL = 1 << 21;
    /// `x`
    const TRUTHY = 1 << 22;
    /// `!x`
    const FALSY = 1 << 23;
    /// Contains undefined or intersection with undefined
    const IS_UNDEFINED = 1 << 24;
    /// Contains null or intersection with null
    const IS_NULL = 1 << 25;
    const IS_UNDEFINED_OR_NULL = Self::IS_UNDEFINED.bits() | Self::IS_NULL.bits();
    const ALL = (1 << 27) - 1;

    // Base and derived facts
    const BASE_STRING_STRICT_FACTS = Self::TYPEOF_EQ_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::NE_NULL.bits()
        | Self::NE_UNDEFINED_OR_NULL.bits();

    const BASE_STRING_FACTS = Self::BASE_STRING_STRICT_FACTS.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::FALSY.bits();

    const STRING_STRICT_FACTS = Self::BASE_STRING_STRICT_FACTS.bits()
        | Self::TRUTHY.bits()
        | Self::FALSY.bits();

    const STRING_FACTS = Self::BASE_STRING_FACTS.bits() | Self::TRUTHY.bits();

    const EMPTY_STRING_STRICT_FACTS = Self::BASE_STRING_STRICT_FACTS.bits() | Self::FALSY.bits();

    const EMPTY_STRING_FACTS = Self::BASE_STRING_FACTS.bits();

    const NON_EMPTY_STRING_STRICT_FACTS = Self::BASE_STRING_STRICT_FACTS.bits() | Self::TRUTHY.bits();

    const NON_EMPTY_STRING_FACTS = Self::BASE_STRING_FACTS.bits() | Self::TRUTHY.bits();

    const BASE_NUMBER_STRICT_FACTS = Self::TYPEOF_EQ_NUMBER.bits()
        | Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::NE_NULL.bits()
        | Self::NE_UNDEFINED_OR_NULL.bits();

    const BASE_NUMBER_FACTS = Self::BASE_NUMBER_STRICT_FACTS.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::FALSY.bits();

    const NUMBER_STRICT_FACTS = Self::BASE_NUMBER_STRICT_FACTS.bits()
        | Self::TRUTHY.bits()
        | Self::FALSY.bits();

    const NUMBER_FACTS = Self::BASE_NUMBER_FACTS.bits() | Self::TRUTHY.bits();

    const ZERO_NUMBER_STRICT_FACTS = Self::BASE_NUMBER_STRICT_FACTS.bits() | Self::FALSY.bits();

    const ZERO_NUMBER_FACTS = Self::BASE_NUMBER_FACTS.bits();

    const NON_ZERO_NUMBER_STRICT_FACTS = Self::BASE_NUMBER_STRICT_FACTS.bits() | Self::TRUTHY.bits();

    const NON_ZERO_NUMBER_FACTS = Self::BASE_NUMBER_FACTS.bits() | Self::TRUTHY.bits();

    const BASE_BIGINT_STRICT_FACTS = Self::TYPEOF_EQ_BIGINT.bits()
        | Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::NE_NULL.bits()
        | Self::NE_UNDEFINED_OR_NULL.bits();

    const BASE_BIGINT_FACTS = Self::BASE_BIGINT_STRICT_FACTS.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::FALSY.bits();

    const BIGINT_STRICT_FACTS = Self::BASE_BIGINT_STRICT_FACTS.bits()
        | Self::TRUTHY.bits()
        | Self::FALSY.bits();

    const BIGINT_FACTS = Self::BASE_BIGINT_FACTS.bits() | Self::TRUTHY.bits();

    const ZERO_BIGINT_STRICT_FACTS = Self::BASE_BIGINT_STRICT_FACTS.bits() | Self::FALSY.bits();

    const ZERO_BIGINT_FACTS = Self::BASE_BIGINT_FACTS.bits();

    const NON_ZERO_BIGINT_STRICT_FACTS = Self::BASE_BIGINT_STRICT_FACTS.bits() | Self::TRUTHY.bits();

    const NON_ZERO_BIGINT_FACTS = Self::BASE_BIGINT_FACTS.bits() | Self::TRUTHY.bits();

    const BASE_BOOLEAN_STRICT_FACTS = Self::TYPEOF_EQ_BOOLEAN.bits()
        | Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::NE_NULL.bits()
        | Self::NE_UNDEFINED_OR_NULL.bits();

    const BASE_BOOLEAN_FACTS = Self::BASE_BOOLEAN_STRICT_FACTS.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::FALSY.bits();

    const BOOLEAN_STRICT_FACTS = Self::BASE_BOOLEAN_STRICT_FACTS.bits()
        | Self::TRUTHY.bits()
        | Self::FALSY.bits();

    const BOOLEAN_FACTS = Self::BASE_BOOLEAN_FACTS.bits() | Self::TRUTHY.bits();

    const FALSE_STRICT_FACTS = Self::BASE_BOOLEAN_STRICT_FACTS.bits() | Self::FALSY.bits();

    const FALSE_FACTS = Self::BASE_BOOLEAN_FACTS.bits();

    const TRUE_STRICT_FACTS = Self::BASE_BOOLEAN_STRICT_FACTS.bits() | Self::TRUTHY.bits();

    const TRUE_FACTS = Self::BASE_BOOLEAN_FACTS.bits() | Self::TRUTHY.bits();

    const SYMBOL_STRICT_FACTS = Self::TYPEOF_EQ_SYMBOL.bits()
        | Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::NE_NULL.bits()
        | Self::NE_UNDEFINED_OR_NULL.bits()
        | Self::TRUTHY.bits();

    const SYMBOL_FACTS = Self::SYMBOL_STRICT_FACTS.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::FALSY.bits();

    const OBJECT_STRICT_FACTS = Self::TYPEOF_EQ_OBJECT.bits()
        | Self::TYPEOF_EQ_HOST_OBJECT.bits()
        | Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::NE_NULL.bits()
        | Self::NE_UNDEFINED_OR_NULL.bits()
        | Self::TRUTHY.bits();

    const OBJECT_FACTS = Self::OBJECT_STRICT_FACTS.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::FALSY.bits();

    const FUNCTION_STRICT_FACTS = Self::TYPEOF_EQ_FUNCTION.bits()
        | Self::TYPEOF_EQ_HOST_OBJECT.bits()
        | Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::NE_NULL.bits()
        | Self::NE_UNDEFINED_OR_NULL.bits()
        | Self::TRUTHY.bits();

    const FUNCTION_FACTS = Self::FUNCTION_STRICT_FACTS.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::FALSY.bits();

    const VOID_FACTS = Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::NE_NULL.bits()
        | Self::FALSY.bits();

    const UNDEFINED_FACTS = Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::EQ_UNDEFINED.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::NE_NULL.bits()
        | Self::FALSY.bits()
        | Self::IS_UNDEFINED.bits();

    const NULL_FACTS = Self::TYPEOF_EQ_OBJECT.bits()
        | Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::TYPEOF_NE_HOST_OBJECT.bits()
        | Self::EQ_NULL.bits()
        | Self::EQ_UNDEFINED_OR_NULL.bits()
        | Self::NE_UNDEFINED.bits()
        | Self::FALSY.bits()
        | Self::IS_NULL.bits();

    const EMPTY_OBJECT_STRICT_FACTS = Self::ALL.bits() & !(Self::EQ_UNDEFINED.bits() | Self::EQ_NULL.bits() | Self::EQ_UNDEFINED_OR_NULL.bits() | Self::IS_UNDEFINED_OR_NULL.bits());

    const EMPTY_OBJECT_FACTS = Self::ALL.bits() & !Self::IS_UNDEFINED_OR_NULL.bits();

    const UNKNOWN_FACTS = Self::ALL.bits() & !Self::IS_UNDEFINED_OR_NULL.bits();

    const ALL_TYPEOF_NE = Self::TYPEOF_NE_STRING.bits()
        | Self::TYPEOF_NE_NUMBER.bits()
        | Self::TYPEOF_NE_BIGINT.bits()
        | Self::TYPEOF_NE_BOOLEAN.bits()
        | Self::TYPEOF_NE_SYMBOL.bits()
        | Self::TYPEOF_NE_OBJECT.bits()
        | Self::TYPEOF_NE_FUNCTION.bits()
        | Self::NE_UNDEFINED.bits();

    // Masks
    const OR_FACTS_MASK = Self::TYPEOF_EQ_FUNCTION.bits() | Self::TYPEOF_NE_OBJECT.bits();
    const AND_FACTS_MASK = Self::ALL.bits() & !Self::OR_FACTS_MASK.bits();
  }
}

pub fn has_type_facts(ty: &Ty, mark: TypeFacts) -> bool {
    get_type_facts(ty).intersects(mark)
}

fn get_type_facts(ty: &Ty) -> TypeFacts {
    if let TyKind::NumberLit(lit) = ty.kind {
        let is_zero = lit.val == 0.;
        if is_zero {
            TypeFacts::ZERO_NUMBER_FACTS
        } else {
            TypeFacts::NON_ZERO_NUMBER_FACTS
        }
    } else {
        TypeFacts::empty()
    }
}

pub const TYPEOF_NE_FACTS: [(AtomId, TypeFacts); 8] = [
    (keyword::IDENT_STRING, TypeFacts::TYPEOF_NE_STRING),
    (keyword::IDENT_NUMBER, TypeFacts::TYPEOF_NE_NUMBER),
    (keyword::IDENT_BIGINT, TypeFacts::TYPEOF_NE_BIGINT),
    (keyword::IDENT_BOOLEAN, TypeFacts::TYPEOF_NE_BOOLEAN),
    (keyword::IDENT_SYMBOL, TypeFacts::TYPEOF_NE_SYMBOL),
    (keyword::KW_UNDEFINED, TypeFacts::NE_UNDEFINED),
    (keyword::IDENT_OBJECT, TypeFacts::TYPEOF_NE_OBJECT),
    (keyword::KW_FUNCTION, TypeFacts::TYPEOF_NE_FUNCTION),
];

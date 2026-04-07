use bolt_ts_ast::keyword;
use bolt_ts_atom::Atom;
use bolt_ts_ty::TypeFacts;

pub const TYPEOF_NE_FACTS: [(Atom, TypeFacts); 8] = [
    (keyword::IDENT_STRING, TypeFacts::TYPEOF_NE_STRING),
    (keyword::IDENT_NUMBER, TypeFacts::TYPEOF_NE_NUMBER),
    (keyword::IDENT_BIGINT, TypeFacts::TYPEOF_NE_BIGINT),
    (keyword::IDENT_BOOLEAN, TypeFacts::TYPEOF_NE_BOOLEAN),
    (keyword::IDENT_SYMBOL, TypeFacts::TYPEOF_NE_SYMBOL),
    (keyword::KW_UNDEFINED, TypeFacts::NE_UNDEFINED),
    (keyword::IDENT_OBJECT, TypeFacts::TYPEOF_NE_OBJECT),
    (keyword::KW_FUNCTION, TypeFacts::TYPEOF_NE_FUNCTION),
];

pub const fn typeof_ne_facts(atom: Atom) -> Option<TypeFacts> {
    match atom {
        keyword::IDENT_STRING => Some(TypeFacts::TYPEOF_NE_STRING),
        keyword::IDENT_NUMBER => Some(TypeFacts::TYPEOF_NE_NUMBER),
        keyword::IDENT_BIGINT => Some(TypeFacts::TYPEOF_NE_BIGINT),
        keyword::IDENT_BOOLEAN => Some(TypeFacts::TYPEOF_NE_BOOLEAN),
        keyword::IDENT_SYMBOL => Some(TypeFacts::TYPEOF_NE_SYMBOL),
        keyword::KW_UNDEFINED => Some(TypeFacts::NE_UNDEFINED),
        keyword::IDENT_OBJECT => Some(TypeFacts::TYPEOF_NE_OBJECT),
        keyword::KW_FUNCTION => Some(TypeFacts::TYPEOF_NE_FUNCTION),
        _ => None,
    }
}

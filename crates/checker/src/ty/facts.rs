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

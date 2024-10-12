use super::token::TokenKind;

pub trait ListContext {
    fn is_ele(t: TokenKind) -> bool;
    fn is_closing(t: TokenKind) -> bool;
}

pub struct BlockStmt;
impl ListContext for BlockStmt {
    fn is_ele(t: TokenKind) -> bool {
        !matches!(t, TokenKind::Semi) && t.is_start_of_stmt()
    }

    fn is_closing(t: TokenKind) -> bool {
        matches!(t, TokenKind::RBrace)
    }
}

pub struct ArgExprs;
impl ListContext for ArgExprs {
    fn is_ele(t: TokenKind) -> bool {
        t.is_start_of_expr()
    }

    fn is_closing(t: TokenKind) -> bool {
        matches!(t, TokenKind::RParen)
    }
}

pub struct ObjectLitMembers;
impl ListContext for ObjectLitMembers {
    fn is_ele(t: TokenKind) -> bool {
        use TokenKind::*;
        matches!(t, LBrace) || t.is_lit_prop_name()
    }

    fn is_closing(t: TokenKind) -> bool {
        matches!(t, TokenKind::RBrace)
    }
}

pub struct Params;
impl ListContext for Params {
    fn is_ele(t: TokenKind) -> bool {
        t.is_start_of_param()
    }

    fn is_closing(t: TokenKind) -> bool {
        use TokenKind::*;
        matches!(t, RParen | RBracket)
    }
}
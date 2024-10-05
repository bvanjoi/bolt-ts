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

pub struct ArgumentExpressions;
impl ListContext for ArgumentExpressions {
    fn is_ele(t: TokenKind) -> bool {
        t.is_start_of_expr()
    }

    fn is_closing(t: TokenKind) -> bool {
        matches!(t, TokenKind::RParen)
    }
}

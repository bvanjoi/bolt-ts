use super::{token::TokenKind, ParserState};

pub trait ListContext {
    fn is_ele(s: &mut ParserState) -> bool;
    fn is_closing(s: &mut ParserState) -> bool;
}

pub struct BlockStmt;
impl ListContext for BlockStmt {
    fn is_ele(s: &mut ParserState) -> bool {
        !matches!(s.token.kind, TokenKind::Semi) && s.token.kind.is_start_of_stmt()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

pub struct ArgExprs;
impl ListContext for ArgExprs {
    fn is_ele(s: &mut ParserState) -> bool {
        s.token.kind.is_start_of_expr()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RParen)
    }
}

pub struct ObjectLitMembers;
impl ListContext for ObjectLitMembers {
    fn is_ele(s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, LBrace) || s.token.kind.is_lit_prop_name()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

pub struct Params;
impl ListContext for Params {
    fn is_ele(s: &mut ParserState) -> bool {
        s.token.kind.is_start_of_param()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, RParen | RBracket)
    }
}

pub struct TyParams;
impl ListContext for TyParams {
    fn is_ele(s: &mut ParserState) -> bool {
        // FIXME: parse_state.is_ident
        s.token.kind.is_binding_ident()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, Great)
    }
}

pub struct HeritageClauses;
impl ListContext for HeritageClauses {
    fn is_ele(s: &mut ParserState) -> bool {
        s.token.kind.is_heritage_clause()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, LBrace | RBrace)
    }
}

pub struct HeritageClause;
impl ListContext for HeritageClause {
    fn is_ele(s: &mut ParserState) -> bool {
        // TODO: fixme
        s.token.kind.is_binding_ident()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, LBrace) || s.token.kind.is_heritage_clause()
    }
}

pub struct VarDecl;
impl ListContext for VarDecl {
    fn is_ele(s: &mut ParserState) -> bool {
        s.token.kind.is_binding_ident_or_private_ident_or_pat()
    }

    fn is_closing(s: &mut ParserState) -> bool {
        s.can_parse_semi()
    }
}

fn is_class_ele_start(s: &mut ParserState) -> bool {
    // if s.token.kind == TokenKind::At {
    //     return true;
    // }

    let mut id_token = None;
    if s.token.kind.is_lit_prop_name() {
        id_token = Some(s.token.kind);
        s.next_token();
    }

    if let Some(t) = id_token {
        if !t.is_keyword() {
            return true;
        }
    }
    false
}

pub struct ClassElements;
impl ListContext for ClassElements {
    fn is_ele(s: &mut ParserState) -> bool {
        s.lookahead(is_class_ele_start) || s.token.kind == TokenKind::Semi
    }

    fn is_closing(s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

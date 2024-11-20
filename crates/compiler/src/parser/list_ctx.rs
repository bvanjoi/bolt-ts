use super::{token::TokenKind, ParserState};

pub(super) trait ListContext: Copy {
    fn is_ele(&self, s: &mut ParserState) -> bool;
    fn is_closing(&self, s: &mut ParserState) -> bool;
}

#[derive(Copy, Clone)]
pub(super) struct BlockStmt;
impl ListContext for BlockStmt {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        !matches!(s.token.kind, TokenKind::Semi) && s.token.kind.is_start_of_stmt()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

#[derive(Copy, Clone)]
pub(super) struct ArgExprs;
impl ListContext for ArgExprs {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        s.token.kind.is_start_of_expr()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RParen)
    }
}

#[derive(Copy, Clone)]
pub(super) struct ObjectLitMembers;
impl ListContext for ObjectLitMembers {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, LBrace) || s.token.kind.is_lit_prop_name()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

fn is_ty_member_start(s: &mut ParserState) -> bool {
    use TokenKind::*;
    if s.token.kind == LParen || s.token.kind == Less {
        return true;
    }

    let mut id_token = false;
    while s.token.kind.is_modifier_kind() {
        id_token = true;
        s.next_token();
    }

    if s.token.kind == LBracket {
        return true;
    }

    if s.token.kind.is_lit_prop_name() {
        id_token = true;
        s.next_token();
    }

    id_token
        && (matches!(s.token.kind, LParen | Less | Question | Colon | Comma) || s.can_parse_semi())
}

#[derive(Copy, Clone)]
pub(super) struct TyMembers;
impl ListContext for TyMembers {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        s.lookahead(is_ty_member_start)
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

#[derive(Copy, Clone)]
pub(super) struct Params;
impl ListContext for Params {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        s.token.kind.is_start_of_param()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, RParen | RBracket)
    }
}

#[derive(Copy, Clone)]
pub(super) struct TyParams;
impl ListContext for TyParams {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        // FIXME: parse_state.is_ident
        s.token.kind.is_binding_ident()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, Great)
    }
}

#[derive(Copy, Clone)]
pub(super) struct HeritageClauses;
impl ListContext for HeritageClauses {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        s.token.kind.is_heritage_clause()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, LBrace | RBrace)
    }
}

#[derive(Copy, Clone)]
pub(super) struct HeritageClause;
impl ListContext for HeritageClause {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        // TODO: fixme
        s.token.kind.is_binding_ident()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use TokenKind::*;
        matches!(s.token.kind, LBrace) || s.token.kind.is_heritage_clause()
    }
}

#[derive(Copy, Clone)]
pub(super) struct VarDecl;
impl ListContext for VarDecl {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        s.token.kind.is_binding_ident_or_private_ident_or_pat()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        s.can_parse_semi()
    }
}

#[derive(Copy, Clone)]
pub(super) struct ArrayLiteralMembers;
impl ListContext for ArrayLiteralMembers {
    fn is_ele(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::Comma) || s.token.kind.is_start_of_expr()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBracket)
    }
}

use super::{ParserState, errors, lookahead::Lookahead};
use bolt_ts_ast::TokenKind;

pub(super) trait ListContext: Copy {
    fn is_ele(&self, s: &mut ParserState, is_error_recovery: bool) -> bool;
    fn is_closing(&self, s: &mut ParserState) -> bool;
    fn parsing_context_errors(&self, _: &mut ParserState) {}
    fn is_in_some_parsing_context(&self, s: &mut ParserState) -> bool {
        self.is_ele(s, true) || self.is_closing(s)
    }
}

#[derive(Copy, Clone)]
pub(super) struct SourceElems;
impl ListContext for SourceElems {
    fn is_ele(&self, s: &mut ParserState, in_error_recovery: bool) -> bool {
        !(s.token.kind == TokenKind::Semi && in_error_recovery) && s.is_start_of_stmt()
    }

    fn is_closing(&self, _: &mut ParserState) -> bool {
        // ensure terminal by `is_list_terminator`
        false
    }

    fn parsing_context_errors(&self, s: &mut ParserState) {
        if matches!(s.token.kind, TokenKind::Default) {
            let error = errors::ExpectX {
                x: "export".to_string(),
                span: s.token.span,
            };
            s.push_error(Box::new(error));
        } else {
            let error = errors::DeclarationOrStatementExpected { span: s.token.span };
            s.push_error(Box::new(error));
        }
    }
}

#[derive(Copy, Clone)]
pub(super) struct EnumMembers;
impl ListContext for EnumMembers {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        s.token.kind == TokenKind::LBracket || s.token.kind.is_lit_prop_name()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

#[derive(Copy, Clone)]
pub(super) struct BlockStmts;
impl ListContext for BlockStmts {
    fn is_ele(&self, s: &mut ParserState, is_error_recovery: bool) -> bool {
        !(matches!(s.token.kind, TokenKind::Semi) && is_error_recovery) && s.is_start_of_stmt()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }

    fn parsing_context_errors(&self, s: &mut ParserState) {
        let error = errors::DeclarationOrStatementExpected { span: s.token.span };
        s.push_error(Box::new(error));
    }
}

#[derive(Copy, Clone)]
pub(super) struct ArgExprs;
impl ListContext for ArgExprs {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        matches!(s.token.kind, TokenKind::DotDotDot) || s.is_start_of_expr()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RParen)
    }

    fn parsing_context_errors(&self, s: &mut ParserState) {
        let error = errors::ArgumentExpressionExpected { span: s.token.span };
        s.push_error(Box::new(error));
    }
}

#[derive(Copy, Clone)]
pub(super) struct ObjectLitMembers;
impl ListContext for ObjectLitMembers {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        use bolt_ts_ast::TokenKind::*;
        matches!(s.token.kind, LBrace | Asterisk | Dot | DotDotDot)
            || s.token.kind.is_lit_prop_name()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

fn is_ty_member_start(s: &mut ParserState) -> bool {
    use bolt_ts_ast::TokenKind::*;
    if matches!(s.token.kind, LParen | Less | Get | Set) {
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
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        s.lookahead(|l| is_ty_member_start(l.p()))
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

#[derive(Copy, Clone)]
pub(super) struct Params;
impl ListContext for Params {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        s.is_start_of_param()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use bolt_ts_ast::TokenKind::*;
        matches!(s.token.kind, RParen | RBracket)
    }
}

#[derive(Copy, Clone)]
pub(super) struct TyParams;
impl ListContext for TyParams {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        // FIXME: parse_state.is_ident
        matches!(s.token.kind, TokenKind::In | TokenKind::Const) || s.token.kind.is_binding_ident()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use bolt_ts_ast::TokenKind::*;
        // Tokens other than '>' are here for better error recovery
        matches!(s.token.kind, Great | LParen | LBrace | Extends | Implements)
    }
}

#[derive(Copy, Clone)]
pub(super) struct HeritageClause;
impl ListContext for HeritageClause {
    fn is_ele(&self, s: &mut ParserState, is_error_recovery: bool) -> bool {
        if s.token.kind == TokenKind::LBrace {
            s.lookahead(Lookahead::is_invalid_heritage_clause_object)
        } else if !is_error_recovery {
            s.is_start_of_left_hand_side_expr()
                && !s.is_heritage_clause_extends_or_implements_keyword()
        } else {
            s.is_ident() && !s.is_heritage_clause_extends_or_implements_keyword()
        }
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        use bolt_ts_ast::TokenKind::*;
        matches!(s.token.kind, LBrace) || s.token.kind.is_heritage_clause()
    }
}

#[derive(Copy, Clone)]
pub(super) struct VarDecls;
impl ListContext for VarDecls {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        s.token.kind.is_binding_ident_or_private_ident_or_pat()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        s.can_parse_semi()
            || s.token.kind.is_in_or_of_keyword()
            || s.token.kind == TokenKind::EqGreat
    }
}

#[derive(Copy, Clone)]
pub(super) struct ArrayLiteralMembers;
impl ListContext for ArrayLiteralMembers {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        use TokenKind::*;
        // TokenKind::{Comma, Dot} is not an array literal, just for close the array.
        matches!(s.token.kind, Comma | Dot | DotDotDot) || s.is_start_of_expr()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBracket)
    }
}

#[derive(Copy, Clone)]
pub(super) struct TyArgs;
impl ListContext for TyArgs {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        matches!(s.token.kind, TokenKind::Comma) || s.is_start_of_ty(false)
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        !matches!(s.token.kind, TokenKind::Comma)
    }
}

#[derive(Copy, Clone)]
pub(super) struct ObjectBindingElems;
impl ListContext for ObjectBindingElems {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        let t = s.token.kind;
        matches!(t, TokenKind::LBracket | TokenKind::DotDotDot) || t.is_lit_prop_name()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBrace)
    }
}

#[derive(Copy, Clone)]
pub(super) struct ArrayBindingElems;
impl ListContext for ArrayBindingElems {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        let t = s.token.kind;
        matches!(t, TokenKind::Comma | TokenKind::DotDotDot)
            || t.is_binding_ident_or_private_ident_or_pat()
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::RBracket)
    }
}

#[derive(Copy, Clone)]
pub(super) struct JsxAttrs;
impl ListContext for JsxAttrs {
    fn is_ele(&self, s: &mut ParserState, _: bool) -> bool {
        let t = s.token.kind;
        t.is_ident_or_keyword() || t == TokenKind::LBrace
    }

    fn is_closing(&self, s: &mut ParserState) -> bool {
        matches!(s.token.kind, TokenKind::Great | TokenKind::Slash)
    }
}

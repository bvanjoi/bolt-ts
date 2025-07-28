use crate::{errors, parse_class_like::is_class_ele_start};

use super::{ParserState, lookahead::Lookahead};
use bolt_ts_ast::TokenKind;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub(super) struct ParsingContext: u32 {
        const SOURCE_ELEMENTS = 1 << 0;
        const BLOCK_STATEMENTS = 1 << 1;
        const SWITCH_CLAUSES = 1 << 2;
        const SWITCH_CLAUSE_STATEMENTS = 1 << 3;
        const TYPE_MEMBERS = 1 << 4;
        const CLASS_MEMBERS = 1 << 5;
        const ENUM_MEMBERS = 1 << 6;
        const HERITAGE_CLAUSE_ELEMENT = 1 << 7;
        const VARIABLE_DECLARATIONS = 1 << 8;
        const OBJECT_BINDING_ELEMENTS = 1 << 9;
        const ARRAY_BINDING_ELEMENTS = 1 << 10;
        const ARGUMENT_EXPRESSIONS = 1 << 11;
        const OBJECT_LITERAL_MEMBERS = 1 << 12;
        const JSX_ATTRIBUTES = 1 << 13;
        const JSX_CHILDREN = 1 << 14;
        const ARRAY_LITERAL_MEMBERS = 1 << 15;
        const PARAMETERS = 1 << 16;
        const JSDOC_PARAMETERS = 1 << 17;
        const REST_PROPERTIES = 1 << 18;
        const TYPE_PARAMETERS = 1 << 19;
        const TYPE_ARGUMENTS = 1 << 20;
        const TUPLE_ELEMENT_TYPES = 1 << 21;
        const HERITAGE_CLAUSES = 1 << 22;
        const IMPORT_OR_EXPORT_SPECIFIERS = 1 << 23;
        const IMPORT_ATTRIBUTES = 1 << 24;
        const JSDOC_COMMENT = 1 << 25;
        const COUNT = 1 << 26;
    }
}

impl ParsingContext {
    pub const fn count() -> u32 {
        Self::COUNT.bits().trailing_zeros()
    }
}

impl ParserState<'_, '_> {
    pub(super) fn is_list_element(&mut self, ctx: ParsingContext, in_error_recovery: bool) -> bool {
        match ctx {
            ParsingContext::SOURCE_ELEMENTS
            | ParsingContext::BLOCK_STATEMENTS
            | ParsingContext::SWITCH_CLAUSE_STATEMENTS => {
                !(self.token.kind == TokenKind::Semi && in_error_recovery)
                    && self.is_start_of_stmt()
            }
            ParsingContext::SWITCH_CLAUSES => {
                todo!()
            }
            ParsingContext::TYPE_MEMBERS => self.lookahead(|l| is_ty_member_start(l.p())),
            ParsingContext::CLASS_MEMBERS => {
                self.lookahead(|l| is_class_ele_start(l.p()))
                    || (self.token.kind == TokenKind::Semi && !in_error_recovery)
            }
            ParsingContext::ENUM_MEMBERS => {
                self.token.kind == TokenKind::LBracket || self.token.kind.is_lit_prop_name()
            }
            ParsingContext::OBJECT_LITERAL_MEMBERS => {
                use bolt_ts_ast::TokenKind::*;
                matches!(self.token.kind, LBrace | Asterisk | Dot | DotDotDot)
                    || self.token.kind.is_lit_prop_name()
            }
            ParsingContext::REST_PROPERTIES => self.token.kind.is_lit_prop_name(),
            ParsingContext::OBJECT_BINDING_ELEMENTS => {
                let t = self.token.kind;
                matches!(t, TokenKind::LBracket | TokenKind::DotDotDot) || t.is_lit_prop_name()
            }
            ParsingContext::IMPORT_ATTRIBUTES => {
                todo!()
            }
            ParsingContext::HERITAGE_CLAUSE_ELEMENT => {
                if self.token.kind == TokenKind::LBrace {
                    self.lookahead(Lookahead::is_invalid_heritage_clause_object)
                } else if !in_error_recovery {
                    self.is_start_of_left_hand_side_expr()
                        && !self.is_heritage_clause_extends_or_implements_keyword()
                } else {
                    self.is_ident() && !self.is_heritage_clause_extends_or_implements_keyword()
                }
            }
            ParsingContext::VARIABLE_DECLARATIONS => {
                self.token.kind.is_binding_ident_or_private_ident_or_pat()
            }
            ParsingContext::ARRAY_BINDING_ELEMENTS => {
                let t = self.token.kind;
                matches!(t, TokenKind::Comma | TokenKind::DotDotDot)
                    || t.is_binding_ident_or_private_ident_or_pat()
            }
            ParsingContext::TYPE_PARAMETERS => {
                matches!(self.token.kind, TokenKind::In | TokenKind::Const) || self.is_ident()
            }
            ParsingContext::ARRAY_LITERAL_MEMBERS => {
                matches!(
                    self.token.kind,
                    TokenKind::Comma | TokenKind::Dot | TokenKind::DotDotDot
                ) || self.is_start_of_expr()
            }
            ParsingContext::ARGUMENT_EXPRESSIONS => {
                matches!(self.token.kind, TokenKind::DotDotDot) || self.is_start_of_expr()
            }
            ParsingContext::PARAMETERS => self.is_start_of_param(),
            ParsingContext::TYPE_ARGUMENTS | ParsingContext::TUPLE_ELEMENT_TYPES => {
                matches!(self.token.kind, TokenKind::Comma) || self.is_start_of_ty(false)
            }
            ParsingContext::HERITAGE_CLAUSES => {
                matches!(self.token.kind, TokenKind::Extends | TokenKind::Implements)
            }
            ParsingContext::IMPORT_OR_EXPORT_SPECIFIERS => {
                use bolt_ts_ast::TokenKind::*;
                if self.token.kind == From
                    && self.lookahead(|this| {
                        this.p().next_token();
                        matches!(this.p().token.kind, String)
                    })
                {
                    false
                } else if self.token.kind == String {
                    true
                } else {
                    self.token.kind.is_ident_or_keyword()
                }
            }
            ParsingContext::JSX_ATTRIBUTES => {
                self.token.kind.is_ident_or_keyword() || self.token.kind == TokenKind::LBrace
            }
            ParsingContext::JSX_CHILDREN => true,
            _ => unreachable!(),
        }
    }

    pub(super) fn is_list_terminator(&mut self, ctx: ParsingContext) -> bool {
        if self.token.kind == TokenKind::EOF {
            return true;
        }

        match ctx {
            ParsingContext::BLOCK_STATEMENTS
            | ParsingContext::SWITCH_CLAUSES
            | ParsingContext::TYPE_MEMBERS
            | ParsingContext::CLASS_MEMBERS
            | ParsingContext::ENUM_MEMBERS
            | ParsingContext::OBJECT_LITERAL_MEMBERS
            | ParsingContext::OBJECT_BINDING_ELEMENTS
            | ParsingContext::IMPORT_OR_EXPORT_SPECIFIERS
            | ParsingContext::IMPORT_ATTRIBUTES => {
                matches!(self.token.kind, TokenKind::RBrace)
            }
            ParsingContext::SWITCH_CLAUSE_STATEMENTS => {
                matches!(
                    self.token.kind,
                    TokenKind::RBrace | TokenKind::Case | TokenKind::Default
                )
            }
            ParsingContext::HERITAGE_CLAUSE_ELEMENT => {
                matches!(self.token.kind, TokenKind::LBrace) || self.token.kind.is_heritage_clause()
            }
            ParsingContext::VARIABLE_DECLARATIONS => {
                self.can_parse_semi()
                    || self.token.kind.is_in_or_of_keyword()
                    || self.token.kind == TokenKind::EqGreat
            }
            ParsingContext::TYPE_PARAMETERS => {
                matches!(
                    self.token.kind,
                    TokenKind::Great
                        | TokenKind::LParen
                        | TokenKind::LBrace
                        | TokenKind::Extends
                        | TokenKind::Implements
                )
            }
            ParsingContext::ARGUMENT_EXPRESSIONS => {
                matches!(self.token.kind, TokenKind::RParen | TokenKind::Semi)
            }
            ParsingContext::ARRAY_LITERAL_MEMBERS
            | ParsingContext::TUPLE_ELEMENT_TYPES
            | ParsingContext::ARRAY_BINDING_ELEMENTS => {
                matches!(self.token.kind, TokenKind::RBracket)
            }
            ParsingContext::PARAMETERS | ParsingContext::REST_PROPERTIES => {
                matches!(self.token.kind, TokenKind::RParen | TokenKind::RBracket)
            }
            ParsingContext::TYPE_ARGUMENTS => !matches!(self.token.kind, TokenKind::Comma),
            ParsingContext::HERITAGE_CLAUSES => {
                matches!(self.token.kind, TokenKind::LBrace | TokenKind::RBrace)
            }
            ParsingContext::JSX_ATTRIBUTES => {
                matches!(self.token.kind, TokenKind::Great | TokenKind::Slash)
            }
            ParsingContext::JSX_CHILDREN => {
                self.token.kind == TokenKind::Less
                    && self.lookahead(|l| {
                        l.p().next_token();
                        matches!(l.p().token.kind, TokenKind::Slash)
                    })
            }
            ParsingContext::SOURCE_ELEMENTS => false,
            _ => {
                unreachable!("ctx: {ctx:?}")
            }
        }
    }

    pub(super) fn is_in_some_parsing_context(&mut self) -> bool {
        for i in 0..ParsingContext::count() {
            let parsing_ctx = ParsingContext::from_bits(1 << i).unwrap();
            if self.parsing_context.intersects(parsing_ctx)
                && (self.is_list_element(parsing_ctx, true) || self.is_list_terminator(parsing_ctx)) {
                    return true;
                }
        }

        false
    }

    pub(super) fn parsing_context_errors(&mut self, ctx: ParsingContext) {
        match ctx {
            ParsingContext::SOURCE_ELEMENTS => {
                if matches!(self.token.kind, TokenKind::Default) {
                    let error = errors::ExpectX {
                        x: "export".to_string(),
                        span: self.token.span,
                    };
                    self.push_error(Box::new(error));
                } else {
                    let error = errors::DeclarationOrStatementExpected {
                        span: self.token.span,
                    };
                    self.push_error(Box::new(error));
                }
            }
            ParsingContext::BLOCK_STATEMENTS => {
                let error = errors::DeclarationOrStatementExpected {
                    span: self.token.span,
                };
                self.push_error(Box::new(error));
            }
            ParsingContext::ARGUMENT_EXPRESSIONS => {
                let error = errors::ArgumentExpressionExpected {
                    span: self.token.span,
                };
                self.push_error(Box::new(error));
            }
            _ => {}
        }
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

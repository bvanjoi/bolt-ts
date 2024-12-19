use crate::atoms::AtomId;

use super::list_ctx;
use super::token::{TokenFlags, TokenKind};
use super::{ast, errors};
use super::{PResult, ParserState};

pub(super) trait ParseSuccess {
    fn is_success(&self) -> bool;
}

impl ParseSuccess for bool {
    fn is_success(&self) -> bool {
        *self
    }
}

impl<T, E> ParseSuccess for Result<Option<T>, E> {
    fn is_success(&self) -> bool {
        !self.is_err()
    }
}

pub(super) fn is_left_hand_side_expr_kind(expr: &ast::Expr) -> bool {
    use ast::ExprKind::*;
    matches!(
        expr.kind,
        PropAccess(_)
            | EleAccess(_)
            | New(_)
            | Call(_)
            | ArrayLit(_)
            | Paren(_)
            | Class(_)
            | Fn(_)
            | Ident(_)
            | This(_)
    )
}

impl<'p, 't> ParserState<'p, 't> {
    fn _is_paren_arrow_fn_expr(&mut self) -> bool {
        use TokenKind::*;
        if self.token.kind == TokenKind::Async {
            self.next_token();
            if self.has_preceding_line_break() {
                return false;
            } else if matches!(self.token.kind, LParen | Less) {
                return false;
            }
        }

        let first = self.token.kind;
        self.next_token();
        let second = self.token.kind;

        if first == LParen {
            if second == RParen {
                self.next_token();
                matches!(self.token.kind, EqGreater | Colon | RBrace)
            } else if second == LBracket || second == LBrace {
                todo!()
            } else if second == DotDotDot {
                true
            } else if second != Async
                && second.is_modifier_kind()
                && self
                    .lookahead(Self::next_token_is_ident)
                    .unwrap_or_default()
            {
                self.next_token();
                self.token.kind != As
            } else if !self.is_ident() && second != This {
                false
            } else {
                self.next_token();
                match self.token.kind {
                    Colon => true,
                    Question => todo!(),
                    Comma | Eq | RParen => {
                        // TODO: unknown
                        false
                    }
                    _ => false,
                }
            }
        } else {
            assert_eq!(first, Less);
            // TODO: unknown
            false
        }
    }

    pub(super) fn is_paren_arrow_fn_expr(&mut self) -> bool {
        let t = self.token.kind;

        if t == TokenKind::LParen {
            return self.lookahead(Self::_is_paren_arrow_fn_expr);
        }

        false
    }

    pub(super) fn parse_fn_block(&mut self) -> PResult<Option<&'p ast::BlockStmt<'p>>> {
        if self.token.kind != TokenKind::LBrace && self.can_parse_semi() {
            self.parse_semi();
            return Ok(None);
        }
        self.parse_block().map(|block| Some(block))
    }

    pub(super) fn parse_block(&mut self) -> PResult<&'p ast::BlockStmt<'p>> {
        let id = self.next_node_id();
        let start = self.token.start();
        use TokenKind::*;
        self.expect(LBrace)?;
        let stmts = self.with_parent(id, |this| {
            this.parse_list(list_ctx::BlockStmt, Self::parse_stmt)
        });
        self.expect(RBrace)?;
        let stmt = self.alloc(ast::BlockStmt {
            id,
            span: self.new_span(start as usize, self.pos),
            stmts,
        });
        self.insert_map(id, ast::Node::BlockStmt(stmt));
        Ok(stmt)
    }

    pub(super) fn parse_ty_params(&mut self) -> PResult<Option<ast::TyParams<'p>>> {
        if self.token.kind == TokenKind::Less {
            let less_token_span = self.token.span;
            let ty_params = self.parse_bracketed_list(
                list_ctx::TyParams,
                TokenKind::Less,
                Self::parse_ty_param,
                TokenKind::Great,
            )?;
            if ty_params.is_empty() {
                let error = errors::TypeParameterListCannotBeEmpty {
                    span: less_token_span,
                };
                self.push_error(Box::new(error));
                Ok(None)
            } else {
                Ok(Some(ty_params))
            }
        } else {
            Ok(None)
        }
    }

    pub(super) fn is_start_of_ty(&mut self) -> bool {
        use TokenKind::*;
        if matches!(
            self.token.kind,
            String | Number | LBrace | LBracket | DotDotDot
        ) {
            true
        } else {
            self.is_ident()
        }
    }

    pub(super) fn is_start_of_expr(&self) -> bool {
        self.token.kind.is_start_of_left_hand_side_expr()
    }

    pub(super) fn is_start_of_stmt(&mut self) -> bool {
        use TokenKind::*;
        let t = self.token.kind;
        if matches!(t, Export | Const) {
            self.is_start_of_decl()
        } else {
            matches!(
                t,
                Interface
                | Module
                | Namespace
                | Type
                // ==
                | Semi
                | Var
                | Let
                | Function
                | If
                | Return
                | Class
                | Throw
                | Try
                | Catch
                | Finally
                | Debugger
            ) || self.is_start_of_expr()
        }
    }

    fn parse_ty_param(&mut self) -> PResult<&'p ast::TyParam<'p>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let name = self.with_parent(id, Self::parse_binding_ident);
        let constraint = if self.parse_optional(TokenKind::Extends).is_some() {
            if self.is_start_of_ty() || !self.is_start_of_expr() {
                Some(self.parse_ty()?)
            } else {
                todo!("token: {:#?}", self.token.kind)
            }
        } else {
            None
        };
        let default = if self.parse_optional(TokenKind::Eq).is_some() {
            Some(self.parse_ty()?)
        } else {
            None
        };
        let ty_param = self.alloc(ast::TyParam {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            constraint,
            default,
        });
        self.insert_map(id, ast::Node::TyParam(ty_param));
        Ok(ty_param)
    }

    pub(super) fn parse_ident(&mut self) -> &'p ast::Expr<'p> {
        let kind = self.create_ident(self.token.kind.is_ident());
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Ident(kind),
        });
        expr
    }

    pub(super) fn parse_prop_name(&mut self) -> PResult<&'p ast::PropName<'p>> {
        let kind = match self.token.kind {
            TokenKind::String => {
                let lit = self.parse_string_lit();
                ast::PropNameKind::StringLit(lit)
            }
            TokenKind::Number => {
                let lit = self.parse_num_lit(self.number_token(), false);
                ast::PropNameKind::NumLit(lit)
            }
            _ => {
                let ident = self.parse_ident_name()?;
                ast::PropNameKind::Ident(ident)
            }
        };
        let prop_name = self.alloc(ast::PropName { kind });
        Ok(prop_name)
    }

    pub(super) fn is_ident(&self) -> bool {
        matches!(self.token.kind, TokenKind::Ident | TokenKind::Abstract)
    }

    pub(super) fn next_token_is_ident(&mut self) -> PResult<bool> {
        self.next_token();
        Ok(self.is_ident())
    }

    pub(super) fn next_token_is_ident_or_keyword(&mut self) -> PResult<bool> {
        self.next_token();
        Ok(self.token.kind.is_ident_or_keyword())
    }

    pub(super) fn next_token_is_ident_or_keyword_on_same_line(&mut self) -> PResult<bool> {
        self.next_token();
        return Ok(self.token.kind.is_ident_or_keyword() && !self.has_preceding_line_break());
    }

    pub(super) fn parse_modifiers(
        &mut self,
        permit_const_as_modifier: bool,
    ) -> PResult<Option<&'p ast::Modifiers<'p>>> {
        let start = self.token.start();
        let mut list = Vec::with_capacity(4);
        loop {
            let Ok(Some(m)) = self.parse_modifier(permit_const_as_modifier) else {
                break;
            };
            list.push(m);
        }
        if list.is_empty() {
            Ok(None)
        } else {
            let span = self.new_span(start as usize, self.pos);
            let flags = list
                .iter()
                .fold(Default::default(), |flags, m| flags | m.kind);
            let ms = self.alloc(ast::Modifiers {
                span,
                flags,
                list: self.alloc(list),
            });
            Ok(Some(ms))
        }
    }

    fn can_follow_modifier(&self) -> bool {
        let t = self.token.kind;
        use TokenKind::*;
        matches!(t, LBracket | LBrace | Asterisk | DotDotDot) || t.is_lit_prop_name()
    }

    fn next_token_is_on_same_line_and_can_follow_modifier(&mut self) -> bool {
        self.next_token();
        if self.has_preceding_line_break() {
            false
        } else {
            self.can_follow_modifier()
        }
    }

    fn next_token_can_follow_modifier(&mut self) -> bool {
        use TokenKind::*;
        match self.token.kind {
            Const => {
                self.next_token();
                self.token.kind == Enum
            }
            _ => self.next_token_is_on_same_line_and_can_follow_modifier(),
        }
    }

    fn parse_any_contextual_modifier(&mut self) -> bool {
        self.token.kind.is_modifier_kind() && self.try_parse(Self::next_token_can_follow_modifier)
    }

    fn parse_modifier(
        &mut self,
        permit_const_as_modifier: bool,
    ) -> PResult<Option<&'p ast::Modifier>> {
        let span = self.token.span;
        let t = self.token.kind;
        if t == TokenKind::Const && permit_const_as_modifier {
            if self.try_parse(Self::next_token_is_on_same_line_and_can_follow_modifier) {
                return Ok(None);
            }
        } else if !self.parse_any_contextual_modifier() {
            return Ok(None);
        }

        let id = self.next_node_id();
        let kind = t.try_into().unwrap();
        let m = self.alloc(ast::Modifier { id, span, kind });
        self.insert_map(id, ast::Node::Modifier(m));
        Ok(Some(m))
    }

    pub(super) fn try_parse<T: ParseSuccess>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_pos = self.pos;
        let old_full_start_pos = self.full_start_pos;
        let old_token = self.token;
        let old_token_value = self.token_value;

        let res = f(self);

        if !res.is_success() {
            self.token_value = old_token_value;
            self.token = old_token;
            self.full_start_pos = old_full_start_pos;
            self.pos = old_pos;
        }

        res
    }

    pub(super) fn lookahead<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_pos = self.pos;
        let old_full_start_pos = self.full_start_pos;
        let old_token = self.token;
        let old_token_value = self.token_value;

        let r = f(self);

        self.token_value = old_token_value;
        self.token = old_token;
        self.full_start_pos = old_full_start_pos;
        self.pos = old_pos;

        r
    }

    pub(super) fn parse_ident_name(&mut self) -> PResult<&'p ast::Ident> {
        Ok(self.create_ident(true))
    }

    pub(super) fn parse_semi_after_prop_name(&mut self) {
        self.parse_semi();
    }

    pub(super) fn is_implements_clause(&mut self) -> bool {
        self.token.kind == TokenKind::Implements
            && self
                .lookahead(Self::next_token_is_ident)
                .unwrap_or_default()
    }

    fn is_unambiguously_index_sig(&mut self) -> PResult<bool> {
        self.next_token();

        if self.token.kind == TokenKind::DotDotDot || self.token.kind == TokenKind::RBracket {
            return Ok(true);
        } else if self.token.kind.is_modifier_kind() {
            self.next_token();
            if self.is_ident() {
                return Ok(true);
            }
        } else if !self.is_ident() {
            return Ok(false);
        } else {
            self.next_token();
        }

        if self.token.kind == TokenKind::Colon || self.token.kind == TokenKind::Comma {
            return Ok(true);
        } else if self.token.kind != TokenKind::Question {
            return Ok(false);
        }

        self.next_token();

        Ok(self.token.kind == TokenKind::Colon
            || self.token.kind == TokenKind::Comma
            || self.token.kind == TokenKind::RBracket)
    }

    pub(super) fn is_index_sig(&mut self) -> bool {
        self.token.kind == TokenKind::LBracket
            && self
                .lookahead(Self::is_unambiguously_index_sig)
                .unwrap_or_default()
    }

    pub(super) fn parse_params(&mut self) -> PResult<ast::ParamsDecl<'p>> {
        use TokenKind::*;
        self.expect(LParen)?;
        let params = self.parse_delimited_list(list_ctx::Params, Self::parse_param);
        self.expect(RParen)?;
        Ok(params)
    }

    pub(super) fn parse_param(&mut self) -> PResult<&'p ast::ParamDecl<'p>> {
        let start = self.token.start();
        let id = self.next_node_id();
        let dotdotdot = self.parse_optional(TokenKind::DotDotDot).map(|t| t.span);
        let name = self.with_parent(id, Self::parse_ident_name)?;
        let question = self.parse_optional(TokenKind::Question).map(|t| t.span);
        let ty = self.with_parent(id, Self::parse_ty_anno)?;
        let init = self.with_parent(id, Self::parse_init);
        let decl = self.alloc(ast::ParamDecl {
            id,
            span: self.new_span(start as usize, self.pos),
            dotdotdot,
            name,
            question,
            ty,
            init,
        });
        self.insert_map(id, ast::Node::ParamDecl(decl));
        Ok(decl)
    }

    pub(super) fn parse_ty_member_semi(&mut self) {
        if self.parse_optional(TokenKind::Semi).is_some() {
            return;
        }
        self.parse_semi();
    }

    pub(super) fn is_start_of_fn_or_ctor_ty(&mut self) -> bool {
        let t = self.token.kind;
        if t == TokenKind::Less {
            true
        } else if t == TokenKind::LParen && self.lookahead(Self::is_unambiguously_start_of_fn_ty) {
            true
        } else {
            false
        }
    }

    fn skip_param_start(&mut self) -> PResult<bool> {
        if self.token.kind.is_modifier_kind() {
            self.parse_modifiers(false)?;
        }
        if self.token.kind.is_ident() || self.token.kind == TokenKind::This {
            self.next_token();
            Ok(true)
        } else if matches!(self.token.kind, TokenKind::LBracket | TokenKind::LBrace) {
            // todo: parse ident or pattern
            self.parse_ident();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn is_unambiguously_start_of_fn_ty(&mut self) -> bool {
        self.next_token();
        let t = self.token.kind;
        use TokenKind::*;
        if matches!(t, TokenKind::RParen | TokenKind::DotDotDot) {
            return true;
        } else if self.skip_param_start().unwrap_or_default() {
            if matches!(self.token.kind, Colon | Comma | Question | Eq) {
                return true;
            } else if self.token.kind == RParen {
                self.next_token();
                if self.token.kind == EqGreater {
                    return true;
                }
            }
        }

        false
    }

    pub(super) fn parse_contextual_modifier(&mut self, t: TokenKind) -> bool {
        return self.token.kind == t && self.try_parse(Self::next_token_can_follow_modifier);
    }

    pub(super) fn parse_num_lit(&mut self, val: f64, neg: bool) -> &'p ast::NumLit {
        let val = if neg { -val } else { val };
        let lit = self.create_lit(val, self.token.span);
        self.insert_map(lit.id, ast::Node::NumLit(lit));
        self.next_token();
        lit
    }

    pub(super) fn parse_string_lit(&mut self) -> &'p ast::StringLit {
        let val = self.string_token();
        let lit = self.create_lit(val, self.token.span);
        self.insert_map(lit.id, ast::Node::StringLit(lit));
        self.next_token();
        lit
    }

    pub(super) fn has_preceding_line_break(&self) -> bool {
        self.token_flags
            .intersects(TokenFlags::PRECEDING_LINE_BREAK)
    }

    pub(super) fn parse_index_sig_decl(
        &mut self,
        id: ast::NodeID,
        start: usize,
        modifiers: Option<&'p ast::Modifiers<'p>>,
    ) -> PResult<&'p ast::IndexSigDecl<'p>> {
        let params = self.parse_bracketed_list(
            list_ctx::Params,
            TokenKind::LBracket,
            Self::parse_param,
            TokenKind::RBracket,
        )?;
        let Some(ty) = self.parse_ty_anno()? else {
            todo!("error handler")
        };
        self.parse_ty_member_semi();
        let sig = self.alloc(ast::IndexSigDecl {
            id,
            span: self.new_span(start, self.pos),
            modifiers,
            params,
            ty,
        });
        self.insert_map(id, ast::Node::IndexSigDecl(sig));
        Ok(sig)
    }
}

use super::list_ctx;
use super::token::{TokenFlags, TokenKind};
use super::{ast, errors};
use super::{PResult, ParserState};

pub(super) fn is_left_hand_side_expr_kind(expr: &ast::Expr) -> bool {
    use ast::ExprKind::*;
    matches!(expr.kind, PropAccess(_) | Ident(_) | New(_) | Call(_))
}

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub(super) fn is_decl(&mut self) -> bool {
        use TokenKind::*;
        loop {
            match self.token.kind {
                Var | Let | Const | Function | Class => return true,
                Abstract | Declare | Public => {
                    // let prev = self.token.kind;
                    self.next_token();
                    continue;
                }
                _ => unreachable!("{:#?}", self.token.kind),
            }
        }
    }

    pub(super) fn is_start_of_decl(&mut self) -> bool {
        self.lookahead(Self::is_decl)
    }

    fn _is_paren_arrow_fn_expr(&mut self) -> bool {
        use TokenKind::*;
        let first = self.token.kind;
        self.next_token();
        let second = self.token.kind;

        if first == LParen {
            if second == RParen {
                self.next_token();
                let third = self.token.kind;
                return matches!(third, EqGreater | Colon | RBrace);
            } else if second == LBracket || second == LBrace {
                todo!()
            } else if second == DotDotDot {
                return true;
            } else if second != TokenKind::Async
                && second.is_modifier_kind()
                && self
                    .lookahead(Self::next_token_is_ident)
                    .unwrap_or_default()
            {
                self.next_token();
                return if self.token.kind == TokenKind::As {
                    false
                } else {
                    true
                };
            } else if !self.is_ident() && second != TokenKind::This {
                return false;
            } else {
                self.next_token();
                todo!()
            }
        }

        false
    }

    pub(super) fn is_paren_arrow_fn_expr(&mut self) -> bool {
        let t = self.token.kind;

        if t == TokenKind::LParen {
            return self.lookahead(Self::_is_paren_arrow_fn_expr);
        }

        false
    }

    pub(super) fn parse_fn_block(&mut self) -> PResult<Option<&'cx ast::BlockStmt<'cx>>> {
        if self.token.kind != TokenKind::LBrace {
            if self.can_parse_semi() {
                self.parse_semi();
                return Ok(None);
            }
        }
        self.parse_block().map(|block| Some(block))
    }

    pub(super) fn parse_block(&mut self) -> PResult<&'cx ast::BlockStmt<'cx>> {
        let id = self.p.next_node_id();
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

    pub(super) fn parse_ty_params(&mut self) -> PResult<Option<ast::TyParams<'cx>>> {
        if self.token.kind == TokenKind::Less {
            let less_token_span = self.token.span;
            let ty_params = self.parse_bracketed_list(
                TokenKind::Less,
                list_ctx::TyParams,
                Self::parse_ty_param,
                TokenKind::Great,
            )?;
            if ty_params.is_empty() {
                let error = errors::TypeParameterListCannotBeEmpty {
                    span: less_token_span,
                };
                self.push_error(self.module_id, Box::new(error));
                Ok(None)
            } else {
                Ok(Some(ty_params))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_ty_param(&mut self) -> PResult<&'cx ast::TyParam<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        let name = self.with_parent(id, Self::parse_binding_ident);
        let ty_param = self.alloc(ast::TyParam {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
        });
        self.insert_map(id, ast::Node::TyParam(ty_param));
        Ok(ty_param)
    }

    pub(super) fn parse_ident(&mut self) -> &'cx ast::Expr<'cx> {
        let kind = self.create_ident(true);
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Ident(kind),
        });
        expr
    }

    pub(super) fn parse_prop_name(&mut self) -> PResult<&'cx ast::PropName<'cx>> {
        let ident = self.parse_ident_name()?;
        let prop_name = self.alloc(ast::PropName {
            kind: ast::PropNameKind::Ident(ident),
        });
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

    pub(super) fn is_start_of_mapped_ty(&mut self) -> PResult<bool> {
        self.next_token();
        if self.token.kind == TokenKind::Plus || self.token.kind == TokenKind::Minus {
            self.next_token();
            // return self.token.kind == TokenKind::Readonly;
            todo!()
        }

        // self.token.kind == TokenKind::LBracket && self.next_token_is_ident().unwrap_or_default() &&
        todo!()
    }

    pub(super) fn parse_modifiers(&mut self) -> PResult<Option<&'cx ast::Modifiers<'cx>>> {
        let start = self.token.start();
        let mut list = Vec::with_capacity(4);
        loop {
            let Ok(Some(m)) = self.parse_modifier() else {
                break;
            };
            list.push(m);
        }
        if list.is_empty() {
            Ok(None)
        } else {
            let span = self.new_span(start as usize, self.pos);
            let ms = self.alloc(ast::Modifiers {
                span,
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
        if self.token_flags.contains(TokenFlags::PRECEDING_LINE_BREAK) {
            false
        } else {
            self.can_follow_modifier()
        }
    }

    fn next_token_can_follow_modifier(&mut self) -> bool {
        use TokenKind::*;
        match self.token.kind {
            Const => todo!(),
            _ => self.next_token_is_on_same_line_and_can_follow_modifier(),
        }
    }

    fn parse_modifier(&mut self) -> PResult<Option<&'cx ast::Modifier>> {
        let span = self.token.span;
        let t = self.token.kind;
        if !(self.token.kind.is_modifier_kind()
            && self.try_parse(Self::next_token_can_follow_modifier))
        {
            return Ok(None);
        }
        let id = self.p.next_node_id();
        let kind = t.into();
        let m = self.alloc(ast::Modifier { id, span, kind });
        self.insert_map(id, ast::Node::Modifier(m));
        Ok(Some(m))
    }

    pub(super) fn try_parse<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.speculation_helper(f, true)
    }

    pub(super) fn parse_ident_name(&mut self) -> PResult<&'cx ast::Ident> {
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

    pub(super) fn parse_params(&mut self) -> PResult<ast::ParamsDecl<'cx>> {
        use TokenKind::*;
        self.expect(LParen)?;
        let params = self.parse_delimited_list(list_ctx::Params, Self::parse_param);
        self.expect(RParen)?;
        Ok(params)
    }

    pub(super) fn parse_param(&mut self) -> PResult<&'cx ast::ParamDecl<'cx>> {
        let start = self.token.start();
        let id = self.p.next_node_id();
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
            self.parse_modifiers()?;
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
}

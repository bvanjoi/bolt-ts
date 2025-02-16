use bolt_ts_span::Span;

use crate::ast::ModifierKind;
use crate::{ecma_rules, keyword};

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
            | NumLit(_)
            | StringLit(_)
            | BoolLit(_)
            | Super(_)
    )
}

impl<'cx> ParserState<'cx, '_> {
    pub(super) fn parse_fn_block(&mut self) -> PResult<Option<&'cx ast::BlockStmt<'cx>>> {
        if self.token.kind != TokenKind::LBrace && self.can_parse_semi() {
            self.parse_semi();
            return Ok(None);
        }
        self.parse_block().map(Some)
    }

    pub(super) fn parse_expected_matching_brackets(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        open_parsed: bool,
        open_pos: usize,
    ) -> PResult<()> {
        if self.token.kind == close {
            self.next_token();
            return Ok(());
        }

        let mut expected_error = errors::KindExpected {
            span: self.token.span,
            expected: close.as_str().to_string(),
            related: None,
        };

        if open_parsed {
            expected_error.related = Some(errors::ExpectedToFindAToMatchTheBTokenHere {
                span: Span::new(open_pos as u32, open_pos as u32 + 1, self.module_id),
                expected: open.as_str().to_string(),
                found: close.as_str().to_string(),
            });
        }
        self.push_error(Box::new(expected_error));
        Ok(())
    }

    pub(super) fn parse_block(&mut self) -> PResult<&'cx ast::BlockStmt<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        use TokenKind::*;
        let open = LBrace;
        let open_brace_parsed = self.expect(LBrace);
        let stmts = self.with_parent(id, |this| {
            this.parse_list(list_ctx::BlockStmts, Self::parse_stmt)
        });
        self.parse_expected_matching_brackets(open, RBrace, open_brace_parsed, start as usize)?;
        let stmt = self.alloc(ast::BlockStmt {
            id,
            span: self.new_span(start),
            stmts,
        });
        self.insert_map(id, ast::Node::BlockStmt(stmt));
        Ok(stmt)
    }

    pub(super) fn parse_ty_params(&mut self) -> PResult<Option<ast::TyParams<'cx>>> {
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

    pub(super) fn is_start_of_ty(&mut self, is_start_of_param: bool) -> bool {
        use TokenKind::*;
        if matches!(
            self.token.kind,
            LBrace
                | LBracket
                | DotDotDot
                | Typeof
                | String
                | Number
                | Null
                | Undefined
                | Void
                | True
                | False
                | Readonly
        ) {
            true
        } else if self.token.kind == TokenKind::Function {
            !is_start_of_param
        } else if self.token.kind == TokenKind::LParen && !is_start_of_param {
            self.lookahead(|this| {
                this.next_token();
                let t = this.token.kind;
                t == TokenKind::RParen || t.is_start_of_param() || this.is_start_of_ty(false)
            })
        } else {
            self.is_ident()
        }
    }

    pub(super) fn is_start_of_expr(&self) -> bool {
        use TokenKind::*;
        let t = self.token.kind;
        if t.is_start_of_left_hand_side_expr()
            || matches!(
                t,
                Plus | Minus
                    | Tilde
                    | Excl
                    | Delete
                    | Typeof
                    | Void
                    | PlusPlus
                    | MinusMinus
                    | Less
                    | Await
                    | Yield
                    | Private
                    | At
            )
        {
            true
        } else {
            self.is_ident()
        }
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
                | For
                | Continue
                | Break
                | Throw
                | Try
                | Catch
                | Finally
                | Debugger
            ) || self.is_start_of_expr()
        }
    }

    fn parse_ty_param(&mut self) -> PResult<&'cx ast::TyParam<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let name = self.with_parent(id, Self::parse_binding_ident);
        let constraint = if self.parse_optional(TokenKind::Extends).is_some() {
            if self.is_start_of_ty(false) || !self.is_start_of_expr() {
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
            span: self.new_span(start),
            name,
            constraint,
            default,
        });
        self.insert_map(id, ast::Node::TyParam(ty_param));
        Ok(ty_param)
    }

    pub(super) fn parse_ident(
        &mut self,
        missing_ident_kind: Option<errors::MissingIdentKind>,
    ) -> &'cx ast::Expr<'cx> {
        let kind = self.create_ident(self.token.kind.is_ident(), missing_ident_kind);
        let expr = self.alloc(ast::Expr {
            kind: ast::ExprKind::Ident(kind),
        });
        expr
    }

    pub(super) fn parse_prop_name(&mut self) -> PResult<&'cx ast::PropName<'cx>> {
        let kind = match self.token.kind {
            TokenKind::String => {
                let raw = self.parse_string_lit();
                let key = self.string_key_value.unwrap();
                ast::PropNameKind::StringLit { raw, key }
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

    #[inline(always)]
    pub(super) fn is_ident(&self) -> bool {
        let t = self.token.kind;
        if t == TokenKind::Ident {
            return true;
        }

        t.is_contextual_keyword() || t.is_strict_mode_reserved_word()
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
        Ok(self.token.kind.is_ident_or_keyword() && !self.has_preceding_line_break())
    }

    pub(super) fn parse_modifiers(
        &mut self,
        permit_const_as_modifier: bool,
    ) -> PResult<Option<&'cx ast::Modifiers<'cx>>> {
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
            let span = self.new_span(start);
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

    pub(super) fn parse_ident_name(&mut self) -> PResult<&'cx ast::Ident> {
        let is_ident = self.token.kind.is_ident_or_keyword();
        Ok(self.create_ident(is_ident, None))
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
        self.expect(LParen);
        let old_error = self.diags.len();
        let params = self.parse_delimited_list(list_ctx::Params, Self::parse_param);
        let has_error = self.diags.len() > old_error;
        if !has_error {
            let last_is_rest = params
                .last()
                .map(|p| p.dotdotdot.is_some())
                .unwrap_or_default();
            if let Some(rest_param_but_not_last) = (!last_is_rest).then(|| {
                params
                    .iter()
                    .rev()
                    .skip(1)
                    .filter(|param| param.dotdotdot.is_some())
                    .rev()
                    .collect::<Vec<_>>()
            }) {
                for error_param in rest_param_but_not_last {
                    let error = ecma_rules::ARestParameterMustBeLastInAParameterList {
                        span: error_param.span,
                    };
                    self.push_error(Box::new(error));
                }
            }
        }

        self.expect(RParen);
        Ok(params)
    }

    pub(super) fn parse_param(&mut self) -> PResult<&'cx ast::ParamDecl<'cx>> {
        let start = self.token.start();
        let id = self.next_node_id();
        let modifiers = self.parse_modifiers(false)?;
        const INVALID_MODIFIERS: enumflags2::BitFlags<ModifierKind, u16> =
            enumflags2::make_bitflags!(ModifierKind::{Static | Export});
        if modifiers
            .map(|ms| ms.flags.intersects(INVALID_MODIFIERS))
            .unwrap_or_default()
        {
            if let Some(ms) = modifiers.map(|ms| {
                ms.list
                    .iter()
                    .filter(|m| INVALID_MODIFIERS.intersects(m.kind))
                    .copied()
            }) {
                for m in ms {
                    let error = errors::ModifierCannotAppearOnAParameter {
                        span: m.span,
                        kind: m.kind,
                    };
                    self.push_error(Box::new(error));
                }
            }
        }

        let dotdotdot = self.parse_optional(TokenKind::DotDotDot).map(|t| t.span);
        let name = self.with_parent(id, Self::parse_ident_name)?;
        if dotdotdot.is_some() {
            if let Some(ms) = modifiers {
                if ms.flags.intersects(ModifierKind::PARAMETER_PROPERTY) {
                    let kinds = ms
                        .list
                        .iter()
                        .filter_map(|m| {
                            if ModifierKind::PARAMETER_PROPERTY.intersects(m.kind) {
                                Some(m.kind)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    let span = Span::new(ms.span.lo, name.span.hi, name.span.module);
                    let error = errors::AParameterPropertyCannotBeDeclaredUsingARestParameter {
                        span,
                        kinds,
                    };
                    self.push_error(Box::new(error));
                }
            }
        };
        let question = self.parse_optional(TokenKind::Question).map(|t| t.span);
        let ty = self.with_parent(id, Self::parse_ty_anno)?;
        let init = self.with_parent(id, Self::parse_init)?;
        let decl = self.alloc(ast::ParamDecl {
            id,
            span: self.new_span(start),
            modifiers,
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
        let skip_param_start = |this: &mut Self| -> PResult<bool> {
            if this.token.kind.is_modifier_kind() {
                this.parse_modifiers(false)?;
            }
            if this.token.kind.is_ident() || this.token.kind == TokenKind::This {
                this.next_token();
                Ok(true)
            } else if matches!(this.token.kind, TokenKind::LBracket | TokenKind::LBrace) {
                let len = this.diags.len();
                // todo: parse ident or pattern
                this.parse_ident(None);
                this.diags.truncate(len);
                Ok(true)
            } else {
                Ok(false)
            }
        };
        let is_unambiguously_start_of_fn_ty = |this: &mut Self| {
            this.next_token();
            let t = this.token.kind;
            use TokenKind::*;
            if matches!(t, TokenKind::RParen | TokenKind::DotDotDot) {
                return true;
            } else if skip_param_start(this).unwrap_or_default() {
                if matches!(this.token.kind, Colon | Comma | Question | Eq) {
                    return true;
                } else if this.token.kind == RParen {
                    this.next_token();
                    if this.token.kind == EqGreat {
                        return true;
                    }
                }
            }
            false
        };

        let t = self.token.kind;
        (t == TokenKind::Less)
            || (t == TokenKind::LParen && self.lookahead(is_unambiguously_start_of_fn_ty))
            || (t == TokenKind::New
                || t == TokenKind::Abstract
                    && self.lookahead(|this| {
                        this.next_token();
                        this.token.kind == TokenKind::New
                    }))
    }

    pub(super) fn parse_contextual_modifier(&mut self, t: TokenKind) -> bool {
        self.token.kind == t && self.try_parse(Self::next_token_can_follow_modifier)
    }

    pub(super) fn parse_num_lit(&mut self, val: f64, neg: bool) -> &'cx ast::NumLit {
        let val = if neg { -val } else { val };
        let lit = self.create_lit(val, self.token.span);
        self.insert_map(lit.id, ast::Node::NumLit(lit));
        self.next_token();
        lit
    }

    pub(super) fn parse_string_lit(&mut self) -> &'cx ast::StringLit {
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

    fn create_missing_ty(&mut self) -> &'cx ast::Ty<'cx> {
        let id = self.next_node_id();
        let start = self.token.start();
        let ident = self.create_ident_by_atom(keyword::IDENT_EMPTY, self.token.span);
        let name = self.alloc(ast::EntityName {
            kind: ast::EntityNameKind::Ident(ident),
        });
        let ty = self.alloc(ast::ReferTy {
            id,
            span: self.new_span(start),
            name,
            ty_args: None,
        });
        self.alloc(ast::Ty {
            kind: ast::TyKind::Refer(ty),
        })
    }

    pub(super) fn parse_index_sig_decl(
        &mut self,
        id: ast::NodeID,
        start: usize,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::IndexSigDecl<'cx>> {
        let params = self.parse_bracketed_list(
            list_ctx::Params,
            TokenKind::LBracket,
            Self::parse_param,
            TokenKind::RBracket,
        )?;
        let ty = match self.parse_ty_anno()? {
            Some(ty) => ty,
            None => {
                let lo = self.token.span.lo;
                let span = Span::new(lo, lo, self.module_id);
                let error = errors::AnIndexSignatureMustHaveATypeAnnotation { span };
                self.push_error(Box::new(error));
                self.create_missing_ty()
            }
        };
        self.parse_ty_member_semi();
        let sig = self.alloc(ast::IndexSigDecl {
            id,
            span: self.new_span(start as u32),
            modifiers,
            params,
            ty,
        });
        self.insert_map(id, ast::Node::IndexSigDecl(sig));
        Ok(sig)
    }
}

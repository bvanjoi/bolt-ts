use super::list_ctx::{self, ListContext};
use super::token::TokenKind;
use super::{ast, errors};
use super::{PResult, ParserState};

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub(super) fn parse_fn_block(&mut self) -> PResult<&'cx ast::BlockStmt<'cx>> {
        self.parse_block()
    }

    pub(super) fn parse_block(&mut self) -> PResult<&'cx ast::BlockStmt<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        use TokenKind::*;
        self.expect(LBrace)?;
        let stmts = self.with_parent(id, |this| {
            this.parse_list(
                list_ctx::BlockStmt::is_ele,
                Self::parse_stmt,
                list_ctx::BlockStmt::is_closing,
            )
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
                list_ctx::TyParams::is_ele,
                Self::parse_ty_param,
                list_ctx::TyParams::is_closing,
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
        matches!(self.token.kind, TokenKind::Ident)
    }

    pub(super) fn next_token_is_ident(&mut self) -> PResult<bool> {
        self.next_token();
        Ok(self.is_ident())
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
}

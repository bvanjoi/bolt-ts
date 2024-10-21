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
        let id = self.p.next_node_id();
        let kind = self.with_parent(id, |this| this.create_ident(true));
        let expr = self.alloc(ast::Expr {
            id,
            kind: ast::ExprKind::Ident(kind),
        });
        self.insert_map(id, ast::Node::Expr(expr));
        expr
    }
}

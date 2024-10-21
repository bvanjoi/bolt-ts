use crate::ast::HeritageClauseKind;

use super::list_ctx::{self, ListContext};
use super::token::TokenKind;
use super::{ast, errors};
use super::{PResult, ParserState};

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub fn parse_stmt(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        let id = self.p.next_node_id();
        let stmt = self.with_parent(id, |this| {
            use TokenKind::*;
            let kind = match this.token.kind {
                Semi => ast::StmtKind::Empty(this.parse_empty_stmt()?),
                Var | Let | Const => ast::StmtKind::Var(this.parse_var_stmt()),
                Function => ast::StmtKind::Fn(this.parse_fn_decl()?),
                If => ast::StmtKind::If(this.parse_if_stmt()?),
                LBrace => ast::StmtKind::Block(this.parse_block()?),
                Return => ast::StmtKind::Return(this.parse_ret_stmt()?),
                Class => ast::StmtKind::Class(this.parse_class_decl()?),
                _ => ast::StmtKind::Expr(this.parse_expr_or_labeled_stmt()?),
            };
            let stmt = this.alloc(ast::Stmt { id, kind });
            Ok(stmt)
        })?;
        self.insert_map(id, ast::Node::Stmt(stmt));
        Ok(stmt)
    }

    fn parse_class_decl(&mut self) -> PResult<&'cx ast::ClassDecl<'cx>> {
        let start = self.token.start();
        let id = self.p.next_node_id();
        self.expect(TokenKind::Class)?;
        let name = self.parse_ident_name()?;
        let ty_params = self.parse_ty_params()?;
        let extends = self.parse_class_extends()?;
        let implements = self.parse_heritage_clauses(true)?;
        self.expect(TokenKind::LBrace)?;
        self.expect(TokenKind::RBrace)?;
        let decl = self.alloc(ast::ClassDecl {
            id,
            name,
            span: self.new_span(start as usize, self.pos),
            extends,
            implements,
            ty_params,
        });
        self.insert_map(id, ast::Node::ClassDecl(decl));
        Ok(decl)
    }

    fn parse_heritage_clause(
        &mut self,
        is_class_extends: bool,
    ) -> PResult<&'cx ast::HeritageClause<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.next_token();
        let ele_list = self.with_parent(id, |this| {
            this.parse_delimited_list(
                list_ctx::HeritageClause::is_ele,
                Self::parse_expr_with_ty_args,
                list_ctx::HeritageClause::is_closing,
            )
        });

        let ele_list = if is_class_extends && ele_list.len() > 1 {
            let lo = ele_list[0].span().hi;
            assert_eq!(
                self.input[lo as usize], b',',
                "`parse_delimited_list` ensure it must be comma."
            );
            let hi = ele_list.last().unwrap().span().hi;
            let error = errors::ClassesCanOnlyExtendASingleClass {
                span: self.new_span(lo as usize, lo as usize),
                extra_extends: self.new_span(lo as usize, hi as usize),
            };
            self.push_error(self.module_id, Box::new(error));
            &ele_list[0..1]
        } else {
            ele_list
        };
        let span = self.new_span(start as usize, self.pos);
        let clause = self.alloc(ast::HeritageClause {
            id,
            span,
            tys: ele_list,
        });

        self.insert_map(id, ast::Node::HeritageClause(clause));
        Ok(clause)
    }

    fn parse_class_extends(&mut self) -> PResult<Option<&'cx ast::HeritageClause<'cx>>> {
        if let Some(kind) = self.token.kind.into_heritage_clause_kind() {
            if kind == HeritageClauseKind::Extends {
                return self.parse_heritage_clause(true).map(|clause| Some(clause));
            }
        }
        Ok(None)
    }

    fn parse_heritage_clauses(
        &mut self,
        is_class: bool,
    ) -> PResult<Option<&'cx ast::HeritageClauses<'cx>>> {
        if let Some(kind) = self.token.kind.into_heritage_clause_kind() {
            let is_class_extends = is_class && matches!(kind, ast::HeritageClauseKind::Extends);
            let id = self.p.next_node_id();
            let start = self.token.start();
            let clauses = self.with_parent(id, |this| {
                this.parse_list(
                    list_ctx::HeritageClauses::is_ele,
                    |this| this.parse_heritage_clause(is_class_extends),
                    list_ctx::HeritageClauses::is_closing,
                )
            });
            let clauses = self.alloc(ast::HeritageClauses {
                id,
                span: self.new_span(start as usize, self.pos),
                clauses,
            });
            self.insert_map(id, ast::Node::HeritageClauses(clauses));
            Ok(Some(clauses))
        } else {
            Ok(None)
        }
    }

    fn parse_empty_stmt(&mut self) -> PResult<&'cx ast::EmptyStmt> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Semi);
        let stmt = self.alloc(ast::EmptyStmt {
            id,
            span: self.new_span(start as usize, self.pos),
        });
        self.insert_map(id, ast::Node::EmptyStmt(stmt));
        Ok(stmt)
    }

    fn parse_var_stmt(&mut self) -> &'cx ast::VarStmt<'cx> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        use TokenKind::*;
        let kind = match self.token.kind {
            Var | Let | Const => unsafe {
                std::mem::transmute::<u8, ast::VarKind>(self.token.kind as u8 - Var as u8)
            },
            _ => unreachable!(),
        };
        let list = self.with_parent(id, Self::parse_var_decl_list);
        let span = self.new_span(start as usize, self.pos);
        let node = self.alloc(ast::VarStmt {
            id,
            kind,
            span,
            list,
        });
        self.insert_map(id, ast::Node::VarStmt(node));
        self.parse_semi();
        node
    }

    fn parse_name_of_param(&mut self) -> PResult<&'cx ast::Ident> {
        let name = self.parse_ident_or_pat();
        Ok(name)
    }

    fn parse_ident_or_pat(&mut self) -> &'cx ast::Ident {
        self.parse_binding_ident()
    }

    fn parse_var_decl(&mut self) -> PResult<&'cx ast::VarDecl<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.with_parent(id, |this| {
            let name = this.parse_ident_or_pat();
            // todo: parse type annotation
            let ty = this.parse_ty_anno()?;
            let init = this.parse_init();
            let span = this.new_span(start as usize, this.pos);
            let node = this.alloc(ast::VarDecl {
                id,
                span,
                name,
                ty,
                init,
            });
            this.insert_map(id, ast::Node::VarDecl(node));
            Ok(node)
        })
    }

    fn parse_var_decl_list(&mut self) -> &'cx [&'cx ast::VarDecl<'cx>] {
        self.next_token();
        self.parse_delimited_list(
            list_ctx::VarDecl::is_ele,
            Self::parse_var_decl,
            list_ctx::VarDecl::is_closing,
        )
    }

    fn parse_param(&mut self) -> PResult<&'cx ast::ParamDecl<'cx>> {
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

    pub(super) fn parse_params(&mut self) -> PResult<ast::ParamsDecl<'cx>> {
        use TokenKind::*;
        self.expect(LParen)?;
        let params = self.parse_delimited_list(
            list_ctx::Params::is_ele,
            Self::parse_param,
            list_ctx::Params::is_closing,
        );
        self.expect(RParen)?;
        Ok(params)
    }

    fn parse_fn_decl_ret_type(&mut self) -> PResult<Option<&'cx ast::Ty<'cx>>> {
        if self.parse_optional(TokenKind::Colon).is_some() {
            self.parse_ty_or_ty_pred().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_fn_decl(&mut self) -> PResult<&'cx ast::FnDecl<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Function)?;
        let name = self.with_parent(id, Self::parse_binding_ident);
        // TODO: type params
        let params = self.with_parent(id, Self::parse_params)?;
        let ret_ty = self.with_parent(id, Self::parse_fn_decl_ret_type)?;
        let body = self.parse_fn_block()?;
        let f = self.alloc(ast::FnDecl {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            params,
            ret_ty,
            body,
        });
        self.insert_map(id, ast::Node::FnDecl(f));
        Ok(f)
    }

    fn parse_if_stmt(&mut self) -> PResult<&'cx ast::IfStmt<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LParen)?;
        let expr = self.with_parent(id, Self::parse_expr)?;
        self.expect(TokenKind::RParen)?;
        let then = self.with_parent(id, Self::parse_stmt)?;
        let else_then = if self.parse_optional(TokenKind::Else).is_some() {
            Some(self.with_parent(id, Self::parse_stmt)?)
        } else {
            None
        };
        let stmt = self.alloc(ast::IfStmt {
            id,
            span: self.new_span(start as usize, self.pos),
            expr,
            then,
            else_then,
        });
        self.insert_map(id, ast::Node::IfStmt(stmt));
        Ok(stmt)
    }

    fn parse_ret_stmt(&mut self) -> PResult<&'cx ast::RetStmt<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Return);
        let expr = if self.can_parse_semi() {
            None
        } else {
            Some(self.with_parent(id, Self::parse_expr)?)
        };
        self.parse_semi();
        let stmt = self.alloc(ast::RetStmt {
            id,
            span: self.new_span(start as usize, self.pos),
            expr,
        });
        self.insert_map(id, ast::Node::RetStmt(stmt));
        Ok(stmt)
    }

    fn parse_expr_or_labeled_stmt(&mut self) -> PResult<&'cx ast::Expr<'cx>> {
        let expr = self.parse_expr();
        self.parse_semi();
        expr
    }
}

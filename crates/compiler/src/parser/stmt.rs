use super::ast;
use super::list_ctx;
use super::parse_class_like::ParseClassDecl;
use super::token::TokenKind;
use super::{PResult, ParserState};

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub fn parse_stmt(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        use TokenKind::*;
        if matches!(self.token.kind, Abstract) && self.is_start_of_decl() {
            return self.parse_decl();
        }
        let kind = match self.token.kind {
            Semi => ast::StmtKind::Empty(self.parse_empty_stmt()?),
            Var | Let | Const => ast::StmtKind::Var(self.parse_var_stmt()),
            Interface => ast::StmtKind::Interface(self.parse_interface_decl()?),
            Function => ast::StmtKind::Fn(self.parse_fn_decl()?),
            If => ast::StmtKind::If(self.parse_if_stmt()?),
            LBrace => ast::StmtKind::Block(self.parse_block()?),
            Return => ast::StmtKind::Return(self.parse_ret_stmt()?),
            Class => ast::StmtKind::Class(self.parse_class_decl(None)?),
            _ => ast::StmtKind::Expr(self.parse_expr_or_labeled_stmt()?),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_decl(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        use TokenKind::*;
        let mods = self.parse_modifiers()?;
        let kind = match self.token.kind {
            Var | Let | Const => ast::StmtKind::Var(self.parse_var_stmt()),
            Function => ast::StmtKind::Fn(self.parse_fn_decl()?),
            Class => ast::StmtKind::Class(self.parse_class_decl(mods)?),
            _ => unreachable!(),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_interface_extends_clause(
        &mut self,
    ) -> PResult<Option<&'cx ast::InterfaceExtendsClause<'cx>>> {
        if self.token.kind == TokenKind::Extends {
            let id = self.p.next_node_id();
            let start = self.token.start();
            self.next_token();
            let tys = self.with_parent(id, |this| {
                this.parse_delimited_list(list_ctx::HeritageClause, Self::parse_expr_with_ty_args)
            });
            let span = self.new_span(start as usize, self.pos);
            let clause = self.alloc(ast::InterfaceExtendsClause { id, span, tys });
            self.insert_map(id, ast::Node::InterfaceExtendsClause(clause));
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_interface_decl(&mut self) -> PResult<&'cx ast::InterfaceDecl<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Interface)?;
        let name = self.with_parent(id, Self::parse_ident_name)?;
        let ty_params = self.with_parent(id, Self::parse_ty_params)?;
        let extends = self.with_parent(id, Self::parse_interface_extends_clause)?;
        // let implements = self.with_parent(id, Self::parse_implements_clause)?;
        let members = self.with_parent(id, Self::parse_object_ty_members)?;
        let decl = self.alloc(ast::InterfaceDecl {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            // ty_params,
            extends,
            members,
        });
        Ok(decl)
    }

    fn parse_class_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassDecl<'cx>> {
        self.parse_class_decl_or_expr(ParseClassDecl, modifiers)
    }

    pub(super) fn parse_implements_clause(
        &mut self,
    ) -> PResult<Option<&'cx ast::ImplementsClause<'cx>>> {
        if self.token.kind == TokenKind::Implements {
            let id = self.p.next_node_id();
            let start = self.token.start();
            self.next_token();
            let tys = self.with_parent(id, |this| {
                this.parse_delimited_list(list_ctx::HeritageClause, Self::parse_expr_with_ty_args)
            });
            let span = self.new_span(start as usize, self.pos);
            let clause = self.alloc(ast::ImplementsClause { id, span, tys });
            self.insert_map(id, ast::Node::ImplementsClause(clause));
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_empty_stmt(&mut self) -> PResult<&'cx ast::EmptyStmt> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Semi)?;
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
        let binding = self.with_parent(id, Self::parse_ident_or_pat);
        let ty = self.with_parent(id, Self::parse_ty_anno)?;
        let init = self.with_parent(id, Self::parse_init);
        let span = self.new_span(start as usize, self.pos);
        let node = self.alloc(ast::VarDecl {
            id,
            span,
            binding,
            ty,
            init,
        });
        self.insert_map(id, ast::Node::VarDecl(node));
        Ok(node)
    }

    fn parse_var_decl_list(&mut self) -> &'cx [&'cx ast::VarDecl<'cx>] {
        self.next_token();
        self.parse_delimited_list(list_ctx::VarDecl, Self::parse_var_decl)
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

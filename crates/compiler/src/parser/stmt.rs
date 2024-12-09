use super::ast;
use super::list_ctx;
use super::parse_class_like::ParseClassDecl;
use super::parse_fn_like::ParseFnDecl;
use super::token::TokenKind;
use super::{PResult, ParserState};

impl<'p> ParserState<'p> {
    pub fn parse_stmt(&mut self) -> PResult<&'p ast::Stmt<'p>> {
        use TokenKind::*;
        if matches!(self.token.kind, Abstract | Declare) && self.is_start_of_decl() {
            return self.parse_decl();
        }
        let kind = match self.token.kind {
            Semi => ast::StmtKind::Empty(self.parse_empty_stmt()?),
            Var | Let | Const => ast::StmtKind::Var(self.parse_var_stmt()),
            Function => ast::StmtKind::Fn(self.parse_fn_decl(None)?),
            If => ast::StmtKind::If(self.parse_if_stmt()?),
            LBrace => ast::StmtKind::Block(self.parse_block()?),
            Return => ast::StmtKind::Return(self.parse_ret_stmt()?),
            Class => ast::StmtKind::Class(self.parse_class_decl(None)?),
            Interface => ast::StmtKind::Interface(self.parse_interface_decl()?),
            Type => ast::StmtKind::Type(self.parse_type_decl()?),
            Module | Namespace => ast::StmtKind::Namespace(self.parse_ns_decl(None)?),
            _ => ast::StmtKind::Expr(self.parse_expr_or_labeled_stmt()?),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_ns_decl(
        &mut self,
        mods: Option<&'p ast::Modifiers<'p>>,
    ) -> PResult<&'p ast::NsDecl<'p>> {
        let id = self.next_node_id();
        let start = self.token.start();
        if self.parse_optional(TokenKind::Namespace).is_none() {
            self.expect(TokenKind::Module)?;
        }
        let name = self.parse_ident_name()?;
        let block = self.parse_block()?;
        let span = self.new_span(start as usize, self.pos);
        let decl = self.alloc(ast::NsDecl {
            id,
            span,
            modifiers: mods,
            name,
            block,
        });
        self.insert_map(id, ast::Node::NamespaceDecl(decl));
        Ok(decl)
    }

    fn parse_type_decl(&mut self) -> PResult<&'p ast::TypeDecl<'p>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Type)?;
        let name = self.with_parent(id, Self::parse_ident_name)?;
        let ty_params = self.with_parent(id, Self::parse_ty_params)?;
        self.expect(TokenKind::Eq)?;
        let ty = self.with_parent(id, Self::parse_ty)?;
        self.parse_semi();
        let decl = self.alloc(ast::TypeDecl {
            id,
            span: self.new_span(start as usize, self.pos),
            name,
            ty_params,
            ty,
        });
        self.insert_map(id, ast::Node::TypeDecl(decl));
        Ok(decl)
    }

    fn contain_declare_mod(mods: &ast::Modifiers<'p>) -> bool {
        mods.list
            .iter()
            .any(|m| matches!(m.kind, ast::ModifierKind::Declare))
    }

    fn _parse_decl(&mut self, mods: Option<&'p ast::Modifiers<'p>>) -> PResult<&'p ast::Stmt<'p>> {
        use TokenKind::*;
        let kind = match self.token.kind {
            Var | Let | Const => ast::StmtKind::Var(self.parse_var_stmt()),
            Function => ast::StmtKind::Fn(self.parse_fn_decl(mods)?),
            Class => ast::StmtKind::Class(self.parse_class_decl(mods)?),
            Module | Namespace => ast::StmtKind::Namespace(self.parse_ns_decl(mods)?),
            _ => unreachable!("{:#?}", self.token.kind),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_decl(&mut self) -> PResult<&'p ast::Stmt<'p>> {
        let mods = self.parse_modifiers()?;
        if mods.map_or(false, Self::contain_declare_mod) {
            // todo
            self._parse_decl(mods)
        } else {
            self._parse_decl(mods)
        }
    }

    fn parse_interface_extends_clause(
        &mut self,
    ) -> PResult<Option<&'p ast::InterfaceExtendsClause<'p>>> {
        if self.token.kind == TokenKind::Extends {
            let id = self.next_node_id();
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

    fn parse_interface_decl(&mut self) -> PResult<&'p ast::InterfaceDecl<'p>> {
        let id = self.next_node_id();
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
            ty_params,
            extends,
            members,
        });
        self.insert_map(id, ast::Node::InterfaceDecl(decl));
        Ok(decl)
    }

    fn parse_class_decl(
        &mut self,
        modifiers: Option<&'p ast::Modifiers<'p>>,
    ) -> PResult<&'p ast::ClassDecl<'p>> {
        self.parse_class_decl_or_expr(ParseClassDecl, modifiers)
    }

    pub(super) fn parse_implements_clause(
        &mut self,
    ) -> PResult<Option<&'p ast::ImplementsClause<'p>>> {
        if self.token.kind == TokenKind::Implements {
            let id = self.next_node_id();
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

    fn parse_empty_stmt(&mut self) -> PResult<&'p ast::EmptyStmt> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Semi)?;
        let stmt = self.alloc(ast::EmptyStmt {
            id,
            span: self.new_span(start as usize, self.pos),
        });
        self.insert_map(id, ast::Node::EmptyStmt(stmt));
        Ok(stmt)
    }

    fn parse_var_stmt(&mut self) -> &'p ast::VarStmt<'p> {
        let id = self.next_node_id();
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

    fn parse_name_of_param(&mut self) -> PResult<&'p ast::Ident> {
        let name = self.parse_ident_or_pat();
        Ok(name)
    }

    fn parse_ident_or_pat(&mut self) -> &'p ast::Ident {
        self.parse_binding_ident()
    }

    fn parse_var_decl(&mut self) -> PResult<&'p ast::VarDecl<'p>> {
        let id = self.next_node_id();
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

    fn parse_var_decl_list(&mut self) -> &'p [&'p ast::VarDecl<'p>] {
        self.next_token();
        self.parse_delimited_list(list_ctx::VarDecl, Self::parse_var_decl)
    }

    fn parse_fn_decl(
        &mut self,
        modifiers: Option<&'p ast::Modifiers<'p>>,
    ) -> PResult<&'p ast::FnDecl<'p>> {
        self.parse_fn_decl_or_expr(ParseFnDecl, modifiers)
        // let f = self.alloc(ast::FnDecl {
        //     id,
        //     span: self.new_span(start as usize, self.pos),
        //     modifiers,
        //     name,
        //     params,
        //     ret_ty,
        //     body,
        // });
        // self.insert_map(id, ast::Node::FnDecl(f));
        // Ok(f)
    }

    fn parse_if_stmt(&mut self) -> PResult<&'p ast::IfStmt<'p>> {
        let id = self.next_node_id();
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

    fn parse_ret_stmt(&mut self) -> PResult<&'p ast::RetStmt<'p>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Return)?;
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

    fn parse_expr_or_labeled_stmt(&mut self) -> PResult<&'p ast::Expr<'p>> {
        let expr = self.parse_expr();
        self.parse_semi();
        expr
    }
}

use crate::ast::NodeFlags;

use super::ast;
use super::errors;
use super::list_ctx;
use super::parse_class_like::ParseClassDecl;
use super::parse_fn_like::ParseFnDecl;
use super::token::TokenKind;
use super::{PResult, ParserState};

impl<'cx> ParserState<'cx, '_> {
    pub fn parse_stmt(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        use TokenKind::*;
        if matches!(self.token.kind, Abstract | Declare | Export | Import)
            && self.is_start_of_decl()
        {
            return self.parse_decl();
        }
        let kind = match self.token.kind {
            Semi => ast::StmtKind::Empty(self.parse_empty_stmt()?),
            Var | Let | Const => ast::StmtKind::Var(self.parse_var_stmt(None)),
            Function => ast::StmtKind::Fn(self.parse_fn_decl(None)?),
            If => ast::StmtKind::If(self.parse_if_stmt()?),
            LBrace => ast::StmtKind::Block(self.parse_block()?),
            Return => ast::StmtKind::Return(self.parse_ret_stmt()?),
            Class => ast::StmtKind::Class(self.parse_class_decl(None)?),
            Interface => ast::StmtKind::Interface(self.parse_interface_decl(None)?),
            Type => ast::StmtKind::Type(self.parse_type_decl()?),
            Module | Namespace => ast::StmtKind::Namespace(self.parse_ns_decl(None)?),
            Enum => ast::StmtKind::Enum(self.parse_enum_decl(None)?),
            Throw => ast::StmtKind::Throw(self.parse_throw_stmt()?),
            _ => ast::StmtKind::Expr(self.parse_expr_or_labeled_stmt()?),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_enum_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::EnumDecl<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Enum)?;
        let name = self.create_ident(self.is_ident(), None);
        let members = if self.expect(TokenKind::LBrace).is_ok() {
            let member = self.parse_delimited_list(list_ctx::EnumMembers, Self::parse_enum_member);
            self.expect(TokenKind::RBrace)?;
            member
        } else {
            todo!("error handler")
        };

        let decl = self.alloc(ast::EnumDecl {
            id,
            span: self.new_span(start),
            modifiers,
            name,
            members,
        });
        self.insert_map(id, ast::Node::EnumDecl(decl));
        Ok(decl)
    }

    fn parse_enum_member(&mut self) -> PResult<&'cx ast::EnumMember<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let name = self.with_parent(id, Self::parse_prop_name)?;
        let init = self.with_parent(id, Self::parse_init);
        let span = self.new_span(start);
        let member = self.alloc(ast::EnumMember {
            id,
            span,
            name,
            init,
        });
        self.insert_map(id, ast::Node::EnumMember(member));
        Ok(member)
    }

    fn try_parse_semi(&mut self) -> PResult<bool> {
        if !self.can_parse_semi() {
            Ok(false)
        } else if self.token.kind == TokenKind::Semi {
            self.next_token();
            Ok(true)
        } else {
            Ok(true)
        }
    }

    fn parse_throw_stmt(&mut self) -> PResult<&'cx ast::ThrowStmt<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Throw)?;
        if self.has_preceding_line_break() {
            todo!("error handle")
        } else {
            let id = self.next_node_id();
            let expr = self.with_parent(id, Self::parse_expr)?;
            let t = self.alloc(ast::ThrowStmt {
                id,
                span: self.new_span(start),
                expr,
            });
            if !self.try_parse_semi()? {
                todo!()
            }
            self.insert_map(id, ast::Node::ThrowStmt(t));
            Ok(t)
        }
    }

    fn parse_ns_decl(
        &mut self,
        mods: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::NsDecl<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        if self.parse_optional(TokenKind::Namespace).is_none() {
            self.expect(TokenKind::Module)?;
        }
        let name = self.parse_ident_name()?;
        let block = self.parse_block()?;
        let span = self.new_span(start);
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

    fn parse_type_decl(&mut self) -> PResult<&'cx ast::TypeDecl<'cx>> {
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
            span: self.new_span(start),
            name,
            ty_params,
            ty,
        });
        self.insert_map(id, ast::Node::TypeDecl(decl));
        Ok(decl)
    }

    fn contain_declare_mod(mods: &ast::Modifiers<'cx>) -> bool {
        mods.flags.contains(ast::ModifierKind::Declare)
    }

    fn _parse_decl(
        &mut self,
        mods: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::Stmt<'cx>> {
        use TokenKind::*;
        let kind = match self.token.kind {
            Var | Let | Const => ast::StmtKind::Var(self.parse_var_stmt(mods)),
            Function => ast::StmtKind::Fn(self.parse_fn_decl(mods)?),
            Class => ast::StmtKind::Class(self.parse_class_decl(mods)?),
            Module | Namespace => ast::StmtKind::Namespace(self.parse_ns_decl(mods)?),
            Ident => {
                let id = self.ident_token();
                unreachable!("{:#?}", self.atoms.lock().unwrap().get(id));
            }
            Interface => ast::StmtKind::Interface(self.parse_interface_decl(mods)?),
            Enum => ast::StmtKind::Enum(self.parse_enum_decl(mods)?),
            _ => unreachable!("{:#?}", self.token.kind),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn set_context_flags(&mut self, val: bool, flag: NodeFlags) {
        if val {
            self.context_flags |= flag
        } else {
            self.context_flags &= !flag;
        }
    }

    fn do_inside_of_context<T>(&mut self, context: NodeFlags, f: impl FnOnce(&mut Self) -> T) -> T {
        let set = context & !self.context_flags;
        if !set.is_empty() {
            self.set_context_flags(true, set);
            let res = f(self);
            self.set_context_flags(false, set);
            res
        } else {
            f(self)
        }
    }

    fn parse_decl(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        let mods = self.parse_modifiers(false)?;
        let is_ambient = mods.map_or(false, Self::contain_declare_mod);
        if is_ambient {
            self.do_inside_of_context(NodeFlags::AMBIENT, |this| this._parse_decl(mods))
        } else {
            self._parse_decl(mods)
        }
    }

    fn parse_interface_extends_clause(
        &mut self,
    ) -> PResult<Option<&'cx ast::InterfaceExtendsClause<'cx>>> {
        if self.token.kind == TokenKind::Extends {
            let id = self.next_node_id();
            let start = self.token.start();
            self.next_token();
            let list = self.with_parent(id, |this| {
                this.parse_delimited_list(
                    list_ctx::HeritageClause,
                    Self::parse_entity_name_of_ty_reference,
                )
            });
            let span = self.new_span(start);
            let clause = self.alloc(ast::InterfaceExtendsClause { id, span, list });
            self.insert_map(id, ast::Node::InterfaceExtendsClause(clause));
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_interface_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::InterfaceDecl<'cx>> {
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
            span: self.new_span(start),
            modifiers,
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
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassDecl<'cx>> {
        self.parse_class_decl_or_expr(ParseClassDecl, modifiers)
    }

    pub(super) fn parse_implements_clause(
        &mut self,
    ) -> PResult<Option<&'cx ast::ClassImplementsClause<'cx>>> {
        if self.token.kind == TokenKind::Implements {
            let id = self.next_node_id();
            let start = self.token.start();
            self.next_token();
            let list = self.with_parent(id, |this| {
                this.parse_delimited_list(
                    list_ctx::HeritageClause,
                    Self::parse_entity_name_of_ty_reference,
                )
            });
            let span = self.new_span(start);
            let clause = self.alloc(ast::ClassImplementsClause { id, span, list });
            self.insert_map(id, ast::Node::ClassImplementsClause(clause));
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_empty_stmt(&mut self) -> PResult<&'cx ast::EmptyStmt> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Semi)?;
        let stmt = self.alloc(ast::EmptyStmt {
            id,
            span: self.new_span(start),
        });
        self.insert_map(id, ast::Node::EmptyStmt(stmt));
        Ok(stmt)
    }

    fn parse_var_stmt(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> &'cx ast::VarStmt<'cx> {
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
        if list.is_empty() {
            let span = self.new_span(start);
            self.push_error(Box::new(errors::VariableDeclarationListCannotBeEmpty {
                span,
            }));
        }
        let span = self.new_span(start);
        let node = self.alloc(ast::VarStmt {
            id,
            kind,
            span,
            modifiers,
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
        let id = self.next_node_id();
        let start = self.token.start();
        let binding = self.with_parent(id, Self::parse_ident_or_pat);
        let ty = self.with_parent(id, Self::parse_ty_anno)?;
        let init = self.with_parent(id, Self::parse_init);
        let span = self.new_span(start);
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

    fn parse_fn_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::FnDecl<'cx>> {
        self.parse_fn_decl_or_expr(ParseFnDecl, modifiers)
        // let f = self.alloc(ast::FnDecl {
        //     id,
        //     span: self.new_span(start ),
        //     modifiers,
        //     name,
        //     params,
        //     ret_ty,
        //     body,
        // });
        // self.insert_map(id, ast::Node::FnDecl(f));
        // Ok(f)
    }

    fn parse_if_stmt(&mut self) -> PResult<&'cx ast::IfStmt<'cx>> {
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
            span: self.new_span(start),
            expr,
            then,
            else_then,
        });
        self.insert_map(id, ast::Node::IfStmt(stmt));
        Ok(stmt)
    }

    fn parse_ret_stmt(&mut self) -> PResult<&'cx ast::RetStmt<'cx>> {
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
            span: self.new_span(start),
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

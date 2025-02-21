use bolt_ts_atom::AtomId;

use bolt_ts_ast::TokenKind;
use bolt_ts_ast::{NodeFlags, VarDecls};

use super::ast;
use super::errors;
use super::list_ctx;
use super::parse_class_like::ParseClassDecl;
use super::parse_fn_like::ParseFnDecl;
use super::parse_import_export_spec::ParseNamedExports;
use super::parse_import_export_spec::ParseNamedImports;
use super::{PResult, ParserState};
use crate::keyword::{self, IDENT_GLOBAL};
use crate::parser::parse_break_or_continue::{ParseBreak, ParseContinue};

impl<'cx> ParserState<'cx, '_> {
    pub fn parse_stmt(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        use bolt_ts_ast::TokenKind::*;
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
            For => self.parse_for_stmt()?,
            Break => ast::StmtKind::Break(self.parse_break_or_continue(&ParseBreak)?),
            Continue => ast::StmtKind::Continue(self.parse_break_or_continue(&ParseContinue)?),
            Try => ast::StmtKind::Try(self.parse_try_stmt()?),
            While => ast::StmtKind::While(self.parse_while_stmt()?),
            Do => ast::StmtKind::Do(self.parse_do_stmt()?),
            Debugger => ast::StmtKind::Debugger(self.parse_debugger_stmt()?),
            _ => ast::StmtKind::Expr(self.parse_expr_or_labeled_stmt()?),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_debugger_stmt(&mut self) -> PResult<&'cx ast::DebuggerStmt> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Debugger);
        self.parse_optional(TokenKind::Semi);
        let stmt = self.alloc(ast::DebuggerStmt {
            id,
            span: self.new_span(start),
        });
        self.insert_map(id, ast::Node::DebuggerStmt(stmt));
        Ok(stmt)
    }

    fn parse_do_stmt(&mut self) -> PResult<&'cx ast::DoStmt<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Do);
        let stmt = self.parse_stmt()?;
        self.expect(TokenKind::While);
        let open_pos = self.token.start() as usize;
        let open_parsed = self.expect(TokenKind::LParen);
        let expr = self.parse_expr()?;
        self.parse_expected_matching_brackets(
            TokenKind::LParen,
            TokenKind::RParen,
            open_parsed,
            open_pos,
        )?;
        self.parse_optional(TokenKind::Semi);
        let stmt = self.alloc(ast::DoStmt {
            id,
            span: self.new_span(start),
            stmt,
            expr,
        });
        self.insert_map(id, ast::Node::DoStmt(stmt));
        Ok(stmt)
    }

    fn parse_while_stmt(&mut self) -> PResult<&'cx ast::WhileStmt<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::While);
        let open_parsed = self.expect(TokenKind::LParen);
        let expr = self.allow_in_and(Self::parse_expr)?;
        self.parse_expected_matching_brackets(
            TokenKind::LParen,
            TokenKind::RParen,
            open_parsed,
            start as usize,
        )?;
        let stmt = self.parse_stmt()?;
        let stmt = self.alloc(ast::WhileStmt {
            id,
            span: self.new_span(start),
            expr,
            stmt,
        });
        self.insert_map(id, ast::Node::WhileStmt(stmt));
        Ok(stmt)
    }

    fn parse_catch_clause(&mut self) -> PResult<&'cx ast::CatchClause<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Catch);

        let var = if self.parse_optional(TokenKind::LParen).is_some() {
            let v = self.with_parent(id, Self::parse_var_decl)?;
            self.expect(TokenKind::RParen);
            Some(v)
        } else {
            None
        };
        let block = self.with_parent(id, Self::parse_block)?;
        let clause = self.alloc(ast::CatchClause {
            id,
            span: self.new_span(start),
            var,
            block,
        });
        self.insert_map(id, ast::Node::CatchClause(clause));
        Ok(clause)
    }

    fn parse_try_stmt(&mut self) -> PResult<&'cx ast::TryStmt<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Try);
        let try_block = self.parse_block()?;
        let catch_clause = if self.token.kind == TokenKind::Catch {
            Some(self.with_parent(id, Self::parse_catch_clause)?)
        } else {
            None
        };
        let finally_block =
            if catch_clause.is_none() || self.parse_optional(TokenKind::Finally).is_some() {
                Some(self.with_parent(id, Self::parse_block)?)
            } else {
                None
            };
        let stmt = self.alloc(ast::TryStmt {
            id,
            span: self.new_span(start),
            try_block,
            catch_clause,
            finally_block,
        });
        self.insert_map(id, ast::Node::TryStmt(stmt));
        Ok(stmt)
    }

    fn parse_for_stmt(&mut self) -> PResult<ast::StmtKind<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        let start = self.token.start();
        let id = self.next_node_id();
        self.expect(For);
        let await_token = self.parse_optional(Await);
        self.expect(LParen);
        let t = self.token.kind;
        let init = if t != Semi {
            if matches!(t, Var | Let | Const) {
                Some(ast::ForInitKind::Var((
                    t.try_into().unwrap(),
                    self.with_parent(id, |this| this.parse_var_decl_list(true)),
                )))
            } else {
                Some(ast::ForInitKind::Expr(self.with_parent(id, |this| {
                    this.disallow_in_and(Self::parse_expr)
                })?))
            }
        } else {
            None
        };

        if (await_token.is_some() && self.expect(Of)) || self.parse_optional(Of).is_some() {
            let init = init.unwrap();
            let expr = self.with_parent(id, |this| {
                this.allow_in_and(|this| this.parse_assign_expr(true))
            })?;
            self.expect(RParen);
            let body = self.parse_stmt()?;
            let kind = self.alloc(ast::ForOfStmt {
                id,
                span: self.new_span(start),
                r#await: await_token.map(|t| t.span),
                init,
                expr,
                body,
            });
            self.insert_map(id, ast::Node::ForOfStmt(kind));
            Ok(ast::StmtKind::ForOf(kind))
        } else if self.parse_optional(In).is_some() {
            let init = init.unwrap();
            let expr = self.with_parent(id, |this| this.allow_in_and(Self::parse_expr))?;
            self.expect(RParen);
            let body = self.parse_stmt()?;
            let kind = self.alloc(ast::ForInStmt {
                id,
                span: self.new_span(start),
                init,
                expr,
                body,
            });
            self.insert_map(id, ast::Node::ForInStmt(kind));
            Ok(ast::StmtKind::ForIn(kind))
        } else {
            self.expect(Semi);
            let cond = if !matches!(self.token.kind, Semi | RParen) {
                Some(self.with_parent(id, |this| this.allow_in_and(Self::parse_expr))?)
            } else {
                None
            };
            self.expect(Semi);
            let incr = if !matches!(self.token.kind, RParen) {
                Some(self.with_parent(id, |this| this.allow_in_and(Self::parse_expr))?)
            } else {
                None
            };
            self.expect(RParen);

            let body = self.parse_stmt()?;
            let kind = self.alloc(ast::ForStmt {
                id,
                span: self.new_span(start),
                init,
                cond,
                incr,
                body,
            });
            self.insert_map(id, ast::Node::ForStmt(kind));
            Ok(ast::StmtKind::For(kind))
        }
    }

    fn parse_enum_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::EnumDecl<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Enum);
        let name = self.create_ident(self.is_ident(), None);
        let members = if self.expect(TokenKind::LBrace) {
            let member = self.parse_delimited_list(list_ctx::EnumMembers, Self::parse_enum_member);
            self.expect(TokenKind::RBrace);
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
        let init = self.with_parent(id, Self::parse_init)?;
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
        self.expect(TokenKind::Throw);
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
            self.expect(TokenKind::Module);
            if self.token.kind == TokenKind::String {
                return self.parse_ambient_external_module_decl(id, start, mods);
            }
        }
        let name = self.parse_ident_name()?;
        let block = self.parse_module_block()?;
        let span = self.new_span(start);
        let decl = self.alloc(ast::NsDecl {
            id,
            span,
            modifiers: mods,
            name: ast::ModuleName::Ident(name),
            block: Some(block),
        });
        self.insert_map(id, ast::Node::NamespaceDecl(decl));
        Ok(decl)
    }

    pub(super) fn is_ident_name(&self, name: AtomId) -> bool {
        self.token.kind.is_ident_or_keyword() && self.ident_token() == name
    }

    fn parse_module_block(&mut self) -> PResult<&'cx ast::BlockStmt<'cx>> {
        self.parse_block()
    }

    fn parse_ambient_external_module_decl(
        &mut self,
        id: ast::NodeID,
        start: u32,
        mods: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::NsDecl<'cx>> {
        let name;
        if self.is_ident_name(IDENT_GLOBAL) {
            todo!()
        } else {
            name = ast::ModuleName::StringLit(self.parse_string_lit());
        }
        let block = if self.token.kind == TokenKind::LBrace {
            Some(self.parse_module_block()?)
        } else {
            self.parse_semi();
            None
        };
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
        self.expect(TokenKind::Type);
        let name = self.with_parent(id, Self::parse_ident_name)?;
        let ty_params = self.with_parent(id, Self::parse_ty_params).unwrap();
        self.expect(TokenKind::Eq);
        let ty = if self.token.kind == TokenKind::Intrinsic {
            let t = self.alloc(ast::IntrinsicTy {
                span: self.token.span,
            });
            let t = self.alloc(ast::Ty {
                kind: ast::TyKind::Intrinsic(t),
            });
            self.next_token();
            t
        } else {
            self.with_parent(id, Self::parse_ty)?
        };
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
        use bolt_ts_ast::TokenKind::*;
        let kind = match self.token.kind {
            Var | Let | Const => ast::StmtKind::Var(self.parse_var_stmt(mods)),
            Function => ast::StmtKind::Fn(self.parse_fn_decl(mods)?),
            Class => ast::StmtKind::Class(self.parse_class_decl(mods)?),
            Module | Namespace => ast::StmtKind::Namespace(self.parse_ns_decl(mods)?),
            Interface => ast::StmtKind::Interface(self.parse_interface_decl(mods)?),
            Enum => ast::StmtKind::Enum(self.parse_enum_decl(mods)?),
            Import => ast::StmtKind::Import(self.parse_import_decl()?),
            Export => {
                let start = self.token.start();
                self.next_token();
                match self.token.kind {
                    Default | Eq => todo!(),
                    As => todo!(),
                    _ => ast::StmtKind::Export(self.parse_export_decl(start)?),
                }
            }
            Ident => {
                let id = self.ident_token();
                unreachable!("{:#?}", self.atoms.lock().unwrap().get(id));
            }
            Type => ast::StmtKind::Type(self.parse_type_decl()?),
            _ => unreachable!("{:#?}", self.token.kind),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_export_decl(&mut self, start: u32) -> PResult<&'cx ast::ExportDecl<'cx>> {
        let id = self.next_node_id();

        let is_type_only = self.parse_optional(TokenKind::Type).is_some();

        let ns_export_start = self.token.start();

        let is_asterisk = self.parse_optional(TokenKind::Asterisk).is_some();

        let kind = if is_asterisk && self.parse_optional(TokenKind::As).is_some() {
            // export * as
            let ns = self.with_parent(id, |this| this.parse_ns_export(ns_export_start))?;
            ast::ExportClauseKind::Ns(ns)
        } else if is_asterisk {
            // export *
            let glob = self.with_parent(id, |this| this.parse_glob_export(ns_export_start))?;
            ast::ExportClauseKind::Glob(glob)
        } else {
            // export {
            let specs = self.with_parent(id, |this| this.parse_specs_export(ns_export_start))?;
            ast::ExportClauseKind::Specs(specs)
        };
        let clause = self.alloc(ast::ExportClause { is_type_only, kind });
        let decl = self.alloc(ast::ExportDecl {
            id,
            span: self.new_span(start),
            clause,
        });
        self.insert_map(id, ast::Node::ExportDecl(decl));
        self.parse_semi();
        Ok(decl)
    }

    fn parse_specs_export(&mut self, start: u32) -> PResult<&'cx ast::SpecsExport<'cx>> {
        let id = self.next_node_id();
        let list = self.with_parent(id, |this| {
            this.parse_named_imports_or_exports(ParseNamedExports)
        })?;
        let module = if self.parse_optional(TokenKind::From).is_some() {
            Some(self.parse_module_spec()?)
        } else {
            None
        };
        let specs = self.alloc(ast::SpecsExport {
            id,
            span: self.new_span(start),
            list,
            module,
        });
        self.insert_map(id, ast::Node::SpecsExport(specs));
        Ok(specs)
    }

    fn parse_glob_export(&mut self, start: u32) -> PResult<&'cx ast::GlobExport<'cx>> {
        let id = self.next_node_id();
        self.expect(TokenKind::From);
        let module = self.parse_module_spec()?;
        let n = self.alloc(ast::GlobExport {
            id,
            span: self.new_span(start),
            module,
        });
        self.insert_map(id, ast::Node::GlobExport(n));
        Ok(n)
    }

    fn parse_ns_export(&mut self, start: u32) -> PResult<&'cx ast::NsExport<'cx>> {
        let id = self.next_node_id();
        let name = self.parse_module_export_name(|this| this.create_ident(true, None));
        self.expect(TokenKind::From);
        let module = self.parse_module_spec()?;
        let ns = self.alloc(ast::NsExport {
            id,
            span: self.new_span(start),
            name,
            module,
        });
        self.insert_map(id, ast::Node::NsExport(ns));
        Ok(ns)
    }

    fn parse_import_decl(&mut self) -> PResult<&'cx ast::ImportDecl<'cx>> {
        let start = self.token.start();
        let id = self.next_node_id();

        self.expect(TokenKind::Import);
        let after_import_pos = self.token.start();
        let mut is_type_only = false;
        let mut ident = self
            .token
            .kind
            .is_ident()
            .then(|| self.create_ident(true, None));
        if let Some(i) = ident {
            let t = self.token.kind;
            if (i.name == keyword::KW_TYPE)
                && (!matches!(t, TokenKind::From)
                    || (self.is_ident()
                        && self.lookahead(Self::next_token_is_from_keyword_or_eq_token)))
                && (self.is_ident() || matches!(t, TokenKind::Asterisk | TokenKind::LBrace))
            {
                is_type_only = true;
                ident = if self.is_ident() {
                    Some(self.create_ident(true, None))
                } else {
                    None
                };
            }
        }

        if ident.is_some() && !matches!(self.token.kind, TokenKind::Comma | TokenKind::From) {
            todo!("import_eq_decl")
        }

        let clause =
            self.try_parse_import_clause(ident, after_import_pos as usize, is_type_only)?;
        let module = self.parse_module_spec()?;

        self.parse_semi();

        let import = self.alloc(ast::ImportDecl {
            id,
            span: self.new_span(start),
            clause: clause.unwrap(),
            module,
        });
        self.insert_map(id, ast::Node::ImportDecl(import));
        Ok(import)
    }

    fn try_parse_import_clause(
        &mut self,
        ident: Option<&'cx ast::Ident>,
        pos: usize,
        is_type_only: bool,
    ) -> PResult<Option<&'cx ast::ImportClause<'cx>>> {
        if ident.is_some() || matches!(self.token.kind, TokenKind::Asterisk | TokenKind::LBrace) {
            let clause = self.parse_import_clause(ident, is_type_only)?;
            self.expect(TokenKind::From);
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_import_clause(
        &mut self,
        ident: Option<&'cx ast::Ident>,
        is_type_only: bool,
    ) -> PResult<&'cx ast::ImportClause<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let kind = if ident.is_none() || self.parse_optional(TokenKind::Comma).is_some() {
            Some(if self.token.kind == TokenKind::Asterisk {
                let ns = self.with_parent(id, Self::parse_ns_import)?;
                ast::ImportClauseKind::Ns(ns)
            } else {
                let specs = self.with_parent(id, |this| {
                    this.parse_named_imports_or_exports(ParseNamedImports)
                })?;
                ast::ImportClauseKind::Specs(specs)
            })
        } else {
            None
        };
        let clause = self.alloc(ast::ImportClause {
            id,
            span: self.new_span(start),
            is_type_only,
            ident,
            kind,
        });

        self.insert_map(id, ast::Node::ImportClause(clause));

        Ok(clause)
    }

    fn parse_ns_import(&mut self) -> PResult<&'cx ast::NsImport<'cx>> {
        // `* as binding`
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Asterisk);
        self.expect(TokenKind::As);
        let name = self.create_ident(true, None);
        let ns = self.alloc(ast::NsImport {
            id,
            span: self.new_span(start),
            name,
        });
        self.insert_map(id, ast::Node::NsImport(ns));
        Ok(ns)
    }

    fn parse_module_spec(&mut self) -> PResult<&'cx ast::StringLit> {
        if self.token.kind == TokenKind::String {
            Ok(self.parse_string_lit())
        } else {
            todo!("{:#?}", self.token.kind)
        }
    }

    fn parse_decl(&mut self) -> PResult<&'cx ast::Stmt<'cx>> {
        let mods = self.parse_modifiers(false)?;
        let is_ambient = mods.is_some_and(Self::contain_declare_mod);
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
        self.expect(TokenKind::Interface);
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
        self.expect(TokenKind::Semi);
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
        let kind = self.token.kind.try_into().unwrap();
        let list = self.with_parent(id, |this| this.parse_var_decl_list(false));
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
        todo!()
        // let name = self.parse_ident_or_pat();
        // Ok(name)
    }

    fn parse_ident_or_pat(&mut self) -> PResult<&'cx ast::Binding<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        let binding = match self.token.kind {
            LBrace => ast::Binding::ObjectPat(self.parse_object_binding_pat()?),
            _ => ast::Binding::Ident(self.parse_binding_ident()),
        };
        Ok(self.alloc(binding))
    }

    fn parse_object_binding_ele(&mut self) -> PResult<&'cx ast::ObjectBindingElem<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let dotdotdot = self.parse_optional(TokenKind::DotDotDot).map(|t| t.span);
        let token_is_ident = self.token.kind.is_binding_ident();
        let name = if token_is_ident {
            let name = self.with_parent(id, |this| this.create_ident(true, None));
            if self.token.kind != TokenKind::Colon {
                self.alloc(ast::ObjectBindingName::Shorthand(name))
            } else {
                let prop_name = self.alloc(ast::PropName {
                    kind: ast::PropNameKind::Ident(name),
                });
                self.expect(TokenKind::Colon);
                let name = self.with_parent(id, Self::parse_ident_or_pat)?;
                self.alloc(ast::ObjectBindingName::Prop { prop_name, name })
            }
        } else {
            let prop_name = self.with_parent(id, Self::parse_prop_name)?;
            if self.token.kind != TokenKind::Colon {
                todo!("error")
            } else {
                self.expect(TokenKind::Colon);
                let name = self.with_parent(id, Self::parse_ident_or_pat)?;
                self.alloc(ast::ObjectBindingName::Prop { prop_name, name })
            }
        };
        let init = self.with_parent(id, Self::parse_init)?;
        let ele = self.alloc(ast::ObjectBindingElem {
            id,
            span: self.new_span(start),
            dotdotdot,
            name,
            init,
        });

        Ok(ele)
    }

    fn parse_object_binding_pat(&mut self) -> PResult<&'cx ast::ObjectPat<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::LBrace);
        let elems = self.allow_in_and(|this| {
            this.parse_delimited_list(list_ctx::ObjectBindingElems, Self::parse_object_binding_ele)
        });
        self.expect(TokenKind::RBrace);
        let pat = self.alloc(ast::ObjectPat {
            id,
            span: self.new_span(start),
            elems,
        });
        self.insert_map(id, ast::Node::ObjectPat(pat));
        Ok(pat)
    }

    fn parse_var_decl(&mut self) -> PResult<&'cx ast::VarDecl<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        let binding = self.with_parent(id, Self::parse_ident_or_pat)?;
        let ty = self.with_parent(id, Self::parse_ty_anno)?;
        let init = self.with_parent(id, Self::parse_init)?;
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

    fn parse_var_decl_list(&mut self, in_for_stmt_initializer: bool) -> VarDecls<'cx> {
        self.next_token();
        self.parse_delimited_list(list_ctx::VarDecls, Self::parse_var_decl)
    }

    fn parse_fn_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::FnDecl<'cx>> {
        self.parse_fn_decl_or_expr(ParseFnDecl, modifiers)
    }

    fn parse_if_stmt(&mut self) -> PResult<&'cx ast::IfStmt<'cx>> {
        let id = self.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::If);
        self.expect(TokenKind::LParen);
        let expr = self.with_parent(id, Self::parse_expr)?;
        self.expect(TokenKind::RParen);
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
        self.expect(TokenKind::Return);
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

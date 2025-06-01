use bolt_ts_atom::AtomId;

use bolt_ts_ast::TokenKind;
use bolt_ts_ast::{NodeFlags, VarDecls};

use super::ast;
use super::errors;
use super::list_ctx;
use super::lookahead::Lookahead;
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
        if matches!(
            self.token.kind,
            Const | Enum | Abstract | Declare | Export | Import
        ) && self.is_start_of_decl()
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
            Type => ast::StmtKind::TypeAlias(self.parse_type_alias_decl(None)?),
            Module | Namespace => ast::StmtKind::Module(self.parse_module_decl(None)?),
            Throw => ast::StmtKind::Throw(self.parse_throw_stmt()?),
            For => self.parse_for_stmt()?,
            Break => ast::StmtKind::Break(self.parse_break_or_continue(&ParseBreak)?),
            Continue => ast::StmtKind::Continue(self.parse_break_or_continue(&ParseContinue)?),
            Try => ast::StmtKind::Try(self.parse_try_stmt()?),
            While => ast::StmtKind::While(self.parse_while_stmt()?),
            Do => ast::StmtKind::Do(self.parse_do_stmt()?),
            Debugger => ast::StmtKind::Debugger(self.parse_debugger_stmt()?),
            _ => self.parse_expr_or_labeled_stmt()?,
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_debugger_stmt(&mut self) -> PResult<&'cx ast::DebuggerStmt> {
        let start = self.token.start();
        self.expect(TokenKind::Debugger);
        self.parse_optional(TokenKind::Semi);
        let id = self.next_node_id();
        let stmt = self.alloc(ast::DebuggerStmt {
            id,
            span: self.new_span(start),
        });
        self.nodes.insert(id, ast::Node::DebuggerStmt(stmt));
        Ok(stmt)
    }

    fn parse_do_stmt(&mut self) -> PResult<&'cx ast::DoStmt<'cx>> {
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
        let id = self.next_node_id();
        let stmt = self.alloc(ast::DoStmt {
            id,
            span: self.new_span(start),
            stmt,
            expr,
        });
        self.nodes.insert(id, ast::Node::DoStmt(stmt));
        Ok(stmt)
    }

    fn parse_while_stmt(&mut self) -> PResult<&'cx ast::WhileStmt<'cx>> {
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
        let stmt = self.allow_continue_and(Self::parse_stmt)?;
        let id = self.next_node_id();
        let stmt = self.alloc(ast::WhileStmt {
            id,
            span: self.new_span(start),
            expr,
            stmt,
        });
        self.nodes.insert(id, ast::Node::WhileStmt(stmt));
        self.node_flags_map.insert(id, self.context_flags);
        Ok(stmt)
    }

    fn parse_catch_clause(&mut self) -> PResult<&'cx ast::CatchClause<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Catch);

        let var = if self.parse_optional(TokenKind::LParen).is_some() {
            let v = self.parse_var_decl()?;
            self.expect(TokenKind::RParen);
            Some(v)
        } else {
            None
        };
        let block = self.parse_block()?;
        let id = self.next_node_id();
        let clause = self.alloc(ast::CatchClause {
            id,
            span: self.new_span(start),
            var,
            block,
        });
        self.nodes.insert(id, ast::Node::CatchClause(clause));
        Ok(clause)
    }

    fn parse_try_stmt(&mut self) -> PResult<&'cx ast::TryStmt<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Try);
        let try_block = self.parse_block()?;
        let catch_clause = if self.token.kind == TokenKind::Catch {
            Some(self.parse_catch_clause()?)
        } else {
            None
        };
        let finally_block =
            if catch_clause.is_none() || self.parse_optional(TokenKind::Finally).is_some() {
                Some(self.parse_block()?)
            } else {
                None
            };
        let id = self.next_node_id();
        let stmt = self.alloc(ast::TryStmt {
            id,
            span: self.new_span(start),
            try_block,
            catch_clause,
            finally_block,
        });
        self.nodes.insert(id, ast::Node::TryStmt(stmt));
        Ok(stmt)
    }

    fn parse_for_stmt(&mut self) -> PResult<ast::StmtKind<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        let start = self.token.start();
        self.expect(For);
        let await_token = self.parse_optional(Await);
        self.expect(LParen);
        let t = self.token.kind;
        let init = if t != Semi {
            if matches!(t, Var | Let | Const) {
                Some(ast::ForInitKind::Var(self.parse_var_decl_list(true)))
            } else {
                Some(ast::ForInitKind::Expr(
                    self.disallow_in_and(Self::parse_expr)?,
                ))
            }
        } else {
            None
        };

        if (await_token.is_some() && self.expect(Of)) || self.parse_optional(Of).is_some() {
            let init = init.unwrap();
            let expr = self.allow_in_and(|this| this.parse_assign_expr_or_higher(true))?;
            self.expect(RParen);
            let body = self.allow_continue_and(Self::parse_stmt)?;
            let id = self.next_node_id();
            let kind = self.alloc(ast::ForOfStmt {
                id,
                span: self.new_span(start),
                r#await: await_token.map(|t| t.span),
                init,
                expr,
                body,
            });
            self.nodes.insert(id, ast::Node::ForOfStmt(kind));
            Ok(ast::StmtKind::ForOf(kind))
        } else if self.parse_optional(In).is_some() {
            let init = init.unwrap();
            let expr = self.allow_in_and(Self::parse_expr)?;
            self.expect(RParen);
            let body = self.allow_continue_and(Self::parse_stmt)?;
            let id = self.next_node_id();
            let kind = self.alloc(ast::ForInStmt {
                id,
                span: self.new_span(start),
                init,
                expr,
                body,
            });
            self.nodes.insert(id, ast::Node::ForInStmt(kind));
            Ok(ast::StmtKind::ForIn(kind))
        } else {
            self.expect(Semi);
            let cond = if !matches!(self.token.kind, Semi | RParen) {
                Some(self.allow_in_and(Self::parse_expr)?)
            } else {
                None
            };
            self.expect(Semi);
            let incr = if !matches!(self.token.kind, RParen) {
                Some(self.allow_in_and(Self::parse_expr)?)
            } else {
                None
            };
            self.expect(RParen);

            let body = self.allow_continue_and(Self::parse_stmt)?;
            let id = self.next_node_id();
            let kind = self.alloc(ast::ForStmt {
                id,
                span: self.new_span(start),
                init,
                cond,
                incr,
                body,
            });
            self.nodes.insert(id, ast::Node::ForStmt(kind));
            Ok(ast::StmtKind::For(kind))
        }
    }

    fn parse_enum_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::EnumDecl<'cx>> {
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

        let id = self.next_node_id();
        let decl = self.alloc(ast::EnumDecl {
            id,
            span: self.new_span(start),
            modifiers,
            name,
            members,
        });
        self.set_external_module_indicator(id);
        self.nodes.insert(id, ast::Node::EnumDecl(decl));
        Ok(decl)
    }

    fn parse_enum_member(&mut self) -> PResult<&'cx ast::EnumMember<'cx>> {
        let start = self.token.start();
        let name = self.parse_prop_name(false)?;
        if matches!(name.kind, ast::PropNameKind::NumLit(_)) {
            let error = errors::AnEnumMemberCannotHaveANumericName { span: name.span() };
            self.push_error(Box::new(error));
        }
        let init = self.parse_init()?;
        let id = self.next_node_id();
        let member = self.alloc(ast::EnumMember {
            id,
            span: self.new_span(start),
            name,
            init,
        });
        self.nodes.insert(id, ast::Node::EnumMember(member));
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
            let expr = self.parse_expr()?;
            let id = self.next_node_id();
            let t = self.alloc(ast::ThrowStmt {
                id,
                span: self.new_span(start),
                expr,
            });
            self.nodes.insert(id, ast::Node::ThrowStmt(t));
            if !self.try_parse_semi()? {
                todo!()
            }
            Ok(t)
        }
    }

    fn parse_module_decl(
        &mut self,
        mods: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ModuleDecl<'cx>> {
        let start = self.token.start();
        if matches!(self.token.kind, TokenKind::Ident if self.ident_token() == keyword::IDENT_GLOBAL)
        {
            return self.parse_ambient_external_module_decl(start, mods);
        } else if self.parse_optional(TokenKind::Namespace).is_none() {
            self.expect(TokenKind::Module);
            if self.token.kind == TokenKind::String {
                return self.parse_ambient_external_module_decl(start, mods);
            }
        }
        let name = self.parse_ident_name()?;
        let block = self.parse_module_block()?;
        let span = self.new_span(start);
        let id = self.next_node_id();
        let decl = self.create_ns_decl(
            id,
            span,
            mods,
            ast::ModuleName::Ident(name),
            Some(block),
            false,
        );
        Ok(decl)
    }

    pub(super) fn is_ident_name(&self, name: AtomId) -> bool {
        self.token.kind.is_ident_or_keyword() && self.ident_token() == name
    }

    fn parse_module_block(&mut self) -> PResult<&'cx ast::ModuleBlock<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBrace);

        let save_external_module_indicator = self.external_module_indicator;
        let save_has_export_decl = self.has_export_decl;
        self.has_export_decl = false;

        let stmts = self.parse_list(list_ctx::BlockStmts, Self::parse_stmt);

        self.has_export_decl = save_has_export_decl;
        self.external_module_indicator = save_external_module_indicator;

        self.expect(TokenKind::RBrace);
        let id = self.next_node_id();
        let block = self.alloc(ast::ModuleBlock {
            id,
            span: self.new_span(start),
            stmts,
        });
        self.nodes.insert(id, ast::Node::ModuleBlock(block));
        Ok(block)
    }

    fn parse_ambient_external_module_decl(
        &mut self,
        start: u32,
        mods: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ModuleDecl<'cx>> {
        let name;
        let mut flags = NodeFlags::default();
        let mut is_global_argument = false;
        if self.is_ident_name(IDENT_GLOBAL) {
            name = ast::ModuleName::Ident(self.create_ident(true, None));
            flags |= NodeFlags::GLOBAL_AUGMENTATION;
            is_global_argument = true;
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
        let id = self.next_node_id();
        Ok(self.create_ns_decl(id, span, mods, name, block, is_global_argument))
    }

    fn set_export_context_flags(&self, block_exist: bool) -> NodeFlags {
        if self.context_flags.intersects(ast::NodeFlags::AMBIENT)
            && block_exist
            && !self.has_export_decl
        {
            self.context_flags | NodeFlags::EXPORT_CONTEXT
        } else {
            self.context_flags & !NodeFlags::EXPORT_CONTEXT
        }
    }

    fn create_ns_decl(
        &mut self,
        id: ast::NodeID,
        span: bolt_ts_span::Span,
        modifiers: Option<&'cx bolt_ts_ast::Modifiers<'cx>>,
        name: bolt_ts_ast::ModuleName<'cx>,
        block: Option<&'cx bolt_ts_ast::ModuleBlock<'cx>>,
        is_global_argument: bool,
    ) -> &'cx ast::ModuleDecl<'cx> {
        let flags = self.set_export_context_flags(block.is_some());
        let decl = self.alloc(ast::ModuleDecl {
            id,
            span,
            modifiers,
            name,
            block,
            is_global_argument,
        });
        self.node_flags_map.insert(id, flags);
        self.set_external_module_indicator_if_has_export_mod(modifiers, id);
        self.nodes.insert(id, ast::Node::ModuleDecl(decl));
        decl
    }

    fn parse_type_alias_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::TypeAliasDecl<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Type);
        let name = self.parse_ident_name()?;
        let ty_params = self.parse_ty_params().unwrap();
        self.expect(TokenKind::Eq);
        let ty = if self.token.kind == TokenKind::Intrinsic {
            let id = self.next_node_id();
            let t = self.alloc(ast::IntrinsicTy {
                id,
                span: self.token.span,
            });
            self.nodes.insert(id, ast::Node::IntrinsicTy(t));
            let t = self.alloc(ast::Ty {
                kind: ast::TyKind::Intrinsic(t),
            });
            self.next_token();
            t
        } else {
            self.parse_ty()?
        };
        self.parse_semi();
        let id = self.next_node_id();
        let decl = self.alloc(ast::TypeAliasDecl {
            id,
            span: self.new_span(start),
            modifiers,
            name,
            ty_params,
            ty,
        });
        self.set_external_module_indicator_if_has_export_mod(modifiers, id);
        self.nodes.insert(id, ast::Node::TypeAliasDecl(decl));
        Ok(decl)
    }

    fn contain_declare_mod(mods: &ast::Modifiers<'cx>) -> bool {
        mods.flags.contains(ast::ModifierKind::Ambient)
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
            Module | Namespace => ast::StmtKind::Module(self.parse_module_decl(mods)?),
            Ident => {
                let id = self.ident_token();
                if id == keyword::IDENT_GLOBAL {
                    ast::StmtKind::Module(self.parse_module_decl(mods)?)
                } else {
                    unreachable!("{:#?}", self.atoms.lock().unwrap().get(id));
                }
            }
            Interface => ast::StmtKind::Interface(self.parse_interface_decl(mods)?),
            Enum => ast::StmtKind::Enum(self.parse_enum_decl(mods)?),
            Import => ast::StmtKind::Import(self.parse_import_decl()?),
            Export => {
                let start = self.token.start();
                self.next_token();
                match self.token.kind {
                    Default | Eq => {
                        ast::StmtKind::ExportAssign(self.parse_export_assignment(start, mods)?)
                    }
                    As => todo!(),
                    _ => ast::StmtKind::Export(self.parse_export_decl(start)?),
                }
            }
            Type => ast::StmtKind::TypeAlias(self.parse_type_alias_decl(mods)?),

            _ => unreachable!("{:#?}", self.token.kind),
        };
        let stmt = self.alloc(ast::Stmt { kind });
        Ok(stmt)
    }

    fn parse_export_assignment(
        &mut self,
        start: u32,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ExportAssign<'cx>> {
        self.has_export_decl = true;
        let is_export_equals = self.parse_optional(TokenKind::Eq).is_some();
        if !is_export_equals {
            self.expect(TokenKind::Default);
        }
        let expr = self.parse_assign_expr_or_higher(true)?;
        self.parse_semi();
        let id = self.next_node_id();
        let node = self.alloc(ast::ExportAssign {
            id,
            span: self.new_span(start),
            expr,
            modifiers,
            is_export_equals,
        });
        self.nodes.insert(id, ast::Node::ExportAssign(node));
        Ok(node)
    }

    fn parse_export_decl(&mut self, start: u32) -> PResult<&'cx ast::ExportDecl<'cx>> {
        self.has_export_decl = true;

        let is_type_only = self.parse_optional(TokenKind::Type).is_some();

        let ns_export_start = self.token.start();

        let is_asterisk = self.parse_optional(TokenKind::Asterisk).is_some();

        let kind = if is_asterisk && self.parse_optional(TokenKind::As).is_some() {
            // export * as
            let ns = self.parse_ns_export(ns_export_start)?;
            ast::ExportClauseKind::Ns(ns)
        } else if is_asterisk {
            // export *
            let glob = self.parse_glob_export(ns_export_start)?;
            ast::ExportClauseKind::Glob(glob)
        } else {
            // export {
            let specs = self.parse_specs_export(ns_export_start)?;
            ast::ExportClauseKind::Specs(specs)
        };
        let clause = self.alloc(ast::ExportClause { is_type_only, kind });
        let id = self.next_node_id();
        let decl = self.alloc(ast::ExportDecl {
            id,
            span: self.new_span(start),
            clause,
        });
        self.nodes.insert(id, ast::Node::ExportDecl(decl));
        self.set_external_module_indicator(id);
        self.parse_semi();
        Ok(decl)
    }

    fn parse_specs_export(&mut self, start: u32) -> PResult<&'cx ast::SpecsExport<'cx>> {
        let list = self.parse_named_imports_or_exports(ParseNamedExports)?;
        let module = if self.parse_optional(TokenKind::From).is_some() {
            Some(self.parse_module_spec()?)
        } else {
            None
        };
        let id = self.next_node_id();
        let specs = self.alloc(ast::SpecsExport {
            id,
            span: self.new_span(start),
            list,
            module,
        });
        self.nodes.insert(id, ast::Node::SpecsExport(specs));
        Ok(specs)
    }

    fn parse_glob_export(&mut self, start: u32) -> PResult<&'cx ast::GlobExport<'cx>> {
        self.expect(TokenKind::From);
        let module = self.parse_module_spec()?;
        let id = self.next_node_id();
        let n = self.alloc(ast::GlobExport {
            id,
            span: self.new_span(start),
            module,
        });
        self.nodes.insert(id, ast::Node::GlobExport(n));
        Ok(n)
    }

    fn parse_ns_export(&mut self, start: u32) -> PResult<&'cx ast::NsExport<'cx>> {
        let name = self.parse_module_export_name(|this| this.create_ident(true, None));
        self.expect(TokenKind::From);
        let module = self.parse_module_spec()?;
        let id = self.next_node_id();
        let ns = self.alloc(ast::NsExport {
            id,
            span: self.new_span(start),
            name,
            module,
        });
        self.nodes.insert(id, ast::Node::NsExport(ns));
        Ok(ns)
    }

    fn parse_import_decl(&mut self) -> PResult<&'cx ast::ImportDecl<'cx>> {
        let start = self.token.start();

        self.expect(TokenKind::Import);
        let after_import_pos = self.token.start();
        let mut is_type_only = false;
        let mut name = self
            .token
            .kind
            .is_ident()
            .then(|| self.create_ident(true, None));
        if let Some(i) = name {
            let t = self.token.kind;
            if (i.name == keyword::KW_TYPE)
                && (!matches!(t, TokenKind::From)
                    || (self.is_ident()
                        && self.lookahead(Lookahead::next_token_is_from_keyword_or_eq_token)))
                && (self.is_ident() || matches!(t, TokenKind::Asterisk | TokenKind::LBrace))
            {
                is_type_only = true;
                name = if self.is_ident() {
                    Some(self.create_ident(true, None))
                } else {
                    None
                };
            }
        }

        if name.is_some() && !matches!(self.token.kind, TokenKind::Comma | TokenKind::From) {
            todo!("import_eq_decl")
        }

        let clause = self.try_parse_import_clause(name, after_import_pos as usize, is_type_only)?;
        let module = self.parse_module_spec()?;

        self.parse_semi();

        let id = self.next_node_id();
        let import = self.alloc(ast::ImportDecl {
            id,
            span: self.new_span(start),
            clause: clause.unwrap(),
            module,
        });
        self.set_external_module_indicator(import.id);
        self.nodes.insert(id, ast::Node::ImportDecl(import));
        Ok(import)
    }

    fn try_parse_import_clause(
        &mut self,
        name: Option<&'cx ast::Ident>,
        pos: usize,
        is_type_only: bool,
    ) -> PResult<Option<&'cx ast::ImportClause<'cx>>> {
        if name.is_some() || matches!(self.token.kind, TokenKind::Asterisk | TokenKind::LBrace) {
            let clause = self.parse_import_clause(name, pos, is_type_only)?;
            self.expect(TokenKind::From);
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_import_clause(
        &mut self,
        name: Option<&'cx ast::Ident>,
        start: usize,
        is_type_only: bool,
    ) -> PResult<&'cx ast::ImportClause<'cx>> {
        let kind = if name.is_none_or(|_| self.parse_optional(TokenKind::Comma).is_some()) {
            Some(if self.token.kind == TokenKind::Asterisk {
                let ns = self.parse_ns_import()?;
                ast::ImportClauseKind::Ns(ns)
            } else {
                let specs = self.parse_named_imports_or_exports(ParseNamedImports)?;
                ast::ImportClauseKind::Specs(specs)
            })
        } else {
            None
        };
        let id = self.next_node_id();
        let clause = self.alloc(ast::ImportClause {
            id,
            span: self.new_span(start as u32),
            is_type_only,
            name,
            kind,
        });

        self.nodes.insert(id, ast::Node::ImportClause(clause));

        Ok(clause)
    }

    fn parse_ns_import(&mut self) -> PResult<&'cx ast::NsImport<'cx>> {
        // `* as binding`
        let start = self.token.start();
        self.expect(TokenKind::Asterisk);
        self.expect(TokenKind::As);
        let name = self.create_ident(true, None);
        let id = self.next_node_id();
        let ns = self.alloc(ast::NsImport {
            id,
            span: self.new_span(start),
            name,
        });
        self.nodes.insert(id, ast::Node::NsImport(ns));
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
        let mods = self.parse_modifiers(false, None)?;
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
            let start = self.token.start();
            self.next_token();
            let list = self.parse_delimited_list(
                list_ctx::HeritageClause,
                Self::parse_entity_name_of_ty_reference,
            );
            let span = self.new_span(start);
            let id = self.next_node_id();
            let clause = self.alloc(ast::InterfaceExtendsClause { id, span, list });
            self.nodes
                .insert(id, ast::Node::InterfaceExtendsClause(clause));
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_interface_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::InterfaceDecl<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Interface);
        let name = self.parse_ident_name()?;
        let ty_params = self.parse_ty_params()?;
        let extends = self.parse_interface_extends_clause()?;
        // let implements = self.parse_implements_clause()?;
        let members = self.parse_object_ty_members()?;
        let id = self.next_node_id();
        let decl = self.alloc(ast::InterfaceDecl {
            id,
            span: self.new_span(start),
            modifiers,
            name,
            ty_params,
            extends,
            members,
        });
        self.set_external_module_indicator_if_has_export_mod(modifiers, id);
        self.nodes.insert(id, ast::Node::InterfaceDecl(decl));
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
            let start = self.token.start();
            self.next_token();
            let list = self.parse_delimited_list(
                list_ctx::HeritageClause,
                Self::parse_entity_name_of_ty_reference,
            );
            let span = self.new_span(start);
            let id = self.next_node_id();
            let clause = self.alloc(ast::ClassImplementsClause { id, span, list });
            self.nodes
                .insert(id, ast::Node::ClassImplementsClause(clause));
            Ok(Some(clause))
        } else {
            Ok(None)
        }
    }

    fn parse_empty_stmt(&mut self) -> PResult<&'cx ast::EmptyStmt> {
        let start = self.token.start();
        self.expect(TokenKind::Semi);
        let id = self.next_node_id();
        let stmt = self.alloc(ast::EmptyStmt {
            id,
            span: self.new_span(start),
        });
        self.nodes.insert(id, ast::Node::EmptyStmt(stmt));
        Ok(stmt)
    }

    fn parse_var_stmt(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> &'cx ast::VarStmt<'cx> {
        let start = self.token.start();
        let flags = match self.token.kind {
            TokenKind::Const => ast::NodeFlags::CONST,
            TokenKind::Let => ast::NodeFlags::LET,
            TokenKind::Var => ast::NodeFlags::empty(),
            _ => unreachable!(),
        };
        let list = self.parse_var_decl_list(false);
        if list.is_empty() {
            let span = self.new_span(start);
            self.push_error(Box::new(errors::VariableDeclarationListCannotBeEmpty {
                span,
            }));
        }
        let span = self.new_span(start);
        let id = self.next_node_id();
        let node = self.alloc(ast::VarStmt {
            id,
            span,
            modifiers,
            list,
        });
        self.node_flags_map.insert(id, flags);
        self.set_external_module_indicator_if_has_export_mod(modifiers, node.id);
        self.nodes.insert(id, ast::Node::VarStmt(node));
        self.parse_semi();
        node
    }

    pub(super) fn set_external_module_indicator_if_has_export_mod(
        &mut self,
        mods: Option<&'cx ast::Modifiers<'cx>>,
        node: ast::NodeID,
    ) {
        if mods.is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Export)) {
            self.set_external_module_indicator(node);
        }
    }

    fn set_external_module_indicator(&mut self, node: ast::NodeID) {
        if self.external_module_indicator.is_none() {
            self.external_module_indicator = Some(node);
        }
    }

    pub(super) fn parse_name_of_param(&mut self) -> PResult<&'cx ast::Binding<'cx>> {
        let binding = self.parse_ident_or_pat()?;
        // if (getFullWidth(name) === 0 && !some(modifiers) && isModifierKind(token())) {
        //     // in cases like
        //     // 'use strict'
        //     // function foo(static)
        //     // isParameter('static') === true, because of isModifier('static')
        //     // however 'static' is not a legal identifier in a strict mode.
        //     // so result of this function will be ParameterDeclaration (flags = 0, name = missing, type = undefined, initializer = undefined)
        //     // and current token will not change => parsing of the enclosing parameter list will last till the end of time (or OOM)
        //     // to avoid this we'll advance cursor to the next token.
        //     nextToken();
        // }
        Ok(binding)
    }

    fn parse_ident_or_pat(&mut self) -> PResult<&'cx ast::Binding<'cx>> {
        use bolt_ts_ast::TokenKind::*;
        let start = self.token.start();
        let kind = match self.token.kind {
            LBracket => ast::BindingKind::ArrayPat(self.parse_array_binding_pat()?),
            LBrace => ast::BindingKind::ObjectPat(self.parse_object_binding_pat()?),
            _ => ast::BindingKind::Ident(self.parse_binding_ident()),
        };
        let span = self.new_span(start);
        let id = self.next_node_id();
        let binding = self.alloc(ast::Binding { id, span, kind });
        self.nodes.insert(id, ast::Node::Binding(binding));
        Ok(binding)
    }

    fn parse_object_binding_elem(&mut self) -> PResult<&'cx ast::ObjectBindingElem<'cx>> {
        let start = self.token.start();
        let dotdotdot = self.parse_optional(TokenKind::DotDotDot).map(|t| t.span);
        let token_is_ident = self.token.kind.is_binding_ident();
        let name = if token_is_ident {
            let name = self.create_ident(true, None);
            if self.token.kind != TokenKind::Colon {
                self.alloc(ast::ObjectBindingName::Shorthand(name))
            } else {
                let prop_name = self.alloc(ast::PropName {
                    kind: ast::PropNameKind::Ident(name),
                });
                self.expect(TokenKind::Colon);
                let name = self.parse_ident_or_pat()?;
                self.alloc(ast::ObjectBindingName::Prop { prop_name, name })
            }
        } else {
            let prop_name = self.parse_prop_name(false)?;
            self.expect(TokenKind::Colon);
            let name = self.parse_ident_or_pat()?;
            self.alloc(ast::ObjectBindingName::Prop { prop_name, name })
        };
        let init = self.parse_init()?;
        let id = self.next_node_id();
        let ele = self.alloc(ast::ObjectBindingElem {
            id,
            span: self.new_span(start),
            dotdotdot,
            name,
            init,
        });
        self.nodes.insert(id, ast::Node::ObjectBindingElem(ele));
        Ok(ele)
    }

    fn parse_object_binding_pat(&mut self) -> PResult<&'cx ast::ObjectPat<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBrace);
        let elems = self.allow_in_and(|this| {
            this.parse_delimited_list(
                list_ctx::ObjectBindingElems,
                Self::parse_object_binding_elem,
            )
        });
        self.expect(TokenKind::RBrace);
        let id = self.next_node_id();
        let pat = self.alloc(ast::ObjectPat {
            id,
            span: self.new_span(start),
            elems,
        });
        self.nodes.insert(id, ast::Node::ObjectPat(pat));
        Ok(pat)
    }

    fn parse_array_binding_pat(&mut self) -> PResult<&'cx ast::ArrayPat<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::LBracket);
        // TODO: elements
        let elems = self.allow_in_and(|this| {
            this.parse_delimited_list(list_ctx::ArrayBindingElems, Self::parse_array_binding_elem)
        });
        self.expect(TokenKind::RBracket);
        let id = self.next_node_id();
        let pat = self.alloc(ast::ArrayPat {
            id,
            span: self.new_span(start),
            elems,
        });
        self.nodes.insert(id, ast::Node::ArrayPat(pat));
        Ok(pat)
    }

    fn parse_array_binding_elem(&mut self) -> PResult<&'cx ast::ArrayBindingElem<'cx>> {
        let start = self.token.start();
        if self.token.kind == TokenKind::Comma {
            let id = self.next_node_id();
            let omit_expr = self.alloc(ast::OmitExpr {
                id,
                span: self.new_span(start),
            });
            let elem = self.alloc(ast::ArrayBindingElem {
                id,
                span: self.new_span(start),
                kind: ast::ArrayBindingElemKind::Omit(omit_expr),
            });
            self.nodes.insert(id, ast::Node::ArrayBindingElem(elem));
            return Ok(elem);
        }
        let dotdotdot = self.parse_optional(TokenKind::DotDotDot).map(|t| t.span);
        let name = self.parse_ident_or_pat()?;
        let init = self.parse_init()?;
        let id = self.next_node_id();
        let elem = self.alloc(ast::ArrayBindingElem {
            id,
            span: self.new_span(start),
            kind: ast::ArrayBindingElemKind::Binding {
                dotdotdot,
                name,
                init,
            },
        });
        self.nodes.insert(id, ast::Node::ArrayBindingElem(elem));
        Ok(elem)
    }

    fn parse_var_decl(&mut self) -> PResult<&'cx ast::VarDecl<'cx>> {
        let start = self.token.start();
        let binding = self.parse_ident_or_pat()?;
        let ty = self.parse_ty_anno()?;
        let init = self.parse_init()?;
        let span = self.new_span(start);
        let id = self.next_node_id();
        let node = self.alloc(ast::VarDecl {
            id,
            span,
            binding,
            ty,
            init,
        });
        self.nodes.insert(id, ast::Node::VarDecl(node));
        self.node_flags_map.insert(id, self.context_flags);
        Ok(node)
    }

    fn parse_var_decl_list(&mut self, in_for_stmt_initializer: bool) -> VarDecls<'cx> {
        self.next_token();
        self.parse_delimited_list(list_ctx::VarDecls, |this| this.parse_var_decl())
    }

    fn parse_fn_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::FnDecl<'cx>> {
        self.parse_fn_decl_or_expr(ParseFnDecl, modifiers)
    }

    fn parse_if_stmt(&mut self) -> PResult<&'cx ast::IfStmt<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::If);
        self.expect(TokenKind::LParen);
        let expr = self.parse_expr()?;
        self.expect(TokenKind::RParen);
        let then = self.parse_stmt()?;
        let else_then = if self.parse_optional(TokenKind::Else).is_some() {
            Some(self.parse_stmt()?)
        } else {
            None
        };
        let id = self.next_node_id();
        let stmt = self.alloc(ast::IfStmt {
            id,
            span: self.new_span(start),
            expr,
            then,
            else_then,
        });
        self.nodes.insert(id, ast::Node::IfStmt(stmt));
        Ok(stmt)
    }

    fn parse_ret_stmt(&mut self) -> PResult<&'cx ast::RetStmt<'cx>> {
        let start = self.token.start();
        self.expect(TokenKind::Return);
        let expr = if self.can_parse_semi() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.parse_semi();
        let id = self.next_node_id();
        let stmt = self.alloc(ast::RetStmt {
            id,
            span: self.new_span(start),
            expr,
        });
        self.nodes.insert(id, ast::Node::RetStmt(stmt));
        Ok(stmt)
    }

    fn parse_expr_or_labeled_stmt(&mut self) -> PResult<ast::StmtKind<'cx>> {
        let start = self.token.start();
        let expr = self.allow_in_and(Self::parse_expr)?;
        if let ast::ExprKind::Ident(ident) = expr.kind {
            if self.parse_optional(TokenKind::Colon).is_some() {
                let stmt = self.parse_stmt()?;
                let id = self.next_node_id();
                let stmt = self.alloc(ast::LabeledStmt {
                    id,
                    span: self.new_span(start),
                    label: ident,
                    stmt,
                });
                self.nodes.insert(id, ast::Node::LabeledStmt(stmt));
                return Ok(ast::StmtKind::Labeled(stmt));
            }
        }
        self.parse_semi();
        let id = self.next_node_id();
        let stmt = self.alloc(ast::ExprStmt {
            id,
            span: self.new_span(start),
            expr,
        });
        self.nodes.insert(id, ast::Node::ExprStmt(stmt));
        Ok(ast::StmtKind::Expr(stmt))
    }
}

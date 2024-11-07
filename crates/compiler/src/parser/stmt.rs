use super::list_ctx::{self, ListContext};
use super::token::TokenKind;
use super::{ast, errors};
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

    fn parse_interface_decl(&mut self) -> PResult<&'cx ast::InterfaceDecl<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        self.expect(TokenKind::Interface)?;
        let name = self.with_parent(id, Self::parse_ident_name)?;
        let ty_params = self.with_parent(id, Self::parse_ty_params)?;
        let extends = self.with_parent(id, |this| this.parse_heritage_clauses(false))?;
        let members = self.with_parent(id, Self::parse_object_ty_members)?;
        let decl = self.alloc(ast::InterfaceDecl {
            id,
            name,
            span: self.new_span(start as usize, self.pos),
            // extends,
            // ty_params,
            members,
        });
        Ok(decl)
    }

    fn parse_semi_after_prop_name(&mut self) {
        self.parse_semi();
    }

    fn parse_class_prop_or_method(&mut self) -> PResult<&'cx ast::ClassEle<'cx>> {
        let id = self.p.next_node_id();
        let start = self.token.start();
        let mods = self.with_parent(id, Self::parse_modifiers)?;
        let name = self.with_parent(id, Self::parse_prop_name)?;
        let ty = self.with_parent(id, Self::parse_ty_anno)?;
        let ele = if ty.is_some() {
            // prop
            let init = self.parse_init();
            let prop = self.alloc(ast::ClassPropEle {
                id,
                span: self.new_span(start as usize, self.pos),
                name,
                ty,
                init,
            });
            self.insert_map(id, ast::Node::ClassPropEle(prop));
            self.parse_semi_after_prop_name();
            self.alloc(ast::ClassEle {
                kind: ast::ClassEleKind::Prop(prop),
            })
        } else {
            todo!()
        };
        Ok(ele)
    }

    fn parse_class_ele(&mut self) -> PResult<&'cx ast::ClassEle<'cx>> {
        self.parse_class_prop_or_method()
    }

    fn parse_class_members(&mut self) -> PResult<ast::ClassEles<'cx>> {
        self.expect(TokenKind::LBrace)?;
        let eles = self.parse_list(
            list_ctx::ClassElements::is_ele,
            Self::parse_class_ele,
            list_ctx::ClassElements::is_closing,
        );
        self.expect(TokenKind::RBrace)?;
        Ok(eles)
    }

    fn parse_class_decl(
        &mut self,
        modifiers: Option<&'cx ast::Modifiers<'cx>>,
    ) -> PResult<&'cx ast::ClassDecl<'cx>> {
        let start = self.token.start();
        let id = self.p.next_node_id();
        self.expect(TokenKind::Class)?;
        let name = self.parse_ident_name()?;
        let ty_params = self.parse_ty_params()?;
        let extends = self.parse_class_extends()?;
        let implements = self.parse_heritage_clauses(true)?;
        let eles = self.parse_class_members()?;
        let decl = self.alloc(ast::ClassDecl {
            id,
            span: self.new_span(start as usize, self.pos),
            modifiers,
            name,
            extends,
            implements,
            ty_params,
            eles,
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
            if kind == ast::HeritageClauseKind::Extends {
                let id = self.p.next_node_id();
                let start = self.token.start();
                self.next_token();
                let mut is_first = true;
                let mut extra_comma_span = None;
                let mut ele = None;
                let mut last_ele_span = None;
                loop {
                    if list_ctx::HeritageClause::is_ele(self) {
                        let Ok(e) = self.parse_expr_with_ty_args() else {
                            break;
                        };
                        if is_first {
                            ele = Some(e);
                        } else {
                            last_ele_span = Some(e.span())
                        }
                        if list_ctx::HeritageClause::is_closing(self) {
                            break;
                        }
                        let span = self.token.span;
                        if self.parse_optional(TokenKind::Comma).is_some() {
                            if is_first {
                                extra_comma_span = Some(span);
                            }
                            is_first = false;
                            continue;
                        }
                        unreachable!()
                    }
                    if list_ctx::HeritageClause::is_closing(self) {
                        break;
                    }
                }
                let ele = ele.unwrap();
                if let Some(extra_comma_span) = extra_comma_span {
                    let lo = ele.span().hi;
                    assert_eq!(
                        self.input[lo as usize], b',',
                        "`parse_delimited_list` ensure it must be comma."
                    );
                    let hi = last_ele_span
                        .map(|span| span.hi)
                        .unwrap_or_else(|| extra_comma_span.hi);
                    let extra_extends = last_ele_span
                        .is_some()
                        .then(|| self.new_span(lo as usize, hi as usize));
                    let error = errors::ClassesCanOnlyExtendASingleClass {
                        span: self.new_span(lo as usize, lo as usize),
                        extra_extends,
                    };
                    self.push_error(self.module_id, Box::new(error));
                }
                let clause = self.alloc(ast::HeritageClause {
                    id,
                    span: ele.span(),
                    tys: self.alloc([ele]),
                });
                self.insert_map(id, ast::Node::HeritageClause(clause));
                return Ok(Some(clause));
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
                span: self.new_span(start as usize, self.pos),
                clauses,
            });
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

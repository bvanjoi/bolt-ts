use std::borrow::Cow;

use super::Emit;
use crate::ast;

impl<'cx> Emit<'cx> {
    pub(super) fn emit_stmt(&mut self, stmt: &ast::Stmt<'cx>) {
        use ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.emit_var_stmt(var),
            Expr(expr) => self.emit_expr(expr),
            Fn(f) => self.emit_fn_decl(f),
            If(stmt) => self.emit_if_stmt(stmt),
            Block(block) => self.emit_block_stmt(block),
            Return(ret) => self.emit_ret_stmt(ret),
            Empty(_) => {}
            Class(class) => self.emit_class_decl(class),
            Interface(_) => {}
            Type(_) => {}
            Throw(t) => self.emit_throw_stmt(t),
            Namespace(ns) => self.emit_ns_decl(ns),
            Enum(e) => self.emit_enum_decl(e),
        }
    }

    fn emit_throw_stmt(&mut self, t: &'cx ast::ThrowStmt<'cx>) {
        self.content.p("throw");
        self.content.p_whitespace();
        self.emit_expr(t.expr);
    }

    fn emit_enum_decl(&mut self, e: &'cx ast::EnumDecl) {
        self.emit_with_var_fn_wrapper(e.name, self.atoms.get(e.name.name), |this| {
            for member in e.members {
                this.content.p_newline();
                this.emit_ident(e.name);
                this.content.p_l_bracket();
                this.emit_ident(e.name);
                this.content.p_l_bracket();
                match member.name.kind {
                    ast::PropNameKind::Ident(ident) => this.emit_as_string(ident.name),
                    ast::PropNameKind::StringLit(lit) => todo!(),
                    ast::PropNameKind::NumLit(lit) => todo!(),
                }
                this.content.p_r_bracket();
                this.content.p_whitespace();
                this.content.p_eq();
                this.content.p_whitespace();
                if let Some(init) = member.init {
                    this.emit_expr(init);
                } else {
                    // todo:
                    let val = 0;
                    this.content.p(&val.to_string());
                }
                this.content.p_r_bracket();
                this.content.p_whitespace();
                this.content.p_eq();
                this.content.p_whitespace();
                match member.name.kind {
                    ast::PropNameKind::Ident(ident) => this.emit_as_string(ident.name),
                    ast::PropNameKind::StringLit(lit) => todo!(),
                    ast::PropNameKind::NumLit(lit) => todo!(),
                }
            }
        })
    }

    fn emit_with_var_fn_wrapper(
        &mut self,
        decl_name: &ast::Ident,
        param_name: &str,
        f: impl FnOnce(&mut Self),
    ) {
        self.content.p("var");
        self.content.p_whitespace();
        self.emit_ident(decl_name);
        self.content.p_whitespace();
        self.content.p_eq();
        self.content.p_whitespace();
        self.content.p("{}");
        self.content.p_semi();

        self.content.p_newline();

        self.content.p_l_paren();
        self.content.p("function");
        self.content.p_whitespace();
        self.content.p_l_paren();

        self.content.p(&param_name);
        self.content.p_r_paren();
        self.content.p_whitespace();

        // emit block
        self.content.p_l_brace();
        self.content.p_newline();
        self.content.p_pieces_of_whitespace(self.state.indent);
        self.state.indent += self.options.indent;

        f(self);

        self.state.indent -= self.options.indent;
        self.content.p_newline();
        self.content.p_pieces_of_whitespace(self.state.indent);
        self.content.p_r_brace();

        self.content.p_r_paren();
        self.content.p_l_paren();
        self.emit_ident(decl_name);
        self.content.p_r_paren();
    }

    fn emit_ns_decl(&mut self, ns: &'cx ast::NsDecl) {
        if ns
            .modifiers
            .map(|ms| ms.flags.contains(ast::ModifierKind::Declare))
            .unwrap_or_default()
        {
            return;
        }
        // var name
        let mut sub_names = ns
            .block
            .stmts
            .iter()
            .filter_map(|stmt| match stmt.kind {
                ast::StmtKind::Var(v) => Some(
                    v.list
                        .iter()
                        .map(|item| item.binding.name)
                        .collect::<Vec<_>>(),
                ),
                ast::StmtKind::Class(c) => Some(vec![c.name.name]),
                _ => None,
            })
            .flatten()
            .map(|name| self.atoms.get(name))
            .collect::<Vec<_>>();
        sub_names.sort();
        let mut param_name = Cow::Borrowed(self.atoms.get(ns.name.name));
        if let Some(i) = sub_names.iter().position(|sub| *sub == param_name) {
            let mut offset = 1;
            let mut n = format!("{}_{}", param_name, offset);
            for sub in &sub_names[i + 1..] {
                if n == *sub {
                    offset += 1;
                    n = format!("{}_{}", n, offset);
                } else {
                    break;
                }
            }
            param_name = Cow::Owned(n);
        }

        self.emit_with_var_fn_wrapper(ns.name, &param_name, |this| {
            for stmt in ns.block.stmts {
                use ast::StmtKind::*;
                this.content.p_newline();
                this.emit_stmt(stmt);
                this.content.p_newline();
                let t = match stmt.kind {
                    Var(v) => {
                        if v.modifiers
                            .map(|ms| ms.flags.contains(ast::ModifierKind::Export))
                            .unwrap_or_default()
                        {
                            for item in v.list {
                                this.content.p(&param_name);
                                this.content.p_dot();
                                this.emit_ident(item.binding);
                                this.content.p_whitespace();
                                this.content.p_eq();
                                this.content.p_whitespace();
                                this.emit_ident(item.binding);
                                this.content.p_newline();
                            }
                        }
                        continue;
                    }
                    Fn(f) => f.modifiers.map(|ms| (ms, f.name)),
                    Class(c) => c.modifiers.map(|ms| (ms, c.name)),
                    Interface(_) | Type(_) => None,
                    Namespace(n) => n.modifiers.map(|ms| (ms, n.name)),
                    _ => None,
                };
                let Some((ms, name)) = t else {
                    continue;
                };
                if ms.flags.contains(ast::ModifierKind::Export) {
                    this.content.p(&param_name);
                    this.content.p_dot();
                    this.emit_ident(name);
                    this.content.p_whitespace();
                    this.content.p_eq();
                    this.content.p_whitespace();
                    this.emit_ident(name);
                    this.content.p_newline();
                }
            }
        });
    }

    fn emit_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.emit_class_like(class);
    }

    fn emit_ret_stmt(&mut self, ret: &'cx ast::RetStmt<'cx>) {
        self.content.p("return");
        self.content.p_whitespace();
        if let Some(expr) = ret.expr {
            self.emit_expr(expr);
        }
    }

    fn emit_if_stmt(&mut self, stmt: &'cx ast::IfStmt<'cx>) {
        self.content.p("if");
        self.content.p_whitespace();
        // test
        self.content.p_l_paren();
        self.emit_expr(stmt.expr);
        self.content.p_r_paren();
        self.content.p_whitespace();
        // block
        self.emit_stmt(stmt.then);
        // else
        if let Some(else_then) = stmt.else_then {
            self.content.p_whitespace();
            self.content.p("else");
            self.content.p_whitespace();
            self.emit_stmt(else_then);
        }
        self.content.p_newline();
    }

    pub(super) fn emit_stmts(&mut self, body: ast::Stmts<'cx>) {
        self.emit_list(
            body,
            |this, item| this.emit_stmt(item),
            |this, _| {
                this.content.p_newline();
                this.content.p_pieces_of_whitespace(this.state.indent);
            },
        )
    }

    fn emit_fn_decl(&mut self, f: &'cx ast::FnDecl) {
        if let Some(body) = f.body {
            self.content.p("function");
            self.content.p_whitespace();
            self.emit_ident(f.name);
            self.emit_params(f.params);
            self.content.p_whitespace();
            self.emit_block_stmt(body);
        }
    }

    fn emit_var_stmt(&mut self, var: &'cx ast::VarStmt) {
        self.content.p("var");
        self.content.p_whitespace();
        for (idx, decl) in var.list.iter().enumerate() {
            self.emit_var_decl(decl);
            if idx != var.list.len() - 1 {
                self.content.p_comma();
                self.content.p_whitespace();
            }
        }
    }

    fn emit_var_decl(&mut self, decl: &'cx ast::VarDecl) {
        self.content.p(self.atoms.get(decl.binding.name));
        if let Some(init) = decl.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }
}

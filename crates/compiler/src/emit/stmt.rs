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
            Namespace(ns) => self.emit_ns_decl(ns),
        }
    }

    fn emit_ns_decl(&mut self, ns: &'cx ast::NsDecl) {
        if ns
            .modifiers
            .map(|ms| {
                ms.list
                    .iter()
                    .find(|m| m.kind == ast::ModifierKind::Declare)
                    .is_some()
            })
            .unwrap_or_default()
        {
            return;
        }
        // var name
        self.content.p("var");
        self.content.p_whitespace();
        self.emit_ident(ns.name);
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
        // TODO: don't emit name if no export
        self.emit_ident(ns.name);
        self.content.p_r_paren();
        self.content.p_whitespace();
        self.emit_block_stmt(ns.block);
        self.content.p_r_paren();
        self.content.p_l_paren();
        self.emit_ident(ns.name);
        self.content.p_r_paren();
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

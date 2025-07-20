use std::borrow::Cow;

use super::Emit;
use bolt_ts_ast as ast;

impl<'cx> Emit<'cx> {
    pub(super) fn emit_stmt(&mut self, stmt: &ast::Stmt<'cx>) {
        use bolt_ts_ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.emit_var_stmt(var),
            Expr(expr) => {
                self.emit_leading_comments(expr.span);
                self.emit_expr(expr.expr);
                self.content.p_semi();
            }
            Fn(f) => self.emit_fn_decl(f),
            If(stmt) => self.emit_if_stmt(stmt),
            Block(block) => self.emit_block_stmt(block),
            Ret(ret) => self.emit_ret_stmt(ret),
            Class(class) => self.emit_class_decl(class),
            Throw(t) => self.emit_throw_stmt(t),
            Module(ns) => self.emit_module_decl(ns),
            Enum(e) => self.emit_enum_decl(e),
            Import(n) => self.emit_import_decl(n),
            Export(n) => self.emit_export_decl(n),
            For(n) => self.emit_for_stmt(n),
            ForOf(n) => self.emit_for_of_stmt(n),
            ForIn(n) => self.emit_for_in_stmt(n),
            Break(n) => self.emit_break_stmt(n),
            Continue(n) => self.emit_continue_stmt(n),
            Try(n) => self.emit_try_stmt(n),
            While(n) => self.emit_while_stmt(n),
            Do(n) => self.emit_do_stmt(n),
            Interface(_) => {}
            TypeAlias(_) => {}
            Empty(_) => {}
            Debugger(_) => self.content.p("debugger"),
            ExportAssign(n) => {
                self.emit_leading_comments(n.span);

                self.content.p("export default");
                self.emit_expr(n.expr);
                self.content.p_semi();
            }
            Labeled(n) => {
                self.emit_leading_comments(n.span);

                self.emit_ident(n.label);
                self.content.p_colon();
                self.content.p_whitespace();
                self.emit_stmt(n.stmt);
            }
        }
    }

    fn emit_while_stmt(&mut self, n: &'cx ast::WhileStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("while");
        self.content.p_whitespace();

        self.content.p_l_paren();
        self.emit_expr(n.expr);
        self.content.p_r_paren();

        self.content.p_whitespace();
        self.emit_stmt(n.stmt);
    }

    fn emit_do_stmt(&mut self, n: &'cx ast::DoStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("do");
        self.content.p_whitespace();
        self.emit_stmt(n.stmt);
        self.content.p_whitespace();
        self.content.p("while");
        self.content.p_whitespace();
        self.content.p_l_paren();
        self.emit_expr(n.expr);
        self.content.p_r_paren();
    }

    fn emit_try_stmt(&mut self, n: &'cx ast::TryStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("try");
        self.content.p_whitespace();
        self.emit_block_stmt(n.try_block);
        if let Some(catch) = n.catch_clause {
            self.content.p_whitespace();
            self.emit_catch_block(catch);
        }
        if let Some(finally) = n.finally_block {
            self.content.p("finally");
            self.content.p_whitespace();
            self.emit_block_stmt(finally);
        }
    }

    fn emit_catch_block(&mut self, n: &'cx ast::CatchClause<'cx>) {
        self.content.p("catch");
        self.content.p_whitespace();
        if let Some(var) = n.var {
            self.content.p("(");
            self.emit_var_decl(var);
            self.content.p(")");
        }
        self.content.p_whitespace();
        self.emit_block_stmt(n.block);
    }

    fn emit_break_stmt(&mut self, n: &'cx ast::BreakStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("break");
        if let Some(label) = n.label {
            self.content.p_whitespace();
            self.emit_ident(label);
        }
        self.content.p_semi();
    }

    fn emit_continue_stmt(&mut self, n: &'cx ast::ContinueStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("continue");
        if let Some(label) = n.label {
            self.content.p_whitespace();
            self.emit_ident(label);
        }
        self.content.p_semi();
    }

    fn emit_for_init(&mut self, n: &'cx ast::ForInitKind<'cx>) {
        use bolt_ts_ast::ForInitKind::*;
        match n {
            Var(decls) => {
                self.content.p("var");
                self.content.p_whitespace();
                self.emit_var_decls(decls)
            }
            Expr(expr) => self.emit_expr(expr),
        }
    }

    fn emit_for_stmt(&mut self, n: &'cx ast::ForStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("for");
        self.content.p_whitespace();
        self.content.p("(");
        self.content.p_whitespace();
        if let Some(init) = &n.init {
            self.emit_for_init(init);
        }
        self.content.p_semi();
        self.content.p_whitespace();
        if let Some(cond) = n.cond {
            self.emit_expr(cond);
        }
        self.content.p_semi();
        self.content.p_whitespace();
        if let Some(incr) = n.incr {
            self.emit_expr(incr);
        }
        self.content.p(")");
        self.content.p_whitespace();
        self.emit_stmt(n.body);
    }

    fn emit_for_in_stmt(&mut self, n: &'cx ast::ForInStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("for");
        self.content.p_whitespace();
        self.content.p("(");
        self.content.p_whitespace();
        self.emit_for_init(&n.init);
        self.content.p_whitespace();
        self.content.p("in");
        self.content.p_whitespace();
        self.emit_expr(n.expr);
        self.content.p(")");
        self.content.p_whitespace();
        self.emit_stmt(n.body);
    }

    fn emit_for_of_stmt(&mut self, n: &'cx ast::ForOfStmt<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("for");
        self.content.p_whitespace();
        if n.r#await.is_some() {
            self.content.p("await");
            self.content.p_whitespace();
        }
        self.content.p("(");
        self.content.p_whitespace();
        self.emit_for_init(&n.init);
        self.content.p_whitespace();
        self.content.p("of");
        self.content.p_whitespace();
        self.emit_expr(n.expr);
        self.content.p(")");
        self.content.p_whitespace();
        self.emit_stmt(n.body);
    }

    fn emit_export_spec(&mut self, spec: &'cx ast::ExportSpec<'cx>) {
        use bolt_ts_ast::ExportSpecKind::*;
        match spec.kind {
            Shorthand(n) => self.emit_shorthand_spec(n),
            Named(n) => self.emit_export_named_spec(n),
        }
    }

    fn emit_export_named_spec(&mut self, n: &'cx ast::ExportNamedSpec<'cx>) {
        self.emit_module_export_name(n.prop_name);
        self.content.p_whitespace();
        self.content.p("as");
        self.content.p_whitespace();
        self.emit_module_export_name(n.name);
    }

    fn emit_export_decl(&mut self, n: &'cx ast::ExportDecl<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("export");
        self.content.p_whitespace();
        match n.clause.kind {
            ast::ExportClauseKind::Specs(specs) => {
                self.content.p("{");
                self.content.p_whitespace();
                self.emit_list(
                    specs.list,
                    |this, spec| this.emit_export_spec(spec),
                    |this, _| {
                        this.content.p_comma();
                        this.content.p_whitespace();
                    },
                );
                self.content.p_whitespace();
                self.content.p("}");
                if let Some(module) = specs.module {
                    self.content.p_whitespace();
                    self.content.p("from");
                    self.content.p_whitespace();
                    self.emit_as_string(module.val);
                }
            }
            ast::ExportClauseKind::Ns(n) => self.emit_ns_export(n),
            ast::ExportClauseKind::Glob(n) => {
                self.content.p("*");
                self.content.p_whitespace();
                self.content.p("from");
                self.content.p_whitespace();
                self.emit_as_string(n.module.val);
            }
        }
    }

    fn emit_ns_export(&mut self, ns: &'cx ast::NsExport) {
        self.content.p("*");
        self.content.p_whitespace();
        self.content.p("as");
        self.content.p_whitespace();
        self.emit_module_export_name(ns.name);
        self.content.p_whitespace();
        self.content.p("from");
        self.content.p_whitespace();
        self.emit_as_string(ns.module.val);
    }

    fn emit_ns_import(&mut self, ns: &'cx ast::NsImport) {
        self.content.p_asterisk();
        self.content.p_whitespace();
        self.content.p("as");
        self.content.p_whitespace();
        self.emit_ident(ns.name);
    }

    fn emit_shorthand_spec(&mut self, n: &'cx ast::ShorthandSpec<'cx>) {
        self.emit_ident(n.name);
    }

    fn emit_module_export_name(&mut self, n: &'cx ast::ModuleExportName<'cx>) {
        use bolt_ts_ast::ModuleExportNameKind::*;
        match n.kind {
            Ident(ident) => self.emit_ident(ident),
            StringLit(lit) => self.emit_string_lit(lit),
        }
    }

    fn emit_import_spec(&mut self, spec: &'cx ast::ImportSpec<'cx>) {
        use bolt_ts_ast::ImportSpecKind::*;
        match spec.kind {
            Shorthand(n) => self.emit_shorthand_spec(n),
            Named(n) => {
                self.emit_module_export_name(n.prop_name);
                self.content.p_whitespace();
                self.content.p("as");
                self.content.p_whitespace();
                self.emit_ident(n.name);
            }
        }
    }

    fn emit_import_clause(&mut self, clause: &'cx ast::ImportClause<'cx>) {
        if let Some(name) = clause.name {
            self.emit_ident(name);
            self.content.p_whitespace();
        } else if let Some(kind) = clause.kind {
            match kind {
                ast::ImportClauseKind::Specs(specs) => {
                    self.emit_list(
                        specs,
                        |this, spec| this.emit_import_spec(spec),
                        |this, _| {
                            this.content.p_comma();
                            this.content.p_whitespace();
                        },
                    );
                }
                ast::ImportClauseKind::Ns(ns) => self.emit_ns_import(ns),
            }
        }
    }

    fn emit_import_decl(&mut self, n: &'cx ast::ImportDecl<'cx>) {
        self.emit_leading_comments(n.span);

        self.content.p("import");
        self.content.p_whitespace();
        self.emit_import_clause(n.clause);
        self.content.p_whitespace();
        self.content.p("from");
        self.content.p_whitespace();
        self.emit_as_string(n.module.val);
    }

    fn emit_throw_stmt(&mut self, t: &'cx ast::ThrowStmt<'cx>) {
        self.emit_leading_comments(t.span);
        self.content.p("throw");
        self.content.p_whitespace();
        self.emit_expr(t.expr);
    }

    fn emit_enum_decl(&mut self, e: &'cx ast::EnumDecl) {
        self.emit_leading_comments(e.span);

        if e.modifiers
            .map(|ms| ms.flags.contains(ast::ModifierKind::Ambient))
            .unwrap_or_default()
        {
            return;
        }
        self.emit_with_var_fn_wrapper(e.name, self.atoms.get(e.name.name), |this| {
            for member in e.members {
                this.content.p_newline();
                this.emit_ident(e.name);
                this.content.p_l_bracket();
                this.emit_ident(e.name);
                this.content.p_l_bracket();
                match member.name.kind {
                    ast::PropNameKind::Ident(ident) => this.emit_as_string(ident.name),
                    ast::PropNameKind::StringLit { raw, .. } => this.emit_as_string(raw.val),
                    ast::PropNameKind::NumLit(node) => this.emit_num_lit(node),
                    ast::PropNameKind::Computed(_) => todo!(),
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
                    ast::PropNameKind::StringLit { raw, .. } => this.emit_as_string(raw.val),
                    ast::PropNameKind::NumLit(num) => this.emit_num_lit(num),
                    ast::PropNameKind::Computed(_) => todo!(),
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
        if self.ns_names.insert((self.scope, decl_name.name)) {
            self.content.p("var");
            self.content.p_whitespace();
            self.emit_ident(decl_name);
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.content.p("{}");
            self.content.p_semi();
        }

        self.content.p_newline();

        self.content.p_l_paren();
        self.content.p("function");
        self.content.p_whitespace();
        self.content.p_l_paren();

        self.content.p(param_name);
        self.content.p_r_paren();
        self.content.p_whitespace();

        // emit block
        self.content.p_l_brace();
        self.content.p_newline();
        self.content.indent += self.options.indent;
        let old = self.scope;
        self.scope = self.next_scope();

        f(self);

        self.scope = old;
        self.content.indent -= self.options.indent;
        self.content.p_newline();
        self.content.p_r_brace();

        self.content.p_r_paren();
        self.content.p_l_paren();
        self.emit_ident(decl_name);
        self.content.p_r_paren();
        self.content.p_semi();
    }

    fn emit_object_binding_elem(&mut self, elem: &'cx ast::ObjectBindingElem<'cx>) {
        if elem.dotdotdot.is_some() {
            self.content.p_dot_dot_dot();
        }
        use bolt_ts_ast::ObjectBindingName::*;
        match elem.name {
            Shorthand(ident) => {
                self.emit_ident(ident);
            }
            Prop { prop_name, name } => {
                self.emit_prop_name(prop_name);
                self.content.p(":");
                self.content.p_whitespace();
                self.emit_binding(name);
            }
        }
        if let Some(init) = elem.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }

    fn emit_array_binding_elem(&mut self, elem: &'cx ast::ArrayBindingElem<'cx>) {
        use bolt_ts_ast::ArrayBindingElemKind::*;
        match elem.kind {
            Omit(_) => {}
            Binding {
                dotdotdot,
                name,
                init,
            } => {
                if dotdotdot.is_some() {
                    self.content.p_dot_dot_dot();
                }
                self.emit_binding(name);
                if let Some(init) = init {
                    self.content.p_whitespace();
                    self.content.p_eq();
                    self.content.p_whitespace();
                    self.emit_expr(init);
                }
            }
        }
    }

    pub(super) fn emit_binding(&mut self, binding: &'cx ast::Binding<'cx>) {
        match binding.kind {
            ast::BindingKind::Ident(n) => self.emit_ident(n),
            ast::BindingKind::ObjectPat(n) => {
                self.content.p_l_brace();
                self.emit_list(
                    n.elems,
                    |this, item| this.emit_object_binding_elem(item),
                    |this, _| {
                        this.content.p_comma();
                        this.content.p_whitespace();
                    },
                );
                self.content.p_r_brace();
            }
            bolt_ts_ast::BindingKind::ArrayPat(n) => {
                self.content.p_l_bracket();
                self.emit_list(
                    n.elems,
                    |this, item| {
                        this.emit_array_binding_elem(item);
                    },
                    |this, _| {
                        this.content.p_comma();
                        this.content.p_whitespace();
                    },
                );
                self.content.p_r_bracket();
            }
        };
    }

    fn emit_module_decl(&mut self, ns: &'cx ast::ModuleDecl) {
        self.emit_leading_comments(ns.span);

        if ns
            .modifiers
            .map(|ms| ms.flags.contains(ast::ModifierKind::Ambient))
            .unwrap_or_default()
        {
            return;
        }
        let Some(block) = ns.block else {
            return;
        };

        // var name
        fn sub_names_of_binding<'cx>(binding: &'cx ast::Binding<'cx>) -> Vec<bolt_ts_atom::AtomId> {
            use bolt_ts_ast::BindingKind::*;
            match binding.kind {
                Ident(n) => vec![n.name],
                ObjectPat(n) => n
                    .elems
                    .iter()
                    .flat_map(|elem| {
                        use bolt_ts_ast::ObjectBindingName::*;
                        match elem.name {
                            Shorthand(ident) => vec![ident.name],
                            Prop { name, .. } => sub_names_of_binding(name),
                        }
                    })
                    .collect(),
                ArrayPat(_) => todo!(),
            }
        }

        let mut sub_names = block
            .stmts
            .iter()
            .filter_map(|stmt| match stmt.kind {
                ast::StmtKind::Var(v) => Some(
                    v.list
                        .iter()
                        .flat_map(|item| sub_names_of_binding(item.binding))
                        .collect::<Vec<_>>(),
                ),
                ast::StmtKind::Class(c) => Some(vec![c.name.unwrap().name]),
                ast::StmtKind::Fn(f) => Some(vec![f.name.name]),
                _ => None,
            })
            .flatten()
            .map(|name| self.atoms.get(name))
            .collect::<Vec<_>>();
        sub_names.sort();

        let ident = match ns.name {
            ast::ModuleName::Ident(ident) => ident,
            ast::ModuleName::StringLit(_) => unreachable!(),
        };
        let mut param_name = Cow::Borrowed(self.atoms.get(ident.name));
        if let Some(i) = sub_names.iter().position(|sub| *sub == param_name) {
            let mut offset = 1;
            let mut n = format!("{param_name}_{offset}");
            for sub in &sub_names[i + 1..] {
                if n == *sub {
                    offset += 1;
                    n = format!("{n}_{offset}");
                } else {
                    break;
                }
            }
            param_name = Cow::Owned(n);
        }

        self.emit_with_var_fn_wrapper(ident, &param_name, |this| {
            for stmt in block.stmts {
                use bolt_ts_ast::StmtKind::*;
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
                                this.emit_binding(item.binding);
                                this.content.p_whitespace();
                                this.content.p_eq();
                                this.content.p_whitespace();
                                // TODO: fix
                                this.emit_binding(item.binding);
                                this.content.p_newline();
                            }
                        }
                        continue;
                    }
                    Fn(f) => f.modifiers.map(|ms| (ms, f.name)),
                    Class(c) => c.modifiers.map(|ms| (ms, c.name.unwrap())),
                    Interface(_) | TypeAlias(_) => None,
                    Module(n) => n.modifiers.map(|ms| {
                        let ident = match n.name {
                            ast::ModuleName::Ident(ident) => ident,
                            ast::ModuleName::StringLit(_) => unreachable!(),
                        };
                        (ms, ident)
                    }),
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
                    this.content.p_semi();
                    this.content.p_newline();
                }
            }
        });
    }

    fn emit_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.emit_leading_comments(class.span);
        self.emit_class_like(class);
    }

    fn emit_ret_stmt(&mut self, ret: &'cx ast::RetStmt<'cx>) {
        self.emit_leading_comments(ret.span);
        self.content.p("return");
        self.content.p_whitespace();
        if let Some(expr) = ret.expr {
            self.emit_expr(expr);
        }
    }

    fn emit_if_stmt(&mut self, stmt: &'cx ast::IfStmt<'cx>) {
        self.emit_leading_comments(stmt.span);
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
            },
        )
    }

    fn emit_fn_decl(&mut self, f: &'cx ast::FnDecl) {
        self.emit_leading_comments(f.span);
        if let Some(body) = f.body {
            self.content.p("function");
            self.content.p_whitespace();
            self.emit_ident(f.name);
            self.emit_params(f.params);
            self.content.p_whitespace();
            self.emit_block_stmt(body);
        }
    }

    fn emit_var_stmt(&mut self, var: &'cx ast::VarStmt<'cx>) {
        self.emit_leading_comments(var.span);
        self.content.p("var");
        self.content.p_whitespace();
        self.emit_var_decls(var.list);
        self.content.p_semi();
    }

    fn emit_var_decls(&mut self, decls: ast::VarDecls<'cx>) {
        self.emit_list(
            decls,
            |this, decl| this.emit_var_decl(decl),
            |this, _| {
                this.content.p_comma();
                this.content.p_whitespace();
            },
        );
    }

    fn emit_var_decl(&mut self, decl: &'cx ast::VarDecl) {
        self.emit_binding(decl.binding);
        if let Some(init) = decl.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }
}

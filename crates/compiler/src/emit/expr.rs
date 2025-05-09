use super::Emit;
use crate::ensure_sufficient_stack;
use bolt_ts_ast as ast;

impl<'cx> Emit<'cx> {
    pub(super) fn emit_expr(&mut self, expr: &'cx ast::Expr<'cx>) {
        use bolt_ts_ast::ExprKind::*;
        match expr.kind {
            Bin(bin) => self.emit_bin_expr(bin),
            BoolLit(bool) => self.content.p(&bool.val.to_string()),
            NumLit(num) => self.emit_num_lit(num),
            StringLit(s) => {
                self.content.p("\"");
                self.content.p(self.atoms.get(s.val));
                self.content.p("\"");
            }
            NoSubstitutionTemplateLit(n) => {
                self.content.p("`");
                self.content.p(self.atoms.get(n.val));
                self.content.p("`");
            }
            NullLit(_) => self.content.p("null"),
            Ident(ident) => self.emit_ident(ident),
            ArrayLit(lit) => self.emit_array_lit(lit),
            Omit(_) => {}
            Paren(p) => {
                self.content.p_l_paren();
                self.emit_expr(p.expr);
                self.content.p_r_paren();
            }
            Cond(cond) => {
                self.emit_expr(cond.cond);
                self.content.p_whitespace();
                self.content.p_question();
                self.content.p_whitespace();
                self.emit_expr(cond.when_true);
                self.content.p_whitespace();
                self.content.p_colon();
                self.content.p_whitespace();
                self.emit_expr(cond.when_false);
            }
            ObjectLit(lit) => self.emit_object_lit(lit),
            Call(call) => self.emit_call_expr(call),
            Fn(f) => self.emit_fn_expr(f),
            New(new) => self.emit_new_expr(new),
            Assign(assign) => self.emit_assign_expr(assign),
            ArrowFn(arrow_fn) => self.emit_arrow_fn(arrow_fn),
            PrefixUnary(unary) => {
                self.content.p(unary.op.as_str());
                if let PrefixUnary(_) = unary.expr.kind {
                    self.content.p_whitespace()
                };
                self.emit_expr(unary.expr);
            }
            PostfixUnary(unary) => {
                self.emit_expr(unary.expr);
                self.content.p(unary.op.as_str());
            }
            Class(class) => self.emit_class_like(class),
            PropAccess(prop) => {
                if let bolt_ts_ast::ExprKind::NumLit(lit) = prop.expr.kind {
                    self.emit_num_lit(lit);
                    self.content.p(".");
                } else {
                    self.emit_expr(prop.expr);
                }
                self.content.p_dot();
                self.emit_ident(prop.name);
            }
            EleAccess(prop) => {
                self.emit_expr(prop.expr);
                self.content.p_l_bracket();
                self.emit_expr(prop.arg);
                self.content.p_r_bracket();
            }
            This(_) => {
                self.content.p("this");
            }
            Typeof(n) => {
                self.content.p("typeof");
                self.content.p_whitespace();
                self.emit_expr(n.expr);
            }
            Void(n) => {
                self.content.p("void");
                self.content.p_whitespace();
                self.emit_expr(n.expr);
            }
            Super(_) => self.content.p("super"),
            As(n) => {
                self.emit_expr(n.expr);
            }
            Satisfies(n) => {
                self.emit_expr(n.expr);
            }
            NonNull(n) => {
                self.emit_expr(n.expr);
            }
            Template(n) => {
                self.emit_template_expr(n);
            }
            BigIntLit(lit) => {
                if lit.val.0 {
                    self.content.p("-");
                }
                self.content.p(self.atoms.get(lit.val.1));
                self.content.p("n");
            }
            TyAssertion(n) => {
                if matches!(n.expr.kind, ast::ExprKind::ObjectLit(_)) && {
                    let mut p = n.id;
                    loop {
                        let n = self.p.parent(p).unwrap();
                        let p_n = self.p.node(p);
                        if p_n.is_ty_assertion() {
                            p = n;
                            continue;
                        } else if p_n.is_expr_stmt() || p_n.is_arrow_fn_expr() {
                            break true;
                        } else {
                            break false;
                        }
                    }
                } {
                    self.content.p_l_paren();
                    self.emit_expr(n.expr);
                    self.content.p_r_paren();
                } else {
                    self.emit_expr(n.expr);
                }
            }
            ExprWithTyArgs(n) => {
                self.emit_expr(n.expr);
            }
            SpreadElement(n) => {
                self.content.p("...");
                self.emit_expr(n.expr);
            }
            RegExpLit(n) => {
                self.content.p(self.atoms.get(n.val));
            }
            TaggedTemplate(n) => {
                self.emit_expr(n.tag);
                self.emit_expr(n.tpl);
            }
        };
    }

    fn emit_template_expr(&mut self, n: &'cx ast::TemplateExpr<'cx>) {
        self.content.p("`");
        self.content.p(self.atoms.get(n.head.text));
        for span in n.spans {
            self.content.p("${");
            self.emit_expr(span.expr);
            self.content.p("}");
            self.content.p(self.atoms.get(span.text));
        }
        self.content.p("`");
    }

    fn emit_arrow_fn(&mut self, f: &'cx ast::ArrowFnExpr<'cx>) {
        self.emit_params(f.params);
        self.content.p_whitespace();
        self.content.p("=>");
        self.content.p_whitespace();
        match f.body {
            ast::ArrowFnExprBody::Block(block) => self.emit_block_stmt(block),
            ast::ArrowFnExprBody::Expr(expr) => {
                self.emit_expr(expr);
            }
        };
    }

    fn emit_assign_expr(&mut self, assign: &'cx ast::AssignExpr) {
        self.emit_expr(assign.left);
        self.content.p_whitespace();
        self.content.p(assign.op.as_str());
        self.content.p_whitespace();
        self.emit_expr(assign.right);
    }

    fn emit_new_expr(&mut self, new: &'cx ast::NewExpr) {
        self.content.p("new");
        self.content.p_whitespace();
        self.emit_expr(new.expr);
        if let Some(args) = new.args {
            self.emit_args(args);
        }
    }

    fn emit_fn_expr(&mut self, f: &'cx ast::FnExpr) {
        self.content.p("function");
        self.content.p_whitespace();
        if let Some(name) = f.name {
            self.emit_ident(name);
        }
        self.emit_params(f.params);
        self.content.p_whitespace();
        self.emit_block_stmt(f.body);
    }

    fn emit_bin_expr(&mut self, bin_op: &'cx ast::BinExpr) {
        ensure_sufficient_stack(|| {
            self.emit_expr(bin_op.left);
            self.content.p_whitespace();
            self.content.p(bin_op.op.kind.as_str());
            self.content.p_whitespace();
            self.emit_expr(bin_op.right);
        })
    }

    fn emit_array_lit(&mut self, lit: &'cx ast::ArrayLit) {
        self.content.p_l_bracket();
        for (idx, expr) in lit.elems.iter().enumerate() {
            self.emit_expr(expr);
            if idx != lit.elems.len() - 1 {
                self.content.p_comma();
                self.content.p_whitespace();
            }
        }
        self.content.p_r_bracket();
    }

    fn emit_args(&mut self, args: ast::Exprs<'cx>) {
        self.content.p_l_paren();
        self.emit_list(
            args,
            |this, arg| this.emit_expr(arg),
            |this, _| {
                this.content.p_comma();
                this.content.p_whitespace();
            },
        );
        self.content.p_r_paren();
    }

    fn emit_call_expr(&mut self, call: &'cx ast::CallExpr) {
        self.emit_expr(call.expr);
        self.emit_args(call.args);
    }

    fn emit_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) {
        self.content.p_l_brace();
        for (idx, field) in lit.members.iter().enumerate() {
            self.emit_object_member(field);
            if idx != lit.members.len() - 1 {
                self.content.p_comma();
                self.content.p_newline();
            }
        }
        self.content.p_r_brace();
    }

    fn emit_object_member(&mut self, field: &'cx ast::ObjectMember<'cx>) {
        use bolt_ts_ast::ObjectMemberKind::*;
        match field.kind {
            Prop(n) => self.emit_object_prop_member(n),
            Shorthand(n) => self.emit_object_shorthand_member(n),
            Method(n) => self.emit_object_method_member(n),
            SpreadAssignment(n) => {
                self.content.p("...");
                self.emit_expr(n.expr);
            }
        }
    }

    fn emit_object_method_member(&mut self, method: &'cx ast::ObjectMethodMember<'cx>) {
        self.emit_prop_name(method.name);
        self.emit_params(method.params);
        self.content.p_whitespace();
        self.emit_block_stmt(method.body);
    }

    fn emit_object_prop_member(&mut self, prop: &'cx ast::ObjectPropMember<'cx>) {
        self.emit_prop_name(prop.name);
        self.content.p_colon();
        self.content.p_whitespace();
        self.emit_expr(prop.init);
    }

    fn emit_object_shorthand_member(&mut self, shorthand: &'cx ast::ObjectShorthandMember<'cx>) {
        self.emit_ident(shorthand.name);
    }
}

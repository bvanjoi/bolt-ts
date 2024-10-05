use crate::ast;
use crate::atoms::AtomMap;

struct PPrint(String);

impl PPrint {
    fn p(&mut self, content: &str) {
        self.0 += content
    }
    fn p_whitespace(&mut self) {
        self.p(" ");
    }
    fn p_question(&mut self) {
        self.p("?");
    }
    fn p_newline(&mut self) {
        self.p("\n");
    }
    fn p_eq(&mut self) {
        self.p("=");
    }
    fn p_semi(&mut self) {
        self.p(";");
    }
    fn p_colon(&mut self) {
        self.p(":")
    }
    fn p_comma(&mut self) {
        self.p(",");
    }
    fn p_l_bracket(&mut self) {
        self.p("[");
    }
    fn p_r_bracket(&mut self) {
        self.p("]");
    }
    fn p_l_paren(&mut self) {
        self.p("(");
    }
    fn p_r_paren(&mut self) {
        self.p(")");
    }
    fn p_l_brace(&mut self) {
        self.p("{");
    }
    fn p_r_brace(&mut self) {
        self.p("}");
    }
}

pub struct Emit<'cx> {
    pub atoms: &'cx AtomMap<'cx>,
    content: PPrint,
}

impl<'cx> Emit<'cx> {
    pub fn new(atoms: &'cx AtomMap) -> Self {
        Self {
            atoms,
            content: PPrint(String::with_capacity(1024 * 128)),
        }
    }

    pub fn emit(&mut self, root: &'cx ast::Program) -> String {
        self.emit_program(root);
        std::mem::take(&mut self.content.0)
    }

    fn emit_program(&mut self, p: &'cx ast::Program) {
        self.emit_stmts(p.stmts);
    }

    fn emit_stmt(&mut self, stmt: &'cx ast::Stmt<'cx>) {
        use ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.emit_var_stmt(var),
            Expr(expr) => self.emit_expr(expr),
            Fn(f) => self.emit_fn(f),
            If(stmt) => self.emit_if(stmt),
            Block(block) => self.emit_stmts(block),
            Return(ret) => self.emit_return(ret),
            Empty(_) => {},
        }
    }

    fn emit_return(&mut self, ret: &'cx ast::RetStmt<'cx>) {
        self.content.p("return");
        self.content.p_whitespace();
        if let Some(expr) = ret.expr {
            self.emit_expr(expr);
        }
    }

    fn emit_if(&mut self, stmt: &'cx ast::IfStmt<'cx>) {
        self.content.p("if");
        self.content.p_whitespace();
        self.content.p_l_paren();
        self.emit_expr(stmt.expr);
        self.content.p_r_paren();
        self.content.p_whitespace();
        self.content.p_l_brace();
        self.emit_stmt(stmt.then);
        self.content.p_r_brace();
        if let Some(else_then) = stmt.else_then {
            self.content.p_whitespace();
            self.content.p("else");
            self.content.p_whitespace();
            self.emit_stmt(else_then);
        }
        self.content.p_newline();
    }

    fn emit_stmts(&mut self, body: ast::Stmts<'cx>) {
        self.emit_list(
            body,
            |this, item| this.emit_stmt(item),
            |this| this.content.p_newline(),
        )
    }

    fn emit_list<T>(
        &mut self,
        list: &'cx [T],
        emit_item: impl Fn(&mut Self, &'cx T),
        emit_sep: impl Fn(&mut Self),
    ) {
        for (idx, item) in list.iter().enumerate() {
            emit_item(self, item);
            if idx != list.len() {
                emit_sep(self)
            }
        }
    }

    fn emit_fn(&mut self, f: &'cx ast::FnDecl) {
        self.content.p("function");
        self.content.p_whitespace();
        self.emit_ident(f.name);
        self.emit_params(f.params);
        self.content.p_whitespace();
        self.emit_fn_body(f.body);
    }

    fn emit_fn_body(&mut self, body: ast::Stmts<'cx>) {
        self.content.p_l_brace();
        self.content.p_newline();
        self.emit_stmts(body);
        self.content.p_newline();
        self.content.p_r_brace();
    }

    fn emit_params(&mut self, params: ast::ParamsDecl<'cx>) {
        self.content.p_l_paren();
        self.emit_list(
            params,
            |this, item| this.emit_param(item),
            |this| {
                this.content.p_comma();
                this.content.p_whitespace();
            },
        );
        self.content.p_r_paren();
    }

    fn emit_param(&mut self, param: &'cx ast::ParamDecl<'cx>) {}

    fn emit_ident(&mut self, ident: &'cx ast::Ident) {
        self.content.p(self.atoms.get(ident.name));
    }

    fn emit_expr(&mut self, expr: &'cx ast::Expr<'cx>) {
        use ast::ExprKind::*;
        match expr.kind {
            BinOp(bin) => self.emit_bin_op(bin),
            BoolLit(bool) => self.content.p(&bool.val.to_string()),
            NumLit(num) => self.content.p(&num.val.to_string()),
            StringLit(s) => {
                self.content.p("\"");
                self.content.p(self.atoms.get(s.val));
                self.content.p("\"");
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
        }
    }

    fn emit_call_expr(&mut self, call: &'cx ast::CallExpr) {
        self.emit_expr(call.expr);
        self.content.p_l_paren();
        self.emit_list(
            call.args,
            |this, arg| this.emit_expr(arg),
            |this| {
                this.content.p_comma();
                this.content.p_whitespace();
            },
        );
        self.content.p_r_paren();
    }

    fn emit_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) {
        self.content.p_l_brace();
        for (idx, field) in lit.members.iter().enumerate() {
            self.emit_object_member_field(field);
            if idx != lit.members.len() - 1 {
                self.content.p_comma();
                self.content.p_newline();
            }
        }
        self.content.p_r_brace();
    }

    fn emit_object_member_field(&mut self, field: &'cx ast::ObjectMemberField<'cx>) {
        self.emit_prop_name(field.name);
        self.content.p_colon();
        self.content.p_whitespace();
        self.emit_expr(field.value);
    }

    fn emit_prop_name(&mut self, name: &'cx ast::PropName) {
        use ast::PropNameKind::*;
        match name.kind {
            Ident(ident) => self.emit_ident(ident),
        }
    }

    fn emit_array_lit(&mut self, lit: &'cx ast::ArrayLit) {
        self.content.p_l_bracket();
        for (idx, expr) in lit.elems.iter().enumerate() {
            self.emit_expr(&expr);
            if idx != lit.elems.len() - 1 {
                self.content.p_comma();
                self.content.p_whitespace();
            }
        }
        self.content.p_r_bracket();
    }

    fn emit_bin_op(&mut self, bin_op: &'cx ast::BinExpr) {
        self.emit_expr(bin_op.left);
        self.content.p_whitespace();
        self.content.p(bin_op.op.kind.as_str());
        self.content.p_whitespace();
        self.emit_expr(bin_op.right);
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
        self.content.p(self.atoms.get(decl.name.name));
        if let Some(init) = decl.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }
}

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

    pub fn emit(&mut self, root: &ast::Program) -> String {
        self.emit_program(root);
        std::mem::take(&mut self.content.0)
    }

    fn emit_program(&mut self, p: &ast::Program) {
        for stmt in p.stmts {
            self.emit_stmt(stmt);
            self.content.p_newline();
        }
    }

    fn emit_stmt(&mut self, stmt: &ast::Stmt) {
        use ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.emit_var_stmt(var),
            Expr(expr) => self.emit_expr(expr),
        }
    }

    fn emit_expr(&mut self, expr: &ast::Expr) {
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
            Ident(ident) => {
                self.content.p(self.atoms.get(ident.name));
            }
            ArrayLit(lit) => self.emit_array_lit(lit),
            Omit(_) => {},
            Paren(p) => {
                self.content.p_l_paren();
                self.emit_expr(p.expr);
                self.content.p_r_paren();
            },
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
            },
        }
    }

    fn emit_array_lit(&mut self, lit: &ast::ArrayLit) {
        self.content.p_l_bracket();
        for (idx,expr) in lit.elems.iter().enumerate() {
            self.emit_expr(&expr);
            if idx != lit.elems.len() - 1 {
                self.content.p_comma();
                self.content.p_whitespace();
            }
        }
        self.content.p_r_bracket();
    }

    fn emit_bin_op(&mut self, bin_op: &ast::BinExpr) {
        self.emit_expr(bin_op.left);
        self.content.p_whitespace();
        self.content.p(bin_op.op.kind.as_str());
        self.content.p_whitespace();
        self.emit_expr(bin_op.right);
    }

    fn emit_var_stmt(&mut self, var: &ast::VarStmt) {
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

    fn emit_var_decl(&mut self, decl: &ast::VarDecl) {
        self.content.p(self.atoms.get(decl.name.name));
        if let Some(init) = decl.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }
}

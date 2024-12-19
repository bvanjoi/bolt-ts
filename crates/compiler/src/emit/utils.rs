use crate::ast;

use super::Emit;

impl<'cx> Emit<'cx> {
    pub(super) fn emit_params(&mut self, params: ast::ParamsDecl<'cx>) {
        self.content.p_l_paren();
        self.emit_list(
            params,
            |this, item| this.emit_param(item),
            |this, _| {
                this.content.p_comma();
                this.content.p_whitespace();
            },
        );
        self.content.p_r_paren();
    }

    fn emit_param(&mut self, param: &'cx ast::ParamDecl<'cx>) {
        if param.dotdotdot.is_some() {
            self.content.p_dot_dot_dot();
        }
        self.emit_ident(&param.name);
        if let Some(init) = param.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }

    pub(super) fn emit_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        self.emit_block_like(block);
    }

    pub(super) fn emit_prop_name(&mut self, name: &'cx ast::PropName<'cx>) {
        use ast::PropNameKind::*;
        match name.kind {
            Ident(ident) => self.emit_ident(ident),
            NumLit(num) => self.emit_num_lit(num),
            StringLit(lit) => self.emit_string_lit(lit),
        }
    }

    pub(super) fn emit_num_lit(&mut self, num: &'cx ast::NumLit) {
        self.content.p(&num.val.to_string())
    }
    pub(super) fn emit_string_lit(&mut self, s: &'cx ast::StringLit) {
        self.content.p("'");
        self.content.p(self.atoms.get(s.val));
        self.content.p("'");
    }
}

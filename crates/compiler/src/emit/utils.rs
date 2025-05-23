use bolt_ts_ast as ast;
use bolt_ts_atom::AtomId;

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
        self.emit_binding(param.name);
        if let Some(init) = param.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }

    pub(super) fn emit_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        self.emit_leading_comments(block.span);
        self.emit_block_like(block);
    }

    pub(super) fn emit_prop_name(&mut self, name: &'cx ast::PropName<'cx>) {
        use bolt_ts_ast::PropNameKind::*;
        match name.kind {
            Ident(ident) => self.emit_ident(ident),
            NumLit(num) => self.emit_num_lit(num),
            StringLit { raw, .. } => self.emit_string_lit(raw),
            Computed(_) => todo!(),
        }
    }

    pub(super) fn emit_num_lit(&mut self, num: &'cx ast::NumLit) {
        self.content.p(&num.val.to_string())
    }

    #[inline]
    pub(super) fn emit_string_lit(&mut self, s: &'cx ast::StringLit) {
        self.emit_as_string(s.val);
    }

    pub(super) fn emit_as_string(&mut self, val: AtomId) {
        let s = self.atoms.get(val);
        self.content.p("'");
        for c in s.chars() {
            match c {
                '\'' => self.content.p("\\'"),
                _ => self.content.content.push(c),
            }
        }
        self.content.p("'");
    }
}

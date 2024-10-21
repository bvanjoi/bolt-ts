mod expr;
mod stmt;
mod utils;

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
    /// `[`
    fn p_l_bracket(&mut self) {
        self.p("[");
    }
    /// `]`
    fn p_r_bracket(&mut self) {
        self.p("]");
    }
    /// `(`
    fn p_l_paren(&mut self) {
        self.p("(");
    }
    /// `)`
    fn p_r_paren(&mut self) {
        self.p(")");
    }
    /// `{`
    fn p_l_brace(&mut self) {
        self.p("{");
    }
    /// `}`
    fn p_r_brace(&mut self) {
        self.p("}");
    }
    fn p_dot_dot_dot(&mut self) {
        self.p("...");
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

    fn emit_list<T>(
        &mut self,
        list: &'cx [T],
        emit_item: impl Fn(&mut Self, &'cx T),
        emit_sep: impl Fn(&mut Self),
    ) {
        for (idx, item) in list.iter().enumerate() {
            emit_item(self, item);
            if idx != list.len() - 1 {
                emit_sep(self)
            }
        }
    }

    fn emit_ident(&mut self, ident: &'cx ast::Ident) {
        self.content.p(self.atoms.get(ident.name));
    }
}

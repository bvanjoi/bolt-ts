mod emit_block_like;
mod emit_class_like;
mod expr;
mod stmt;
mod utils;

use crate::ast;
use bolt_ts_atom::AtomMap;

struct PPrint(String);

impl PPrint {
    fn p(&mut self, content: &str) {
        self.0 += content
    }
    fn p_asterisk(&mut self) {
        self.p("*");
    }
    fn p_whitespace(&mut self) {
        self.p(" ");
    }
    fn p_pieces_of_whitespace(&mut self, count: u32) {
        self.p(&" ".repeat(count as usize))
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
    /// `.`
    fn p_dot(&mut self) {
        self.p(".");
    }
    /// `...`
    fn p_dot_dot_dot(&mut self) {
        self.p("...");
    }
}

pub struct EmitterOptions {
    indent: u32,
}

struct EmitterState {
    indent: u32,
}

pub struct Emit<'cx> {
    pub atoms: &'cx AtomMap<'cx>,
    content: PPrint,
    options: EmitterOptions,
    state: EmitterState,
}

impl<'cx> Emit<'cx> {
    pub fn new(atoms: &'cx AtomMap) -> Self {
        Self {
            atoms,
            content: PPrint(String::with_capacity(1024 * 128)),
            options: EmitterOptions { indent: 2 },
            state: EmitterState { indent: 0 },
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
        list: &[T],
        emit_item: impl Fn(&mut Self, &T),
        emit_sep: impl Fn(&mut Self, &T),
    ) {
        for (idx, item) in list.iter().enumerate() {
            emit_item(self, item);
            if idx != list.len() - 1 {
                emit_sep(self, item)
            }
        }
    }

    fn emit_ident(&mut self, ident: &ast::Ident) {
        self.content.p(self.atoms.get(ident.name));
    }
}

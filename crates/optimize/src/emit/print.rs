pub(super) struct PPrint {
    pub(super) content: String,
    pub(super) indent: u32,
}

impl PPrint {
    pub fn new(input_len: usize) -> Self {
        PPrint {
            content: String::with_capacity(input_len * 2),
            indent: 0,
        }
    }
    pub(super) fn p(&mut self, content: &str) {
        self.content += content
    }
    pub(super) fn p_asterisk(&mut self) {
        self.p("*");
    }
    pub(super) fn p_whitespace(&mut self) {
        self.p(" ");
    }
    pub(super) fn p_pieces_of_whitespace(&mut self, count: u32) {
        self.p(&" ".repeat(count as usize))
    }
    pub(super) fn p_question(&mut self) {
        self.p("?");
    }
    pub(super) fn p_newline(&mut self) {
        self.p("\n");
        self.p_pieces_of_whitespace(self.indent);
    }
    pub(super) fn p_eq(&mut self) {
        self.p("=");
    }
    pub(super) fn p_semi(&mut self) {
        self.p(";");
    }
    pub(super) fn p_colon(&mut self) {
        self.p(":")
    }
    pub(super) fn p_comma(&mut self) {
        self.p(",");
    }
    /// `[`
    pub(super) fn p_l_bracket(&mut self) {
        self.p("[");
    }
    /// `]`
    pub(super) fn p_r_bracket(&mut self) {
        self.p("]");
    }
    /// `(`
    pub(super) fn p_l_paren(&mut self) {
        self.p("(");
    }
    /// `)`
    pub(super) fn p_r_paren(&mut self) {
        self.p(")");
    }
    /// `{`
    pub(super) fn p_l_brace(&mut self) {
        self.p("{");
    }
    /// `}`
    pub(super) fn p_r_brace(&mut self) {
        self.p("}");
    }
    /// `.`
    pub(super) fn p_dot(&mut self) {
        self.p(".");
    }
    /// `...`
    pub(super) fn p_dot_dot_dot(&mut self) {
        self.p("...");
    }
}

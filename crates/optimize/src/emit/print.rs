pub struct PPrint {
    pub(super) content: String,
    pub(super) indent: u32,
}

impl PPrint {
    pub fn take_content(&mut self) -> String {
        std::mem::take(&mut self.content)
    }

    pub fn new(input_len: usize) -> Self {
        PPrint {
            content: String::with_capacity(input_len * 2),
            indent: 0,
        }
    }
    pub fn p(&mut self, content: &str) {
        self.content += content
    }
    pub fn p_asterisk(&mut self) {
        self.p("*");
    }
    /// `=>`
    pub fn p_arrow_right(&mut self) {
        self.p("=>");
    }
    pub fn p_whitespace(&mut self) {
        self.p(" ");
    }
    pub fn p_pieces_of_whitespace(&mut self, count: u32) {
        self.p(&" ".repeat(count as usize))
    }
    pub fn p_question(&mut self) {
        self.p("?");
    }
    pub fn p_newline(&mut self) {
        self.p("\n");
        self.p_pieces_of_whitespace(self.indent);
    }
    pub fn p_eq(&mut self) {
        self.p("=");
    }
    /// `;`
    pub fn p_semi(&mut self) {
        self.p(";");
    }
    /// `:`
    pub fn p_colon(&mut self) {
        self.p(":")
    }
    /// `,`
    pub fn p_comma(&mut self) {
        self.p(",");
    }
    /// `[`
    pub fn p_l_bracket(&mut self) {
        self.p("[");
    }
    /// `]`
    pub fn p_r_bracket(&mut self) {
        self.p("]");
    }
    /// `(`
    pub fn p_l_paren(&mut self) {
        self.p("(");
    }
    /// `)`
    pub fn p_r_paren(&mut self) {
        self.p(")");
    }
    /// `{`
    pub fn p_l_brace(&mut self) {
        self.p("{");
    }
    /// `}`
    pub fn p_r_brace(&mut self) {
        self.p("}");
    }
    /// `.`
    pub fn p_dot(&mut self) {
        self.p(".");
    }
    /// `...`
    pub fn p_dot_dot_dot(&mut self) {
        self.p("...");
    }
    /// `<`
    pub fn p_less(&mut self) {
        self.p("<")
    }
    /// `>`
    pub fn p_great(&mut self) {
        self.p(">")
    }
    /// `|`
    pub fn p_pipe(&mut self) {
        self.p("|")
    }
}

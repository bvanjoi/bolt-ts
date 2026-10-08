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
    pub fn p_whitespace(&mut self) {
        self.p(" ");
    }
    pub fn p_pieces_of_whitespace(&mut self, count: u32) {
        for _ in 0..count {
            self.p_whitespace();
        }
    }
    pub fn p_newline(&mut self) {
        self.p("\n");
        self.p_pieces_of_whitespace(self.indent);
    }
    pub fn p_token(&mut self, token: bolt_ts_ast::TokenKind) {
        self.p(token.as_str());
    }
    pub fn p_assign_op(&mut self, op: bolt_ts_ast::AssignOp) {
        self.p(op.as_str());
    }
    pub fn p_bin_op(&mut self, op: bolt_ts_ast::BinOpKind) {
        self.p(op.as_str());
    }
    // `${`
    pub fn p_dollar_and_brace(&mut self) {
        self.p("${");
    }
    pub fn p_string_literal(&mut self, s: &str) {
        self.p("'");
        for c in s.chars() {
            match c {
                '\'' => self.p("\\'"),
                _ => self.content.push(c),
            }
        }
        self.p("'");
    }
}

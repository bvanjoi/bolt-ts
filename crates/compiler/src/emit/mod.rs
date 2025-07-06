mod emit_block_like;
mod emit_class_like;
mod expr;
mod helper;
mod stmt;
mod utils;

use bolt_ts_ast as ast;
use bolt_ts_atom::AtomMap;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashset_with_capacity;
use rustc_hash::FxHashSet;

use bolt_ts_parser::Parser;

struct PPrint {
    content: String,
    indent: u32,
}

impl PPrint {
    pub fn new(input_len: usize) -> Self {
        PPrint {
            content: String::with_capacity(input_len * 2),
            indent: 0,
        }
    }
    fn p(&mut self, content: &str) {
        self.content += content
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
        self.p_pieces_of_whitespace(self.indent);
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

bolt_ts_utils::index! {
    ScopeID
}

pub struct Emit<'cx> {
    module_id: ModuleID,
    atoms: &'cx AtomMap<'cx>,
    content: PPrint,
    options: EmitterOptions,
    ns_names: FxHashSet<(ScopeID, bolt_ts_atom::AtomId)>,
    scope: ScopeID,
    max_scope: ScopeID,
    config: &'cx bolt_ts_config::NormalizedCompilerOptions,
    helper_flags: helper::EmitHelperFlags,
    comment_index: usize,
    input: &'cx str,
    p: &'cx Parser<'cx>,
}

impl<'cx> Emit<'cx> {
    fn next_scope(&mut self) -> ScopeID {
        let scope = self.max_scope;
        self.max_scope = self.max_scope.next();
        scope
    }

    pub fn new(
        module_id: ModuleID,
        atoms: &'cx AtomMap,
        input: &'cx str,
        config: &'cx bolt_ts_config::NormalizedCompilerOptions,
        parser: &'cx Parser<'cx>,
    ) -> Self {
        Self {
            atoms,
            module_id,
            content: PPrint::new(input.len() * 2),
            options: EmitterOptions { indent: 2 },
            ns_names: fx_hashset_with_capacity(256),
            scope: ScopeID::root(),
            max_scope: ScopeID::root(),
            config,
            helper_flags: helper::EmitHelperFlags::empty(),
            p: parser,
            input,
            comment_index: 0,
        }
    }

    pub fn emit_root(&mut self, root: &'cx ast::Program) -> String {
        self.emit_program(root);
        std::mem::take(&mut self.content.content)
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

    fn emit_entity_name(&mut self, name: &ast::EntityName) {
        use bolt_ts_ast::EntityNameKind::*;
        match name.kind {
            Ident(n) => self.emit_ident(n),
            Qualified(n) => {
                self.emit_entity_name(n.left);
                self.content.p_dot();
                self.emit_ident(n.right);
            }
        };
    }

    fn emit_leading_comments(&mut self, node_span: bolt_ts_span::Span) {
        // debug_assert_eq!(self.module_id, node_span.module);
        // let comments = &self.p.get(self.module_id).comments;
        // while let Some(comment) = comments.get(self.comment_index) {
        //     let comment_span = comment.span();
        //     if comment_span.hi <= node_span.lo {
        //         use bolt_ts_ast::Comment::*;
        //         let content = &self.input[comment_span.lo as usize..comment_span.hi as usize];
        //         self.content.p(content);
        //         if let SingleLine(_) = comment {
        //             self.content.p_newline();
        //         }
        //         self.comment_index += 1;
        //         continue;
        //     } else {
        //         break;
        //     }
        // }
    }
}

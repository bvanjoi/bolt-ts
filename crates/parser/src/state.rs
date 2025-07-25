use bolt_ts_ast::{self as ast, Node, NodeFlags, NodeID, keyword};
use bolt_ts_ast::{Token, TokenFlags, TokenKind};
use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_span::{ModuleID, Span};
use bolt_ts_utils::path::NormalizePath;

use std::sync::{Arc, Mutex};

use super::PResult;
use super::parsing_ctx::ParsingContext;
use super::utils::is_declaration_filename;
use super::{CommentDirective, FileReference, NodeFlagsMap, Nodes, TokenValue};
use super::{PragmaMap, errors};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanguageVariant {
    Standard,
    Jsx,
}

pub(super) struct ParserState<'cx, 'p> {
    pub(super) atoms: Arc<Mutex<AtomMap<'cx>>>,
    pub(super) input: &'p [u8],
    pub(super) token: Token,
    pub(super) token_value: Option<TokenValue>,
    pub(super) string_key_value: Option<AtomId>,
    pub(super) token_flags: TokenFlags,
    pub(super) full_start_pos: usize,
    pub(super) pos: usize,
    pub(super) module_id: ModuleID,
    pub(super) diags: Vec<bolt_ts_errors::Diag>,
    pub(super) nodes: Nodes<'cx>,
    pub(super) node_flags_map: NodeFlagsMap,
    pub(super) arena: &'p bolt_ts_arena::bumpalo_herd::Member<'cx>,
    pub(super) context_flags: NodeFlags,
    pub(super) external_module_indicator: Option<ast::NodeID>,
    pub(super) commonjs_module_indicator: Option<ast::NodeID>,
    pub(super) lib_reference_directives: Vec<FileReference>,
    pub(super) pragmas: PragmaMap,
    pub(super) has_export_decl: bool,
    pub(super) comment_directives: Vec<CommentDirective>,
    pub(super) comments: Vec<ast::Comment>,
    pub(super) line: usize,
    pub(super) line_start: usize, // offset
    pub(super) line_map: Vec<u32>,
    pub(super) is_declaration: bool,
    pub(super) filepath: AtomId,
    pub(super) in_ambient_module: bool,
    pub(super) has_no_default_lib: bool,
    pub(super) variant: LanguageVariant,
    pub(super) parsing_context: ParsingContext,
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    pub(super) fn new(
        atoms: Arc<Mutex<AtomMap<'cx>>>,
        arena: &'p bolt_ts_arena::bumpalo_herd::Member<'cx>,
        nodes: Nodes<'cx>,
        input: &'p [u8],
        module_id: ModuleID,
        file_path: &std::path::Path,
        variant: LanguageVariant,
    ) -> Self {
        let token = Token::new(TokenKind::EOF, Span::new(u32::MAX, u32::MAX, module_id));
        let p = file_path.to_string_lossy();
        let p = p.as_bytes();
        let atom = AtomId::from_bytes(p);
        debug_assert!(file_path.is_normalized());
        debug_assert!(atoms.lock().unwrap().contains(atom));
        let mut context_flags = NodeFlags::default();
        let is_declaration = is_declaration_filename(p);
        if is_declaration {
            context_flags |= NodeFlags::AMBIENT;
        }
        Self {
            input,
            token,
            token_value: None,
            string_key_value: None,
            pos: 0,
            full_start_pos: 0,
            atoms,
            module_id,
            diags: vec![],
            token_flags: TokenFlags::empty(),
            arena,
            nodes,
            context_flags,
            node_flags_map: NodeFlagsMap::new(),
            external_module_indicator: None,
            commonjs_module_indicator: None,
            has_export_decl: false,

            comment_directives: Vec::with_capacity(16),
            comments: Vec::with_capacity(256),

            line_start: 0,
            line_map: Vec::with_capacity(input.len() / 12),
            line: 0,
            filepath: atom,
            is_declaration,
            in_ambient_module: false,
            lib_reference_directives: Vec::with_capacity(8),
            pragmas: PragmaMap::default(),
            has_no_default_lib: false,
            variant,
            parsing_context: ParsingContext::default(),
        }
    }

    pub(super) fn alloc<T>(&self, t: T) -> &'cx T {
        self.arena.alloc(t)
    }

    pub(super) fn next_node_id(&mut self) -> NodeID {
        let idx = self.nodes.0.len();
        NodeID::new(self.module_id, idx as u32)
    }

    pub(super) fn current_node_id(&self) -> NodeID {
        NodeID::new(self.module_id, self.nodes.0.len() as u32)
    }

    pub(super) fn reset_node_id(&mut self, to: NodeID) {
        debug_assert!(to.module() == self.module_id);
        debug_assert!(to.index_as_usize() <= self.nodes.0.len());
        let to = to.index_as_usize();
        for i in to..self.nodes.0.len() {
            self.node_flags_map.0.remove(&(i as u32));
        }
        self.nodes.0.truncate(to);
    }

    #[inline]
    pub(super) fn end(&self) -> usize {
        self.input.len()
    }

    pub(super) fn can_parse_semi(&self) -> bool {
        if self.token.kind == TokenKind::Semi {
            true
        } else {
            matches!(self.token.kind, TokenKind::RBrace | TokenKind::EOF)
                || self
                    .token_flags
                    .intersects(TokenFlags::PRECEDING_LINE_BREAK)
        }
    }

    pub(super) fn parse_bracketed_list<const CONSIDER_SEMICOLON_AS_DELIMITER: bool, T>(
        &mut self,
        ctx: ParsingContext,
        open: TokenKind,
        ele: impl Fn(&mut Self) -> PResult<T>,
        close: TokenKind,
    ) -> PResult<&'cx [T]> {
        if self.expect(open) {
            let elems = self.parse_delimited_list::<CONSIDER_SEMICOLON_AS_DELIMITER, T>(ctx, ele);
            self.expect(close);
            Ok(elems)
        } else {
            Ok(&[])
        }
    }

    pub(super) fn parse_list<T>(
        &mut self,
        ctx: ParsingContext,
        ele: impl Fn(&mut Self) -> PResult<T>,
    ) -> &'cx [T] {
        let mut list = Vec::with_capacity(8);
        while !self.is_list_terminator(ctx) {
            if self.is_list_element(ctx, false) {
                if let Ok(ele) = ele(self) {
                    list.push(ele);
                }
                continue;
            } else if self.abort_parsing_list_or_move_to_next_token(ctx) {
                break;
            }
        }
        self.alloc(list)
    }

    pub(super) fn abort_parsing_list_or_move_to_next_token(&mut self, ctx: ParsingContext) -> bool {
        self.parsing_context_errors(ctx);
        if self.is_in_some_parsing_context() {
            true
        } else {
            self.next_token();
            false
        }
    }

    pub(super) fn parse_delimited_list<const CONSIDER_SEMICOLON_AS_DELIMITER: bool, T>(
        &mut self,
        ctx: ParsingContext,
        ele: impl Fn(&mut Self) -> PResult<T>,
    ) -> &'cx [T] {
        let mut list = Vec::with_capacity(8);
        self.parsing_context.insert(ctx);
        loop {
            if self.is_list_element(ctx, false) {
                let start_pos = self.token.start();
                let Ok(ele) = ele(self) else {
                    break;
                };
                list.push(ele);
                if self.parse_optional(TokenKind::Comma).is_some() {
                    continue;
                }
                if self.is_list_terminator(ctx) {
                    break;
                }

                self.expect(TokenKind::Comma);

                if CONSIDER_SEMICOLON_AS_DELIMITER
                    && self.token.kind == TokenKind::Semi
                    && !self
                        .token_flags
                        .intersects(TokenFlags::PRECEDING_LINE_BREAK)
                {
                    self.next_token();
                }

                if start_pos == self.token.start() {
                    self.next_token();
                }
                continue;
            }
            if self.is_list_terminator(ctx) || self.abort_parsing_list_or_move_to_next_token(ctx) {
                break;
            }
        }
        self.parsing_context.remove(ctx);
        self.alloc(list)
    }

    pub(super) fn parse_token_node(&mut self) -> Token {
        let t = self.token;
        self.next_token();
        t
    }

    pub(super) fn parse_optional(&mut self, t: TokenKind) -> Option<Token> {
        (self.token.kind == t).then(|| self.parse_token_node())
    }

    pub(super) fn ident_token(&self) -> AtomId {
        assert!(
            self.token.kind.is_ident_or_keyword()
                || matches!(self.token.kind, TokenKind::BigInt | TokenKind::Regexp),
            "{:#?}",
            self.token
        );
        self.token_value.unwrap().ident()
    }

    pub(super) fn string_token(&self) -> AtomId {
        use bolt_ts_ast::TokenKind::*;
        assert!(
            matches!(
                self.token.kind,
                String | NoSubstitutionTemplate | JSXText | JSXTextAllWhiteSpaces
            ),
            "{:#?}",
            self.token
        );
        self.token_value.unwrap().ident()
    }

    pub(super) fn number_token(&self) -> f64 {
        assert!(matches!(self.token.kind, TokenKind::Number));
        self.token_value.unwrap().number()
    }

    pub(super) fn create_ident_by_atom(&mut self, name: AtomId, span: Span) -> &'cx ast::Ident {
        let id = self.next_node_id();
        let ident = self.alloc(ast::Ident { id, name, span });
        self.nodes.insert(id, Node::Ident(ident));
        self.node_flags_map.insert(id, self.context_flags);
        ident
    }

    pub(super) fn create_ident(
        &mut self,
        is_ident: bool,
        missing_ident_kind: Option<errors::MissingIdentKind>,
    ) -> &'cx ast::Ident {
        if is_ident {
            let res = self.create_ident_by_atom(self.ident_token(), self.token.span);
            self.next_token_without_checked();
            res
        } else if self.token.kind == TokenKind::Private {
            todo!()
        } else {
            let span = self.token.span;
            let kind = missing_ident_kind.unwrap_or(errors::MissingIdentKind::IdentifierExpected);
            let error = errors::MissingIdent { span, kind };
            self.push_error(Box::new(error));
            self.create_ident_by_atom(keyword::IDENT_EMPTY, self.token.span)
        }
    }

    pub(super) fn parse_semi(&mut self) {
        if self.token.kind == TokenKind::Semi {
            self.next_token();
        }
    }

    pub(super) fn parse_binding_ident(&mut self) -> &'cx ast::Ident {
        let is_ident = self.token.kind.is_binding_ident();
        self.create_ident(is_ident, None)
    }

    pub(super) fn parse_optional_binding_ident(&mut self) -> PResult<Option<&'cx ast::Ident>> {
        if self.token.kind.is_binding_ident() {
            Ok(Some(self.parse_binding_ident()))
        } else {
            Ok(None)
        }
    }

    pub(super) fn expect_with<const ADVANCE: bool>(
        &mut self,
        t: TokenKind,
        f: Option<impl FnOnce(&mut Self) -> crate::Diag>,
    ) -> bool {
        if self.token.kind == t {
            if ADVANCE {
                self.next_token();
            }
            true
        } else {
            let error = if let Some(f) = f {
                f(self)
            } else {
                Box::new(errors::ExpectX {
                    span: self.token.span,
                    x: t.as_str().to_string(),
                })
            };
            self.push_error(error);
            false
        }
    }

    pub(super) fn expect(&mut self, t: TokenKind) -> bool {
        let f: Option<fn(&mut Self) -> crate::Diag> = None;
        self.expect_with::<true>(t, f)
    }

    pub(super) fn create_lit<T>(&mut self, val: T, span: Span) -> &'cx ast::Lit<T> {
        let id = self.next_node_id();
        self.alloc(ast::Lit { id, val, span })
    }

    pub(super) fn create_lit_ty(&mut self, kind: ast::LitTyKind, span: Span) -> &'cx ast::LitTy {
        let id = self.next_node_id();
        self.alloc(ast::LitTy { id, kind, span })
    }

    pub(super) fn parse(&mut self) -> &'cx ast::Program<'cx> {
        let start = self.pos;
        self.next_token();
        let stmts = self.parse_list(ParsingContext::SOURCE_ELEMENTS, Self::parse_stmt);
        let id = self.next_node_id();
        let program = self.alloc(ast::Program {
            id,
            stmts,
            span: self.new_span(start as u32),
        });
        self.nodes.insert(id, Node::Program(program));
        program
    }

    pub(super) fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error });
    }

    pub(super) fn disallow_in_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_inside_of_context(NodeFlags::DISALLOW_IN_CONTEXT, f)
    }

    pub(super) fn allow_in_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_outside_of_context(NodeFlags::DISALLOW_IN_CONTEXT, f)
    }

    pub(super) fn do_outside_of_context<T>(
        &mut self,
        context: NodeFlags,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let removed = self.context_flags.intersection(context);
        if !removed.is_empty() {
            debug_assert!(self.context_flags.contains(removed));
            self.context_flags.remove(removed);
            let res = f(self);
            debug_assert!(!self.context_flags.contains(removed));
            self.context_flags.insert(removed);
            res
        } else {
            f(self)
        }
    }

    pub(super) fn do_inside_of_context<T>(
        &mut self,
        context: NodeFlags,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let inserted = self.context_flags.complement().intersection(context);
        if !inserted.is_empty() {
            debug_assert!(!self.context_flags.contains(inserted));
            self.context_flags.insert(inserted);
            let res = f(self);
            debug_assert!(self.context_flags.contains(inserted));
            self.context_flags.remove(inserted);
            res
        } else {
            f(self)
        }
    }

    #[inline]
    pub(super) fn in_context(&self, flags: NodeFlags) -> bool {
        self.context_flags.intersects(flags)
    }

    #[inline]
    pub(super) fn in_disallow_in_context(&self) -> bool {
        self.in_context(NodeFlags::DISALLOW_IN_CONTEXT)
    }

    pub(super) fn allow_conditional_tys_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_outside_of_context(NodeFlags::DISALLOW_CONDITIONAL_TYPES_CONTEXT, f)
    }

    pub(super) fn disallow_conditional_tys_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_inside_of_context(NodeFlags::DISALLOW_CONDITIONAL_TYPES_CONTEXT, f)
    }

    pub(super) fn in_disallow_conditional_tys_context(&self) -> bool {
        self.in_context(NodeFlags::DISALLOW_CONDITIONAL_TYPES_CONTEXT)
    }

    pub(super) fn parse_identifier_name_error_or_unicode_escape_sequence(
        &mut self,
    ) -> PResult<&'cx bolt_ts_ast::Ident> {
        if self
            .token_flags
            .intersects(TokenFlags::UNICODE_ESCAPE.union(TokenFlags::EXTENDED_UNICODE_ESCAPE))
        {
            self.push_error(Box::new(errors::UnicodeEscapeSequenceCannotAppearHere {
                span: self.token.span,
            }));
        }
        self.parse_ident_name()
    }
}

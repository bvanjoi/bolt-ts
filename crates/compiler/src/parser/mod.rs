mod errors;
mod expr;
mod list_ctx;
mod scan;
mod stmt;
mod token;
mod ty;
mod utils;

use std::borrow::Cow;
use std::u32;

use rts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;
use token::{Token, TokenFlags, TokenKind};

use crate::ast::{self, Node, NodeID};
use crate::atoms::{AtomId, AtomMap};
use crate::keyword;

type PResult<T> = Result<T, ()>;

pub struct Nodes<'cx>(FxHashMap<NodeID, Node<'cx>>);

impl<'cx> Nodes<'cx> {
    pub fn get(&self, id: NodeID) -> Node<'cx> {
        self.0.get(&id).copied().unwrap()
    }

    pub fn insert(&mut self, id: NodeID, node: Node<'cx>) {
        let prev = self.0.insert(id, node);
        assert!(prev.is_none())
    }
}

#[derive(Debug)]
pub struct ParentMap(FxHashMap<NodeID, NodeID>);

impl ParentMap {
    pub fn parent(&self, id: NodeID) -> Option<NodeID> {
        self.0.get(&id).copied()
    }

    pub(super) fn insert(&mut self, id: NodeID, parent: NodeID) {
        let prev = self.0.insert(id, parent);
        assert!(prev.is_none())
    }

    pub(super) fn r#override(&mut self, id: NodeID, parent: NodeID) {
        let prev = self.0.insert(id, parent);
        assert!(prev.is_some())
    }
}

pub struct Parser<'cx> {
    arena: &'cx bumpalo::Bump,
    pub nodes: Nodes<'cx>,
    pub parent_map: ParentMap,
    next_node_id: NodeID,
    pub atoms: AtomMap<'cx>,
}

impl<'cx> Parser<'cx> {
    pub fn new(ast_arena: &'cx bumpalo::Bump, mut atoms: AtomMap<'cx>) -> Self {
        assert!(ast_arena.allocation_limit().is_none());

        if cfg!(debug_assertions) {
            for (idx, (name, _)) in keyword::KEYWORDS.iter().enumerate() {
                let t = unsafe { std::mem::transmute::<u8, TokenKind>(idx as u8) };
                assert!(t.is_keyword());
                assert_eq!(format!("{t:?}").to_lowercase(), *name);
            }
        }

        for (atom, id) in keyword::KEYWORDS {
            atoms.insert(*id, Cow::Borrowed(atom));
        }
        for (atom, id) in keyword::IDENTIFIER {
            atoms.insert(*id, Cow::Borrowed(atom))
        }
        Self {
            nodes: Nodes(FxHashMap::default()),
            parent_map: ParentMap(FxHashMap::default()),
            next_node_id: NodeID::root(),
            atoms,
            arena: ast_arena,
        }
    }

    fn next_node_id(&mut self) -> NodeID {
        let old = self.next_node_id;
        self.next_node_id = self.next_node_id.next();
        old
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenValue {
    Number { value: f64 },
    Ident { value: AtomId },
}

impl TokenValue {
    fn number(self) -> f64 {
        if let TokenValue::Number { value } = self {
            value
        } else {
            unreachable!()
        }
    }

    fn ident(self) -> AtomId {
        if let TokenValue::Ident { value } = self {
            value
        } else {
            unreachable!()
        }
    }
}

pub struct ParserState<'cx, 'p> {
    p: &'p mut Parser<'cx>,
    input: &'p [u8],
    token: Token,
    token_value: Option<TokenValue>,
    token_flags: TokenFlags,
    pos: usize,
    parent: NodeID,
    module_id: ModuleID,
    ident_count: usize,
    diags: Vec<rts_errors::Diag>,
}

impl<'cx, 'a, 'p> ParserState<'cx, 'p> {
    pub fn steal_diags(&mut self) -> Vec<rts_errors::Diag> {
        std::mem::take(&mut self.diags)
    }

    pub fn new(p: &'p mut Parser<'cx>, input: &'p [u8], module_id: ModuleID) -> Self {
        let token = Token::new(
            TokenKind::EOF,
            Span::new(u32::MAX, u32::MAX, ModuleID::root()),
        );
        Self {
            input,
            token,
            token_value: None,
            pos: 0,
            p,
            parent: NodeID::root(),
            module_id,
            ident_count: 0,
            diags: vec![],
            token_flags: TokenFlags::empty(),
        }
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.p.arena.alloc(t)
    }

    #[inline]
    fn end(&self) -> usize {
        self.input.len()
    }

    fn can_parse_semi(&self) -> bool {
        if self.token.kind == TokenKind::Semi {
            true
        } else {
            self.token.kind == TokenKind::RBrace
                || self.token.kind == TokenKind::EOF
                || self.token_flags.contains(TokenFlags::PRECEDING_LINE_BREAK)
        }
    }

    fn parse_bracketed_list<T>(
        &mut self,
        open: TokenKind,
        is_ele: impl Fn(&mut Self) -> bool,
        ele: impl Fn(&mut Self) -> PResult<T>,
        is_closing: impl Fn(&mut Self) -> bool,
        close: TokenKind,
    ) -> PResult<&'cx [T]> {
        if self.expect(open).is_ok() {
            let elems = self.parse_delimited_list(is_ele, ele, is_closing);
            self.expect(close)?;
            Ok(elems)
        } else {
            Ok(&[])
        }
    }

    fn parse_list<T>(
        &mut self,
        is_ele: impl Fn(&mut Self) -> bool,
        ele: impl Fn(&mut Self) -> PResult<T>,
        is_closing: impl Fn(&mut Self) -> bool,
    ) -> &'cx [T] {
        let mut list = vec![];
        while !is_closing(self) {
            if is_ele(self) {
                if let Ok(ele) = ele(self) {
                    list.push(ele);
                }
            }
        }
        self.alloc(list)
    }

    fn parse_delimited_list<T>(
        &mut self,
        is_ele: impl Fn(&mut Self) -> bool,
        ele: impl Fn(&mut Self) -> PResult<T>,
        is_closing: impl Fn(&mut Self) -> bool,
    ) -> &'cx [T] {
        let mut list = vec![];
        loop {
            if is_ele(self) {
                let Ok(ele) = ele(self) else {
                    break;
                };
                list.push(ele);
                if is_closing(self) {
                    break;
                }
                if self.parse_optional(TokenKind::Comma).is_some() {
                    continue;
                }
            }
            if is_closing(self) {
                break;
            }
        }
        self.alloc(list)
    }

    fn scan_speculation_helper<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
        is_lookahead: bool,
    ) -> T {
        let old_pos = self.pos;
        let old_token = self.token;
        let old_token_value = self.token_value;

        let r = f(self);

        if is_lookahead {
            self.pos = old_pos;
            self.token = old_token;
            self.token_value = old_token_value;
        }
        r
    }

    fn scan_lookahead<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scan_speculation_helper(f, true)
    }

    fn speculation_helper<T>(&mut self, f: impl FnOnce(&mut Self) -> T, try_parse: bool) -> T {
        let old_token = self.token;

        let r = if try_parse {
            todo!()
        } else {
            self.scan_lookahead(f)
        };

        if !try_parse {
            self.token = old_token;
        }

        r
    }

    fn lookahead<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.speculation_helper(f, false)
    }

    fn is_start_of_fn_or_ctor_ty(&mut self) -> bool {
        let t = self.token.kind;
        if t == TokenKind::LParen && self.lookahead(Self::is_unambiguously_start_of_fn_ty) {
            true
        } else {
            false
        }
    }

    fn is_unambiguously_start_of_fn_ty(&mut self) -> bool {
        self.next_token();
        let t = self.token.kind;
        if t == TokenKind::RParen {
            true
        } else {
            false
        }
    }

    fn parse_token_node(&mut self) -> Token {
        let t = self.token;
        self.next_token();
        t
    }

    fn parse_optional(&mut self, t: TokenKind) -> Option<Token> {
        (self.token.kind == t).then(|| self.parse_token_node())
    }

    fn ident_token(&self) -> AtomId {
        assert!(self.token.kind.is_ident_or_keyword(), "{:#?}", self.token);
        self.token_value.unwrap().ident()
    }

    fn string_token(&self) -> AtomId {
        assert!(matches!(
            self.token.kind,
            TokenKind::String | TokenKind::NoSubstitutionTemplate
        ));
        self.token_value.unwrap().ident()
    }

    fn number_token(&self) -> f64 {
        assert!(matches!(self.token.kind, TokenKind::Number));
        self.token_value.unwrap().number()
    }

    fn create_ident(&mut self, is_ident: bool) -> &'cx ast::Ident {
        if is_ident {
            self.ident_count += 1;
            let id = self.p.next_node_id();
            let name = self.ident_token();
            let span = self.token.span;
            let ident = self.alloc(ast::Ident { id, name, span });
            self.next_token();
            self.insert_map(id, Node::Ident(ident));
            ident
        } else {
            unreachable!()
        }
    }

    fn parse_semi(&mut self) {
        if self.token.kind == TokenKind::Semi {
            self.next_token();
        }
    }

    fn is_ident(&self) -> bool {
        matches!(self.token.kind, TokenKind::Ident)
    }

    fn parse_ident_name(&mut self) -> PResult<&'cx ast::Ident> {
        Ok(self.create_ident(true))
    }

    fn parse_binding_ident(&mut self) -> &'cx ast::Ident {
        self.create_ident(true)
    }

    fn parse_optional_binding_ident(&mut self) -> PResult<Option<&'cx ast::Ident>> {
        if self.token.kind.is_binding_ident() {
            Ok(Some(self.parse_binding_ident()))
        } else {
            Ok(None)
        }
    }

    fn expect(&mut self, t: TokenKind) -> PResult<()> {
        if self.token.kind == t {
            self.next_token();
            Ok(())
        } else {
            Err(())
        }
    }

    fn create_lit<T>(&mut self, val: T, span: Span) -> &'cx ast::Lit<T> {
        let id = self.p.next_node_id();
        let lit = self.alloc(ast::Lit { id, val, span });
        self.next_token();
        lit
    }

    #[inline]
    fn with_parent<T>(&mut self, parent: NodeID, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.parent;
        self.parent = parent;
        let ret = f(self);
        self.parent = old;
        ret
    }

    fn insert_map(&mut self, id: NodeID, node: Node<'cx>) {
        self.p.nodes.insert(id, node);
        self.p.parent_map.insert(id, self.parent);
    }

    pub fn parse(&mut self) -> &'cx ast::Program<'cx> {
        let id = self.p.next_node_id();
        self.with_parent(id, |this| {
            this.next_token();
            let stmts = this.p.arena.alloc(Vec::with_capacity(32));
            while !matches!(this.token.kind, TokenKind::EOF) {
                if let Ok(stmt) = this.parse_stmt() {
                    stmts.push(stmt);
                }
            }
            let program = this.alloc(ast::Program { id, stmts });
            this.p.nodes.insert(id, Node::Program(program));
            program
        })
    }

    fn push_error(&mut self, module_id: ModuleID, error: crate::Diag) {
        self.diags.push(rts_errors::Diag {
            module_id,
            inner: error,
        });
    }
}

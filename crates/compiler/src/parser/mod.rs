mod errors;
mod expr;
mod list_ctx;
mod lookahead;
mod paren_rule;
mod parse_class_like;
mod parse_fn_like;
mod scan;
mod stmt;
pub mod token;
mod ty;
mod utils;

use std::sync::{Arc, Mutex};

use bolt_ts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;
use token::{Token, TokenFlags, TokenKind};

use crate::ast::{self, Node, NodeID};
use crate::atoms::{AtomId, AtomMap};

type PResult<T> = Result<T, ()>;

#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
pub struct ParentMap(FxHashMap<NodeID, NodeID>);

impl ParentMap {
    pub fn parent(&self, id: NodeID) -> Option<NodeID> {
        self.0.get(&id).copied()
    }

    pub(super) fn insert(&mut self, id: NodeID, parent: NodeID) {
        assert!(id.index_as_u32() > parent.index_as_u32());
        let prev = self.0.insert(id, parent);
        assert!(prev.is_none())
    }

    pub(super) fn r#override(&mut self, id: NodeID, parent: NodeID) {
        assert!(
            id.index_as_u32() < parent.index_as_u32(),
            "id: {id:#?} and parent: {parent:#?}"
        );
        let prev = self.0.insert(id, parent);
        assert!(prev.unwrap() != id)
    }
}

pub struct ParseResult<'p> {
    // pub root: &'p ast::Program<'p>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'p>,
    parent_map: ParentMap,
}

pub struct Parser<'cx> {
    map: FxHashMap<ModuleID, ParseResult<'cx>>,
}

impl<'cx> Parser<'cx> {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
        }
    }

    #[inline(always)]
    pub fn insert(&mut self, id: ModuleID, result: ParseResult<'cx>) {
        let prev = self.map.insert(id, result);
        assert!(prev.is_none());
    }

    #[inline(always)]
    fn get(&self, id: ModuleID) -> &ParseResult<'cx> {
        self.map.get(&id).unwrap()
    }

    #[inline(always)]
    pub fn root(&self, id: ModuleID) -> &ast::Program<'cx> {
        self.get(id).nodes.get(NodeID::root(id)).expect_program()
    }

    #[inline(always)]
    pub fn node(&self, id: NodeID) -> ast::Node<'cx> {
        self.get(id.module()).nodes.get(id)
    }

    #[inline(always)]
    pub fn parent(&self, id: NodeID) -> Option<ast::NodeID> {
        self.get(id.module()).parent_map.parent(id)
    }

    pub fn steal_errors(&mut self, id: ModuleID) -> Vec<bolt_ts_errors::Diag> {
        std::mem::take(&mut self.map.get_mut(&id).unwrap().diags)
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

pub fn parse<'p, 't>(
    atoms: Arc<Mutex<AtomMap>>,
    arena: &'t bumpalo_herd::Member<'p>,
    input: &'t [u8],
    module_id: ModuleID,
) -> ParseResult<'p> {
    let nodes = Nodes::default();
    let parent_map = ParentMap::default();
    let mut s = ParserState::new(atoms, &arena, nodes, parent_map, input, module_id);
    let root = s.parse();
    ParseResult {
        // root,
        diags: s.diags,
        nodes: s.nodes,
        parent_map: s.parent_map,
    }
}

pub struct ParserState<'p, 't> {
    atoms: Arc<Mutex<AtomMap>>,
    input: &'t [u8],
    token: Token,
    token_value: Option<TokenValue>,
    token_flags: TokenFlags,
    pos: usize,
    parent: NodeID,
    module_id: ModuleID,
    ident_count: usize,
    diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'p>,
    parent_map: ParentMap,
    arena: &'t bumpalo_herd::Member<'p>,
    next_node_id: NodeID,
}

impl<'p, 't> ParserState<'p, 't> {
    fn new(
        atoms: Arc<Mutex<AtomMap>>,
        arena: &'t bumpalo_herd::Member<'p>,
        nodes: Nodes<'p>,
        parent_map: ParentMap,
        input: &'t [u8],
        module_id: ModuleID,
    ) -> Self {
        let token = Token::new(
            TokenKind::EOF,
            Span::new(u32::MAX, u32::MAX, ModuleID::root()),
        );
        Self {
            input,
            token,
            token_value: None,
            pos: 0,
            atoms,
            parent: NodeID::root(module_id),
            module_id,
            ident_count: 0,
            diags: vec![],
            token_flags: TokenFlags::empty(),
            arena,
            nodes,
            parent_map,
            next_node_id: NodeID::root(module_id),
        }
    }

    fn alloc<T>(&self, t: T) -> &'p T {
        self.arena.alloc(t)
    }

    fn next_node_id(&mut self) -> NodeID {
        let old = self.next_node_id;
        self.next_node_id = self.next_node_id.next();
        old
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
        ctx: impl list_ctx::ListContext,
        open: TokenKind,
        ele: impl Fn(&mut Self) -> PResult<T>,
        close: TokenKind,
    ) -> PResult<&'p [T]> {
        if self.expect(open).is_ok() {
            let elems = self.parse_delimited_list(ctx, ele);
            self.expect(close)?;
            Ok(elems)
        } else {
            Ok(&[])
        }
    }

    fn parse_list<T>(
        &mut self,
        ctx: impl list_ctx::ListContext,
        ele: impl Fn(&mut Self) -> PResult<T>,
    ) -> &'p [T] {
        let mut list = vec![];
        while !ctx.is_closing(self) {
            if ctx.is_ele(self) {
                if let Ok(ele) = ele(self) {
                    list.push(ele);
                }
            }
        }
        self.alloc(list)
    }

    fn parse_delimited_list<T>(
        &mut self,
        ctx: impl list_ctx::ListContext,
        ele: impl Fn(&mut Self) -> PResult<T>,
    ) -> &'p [T] {
        let mut list = vec![];
        loop {
            if ctx.is_ele(self) {
                let Ok(ele) = ele(self) else {
                    break;
                };
                list.push(ele);
                if ctx.is_closing(self) {
                    break;
                }
                if self.parse_optional(TokenKind::Comma).is_some() {
                    continue;
                }
            }
            if ctx.is_closing(self) {
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

    pub(super) fn try_scan<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scan_speculation_helper(f, false)
    }

    fn speculation_helper<T>(&mut self, f: impl FnOnce(&mut Self) -> T, try_parse: bool) -> T {
        let old_token = self.token;

        let r = if try_parse {
            self.try_scan(f)
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

    fn create_ident(&mut self, is_ident: bool) -> &'p ast::Ident {
        if is_ident {
            self.ident_count += 1;
            let id = self.next_node_id();
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

    fn parse_binding_ident(&mut self) -> &'p ast::Ident {
        self.create_ident(true)
    }

    fn parse_optional_binding_ident(&mut self) -> PResult<Option<&'p ast::Ident>> {
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

    fn create_lit<T>(&mut self, val: T, span: Span) -> &'p ast::Lit<T> {
        let id = self.next_node_id();
        self.alloc(ast::Lit { id, val, span })
    }

    #[inline]
    fn with_parent<T>(&mut self, parent: NodeID, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.parent;
        self.parent = parent;
        let ret = f(self);
        self.parent = old;
        ret
    }

    fn insert_map(&mut self, id: NodeID, node: Node<'p>) {
        assert!(id.index_as_u32() > self.parent.index_as_u32());
        self.nodes.insert(id, node);
        self.parent_map.insert(id, self.parent);
    }

    pub fn parse(&mut self) -> &'p ast::Program<'p> {
        let start = self.pos;
        let id = self.next_node_id();
        self.with_parent(id, |this| {
            this.next_token();
            let stmts = this.arena.alloc(Vec::with_capacity(32));
            while !matches!(this.token.kind, TokenKind::EOF) {
                if let Ok(stmt) = this.parse_stmt() {
                    stmts.push(stmt);
                }
            }
            let program = this.alloc(ast::Program {
                id,
                stmts,
                span: this.new_span(start, this.pos),
            });
            this.nodes.insert(id, Node::Program(program));
            program
        })
    }

    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag {
            module_id: self.module_id,
            inner: error,
        });
    }
}

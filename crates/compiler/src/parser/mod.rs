mod errors;
mod expr;
mod list_ctx;
mod lookahead;
mod paren_rule;
mod parse_class_like;
mod parse_fn_like;
mod parse_import_export_spec;
mod parse_modifiers;
mod query;
mod scan;
mod stmt;
pub mod token;
mod ty;
mod utils;

use std::sync::{Arc, Mutex};

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_span::{ModuleArena, ModuleID, Span};
use bolt_ts_utils::fx_hashmap_with_capacity;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

pub use self::token::KEYWORD_TOKEN_START;
use self::token::{Token, TokenFlags, TokenKind};
use crate::ast::{self, Node, NodeFlags, NodeID};
use crate::keyword;

type PResult<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Tristate {
    False,
    True,
    Unknown,
}

#[derive(Debug)]
pub struct Nodes<'cx>(FxHashMap<u32, Node<'cx>>);

impl<'cx> Nodes<'cx> {
    pub fn new() -> Self {
        Self(fx_hashmap_with_capacity(2048))
    }

    pub fn get(&self, id: NodeID) -> Node<'cx> {
        let idx = id.index_as_u32();
        self.0[&idx]
    }

    pub fn insert(&mut self, id: NodeID, node: Node<'cx>) {
        let prev = self.0.insert(id.index_as_u32(), node);
        assert!(prev.is_none())
    }
}

#[derive(Debug)]
pub struct ParentMap(FxHashMap<u32, u32>);

impl ParentMap {
    fn new() -> Self {
        Self(fx_hashmap_with_capacity(2048))
    }
    pub fn parent(&self, node_id: NodeID) -> Option<NodeID> {
        let id = node_id.index_as_u32();
        self.0
            .get(&id)
            .map(|parent| NodeID::new(node_id.module(), *parent))
    }

    pub(super) fn insert(&mut self, id: NodeID, parent: NodeID) {
        assert!(id.index_as_u32() > parent.index_as_u32());
        let prev = self.0.insert(id.index_as_u32(), parent.index_as_u32());
        assert!(prev.is_none())
    }

    pub(super) fn r#override(&mut self, id: NodeID, parent: NodeID) {
        assert!(
            id.index_as_u32() < parent.index_as_u32(),
            "id: {id:#?} and parent: {parent:#?}"
        );
        let prev = self.0.insert(id.index_as_u32(), parent.index_as_u32());
        assert!(prev.unwrap() != id.index_as_u32())
    }
}

pub struct ParseResult<'cx> {
    pub diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'cx>,
    parent_map: ParentMap,
}

impl<'cx> ParseResult<'cx> {
    pub fn root(&self) -> &'cx ast::Program<'cx> {
        let id = NodeID::root(ModuleID::root());
        self.nodes.get(id).expect_program()
    }
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

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        self.map
            .values_mut()
            .flat_map(|result| std::mem::take(&mut result.diags))
            .collect()
    }

    #[inline(always)]
    pub fn module_count(&self) -> usize {
        self.map.len()
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

pub fn parse_parallel<'cx, 'p>(
    atoms: Arc<Mutex<AtomMap<'cx>>>,
    herd: &'cx bumpalo_herd::Herd,
    list: &'p [ModuleID],
    module_arena: &'p ModuleArena,
) -> impl ParallelIterator<Item = (ModuleID, ParseResult<'cx>)> + use<'cx, 'p> {
    list.into_par_iter().map_init(
        || herd.get(),
        move |bump, module_id| {
            let input = module_arena.get_content(*module_id);
            let result = parse(atoms.clone(), bump, input.as_bytes(), *module_id);
            assert!(!module_arena.get_module(*module_id).global || result.diags.is_empty());
            (*module_id, result)
        },
    )
}

fn parse<'cx, 'p>(
    atoms: Arc<Mutex<AtomMap<'cx>>>,
    arena: &'p bumpalo_herd::Member<'cx>,
    input: &'p [u8],
    module_id: ModuleID,
) -> ParseResult<'cx> {
    let nodes = Nodes::new();
    let parent_map = ParentMap::new();
    let mut s = ParserState::new(atoms, arena, nodes, parent_map, input, module_id);
    s.parse();
    ParseResult {
        diags: s.diags,
        nodes: s.nodes,
        parent_map: s.parent_map,
    }
}

struct ParserState<'cx, 'p> {
    atoms: Arc<Mutex<AtomMap<'cx>>>,
    input: &'p [u8],
    token: Token,
    token_value: Option<TokenValue>,
    token_flags: TokenFlags,
    full_start_pos: usize,
    pos: usize,
    parent: NodeID,
    module_id: ModuleID,
    ident_count: usize,
    diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'cx>,
    parent_map: ParentMap,
    arena: &'p bumpalo_herd::Member<'cx>,
    next_node_id: NodeID,
    context_flags: NodeFlags,
}

impl<'cx, 'p> ParserState<'cx, 'p> {
    fn new(
        atoms: Arc<Mutex<AtomMap<'cx>>>,
        arena: &'p bumpalo_herd::Member<'cx>,
        nodes: Nodes<'cx>,
        parent_map: ParentMap,
        input: &'p [u8],
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
            full_start_pos: 0,
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
            context_flags: NodeFlags::empty(),
        }
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
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
            matches!(self.token.kind, TokenKind::RBrace | TokenKind::EOF)
                || self
                    .token_flags
                    .intersects(TokenFlags::PRECEDING_LINE_BREAK)
        }
    }

    fn parse_bracketed_list<T>(
        &mut self,
        ctx: impl list_ctx::ListContext,
        open: TokenKind,
        ele: impl Fn(&mut Self) -> PResult<T>,
        close: TokenKind,
    ) -> PResult<&'cx [T]> {
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
    ) -> &'cx [T] {
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
    ) -> &'cx [T] {
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
                } else {
                    let error = errors::ExpectX {
                        span: self.token.span,
                        x: ",".to_string(),
                    };
                    self.push_error(Box::new(error));
                }
            }
            if ctx.is_closing(self) {
                break;
            }
        }
        self.alloc(list)
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
        use TokenKind::*;
        assert!(
            matches!(self.token.kind, String | NoSubstitutionTemplate),
            "{:#?}",
            self.token
        );
        self.token_value.unwrap().ident()
    }

    fn number_token(&self) -> f64 {
        assert!(matches!(self.token.kind, TokenKind::Number));
        self.token_value.unwrap().number()
    }

    fn create_ident(
        &mut self,
        is_ident: bool,
        missing_ident_kind: Option<errors::MissingIdentKind>,
    ) -> &'cx ast::Ident {
        let ident = |this: &mut Self, name: AtomId| {
            this.ident_count += 1;
            let id = this.next_node_id();
            let span = this.token.span;
            let ident = this.alloc(ast::Ident { id, name, span });
            this.insert_map(id, Node::Ident(ident));
            ident
        };
        if is_ident {
            let res = ident(self, self.ident_token());
            self.next_token();
            res
        } else if self.token.kind == TokenKind::Private {
            todo!()
        } else {
            let span = self.token.span;
            let kind = missing_ident_kind.unwrap_or(errors::MissingIdentKind::IdentifierExpected);
            let error = errors::MissingIdent { span, kind };
            self.push_error(error.into());
            ident(self, keyword::IDENT_EMPTY)
        }
    }

    fn parse_semi(&mut self) {
        if self.token.kind == TokenKind::Semi {
            self.next_token();
        }
    }

    fn parse_binding_ident(&mut self) -> &'cx ast::Ident {
        self.create_ident(true, None)
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
        let id = self.next_node_id();
        self.alloc(ast::Lit { id, val, span })
    }

    fn create_lit_ty<T>(&mut self, val: T, span: Span) -> &'cx ast::LitTy<T> {
        let id = self.next_node_id();
        self.alloc(ast::LitTy { id, val, span })
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
        assert!(id.index_as_u32() > self.parent.index_as_u32());
        self.nodes.insert(id, node);
        self.parent_map.insert(id, self.parent);
    }

    pub fn parse(&mut self) -> &'cx ast::Program<'cx> {
        let start = self.pos;
        let id = self.next_node_id();
        self.with_parent(id, |this| {
            this.next_token();
            let stmts = this.arena.alloc(Vec::with_capacity(512));
            while this.token.kind != TokenKind::EOF {
                if let Ok(stmt) = this.parse_stmt() {
                    stmts.push(stmt);
                }
            }
            let program = this.alloc(ast::Program {
                id,
                stmts,
                span: this.new_span(start as u32),
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

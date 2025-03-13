mod errors;
mod expr;
mod list_ctx;
mod lookahead;
mod paren_rule;
mod parse_break_or_continue;
mod parse_class_like;
mod parse_fn_like;
mod parse_import_export_spec;
mod parse_modifiers;
mod query;
mod scan;
mod stmt;
mod ty;
mod utils;

use std::sync::{Arc, Mutex};

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_span::{ModuleArena, ModuleID, Span};
use bolt_ts_utils::fx_hashmap_with_capacity;
use bolt_ts_utils::no_hashmap_with_capacity;
pub(crate) use utils::is_left_hand_side_expr_kind;

use rayon::prelude::*;
use rustc_hash::FxHashMap;
use utils::is_declaration_filename;

pub use self::query::AccessKind;
pub use self::query::AssignmentKind;
use crate::keyword;
pub use bolt_ts_ast::KEYWORD_TOKEN_START;
use bolt_ts_ast::{self as ast, Node, NodeFlags, NodeID};
use bolt_ts_ast::{Token, TokenFlags, TokenKind};

type PResult<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Tristate {
    False,
    True,
    Unknown,
}

#[derive(Debug)]
pub struct Nodes<'cx>(FxHashMap<u32, Node<'cx>>);

impl Default for Nodes<'_> {
    fn default() -> Self {
        Self::new()
    }
}

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
pub struct ParentMap(nohash_hasher::IntMap<u32, u32>);

impl ParentMap {
    fn new() -> Self {
        Self(no_hashmap_with_capacity(2048))
    }
    pub fn parent(&self, node_id: NodeID) -> Option<NodeID> {
        let id = node_id.index_as_u32();
        self.0
            .get(&id)
            .map(|parent| NodeID::new(node_id.module(), *parent))
    }

    fn insert(&mut self, id: NodeID, parent: NodeID) {
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
    pub node_flags_map: NodeFlagsMap,
}

impl<'cx> ParseResult<'cx> {
    pub fn root(&self) -> &'cx ast::Program<'cx> {
        let id = NodeID::root(ModuleID::root());
        self.nodes.get(id).expect_program()
    }

    pub fn node(&self, id: NodeID) -> Node<'cx> {
        self.nodes.get(id)
    }

    pub fn parent(&self, id: NodeID) -> Option<NodeID> {
        self.parent_map.parent(id)
    }

    pub fn node_flags(&self, id: NodeID) -> NodeFlags {
        self.node_flags_map.get(id)
    }
}

pub struct Parser<'cx> {
    pub(crate) map: Vec<ParseResult<'cx>>,
}

impl Default for Parser<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'cx> Parser<'cx> {
    pub fn new() -> Self {
        Self {
            map: Vec::with_capacity(2048),
        }
    }

    pub fn new_with_maps(map: Vec<ParseResult<'cx>>) -> Self {
        Self { map }
    }

    #[inline(always)]
    pub fn insert(&mut self, id: ModuleID, result: ParseResult<'cx>) {
        assert_eq!(id.as_usize(), self.map.len());
        self.map.push(result);
    }

    #[inline(always)]
    fn get(&self, id: ModuleID) -> &ParseResult<'cx> {
        &self.map[id.as_usize()]
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        self.map
            .iter_mut()
            .flat_map(|result| std::mem::take(&mut result.diags))
            .collect()
    }

    #[inline(always)]
    pub fn module_count(&self) -> usize {
        self.map.len()
    }

    pub fn node_flags(&self, node: ast::NodeID) -> NodeFlags {
        self.map[node.module().as_usize()].node_flags(node)
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
            let result = parse(
                atoms.clone(),
                bump,
                input.as_bytes(),
                *module_id,
                module_arena,
            );
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
    module_arena: &'p ModuleArena,
) -> ParseResult<'cx> {
    let nodes = Nodes::new();
    let parent_map = ParentMap::new();
    let mut s = ParserState::new(atoms, arena, nodes, parent_map, input, module_id);
    let file_path = module_arena.get_path(module_id);
    s.parse(file_path);
    ParseResult {
        diags: s.diags,
        nodes: s.nodes,
        parent_map: s.parent_map,
        node_flags_map: s.node_flags_map,
    }
}

struct ParserState<'cx, 'p> {
    atoms: Arc<Mutex<AtomMap<'cx>>>,
    input: &'p [u8],
    token: Token,
    token_value: Option<TokenValue>,
    string_key_value: Option<AtomId>,
    token_flags: TokenFlags,
    full_start_pos: usize,
    pos: usize,
    parent: NodeID,
    module_id: ModuleID,
    ident_count: usize,
    diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'cx>,
    parent_map: ParentMap,
    node_flags_map: NodeFlagsMap,
    arena: &'p bumpalo_herd::Member<'cx>,
    next_node_id: NodeID,
    context_flags: NodeFlags,
}

#[derive(Debug)]
pub struct NodeFlagsMap(nohash_hasher::IntMap<u32, bolt_ts_ast::NodeFlags>);
impl NodeFlagsMap {
    fn new() -> Self {
        Self(no_hashmap_with_capacity(2048))
    }

    pub fn get(&self, node_id: NodeID) -> bolt_ts_ast::NodeFlags {
        let key = node_id.index_as_u32();
        self.0.get(&key).copied().unwrap_or_default()
    }

    pub fn update(&mut self, node_id: NodeID, f: impl FnOnce(&mut bolt_ts_ast::NodeFlags)) {
        let key = node_id.index_as_u32();
        if let Some(flags) = self.0.get_mut(&key) {
            f(flags);
        } else {
            let mut new_flags = bolt_ts_ast::NodeFlags::empty();
            f(&mut new_flags);
            self.0.insert(key, new_flags);
        }
    }

    fn insert(&mut self, node_id: NodeID, flags: bolt_ts_ast::NodeFlags) {
        let key = node_id.index_as_u32();
        let prev = self.0.insert(key, flags);
        assert!(prev.is_none())
    }
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
            string_key_value: None,
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
            node_flags_map: NodeFlagsMap::new(),
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
        if self.expect(open) {
            let elems = self.parse_delimited_list(ctx, ele);
            self.expect(close);
            Ok(elems)
        } else {
            Ok(&[])
        }
    }

    fn is_list_terminator(&mut self, ctx: impl list_ctx::ListContext) -> bool {
        if self.token.kind == TokenKind::EOF {
            return true;
        }
        ctx.is_closing(self)
    }

    fn parse_list<T>(
        &mut self,
        ctx: impl list_ctx::ListContext,
        ele: impl Fn(&mut Self) -> PResult<T>,
    ) -> &'cx [T] {
        let mut list = vec![];
        while !self.is_list_terminator(ctx) {
            if ctx.is_ele(self, false) {
                if let Ok(ele) = ele(self) {
                    list.push(ele);
                }
            }
        }
        self.alloc(list)
    }

    fn abort_parsing_list_or_move_to_next_token(
        &mut self,
        ctx: impl list_ctx::ListContext,
    ) -> bool {
        ctx.parsing_context_errors(self);
        // TODO: is_in_some_parsing_context
        self.next_token();
        false
    }

    fn parse_delimited_list<T>(
        &mut self,
        ctx: impl list_ctx::ListContext,
        ele: impl Fn(&mut Self) -> PResult<T>,
    ) -> &'cx [T] {
        let mut list = Vec::with_capacity(8);
        loop {
            if ctx.is_ele(self, false) {
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
                continue;
            }
            if self.is_list_terminator(ctx) {
                break;
            } else if self.abort_parsing_list_or_move_to_next_token(ctx) {
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
        assert!(
            self.token.kind.is_ident_or_keyword() || self.token.kind == TokenKind::BigInt,
            "{:#?}",
            self.token
        );
        self.token_value.unwrap().ident()
    }

    fn string_token(&self) -> AtomId {
        use bolt_ts_ast::TokenKind::*;
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

    fn create_ident_by_atom(&mut self, name: AtomId, span: Span) -> &'cx ast::Ident {
        self.ident_count += 1;
        let id = self.next_node_id();
        let ident = self.alloc(ast::Ident { id, name, span });
        self.insert_map(id, Node::Ident(ident));
        ident
    }

    fn create_ident(
        &mut self,
        is_ident: bool,
        missing_ident_kind: Option<errors::MissingIdentKind>,
    ) -> &'cx ast::Ident {
        if is_ident {
            let res = self.create_ident_by_atom(self.ident_token(), self.token.span);
            self.next_token();
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

    fn parse_semi(&mut self) {
        if self.token.kind == TokenKind::Semi {
            self.next_token();
        }
    }

    fn parse_binding_ident(&mut self) -> &'cx ast::Ident {
        let is_ident = self.token.kind.is_binding_ident();
        self.create_ident(is_ident, None)
    }

    fn parse_optional_binding_ident(&mut self) -> PResult<Option<&'cx ast::Ident>> {
        if self.token.kind.is_binding_ident() {
            Ok(Some(self.parse_binding_ident()))
        } else {
            Ok(None)
        }
    }

    fn expect(&mut self, t: TokenKind) -> bool {
        if self.token.kind == t {
            self.next_token();
            true
        } else {
            let error = errors::ExpectX {
                span: self.token.span,
                x: t.as_str().to_string(),
            };
            self.push_error(Box::new(error));
            false
        }
    }

    fn create_lit<T>(&mut self, val: T, span: Span) -> &'cx ast::Lit<T> {
        let id = self.next_node_id();
        self.alloc(ast::Lit { id, val, span })
    }

    fn create_lit_ty(&mut self, kind: ast::LitTyKind, span: Span) -> &'cx ast::LitTy {
        let id = self.next_node_id();
        self.alloc(ast::LitTy { id, kind, span })
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

    pub fn parse(&mut self, file_path: &std::path::Path) -> &'cx ast::Program<'cx> {
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
                is_declaration: is_declaration_filename(file_path.to_str().unwrap_or_default().as_bytes()),
            });
            this.nodes.insert(id, Node::Program(program));
            program
        })
    }

    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error });
    }

    fn set_context_flags(&mut self, val: bool, flag: NodeFlags) {
        if val {
            self.context_flags |= flag
        } else {
            self.context_flags &= !flag;
        }
    }

    fn disallow_in_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_inside_of_context(NodeFlags::DISALLOW_IN_CONTEXT, f)
    }

    fn allow_in_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_outside_of_context(NodeFlags::DISALLOW_IN_CONTEXT, f)
    }

    fn do_outside_of_context<T>(
        &mut self,
        context: NodeFlags,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let set = context & self.context_flags;
        if !set.is_empty() {
            self.set_context_flags(false, set);
            let res = f(self);
            self.set_context_flags(true, set);
            res
        } else {
            f(self)
        }
    }

    fn do_inside_of_context<T>(&mut self, context: NodeFlags, f: impl FnOnce(&mut Self) -> T) -> T {
        let set = context & !self.context_flags;
        if !set.is_empty() {
            self.set_context_flags(true, set);
            let res = f(self);
            self.set_context_flags(false, set);
            res
        } else {
            f(self)
        }
    }

    #[inline]
    fn in_context(&self, flags: NodeFlags) -> bool {
        self.context_flags.intersects(flags)
    }

    #[inline]
    fn in_disallow_in_context(&self) -> bool {
        self.in_context(NodeFlags::DISALLOW_IN_CONTEXT)
    }

    fn allow_conditional_tys_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_outside_of_context(NodeFlags::DISALLOW_CONDITIONAL_TYPES_CONTEXT, f)
    }

    fn disallow_conditional_tys_and<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.do_inside_of_context(NodeFlags::DISALLOW_CONDITIONAL_TYPES_CONTEXT, f)
    }

    fn in_disallow_conditional_tys_context(&self) -> bool {
        self.in_context(NodeFlags::DISALLOW_CONDITIONAL_TYPES_CONTEXT)
    }
}

fn has_export_decls(stmts: &bolt_ts_ast::Stmts<'_>) -> bool {
    stmts.iter().any(|stmt| match stmt.kind {
        bolt_ts_ast::StmtKind::Export(_) => true,
        // TODO: export assignment
        _ => false,
    })
}

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

use bolt_ts_ast::Visitor;
use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_path::NormalizePath;
use bolt_ts_span::{ModuleArena, ModuleID, Span};
use bolt_ts_utils::no_hashmap_with_capacity;
pub(crate) use utils::is_left_hand_side_expr_kind;

use rayon::prelude::*;
use utils::is_declaration_filename;

pub use self::query::AccessKind;
pub use self::query::AssignmentKind;
use crate::bind;
use crate::keyword;
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
pub struct Nodes<'cx>(Vec<Node<'cx>>);

impl Default for Nodes<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'cx> Nodes<'cx> {
    pub fn new() -> Self {
        Self(Vec::with_capacity(1024 * 8))
    }

    pub fn get(&self, id: NodeID) -> Node<'cx> {
        let idx = id.index_as_usize();
        debug_assert!(idx < self.0.len());
        *unsafe { self.0.get_unchecked(idx) }
    }

    pub fn insert(&mut self, id: NodeID, node: Node<'cx>) {
        assert_eq!(id.index_as_usize(), self.0.len());
        self.0.push(node);
    }

    fn root(&self) -> &'cx ast::Program<'cx> {
        let idx = self.0.len() - 1;
        let node = unsafe { self.0.get_unchecked(idx) };
        node.expect_program()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CommentDirective {
    pub line: u32,
    pub range: bolt_ts_span::Span,
    pub kind: CommentDirectiveKind,
}

#[derive(Debug, Clone, Copy)]
pub enum CommentDirectiveKind {
    /// @ts-expect-error
    ExpectError,
    /// @ts-ignore
    Ignore,
}

pub struct ParseResult<'cx> {
    pub diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'cx>,
    parent_map: crate::bind::ParentMap,
    pub node_flags_map: NodeFlagsMap,
    pub external_module_indicator: Option<ast::NodeID>,
    pub commonjs_module_indicator: Option<ast::NodeID>,
    pub comment_directives: Vec<CommentDirective>,
    pub line_map: Vec<u32>,
    pub filepath: AtomId,
    pub is_declaration: bool,
    pub imports: Vec<&'cx ast::StringLit>,
    pub module_augmentations: Vec<ast::NodeID>,
    pub ambient_modules: Vec<AtomId>,
}

impl<'cx> ParseResult<'cx> {
    pub fn root(&self) -> &'cx ast::Program<'cx> {
        self.nodes.root()
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

    pub fn node_len(&self) -> usize {
        self.nodes.0.len()
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

    pub fn new_with_maps(map: Vec<(ParseResult<'cx>, crate::bind::ParentMap)>) -> Self {
        let map = map
            .into_iter()
            .map(|(p, parent_map)| ParseResult { parent_map, ..p })
            .collect::<Vec<_>>();
        Self { map }
    }

    #[inline(always)]
    pub fn insert(&mut self, id: ModuleID, result: ParseResult<'cx>) {
        assert_eq!(id.as_usize(), self.map.len());
        self.map.push(result);
    }

    #[inline(always)]
    pub fn get(&self, id: ModuleID) -> &ParseResult<'cx> {
        let idx = id.as_usize();
        debug_assert!(idx < self.map.len());
        unsafe { self.map.get_unchecked(idx) }
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
        let idx = node.module().as_usize();
        debug_assert!(idx < self.map.len());
        unsafe { self.map.get_unchecked(idx).node_flags(node) }
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
            let p = parse(
                atoms.clone(),
                bump,
                input.as_bytes(),
                *module_id,
                module_arena,
            );
            assert!(!module_arena.get_module(*module_id).global || p.diags.is_empty());
            (*module_id, p)
        },
    )

    // list.iter().map(move |module_id| {
    //     let bump = herd.get();
    //     let input = module_arena.get_content(*module_id);
    //     let result = parse(
    //         atoms.clone(),
    //         &bump,
    //         input.as_bytes(),
    //         *module_id,
    //         module_arena,
    //     );
    //     assert!(!module_arena.get_module(*module_id).global || result.diags.is_empty());
    //     (*module_id, result)
    // })
}

fn parse<'cx, 'p>(
    atoms: Arc<Mutex<AtomMap<'cx>>>,
    arena: &'p bumpalo_herd::Member<'cx>,
    input: &'p [u8],
    module_id: ModuleID,
    module_arena: &'p ModuleArena,
) -> ParseResult<'cx> {
    let nodes = Nodes::new();
    let parent_map = bind::ParentMap::default();
    let file_path = module_arena.get_path(module_id);
    let mut s = ParserState::new(atoms, arena, nodes, parent_map, input, module_id, file_path);

    s.parse();

    s.record_new_line_offset();
    assert_eq!(s.line_map[0], 0);
    debug_assert!(s.line_map.is_sorted(), "line_map: {:#?}", s.line_map);

    let c = collect_deps(
        s.is_declaration,
        s.external_module_indicator.is_some(),
        s.nodes.root(),
        s.atoms,
    );
    ParseResult {
        diags: s.diags,
        nodes: s.nodes,
        parent_map: s.parent_map,
        node_flags_map: s.node_flags_map,
        external_module_indicator: s.external_module_indicator,
        commonjs_module_indicator: s.commonjs_module_indicator,
        comment_directives: s.comment_directives,
        line_map: s.line_map,
        filepath: s.filepath,
        is_declaration: s.is_declaration,
        imports: c.imports,
        module_augmentations: c.module_augmentations,
        ambient_modules: c.ambient_modules,
    }
}

fn collect_deps<'cx>(
    is_declaration: bool,
    is_external_module_file: bool,
    root: &'cx ast::Program<'cx>,
    atoms: Arc<Mutex<AtomMap<'cx>>>,
) -> CollectDepsResult<'cx> {
    let mut visitor = CollectDepsVisitor {
        in_ambient_module: false,
        is_declaration,
        is_external_module_file,
        atoms,
        imports: Vec::with_capacity(32),
        ambient_modules: Vec::with_capacity(8),
        module_augmentations: Vec::with_capacity(8),
    };
    visitor.visit_program(root);
    CollectDepsResult {
        imports: visitor.imports,
        module_augmentations: visitor.module_augmentations,
        ambient_modules: visitor.ambient_modules,
    }
}

struct CollectDepsVisitor<'cx> {
    is_declaration: bool,
    in_ambient_module: bool,
    is_external_module_file: bool,
    atoms: Arc<Mutex<AtomMap<'cx>>>,

    imports: Vec<&'cx ast::StringLit>,
    module_augmentations: Vec<ast::NodeID>,
    ambient_modules: Vec<AtomId>,
}

struct CollectDepsResult<'cx> {
    imports: Vec<&'cx ast::StringLit>,
    module_augmentations: Vec<ast::NodeID>,
    ambient_modules: Vec<AtomId>,
}

impl<'cx> ast::Visitor<'cx> for CollectDepsVisitor<'cx> {
    fn visit_stmt(&mut self, node: &'cx ast::Stmt<'cx>) {
        let module_name = match node.kind {
            ast::StmtKind::Import(n) => Some(n.module),
            ast::StmtKind::Export(n) => n.module_spec(),
            // TODO: import equal
            ast::StmtKind::Namespace(n) => {
                if n.is_ambient()
                    && (self.in_ambient_module
                        || n.modifiers
                            .is_some_and(|ms| ms.flags.intersects(ast::ModifierKind::Ambient))
                        || self.is_declaration)
                {
                    let name = match n.name {
                        bolt_ts_ast::ModuleName::Ident(_) => {
                            assert!(n.is_global_argument);
                            keyword::IDENT_GLOBAL
                        }
                        bolt_ts_ast::ModuleName::StringLit(lit) => lit.val,
                    };
                    if self.is_external_module_file
                        || (self.in_ambient_module
                            && name != keyword::IDENT_GLOBAL
                            && !bolt_ts_path::is_external_module_relative(
                                self.atoms.lock().unwrap().get(name),
                            ))
                    {
                        self.module_augmentations.push(n.name.id());
                    } else if !self.in_ambient_module {
                        if self.is_declaration {
                            self.ambient_modules.push(name);
                        }

                        if let Some(block) = n.block {
                            self.in_ambient_module = true;
                            for stmt in block.stmts {
                                self.visit_stmt(stmt);
                            }
                            self.in_ambient_module = false;
                        }
                    }
                }
                return;
            }
            _ => return,
        };
        if let Some(module_name) = module_name {
            if module_name.val != keyword::IDENT_EMPTY
                && (!self.in_ambient_module
                    || !bolt_ts_path::is_external_module_relative(
                        self.atoms.lock().unwrap().get(module_name.val),
                    ))
            {
                self.imports.push(module_name);
            }
            // TODO: use_uri_style_node_core_modules
        }
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
    module_id: ModuleID,
    ident_count: usize,
    diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'cx>,
    parent_map: crate::bind::ParentMap,
    node_flags_map: NodeFlagsMap,
    arena: &'p bumpalo_herd::Member<'cx>,
    context_flags: NodeFlags,
    external_module_indicator: Option<ast::NodeID>,
    commonjs_module_indicator: Option<ast::NodeID>,
    has_export_decl: bool,
    comment_directives: Vec<CommentDirective>,
    line: usize,
    line_start: usize, // offset
    line_map: Vec<u32>,
    is_declaration: bool,
    filepath: AtomId,
    in_ambient_module: bool,
}

#[derive(Debug)]
// TODO: use vector
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
        parent_map: crate::bind::ParentMap,
        input: &'p [u8],
        module_id: ModuleID,
        file_path: &std::path::Path,
    ) -> Self {
        let token = Token::new(
            TokenKind::EOF,
            Span::new(u32::MAX, u32::MAX, ModuleID::root()),
        );
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
            ident_count: 0,
            diags: vec![],
            token_flags: TokenFlags::empty(),
            arena,
            nodes,
            parent_map,
            context_flags,
            node_flags_map: NodeFlagsMap::new(),
            external_module_indicator: None,
            commonjs_module_indicator: None,
            has_export_decl: false,
            comment_directives: Vec::with_capacity(32),
            line_start: 0,
            line_map: Vec::with_capacity(input.len() / 12),
            line: 0,
            filepath: atom,
            is_declaration,
            in_ambient_module: false,
        }
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.arena.alloc(t)
    }

    fn next_node_id(&mut self) -> NodeID {
        let idx = self.nodes.0.len();
        NodeID::new(self.module_id, idx as u32)
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
            if self.is_list_terminator(ctx) || self.abort_parsing_list_or_move_to_next_token(ctx) {
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
        self.nodes.insert(id, Node::Ident(ident));
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

    pub fn parse(&mut self) -> &'cx ast::Program<'cx> {
        let start = self.pos;
        self.next_token();
        let stmts = self.parse_list(list_ctx::SourceElems, Self::parse_stmt);
        let id = self.next_node_id();
        let program = self.alloc(ast::Program {
            id,
            stmts,
            span: self.new_span(start as u32),
        });
        self.nodes.insert(id, Node::Program(program));
        program
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

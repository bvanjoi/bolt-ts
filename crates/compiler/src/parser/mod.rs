mod errors;
mod expr;
mod list_ctx;
mod lookahead;
mod nodes;
mod paren_rule;
mod parse_break_or_continue;
mod parse_class_like;
mod parse_fn_like;
mod parse_import_export_spec;
mod parse_modifiers;
mod pragmas;
mod query;
mod scan;
mod scan_pragma;
mod state;
mod stmt;
mod ty;
mod utils;

use bolt_ts_ast::Visitor;
use bolt_ts_ast::{self as ast, Node, NodeFlags, NodeID};
use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_fs::PathId;
use bolt_ts_path::NormalizePath;
use bolt_ts_span::{ModuleArena, ModuleID};
use bolt_ts_utils::no_hashmap_with_capacity;

use std::sync::{Arc, Mutex};

pub use self::nodes::Nodes;
pub use self::pragmas::PragmaMap;
pub use self::query::AccessKind;
pub use self::query::AssignmentKind;
use self::state::ParserState;
pub(crate) use self::utils::is_left_hand_side_expr_kind;

use rayon::prelude::*;

use crate::bind;
use crate::keyword;

type PResult<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Tristate {
    False,
    True,
    Unknown,
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

#[derive(Debug, Clone, Copy)]
pub struct FileReference {
    pub filename: AtomId,
}

pub struct ParseResult<'cx> {
    pub diags: Vec<bolt_ts_errors::Diag>,
    nodes: Nodes<'cx>,
    parent_map: crate::bind::ParentMap,
    pub node_flags_map: NodeFlagsMap,
    pub external_module_indicator: Option<ast::NodeID>,
    pub commonjs_module_indicator: Option<ast::NodeID>,
    pub comment_directives: Vec<CommentDirective>,
    pub lib_references: Vec<PathId>,
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
    default_lib_dir: &'p std::path::Path,
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
                default_lib_dir,
            );
            assert!(!module_arena.get_module(*module_id).is_default_lib || p.diags.is_empty());
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
    default_lib_dir: &std::path::Path,
) -> ParseResult<'cx> {
    let nodes = Nodes(Vec::with_capacity(1024 * 8));
    let parent_map = bind::ParentMap::default();
    let file_path = module_arena.get_path(module_id);
    let mut s = ParserState::new(atoms, arena, nodes, parent_map, input, module_id, file_path);

    s.parse();

    s.record_new_line_offset();
    assert_eq!(s.line_map[0], 0);
    debug_assert!(s.line_map.is_sorted(), "line_map: {:#?}", s.line_map);

    let atoms = s.atoms;
    let c = collect_deps(
        s.is_declaration,
        s.external_module_indicator.is_some(),
        s.nodes.root(),
        atoms.clone(),
    );
    let is_default_lib = module_arena.get_module(module_id).is_default_lib;

    let lib_references = process_lib_reference_directives(
        &s.lib_reference_directives,
        is_default_lib,
        &atoms,
        default_lib_dir,
    )
    .into_iter()
    .map(|p| PathId::new(&p, &mut atoms.lock().unwrap()))
    .collect();
    drop(atoms);

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
        lib_references,
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

fn get_lib_filename_from_lib_reference(
    reference_name: AtomId,
    atoms: &Arc<Mutex<AtomMap<'_>>>,
) -> Option<&'static str> {
    let reference_name = atoms.lock().unwrap().get(reference_name).to_lowercase();
    let key = reference_name.as_str();
    bolt_ts_libs::DEFAULT_LIB_MAP.get(key).copied()
}

fn process_lib_reference_directives(
    lib_reference_directives: &[FileReference],
    is_default_lib: bool,
    atoms: &Arc<Mutex<AtomMap<'_>>>,
    default_lib_dir: &std::path::Path,
) -> Vec<std::path::PathBuf> {
    if is_default_lib {
        return lib_reference_directives
            .iter()
            .filter_map(|r| {
                let lib_filename = get_lib_filename_from_lib_reference(r.filename, atoms)?;
                let lib_filepath = default_lib_dir.join(lib_filename);
                debug_assert!(lib_filepath.is_normalized());
                Some(lib_filepath)
            })
            .collect();
    }
    // TODO: reference in user file.
    vec![]
}

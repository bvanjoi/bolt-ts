mod errors;
mod expr;
mod factory;
mod jsx;
mod lookahead;
mod nodes;
mod paren_rule;
mod parse_break_or_continue;
mod parse_class_like;
mod parse_fn_like;
mod parse_import_export_spec;
mod parse_modifiers;
mod parsed_map;
mod parsing_ctx;
mod pragmas;
mod query;
mod scan;
mod scan_integer;
mod scan_pragma;
mod state;
mod stmt;
mod ty;
mod unicode;
mod utils;

use bolt_ts_ast::{self as ast, Node, NodeFlags, NodeID};
use bolt_ts_ast::{Visitor, keyword};
use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_span::{ModuleArena, ModuleID};
use bolt_ts_utils::no_hashmap_with_capacity;
use bolt_ts_utils::path::NormalizePath;

use std::path::PathBuf;
use std::sync::{Arc, Mutex};

pub use self::nodes::Nodes;
pub use self::parsed_map::ParsedMap;
pub use self::pragmas::PragmaMap;
pub use self::query::AccessKind;
use self::state::LanguageVariant;
use self::state::ParserState;

use rayon::prelude::*;

type Diag = Box<dyn bolt_ts_errors::diag_ext::DiagnosticExt + Send + Sync + 'static>;

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
    pub filename: Atom,
}

pub struct ParseResult<'cx> {
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub nodes: Nodes<'cx>,
    pub node_flags_map: NodeFlagsMap,
    pub external_module_indicator: Option<ast::NodeID>,
    pub commonjs_module_indicator: Option<ast::NodeID>,
    pub comment_directives: Vec<CommentDirective>,
    pub comments: Vec<ast::Comment>,
    pub line_map: Vec<u32>,
    pub filepath: Atom,
    pub is_declaration: bool,
    pub imports: Vec<&'cx ast::StringLit>,
    pub module_augmentations: Vec<ast::NodeID>,
    pub ambient_modules: Vec<Atom>,
    lib_reference_directives: Vec<FileReference>,
}

pub struct ParseResultForGraph<'cx> {
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub nodes: Nodes<'cx>,
    pub node_flags_map: NodeFlagsMap,
    pub external_module_indicator: Option<ast::NodeID>,
    pub commonjs_module_indicator: Option<ast::NodeID>,
    pub comment_directives: Vec<CommentDirective>,
    pub comments: Vec<ast::Comment>,
    pub line_map: Vec<u32>,
    pub filepath: Atom,
    pub is_declaration: bool,
    pub imports: Vec<&'cx ast::StringLit>,
    pub module_augmentations: Vec<ast::NodeID>,
    pub ambient_modules: Vec<Atom>,
    pub lib_references: Vec<PathBuf>,
}

impl<'cx> ParseResultForGraph<'cx> {
    pub fn root(&self) -> &'cx ast::Program<'cx> {
        self.nodes.root()
    }

    pub fn node(&self, id: NodeID) -> Node<'cx> {
        self.nodes.get(id)
    }

    pub fn node_flags(&self, id: NodeID) -> NodeFlags {
        self.node_flags_map.get(id)
    }

    pub fn node_len(&self) -> usize {
        self.nodes.0.len()
    }

    pub fn is_external_or_commonjs_module(&self) -> bool {
        self.external_module_indicator.is_some() || self.commonjs_module_indicator.is_some()
    }

    pub fn is_global_source_file(&self, id: ast::NodeID) -> bool {
        !self.is_external_or_commonjs_module() && self.node(id).is_program()
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenValue {
    Number { value: f64 },
    Ident { value: Atom },
}

impl TokenValue {
    fn number(self) -> f64 {
        match self {
            TokenValue::Number { value } => value,
            TokenValue::Ident { .. } => unreachable!(),
        }
    }

    fn ident(self) -> Atom {
        match self {
            TokenValue::Ident { value } => value,
            TokenValue::Number { .. } => unreachable!(),
        }
    }
}

pub fn parse_parallel<'cx, 'p>(
    atoms: Arc<Mutex<AtomIntern>>,
    herd: &'cx bolt_ts_arena::bumpalo_herd::Herd,
    list: &'p [ModuleID],
    module_arena: &'p ModuleArena,
    default_lib_dir: &'p std::path::Path,
) -> impl ParallelIterator<Item = (ModuleID, ParseResultForGraph<'cx>)> {
    // ) -> impl Iterator<Item = (ModuleID, ParseResult<'cx>)> {

    let lib_references =
        |p: &ParseResult, atoms: Arc<Mutex<AtomIntern>>, module_id: ModuleID| -> Vec<PathBuf> {
            if !p.lib_reference_directives.is_empty() {
                let is_default_lib = module_arena.get_module(module_id).is_default_lib();
                process_lib_reference_directives(
                    &p.lib_reference_directives,
                    is_default_lib,
                    &atoms.lock().unwrap(),
                    default_lib_dir,
                )
                .into_iter()
                .collect::<Vec<_>>()
            } else {
                vec![]
            }
        };
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
            assert!(!module_arena.get_module(*module_id).is_default_lib() || p.diags.is_empty());
            let p = ParseResultForGraph {
                lib_references: lib_references(&p, atoms.clone(), *module_id),
                diags: p.diags,
                nodes: p.nodes,
                node_flags_map: p.node_flags_map,
                external_module_indicator: p.external_module_indicator,
                commonjs_module_indicator: p.commonjs_module_indicator,
                comment_directives: p.comment_directives,
                comments: p.comments,
                line_map: p.line_map,
                filepath: p.filepath,
                is_declaration: p.is_declaration,
                imports: p.imports,
                module_augmentations: p.module_augmentations,
                ambient_modules: p.ambient_modules,
            };
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
    //         default_lib_dir,
    //     );
    //     assert!(!module_arena.get_module(*module_id).is_default_lib() || result.diags.is_empty());
    //     (*module_id, result)
    // })
}

pub fn parse<'cx, 'p>(
    atoms: Arc<Mutex<AtomIntern>>,
    arena: &'p bolt_ts_arena::bumpalo_herd::Member<'cx>,
    input: &'p [u8],
    module_id: ModuleID,
    module_arena: &'p ModuleArena,
) -> ParseResult<'cx> {
    let nodes = Nodes(Vec::with_capacity(1024 * 8));
    let file_path = module_arena.get_path(module_id);
    let variant = if file_path
        .extension()
        .is_some_and(|ext| ext.eq_ignore_ascii_case("jsx") || ext.eq_ignore_ascii_case("tsx"))
    {
        LanguageVariant::Jsx
    } else {
        LanguageVariant::Standard
    };
    let mut s = ParserState::new(atoms, arena, nodes, input, module_id, file_path, variant);

    s.parse();

    s.record_new_line_offset();
    assert_eq!(s.line_map[0], 0);
    debug_assert!(s.line_map.is_sorted(), "line_map: {:#?}", s.line_map);

    let c = collect_deps(
        s.is_declaration,
        s.external_module_indicator.is_some(),
        s.nodes.root(),
        s.atoms.clone(),
    );

    ParseResult {
        diags: s.diags,
        nodes: s.nodes,
        node_flags_map: s.node_flags_map,
        external_module_indicator: s.external_module_indicator,
        commonjs_module_indicator: s.commonjs_module_indicator,
        comment_directives: s.comment_directives,
        comments: s.comments,
        line_map: s.line_map,
        filepath: s.filepath,
        is_declaration: s.is_declaration,
        imports: c.imports,
        module_augmentations: c.module_augmentations,
        ambient_modules: c.ambient_modules,
        lib_reference_directives: s.lib_reference_directives,
    }
}

fn collect_deps<'cx>(
    is_declaration: bool,
    is_external_module_file: bool,
    root: &'cx ast::Program<'cx>,
    atoms: Arc<Mutex<AtomIntern>>,
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
    atoms: Arc<Mutex<AtomIntern>>,

    imports: Vec<&'cx ast::StringLit>,
    module_augmentations: Vec<ast::NodeID>,
    ambient_modules: Vec<Atom>,
}

struct CollectDepsResult<'cx> {
    imports: Vec<&'cx ast::StringLit>,
    module_augmentations: Vec<ast::NodeID>,
    ambient_modules: Vec<Atom>,
}

impl<'cx> ast::Visitor<'cx> for CollectDepsVisitor<'cx> {
    fn visit_stmt(&mut self, node: &'cx ast::Stmt<'cx>) {
        let module_name = match node.kind {
            ast::StmtKind::Import(n) => Some(n.module),
            ast::StmtKind::Export(n) => n.module_spec(),
            // TODO: import equal
            ast::StmtKind::Module(n) => {
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
        if let Some(module_name) = module_name
            && module_name.val != keyword::IDENT_EMPTY
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

#[derive(Debug)]
// TODO: use vector
pub struct NodeFlagsMap(nohash_hasher::IntMap<u32, bolt_ts_ast::NodeFlags>);
impl NodeFlagsMap {
    fn new() -> Self {
        Self(no_hashmap_with_capacity(2048))
    }

    pub fn get(&self, node_id: NodeID) -> bolt_ts_ast::NodeFlags {
        let key = node_id.index_as_u32();
        self.0
            .get(&key)
            .copied()
            .unwrap_or(bolt_ts_ast::NodeFlags::empty())
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
    reference_name: Atom,
    atoms: &AtomIntern,
) -> Option<&'static str> {
    let reference_name = atoms.get(reference_name).to_lowercase();
    let key = reference_name.as_str();
    bolt_ts_libs::DEFAULT_LIB_MAP.get(key).copied()
}

fn process_lib_reference_directives(
    lib_reference_directives: &[FileReference],
    is_default_lib: bool,
    atoms: &AtomIntern,
    default_lib_dir: &std::path::Path,
) -> Vec<std::path::PathBuf> {
    if is_default_lib {
        return lib_reference_directives
            .iter()
            .filter_map(|r| process_lib_reference_directive(r, atoms, default_lib_dir))
            .collect();
    }
    // TODO: reference in user file.
    vec![]
}

fn process_lib_reference_directive(
    lib_reference_directive: &FileReference,
    atoms: &AtomIntern,
    default_lib_dir: &std::path::Path,
) -> Option<std::path::PathBuf> {
    let lib_filename =
        get_lib_filename_from_lib_reference(lib_reference_directive.filename, atoms)?;
    let lib_filepath = default_lib_dir.join(lib_filename);
    debug_assert!(lib_filepath.is_normalized());
    Some(lib_filepath)
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    struct SignatureFlags: u8 {
        const YIELD = 1 << 0;
        const AWAIT = 1 << 1;
        const TYPE  = 1 << 2;
        const IGNORE_MISSING_OPEN_BRACE = 1 << 4;
        const JSDOC = 1 << 5;
        const ASYNC = 1 << 6;
    }
}

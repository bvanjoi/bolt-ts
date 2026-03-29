use std::path::PathBuf;

use bolt_ts_ast as ast;
use bolt_ts_atom::AtomIntern;
use bolt_ts_binder::{SymbolFlags, SymbolID, SymbolName};
use bolt_ts_checker::check::{TyChecker, node_query};
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_parser::get_touching_property_name;
use bolt_ts_span::{ModuleArena, ModuleID, Span};
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct OutputFile {
    js_code: String,
    dts_code: String,
}

impl OutputFile {
    pub fn new(js_code: String, dts_code: String) -> Self {
        Self { js_code, dts_code }
    }
}

#[derive(Debug)]
pub struct Entry {
    node: ast::NodeID,
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct FindAllReferenceFlags: u8 {
        const FIND_IN_STRINGS   = 1 << 0;
        const FIND_IN_COMMENTS  = 1 << 1;
        const USE_OTHER         = 1 << 2;
        const USE_REFERENCES    = 1 << 3;
        const USE_RENAME        = 1 << 4;
        const IMPLEMENTATIONS   = 1 << 5;
        const PROVIDE_PREFIX_AND_SUFFIX_TEXT_FOR_RENAME = 1 << 6;
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct SemanticMeaning: u8 {
        const VALUE     = 1 << 0;
        const TYPE      = 1 << 1;
        const NAMESPACE = 1 << 2;
    }
}

impl FindAllReferenceFlags {
    pub fn use_flags(&self) -> FindAllReferenceFlags {
        const USE_FLAGS: FindAllReferenceFlags = FindAllReferenceFlags::USE_OTHER
            .union(FindAllReferenceFlags::USE_REFERENCES.union(FindAllReferenceFlags::USE_RENAME));
        self.intersection(USE_FLAGS)
    }
}

pub struct CompilerResult<'cx, FS: CachedFileSystem> {
    root: PathBuf,
    diags: Vec<bolt_ts_errors::Diag>,
    fs: FS,

    checker: TyChecker<'cx>,
    files: Vec<(ModuleID, OutputFile)>,
}

fn is_for_rename_with_prefix_and_suffix_text(flags: FindAllReferenceFlags) -> bool {
    flags.use_flags() == FindAllReferenceFlags::USE_RENAME
        && flags.contains(FindAllReferenceFlags::PROVIDE_PREFIX_AND_SUFFIX_TEXT_FOR_RENAME)
}

impl<'cx, FS: CachedFileSystem> CompilerResult<'cx, FS> {
    pub fn module_arena(&self) -> &ModuleArena {
        &self.checker.module_arena
    }

    pub fn new(
        root: PathBuf,
        diags: Vec<bolt_ts_errors::Diag>,
        fs: FS,
        files: Vec<(ModuleID, OutputFile)>,
        checker: TyChecker<'cx>,
    ) -> Self {
        Self {
            root,
            diags,
            fs,
            checker,
            files,
        }
    }

    pub fn steal_output_files(&mut self) -> FxHashMap<PathBuf, String> {
        let p = |m: ModuleID| self.checker.module_arena.get_path(m);
        let files = std::mem::take(&mut self.files);
        match self.checker.config.compiler_options().out_dir() {
            bolt_ts_config::OutDir::OwnRoot => files
                .into_iter()
                .flat_map(|(m, file)| {
                    let p = p(m);
                    let js_output_path = p.with_extension("js");
                    let dts_output_path = p.with_extension("d.ts");
                    vec![
                        (js_output_path, file.js_code),
                        (dts_output_path, file.dts_code),
                    ]
                })
                .collect(),
            bolt_ts_config::OutDir::Custom(dir) => {
                let dir = self.root.join(dir);
                files
                    .into_iter()
                    .flat_map(|(m, file)| {
                        let file_path = p(m);
                        let file_name = file_path.file_name().unwrap();
                        let p = dir.join(file_name);
                        let js_output_path = p.with_extension("js");
                        let dts_output_path = p.with_extension("d.ts");
                        vec![
                            (js_output_path, file.js_code),
                            (dts_output_path, file.dts_code),
                        ]
                    })
                    .collect()
            }
        }
    }

    pub fn steal_diags(&mut self) -> Vec<bolt_ts_errors::Diag> {
        std::mem::take(&mut self.diags)
    }

    pub fn steal_module_arena(&mut self) -> ModuleArena {
        std::mem::replace(&mut self.checker.module_arena, ModuleArena::new(0))
    }

    pub fn type_count(&self) -> usize {
        self.checker.ty_len()
    }

    pub fn steal_fs(&mut self) -> FS {
        std::mem::take(&mut self.fs)
    }

    pub fn steal_atoms(&mut self) -> AtomIntern {
        std::mem::replace(&mut self.checker.atoms, AtomIntern::prefill(&[]))
    }

    pub fn get_implementation_at_position(
        &self,
        module: ModuleID,
        pos: usize,
    ) -> Vec<ast::Node<'cx>> {
        let parser_result = self.checker.p.get(module);
        let root = self.checker.p.root(module);
        let Some(node) =
            bolt_ts_parser::get_touching_property_name(root, pos as u32, &parser_result.nodes)
        else {
            return vec![];
        };

        let Some(result) = self.get_implementation_reference_entries(root, node, pos) else {
            return vec![];
        };
        let mut result = result
            .into_iter()
            .map(|entry| self.checker.p.node(entry.node))
            .collect::<Vec<_>>();
        // TODO: remove dedup
        result.dedup_by(|a, b| a.id() == b.id());
        result
    }

    fn get_implementation_reference_entries(
        &self,
        root: &'cx ast::Program<'cx>,
        node: ast::NodeID,
        position: usize,
    ) -> Option<Vec<Entry>> {
        if node == root.id {
            return None;
        }
        let p = self.checker.binder.parent(node).unwrap();
        let p_node = self.checker.p.node(p);
        if p_node.is_object_shorthand_member() {
            todo!()
        } else if p_node.is_super_call() || self.checker.p.node(node).is_super_expr() {
            todo!()
        } else {
            let symbol_and_entires = self.get_reference_entires_for_node(
                position,
                node,
                root,
                FindAllReferenceFlags::IMPLEMENTATIONS.union(FindAllReferenceFlags::USE_REFERENCES),
            )?;
            Some(
                symbol_and_entires
                    .into_iter()
                    .flat_map(|item| item.references)
                    .collect(),
            )
        }
    }

    fn get_reference_entires_for_node(
        &self,
        position: usize,
        node: ast::NodeID,
        root: &'cx ast::Program<'cx>,
        flags: FindAllReferenceFlags,
    ) -> Option<Vec<SymbolAndEntires<'cx>>> {
        self.get_referenced_symbols_for_node(node, flags, root)
    }

    fn get_referenced_symbols_for_node(
        &self,
        node: ast::NodeID,
        flags: FindAllReferenceFlags,
        root: &'cx ast::Program<'cx>,
    ) -> Option<Vec<SymbolAndEntires<'cx>>> {
        let node = self.get_adjust_node(node, flags);
        let n: bolt_ts_ast::Node<'_> = self.checker.p.node(node);
        if n.is_program() {
            todo!()
        }
        if !flags.contains(FindAllReferenceFlags::IMPLEMENTATIONS) {
            todo!()
        }
        // self
        let node = if n.is_class_ctor() {
            self.checker.binder.parent(node).unwrap_or(node)
        } else {
            node
        };
        let Some(symbol) = self.checker.get_symbol_at_location(node) else {
            if !flags.contains(FindAllReferenceFlags::IMPLEMENTATIONS)
                && self.checker.p.node(node).is_string_literal_like()
            {
                todo!()
            }
            return None;
        };

        let s = self.checker.symbol(symbol);
        let s_flags = s.flags;
        if s.name == SymbolName::ExportEquals {
            todo!()
        }
        let module_references =
            self.get_referenced_symbols_for_module_if_declared_by_source_file(symbol);

        if let Some(module_references) = module_references
            && !s_flags.contains(SymbolFlags::TRANSIENT)
        {
            return Some(module_references);
        }

        let alias_symbol =
            self.get_merged_aliased_symbol_of_namespace_export_declaration(node, symbol);
        let module_reference_of_export_target = alias_symbol.map(|alias_symbol| {
            self.get_referenced_symbols_for_module_if_declared_by_source_file(alias_symbol)
        });
        self.get_referenced_symbols_for_symbol(symbol, Some(node), flags)
        // TODO: merge references
    }

    fn get_referenced_symbols_for_symbol(
        &self,
        original_symbol: SymbolID,
        node: Option<ast::NodeID>,
        flags: FindAllReferenceFlags,
    ) -> Option<Vec<SymbolAndEntires<'cx>>> {
        let symbol = node
            .and_then(|node| {
                let use_local_symbol_for_export_specifier =
                    !is_for_rename_with_prefix_and_suffix_text(flags);
                self.skip_past_export_or_import_specifier_or_union(
                    original_symbol,
                    node,
                    use_local_symbol_for_export_specifier,
                )
            })
            .unwrap_or(original_symbol);
        let search_meaning = if let Some(node) = node
            && flags.use_flags() != FindAllReferenceFlags::USE_RENAME
        {
            self.get_intersecting_meaning_from_declarations(node, symbol)
        } else {
            SemanticMeaning::all()
        };
        let mut state = FindReferencesState {
            special_search_kind: node
                .map(|node| get_special_search_kind(self.checker.p.node(node)))
                .unwrap_or(SpecialSearchKind::None),
            search_meaning,
            result: vec![],
        };
        // TODO: export_specifier
        // TODO: node
        let search = state.create_search(&self.checker, symbol);
        self.get_references_in_container_or_files(symbol, &search, &mut state);

        Some(state.result)
    }

    fn get_references_in_container_or_files(
        &self,
        symbol: SymbolID,
        search: &FindReferencesSearch,
        state: &mut FindReferencesState,
    ) {
        // TODO: symbol scope
        for &module in self.checker.module_arena.modules() {
            self.search_for_name(module.id(), search, state);
        }
    }

    fn search_for_name(
        &self,
        module: ModuleID,
        search: &FindReferencesSearch,
        state: &mut FindReferencesState,
    ) {
        // TODO: name table
        let root = self.checker.p.root(module);
        self.get_references_in_source_file(module, root, search, state);
    }

    fn get_references_in_source_file(
        &self,
        module: ModuleID,
        root: &'cx ast::Program<'cx>,
        search: &FindReferencesSearch,
        state: &mut FindReferencesState,
    ) {
        // TODO: cancel
        self.get_references_in_container(module, root, root.id, search, state);
    }

    fn get_possible_symbol_reference_positions(
        &self,
        source_text: &str,
        symbol: SymbolName,
        container_span: bolt_ts_span::Span,
    ) -> Vec<usize> {
        let Some(text) = symbol.as_atom() else {
            todo!()
        };
        let mut positions = vec![];
        let text = self.checker.atoms.get(text).as_bytes();
        let source_text = source_text.as_bytes();

        let lo = container_span.lo() as usize;
        let mut position = find_position(source_text, lo, text);
        while let Some(pos) = position {
            if pos > container_span.hi() as usize {
                break;
            }
            let end_pos = pos + text.len();
            // TODO: check is_identifier_part
            positions.push(pos);
            position = find_position(source_text, pos + text.len() + 1, text);
        }

        positions
    }

    fn get_references_in_container(
        &self,
        module: ModuleID,
        root: &'cx ast::Program<'cx>,
        container: ast::NodeID,
        search: &FindReferencesSearch,
        state: &mut FindReferencesState,
    ) {
        // TODO: mark search symbols

        let container_span = self.checker.p.node(container).span();
        let source_text = self.checker.module_arena.get_content(module);
        for position in
            self.get_possible_symbol_reference_positions(source_text, search.name, container_span)
        {
            // TODO: add_reference_here
            self.get_references_at_position(module, root, position, search, state, true);
        }
    }

    fn get_references_at_position(
        &self,
        module: ModuleID,
        root: &'cx ast::Program<'cx>,
        position: usize,
        search: &FindReferencesSearch,
        state: &mut FindReferencesState,
        add_reference_here: bool,
    ) {
        let reference_location =
            get_touching_property_name(root, position as u32, &self.checker.p.get(module).nodes);
        let Some(reference_location) = reference_location else {
            return;
        };
        let Some(reference_symbol) = self.checker.get_symbol_at_location(reference_location) else {
            return;
        };

        match state.special_search_kind {
            SpecialSearchKind::None => {
                if add_reference_here {
                    self.add_reference(reference_location, reference_symbol, state);
                }
            }
            _ => {
                todo!()
            }
        }
    }

    fn add_reference(
        &self,
        reference_location: ast::NodeID,
        reference_symbol: SymbolID,
        state: &mut FindReferencesState,
    ) {
        state.result.push(SymbolAndEntires {
            definition: Some(Definition::Symbol {
                symbol: reference_symbol,
            }),
            references: vec![Entry {
                node: reference_location,
            }],
        });
    }

    fn get_meaning_from_location(&self, node: ast::NodeID) -> SemanticMeaning {
        let node = self.get_adjust_reference_location(node);
        let parent = self.checker.binder.parent(node);
        let parent_node = parent.map(|p| self.checker.p.node(p));
        let n = self.checker.p.node(node);
        let nq = node_query(node.module(), &self.checker.p, &self.checker.binder);
        if n.is_program() {
            SemanticMeaning::VALUE
        } else if let Some(parent_node) = parent_node
            && match parent_node {
                ast::Node::ExportAssign(_)
                | ast::Node::ExportShorthandSpec(_)
                | ast::Node::ExternalModuleReference(_)
                | ast::Node::ImportShorthandSpec(_)
                | ast::Node::ImportClause(_) => true,
                ast::Node::ImportEqualsDecl(p) if p.name.id == node => true,
                _ => false,
            }
        {
            SemanticMeaning::all()
        } else if nq.is_in_right_side_of_internal_import_equals_declaration(node) {
            todo!()
        } else if nq.is_decl_name(node) {
            todo!()
        }
        // TODO: is entity name expr && jsdoc
        else if nq.is_type_reference(node) {
            SemanticMeaning::TYPE
        }
        // TODO: is_namespace_reference
        else if parent_node.is_some_and(|parent_node| parent_node.is_ty_param()) {
            // TODO: debug_assert! that parent is type js_doc_template_tag
            SemanticMeaning::TYPE
        } else if parent_node.is_some_and(|parent_node| parent_node.is_object_lit_ty()) {
            SemanticMeaning::TYPE.union(SemanticMeaning::VALUE)
        } else {
            SemanticMeaning::VALUE
        }
    }

    fn get_intersecting_meaning_from_declarations(
        &self,
        node: ast::NodeID,
        symbol: SymbolID,
    ) -> SemanticMeaning {
        let mut meaning = self.get_meaning_from_location(node);
        let s = self.checker.symbol(symbol);
        let Some(decls) = s.decls.as_ref() else {
            return meaning;
        };
        let mut last_iteration_meaning;
        loop {
            last_iteration_meaning = meaning;
            for decl in decls {
                let declaration_meaning = self.get_meaning_from_location(*decl);
                if declaration_meaning.intersects(meaning) {
                    meaning |= declaration_meaning;
                }
            }

            if meaning == last_iteration_meaning {
                break;
            }
        }
        meaning
    }

    fn skip_past_export_or_import_specifier_or_union(
        &self,
        symbol: SymbolID,
        node: ast::NodeID,
        use_local_symbol_for_export_specifier: bool,
    ) -> Option<SymbolID> {
        if use_local_symbol_for_export_specifier
            && let Some(p) = self.checker.binder.parent(node)
            && self.checker.p.node(p).is_export_shorthand_spec()
        {
            todo!()
        }

        let s = self.checker.symbol(symbol);
        let decls = s.decls.as_ref()?;
        for decl in decls {
            let p = self.checker.binder.parent(*decl).unwrap();
            let p_node = self.checker.p.node(p);
            let parent_parent = self.checker.binder.parent(p);
            let parent_parent_node = parent_parent.map(|pp| self.checker.p.node(pp));
            if p_node.is_object_lit_ty() && parent_parent_node.map_or(false, |n| n.is_union_ty()) {
                todo!()
            }
        }

        None
    }

    fn get_referenced_symbols_for_module_if_declared_by_source_file(
        &self,
        symbol: SymbolID,
    ) -> Option<Vec<SymbolAndEntires<'cx>>> {
        let s = self.checker.symbol(symbol);
        let module_source_file = if s.flags.intersects(SymbolFlags::MODULE)
            && let Some(decls) = s.decls.as_ref()
            && let Some(module_source_file) = decls
                .iter()
                .find_map(|decl| self.checker.p.node(*decl).as_program())
        {
            module_source_file
        } else {
            return None;
        };
        todo!()
    }

    fn get_merged_aliased_symbol_of_namespace_export_declaration(
        &self,
        node: ast::NodeID,
        symbol: SymbolID,
    ) -> Option<SymbolID> {
        if let Some(p) = self.checker.binder.parent(node)
            && let p_node = self.checker.p.node(p)
        {
            // TODO: is_namespace_export_declaration
        }
        None
    }

    fn get_adjust_node(&self, node: ast::NodeID, flags: FindAllReferenceFlags) -> ast::NodeID {
        let use_flags = flags.use_flags();
        match use_flags {
            FindAllReferenceFlags::USE_REFERENCES => self.get_adjust_reference_location(node),
            FindAllReferenceFlags::USE_RENAME => self.get_adjust_rename_location(node),
            _ => node,
        }
    }

    fn get_adjust_reference_location(&self, node: ast::NodeID) -> ast::NodeID {
        self.get_adjust_location::<false>(node)
    }

    fn get_adjust_rename_location(&self, node: ast::NodeID) -> ast::NodeID {
        self.get_adjust_location::<true>(node)
    }

    fn get_adjust_location<const FOR_NAME: bool>(&self, node: ast::NodeID) -> ast::NodeID {
        // let parent = self.binder.parent(node);
        // TODO: node is modifier
        // TODO: node is `var` or `let` or `const`
        // TODO: node is `type`
        // TODO: node is `as`
        // TODO: node is `import`
        // TODO: node is `export`
        // TODO: node is `require`
        // TODO: node is `from`
        // TODO: node is `extends` or `implements`
        // TODO: node is `infer`
        // TODO: node is `in`
        // TODO: node is `keyof`
        // TODO: node is `readonly`
        if !FOR_NAME {
            // TODO: node is `new`
            // TODO: node is `void`
            // TODO: node is `typeof`
            // TODO: node is `await`
            // TODO: node is `yield`
            // TODO: node is `delete`
            // TODO: node is `in`
            // TODO: node is `as`
            // TODO: node is `keyof`
        }
        node
    }
}

enum SpecialSearchKind {
    None,
    Constructor,
    Class,
}

fn get_special_search_kind(node: ast::Node<'_>) -> SpecialSearchKind {
    match node {
        ast::Node::ClassCtor(_) => SpecialSearchKind::Constructor,
        ast::Node::Ident(_) => {
            // TODO: parent is class like
            SpecialSearchKind::None
        }
        _ => SpecialSearchKind::None,
    }
}

struct FindReferencesState<'cx> {
    special_search_kind: SpecialSearchKind,
    search_meaning: SemanticMeaning,
    result: Vec<SymbolAndEntires<'cx>>,
}

pub struct SymbolAndEntires<'cx> {
    definition: Option<Definition<'cx>>,
    references: Vec<Entry>,
}

pub enum Definition<'cx> {
    Symbol { symbol: SymbolID },
    Label { node: &'cx ast::Ident },
}

impl<'cx> FindReferencesState<'cx> {
    fn create_search(&self, checker: &TyChecker, symbol: SymbolID) -> FindReferencesSearch {
        let name = checker.symbol(symbol).name;
        FindReferencesSearch { name }
    }
}

struct FindReferencesSearch {
    name: SymbolName,
}

pub fn find_position(source: &[u8], start: usize, text: &[u8]) -> Option<usize> {
    for i in start..source.len() - text.len() + 1 {
        if &source[i..i + text.len()] == text {
            return Some(start + i);
        }
    }
    None
}

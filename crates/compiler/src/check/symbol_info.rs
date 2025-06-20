use bolt_ts_ast::keyword;
use bolt_ts_utils::{fx_hashmap_with_capacity, fx_hashset_with_capacity};
use rustc_hash::{FxHashMap, FxHashSet};

use super::resolve_structured_member::MemberOrExportsResolutionKind;
use super::{SymbolLinks, errors};
use crate::bind::{
    MergeSymbol, MergedSymbols, ResolveResult, Symbol, SymbolFlags, SymbolID, SymbolName,
    SymbolTable, Symbols,
};
use crate::check::TyChecker;
use crate::graph::resolve_external_module_name;
use crate::ty::CheckFlags;
use crate::{ir, ty};

fn symbol_of_resolve_results(
    resolve_results: &Vec<ResolveResult>,
    symbol: SymbolID,
) -> &crate::bind::Symbol {
    let idx = symbol.module().as_usize();
    debug_assert!(idx < resolve_results.len());
    unsafe { resolve_results.get_unchecked(idx).symbols.get(symbol) }
}

fn decls_of_symbol<'a>(
    symbol: SymbolID,
    this: &'a TyChecker<'_>,
) -> Option<&'a thin_vec::ThinVec<bolt_ts_ast::NodeID>> {
    let s = symbol_of_resolve_results(this.get_resolve_results(), symbol);
    s.decls.as_ref()
}

fn binder_symbol<'a>(this: &'a TyChecker<'_>, symbol: SymbolID) -> &'a crate::bind::Symbol {
    symbol_of_resolve_results(this.get_resolve_results(), symbol)
}

pub trait SymbolInfo<'cx>: Sized {
    fn arena(&self) -> &'cx bolt_ts_arena::bumpalo::Bump;
    fn empty_symbols(&self) -> &'cx SymbolTable;
    fn mg(&self) -> &crate::graph::ModuleGraph;
    fn p(&self) -> &crate::parser::Parser<'cx>;
    fn atoms(&self) -> &bolt_ts_atom::AtomMap<'cx>;
    fn module_arena(&self) -> &bolt_ts_span::ModuleArena;
    fn push_error(&mut self, error: crate::Diag);

    fn get_symbol_links_map(&mut self) -> &FxHashMap<SymbolID, SymbolLinks<'cx>>;
    fn get_mut_symbol_links_map(&mut self) -> &mut FxHashMap<SymbolID, SymbolLinks<'cx>>;
    fn get_transient_symbol_links_map(&self) -> &Vec<SymbolLinks<'cx>>;
    fn get_mut_transient_symbol_links_map(&mut self) -> &mut Vec<SymbolLinks<'cx>>;
    fn get_transient_symbols(&self) -> &Symbols;
    fn get_mut_transient_symbols(&mut self) -> &mut Symbols;
    fn get_resolve_results(&self) -> &Vec<ResolveResult>;
    fn get_merged_symbols(&self) -> &MergedSymbols;

    fn get_merged_symbol(&self, id: SymbolID) -> SymbolID {
        if id == Symbol::ERR {
            return Symbol::ERR;
        }
        let s = self.get_merged_symbols();
        let symbols = &self.get_resolve_results()[id.module().as_usize()].symbols;
        s.get_merged_symbol(id, symbols)
    }

    fn locals(
        &self,
        module: bolt_ts_span::ModuleID,
    ) -> &FxHashMap<bolt_ts_ast::NodeID, SymbolTable> {
        let idx = module.as_usize();
        let res = self.get_resolve_results();
        debug_assert!(idx < res.len());
        unsafe { &res.get_unchecked(idx).locals }
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.arena().alloc(t)
    }

    fn get_symbol_links(&mut self, symbol: SymbolID) -> &SymbolLinks<'cx> {
        if symbol.module() == bolt_ts_span::ModuleID::TRANSIENT {
            debug_assert!(symbol.index_as_usize() < self.get_transient_symbol_links_map().len());
            unsafe {
                self.get_transient_symbol_links_map()
                    .get_unchecked(symbol.index_as_usize())
            }
        } else {
            self.get_mut_symbol_links_map().entry(symbol).or_default()
        }
    }

    fn get_mut_symbol_links(&mut self, symbol: SymbolID) -> &mut SymbolLinks<'cx> {
        if symbol.module() == bolt_ts_span::ModuleID::TRANSIENT {
            debug_assert!(symbol.index_as_usize() < self.get_transient_symbol_links_map().len());
            unsafe {
                self.get_mut_transient_symbol_links_map()
                    .get_unchecked_mut(symbol.index_as_usize())
            }
        } else {
            self.get_mut_symbol_links_map().get_mut(&symbol).unwrap()
        }
    }

    fn symbol(&self, symbol: SymbolID) -> &Symbol {
        if symbol.module() == bolt_ts_span::ModuleID::TRANSIENT {
            self.get_transient_symbols().get(symbol)
        } else {
            symbol_of_resolve_results(self.get_resolve_results(), symbol)
        }
    }
}

impl<'cx> super::TyChecker<'cx> {
    // TODO: return `Option<&'cx SymbolTable>`
    pub(super) fn members_of_symbol(&mut self, symbol: SymbolID) -> &'cx SymbolTable {
        if let Some(m) = self.get_symbol_links(symbol).get_members() {
            m
        } else {
            let members = self.symbol(symbol).members();
            let members = match members {
                Some(members) => members.clone(),
                None => SymbolTable::new(16),
            };
            let members = self.alloc(members);
            self.get_mut_symbol_links(symbol).set_members(members);
            members
        }
    }

    // TODO: return `Option<&'cx SymbolTable>`
    pub(super) fn exports_of_symbol(&mut self, symbol: SymbolID) -> &'cx SymbolTable {
        if let Some(e) = self.get_symbol_links(symbol).get_exports() {
            e
        } else {
            let exports = self.symbol(symbol).exports();
            let exports = match exports {
                Some(exports) => exports.clone(),
                None => SymbolTable::new(16),
            };
            let exports = self.alloc(exports);
            self.get_mut_symbol_links(symbol).set_exports(exports);
            exports
        }
    }

    pub(super) fn resolve_external_module_name(
        &mut self,
        module_spec_id: bolt_ts_ast::NodeID,
        _module_spec: bolt_ts_atom::AtomId,
    ) -> Option<SymbolID> {
        crate::graph::resolve_external_module_name(self.mg(), module_spec_id, self.p())
    }

    pub(super) fn is_non_local_alias(
        &self,
        symbol: SymbolID,
        excludes: Option<SymbolFlags>,
    ) -> bool {
        const DEFAULT: SymbolFlags = SymbolFlags::VALUE
            .union(SymbolFlags::TYPE)
            .union(SymbolFlags::NAMESPACE);
        let excludes = excludes.unwrap_or(DEFAULT);
        let flags = self.symbol(symbol).flags;
        (flags.intersection(SymbolFlags::ALIAS | excludes) == SymbolFlags::ALIAS)
            || (flags.intersects(SymbolFlags::ALIAS.union(SymbolFlags::ASSIGNMENT)))
    }

    pub(super) fn resolve_alias(&mut self, symbol: SymbolID) -> SymbolID {
        if let Some(alias_target) = self.get_symbol_links(symbol).get_alias_target() {
            return if alias_target == Symbol::RESOLVING {
                self.get_mut_symbol_links(symbol)
                    .override_alias_target(Symbol::ERR);
                Symbol::ERR
            } else {
                alias_target
            };
        };
        self.get_mut_symbol_links(symbol)
            .set_alias_target(Symbol::RESOLVING);
        let s = symbol_of_resolve_results(self.get_resolve_results(), symbol);
        assert!(s.flags.intersects(SymbolFlags::ALIAS), "symbol: {:#?}", s);
        let node = s.get_decl_of_alias_symbol(self.p()).unwrap_or_else(|| {
            let decls = s
                .decls
                .as_ref()
                .unwrap()
                .iter()
                .map(|d| self.p().node(*d))
                .collect::<Vec<_>>();
            panic!("spans of decls: {:#?}", decls);
        });
        let target = self.get_target_of_alias_decl(node, false);
        if self
            .get_symbol_links(symbol)
            .get_alias_target()
            .is_none_or(|alias_target| alias_target == Symbol::RESOLVING)
        {
            let v = target.unwrap_or(Symbol::ERR);
            self.get_mut_symbol_links(symbol).override_alias_target(v);
        } else {
            // TODO: report circle error
        }
        self.get_symbol_links(symbol).expect_alias_target()
    }

    pub(super) fn get_target_of_alias_decl(
        &mut self,
        node: bolt_ts_ast::NodeID,
        dont_recur_resolve: bool,
    ) -> Option<SymbolID> {
        let p = self.p();
        use bolt_ts_ast::Node::*;
        const EXPORT_SPEC_MEANING: SymbolFlags = SymbolFlags::VALUE
            .union(SymbolFlags::TYPE)
            .union(SymbolFlags::NAMESPACE);
        match p.node(node) {
            ImportNamedSpec(_) => get_target_of_import_named_spec(self, node, dont_recur_resolve),
            ShorthandSpec(_) if p.node(p.parent(node).unwrap()).is_import_clause() => {
                get_target_of_import_named_spec(self, node, dont_recur_resolve)
            }
            ShorthandSpec(_) => {
                assert!(p.node(p.parent(node).unwrap()).is_specs_export());
                get_target_of_export_spec(self, node, EXPORT_SPEC_MEANING, dont_recur_resolve)
            }
            ExportNamedSpec(_) => {
                get_target_of_export_spec(self, node, EXPORT_SPEC_MEANING, dont_recur_resolve)
            }
            NsImport(n) => get_target_of_ns_import(self, n, dont_recur_resolve),
            ImportClause(n) => get_target_of_import_clause(self, n, dont_recur_resolve),
            ExportAssign(n) => get_target_of_export_assignment(self, n, dont_recur_resolve),
            _ => todo!(),
        }
    }

    pub(super) fn resolve_symbol(
        &mut self,
        symbol: SymbolID,
        dont_resolve_alias: bool,
    ) -> SymbolID {
        if !dont_resolve_alias && self.is_non_local_alias(symbol, None) {
            self.resolve_alias(symbol)
        } else {
            symbol
        }
    }

    pub(super) fn get_exports_of_symbol(&mut self, symbol: SymbolID) -> &'cx SymbolTable {
        let flags = self.symbol(symbol).flags;
        if flags.intersects(SymbolFlags::LATE_BINDING_CONTAINER) {
            self.get_resolved_members_or_exports_of_symbol(
                symbol,
                MemberOrExportsResolutionKind::ResolvedExports,
            )
        } else if flags.intersects(SymbolFlags::MODULE) {
            self.get_exports_of_module(symbol)
        } else {
            let exports = self.exports_of_symbol(symbol);
            if exports.0.is_empty() {
                self.empty_symbols()
            } else {
                exports
            }
        }
    }

    pub(super) fn get_exports_of_module(&mut self, module_symbol: SymbolID) -> &'cx SymbolTable {
        if let Some(exports) = self.get_symbol_links(module_symbol).get_resolved_exports() {
            return exports;
        }
        let exports = self.get_exports_of_module_worker(module_symbol);
        self.get_mut_symbol_links(module_symbol)
            .set_resolved_exports(exports);
        exports
    }

    pub(super) fn get_export_of_module(
        &mut self,
        symbol: SymbolID,
        name: SymbolName,
        dont_resolve_alias: bool,
    ) -> Option<SymbolID> {
        let s = symbol_of_resolve_results(self.get_resolve_results(), symbol);
        if s.flags.intersects(SymbolFlags::MODULE) {
            let export_symbol = self.get_exports_of_module(symbol).0.get(&name).copied();

            // TODO: mark symbol of alias declaration if type only
            export_symbol
                .map(|export_symbol| self.resolve_symbol(export_symbol, dont_resolve_alias))
        } else {
            None
        }
    }

    pub(super) fn get_exports_of_module_worker(
        &mut self,
        module_symbol: SymbolID,
    ) -> &'cx SymbolTable {
        struct ExportCollisionTracker<'cx> {
            spec: bolt_ts_atom::AtomId,
            exports_with_duplicated: thin_vec::ThinVec<&'cx bolt_ts_ast::ExportDecl<'cx>>,
        }
        struct ExportCollisionTrackerTable<'cx>(FxHashMap<SymbolName, ExportCollisionTracker<'cx>>);

        fn extend_export_symbols<'cx>(
            this: &mut TyChecker<'cx>,
            target: &mut SymbolTable,
            source: Option<SymbolTable>,
            mut lookup_table: Option<&mut ExportCollisionTrackerTable<'cx>>,
            export_node: Option<&'cx bolt_ts_ast::ExportDecl>,
        ) {
            let Some(source) = source else { return };
            for (id, source_symbol) in source.0 {
                // TODO: id == SymbolName::DefaultExport
                match target.0.get(&id).copied() {
                    Some(target_symbol) => {
                        if let Some(lookup_table) = lookup_table.as_mut() {
                            if let Some(export_node) = export_node {
                                if this.resolve_symbol(target_symbol, false)
                                    != this.resolve_symbol(source_symbol, false)
                                {
                                    let collision_tracker = lookup_table.0.get_mut(&id).unwrap();
                                    collision_tracker.exports_with_duplicated.push(export_node);
                                }
                            }
                        }
                    }
                    None => {
                        target.0.insert(id, source_symbol);
                        if let Some(lookup_table) = lookup_table.as_mut() {
                            if let Some(export_node) = export_node {
                                let module_spec = match export_node.clause.kind {
                                    bolt_ts_ast::ExportClauseKind::Glob(node) => node.module.val,
                                    _ => unreachable!(),
                                };
                                lookup_table.0.insert(
                                    id,
                                    ExportCollisionTracker {
                                        spec: module_spec,
                                        exports_with_duplicated: Default::default(),
                                    },
                                );
                            }
                        }
                    }
                }
            }
        }

        fn visit(
            this: &mut TyChecker<'_>,
            symbol: Option<SymbolID>,
            visited: &mut FxHashSet<SymbolID>,
        ) -> Option<SymbolTable> {
            let symbol = symbol?;
            let exports = this.exports_of_symbol(symbol);
            if exports.0.is_empty() || !visited.insert(symbol) {
                return None;
            };
            let mut symbols = exports.clone();
            let Some(export_starts) = exports.0.get(&SymbolName::ExportStar).copied() else {
                return Some(symbols);
            };

            let mut nested_symbols = SymbolTable::new(128);
            let mut lookup_table = ExportCollisionTrackerTable(fx_hashmap_with_capacity(32));
            if let Some(decls) = decls_of_symbol(export_starts, this) {
                for decl in decls.clone() {
                    let node = this.p().node(decl).expect_export_decl();
                    let bolt_ts_ast::ExportClauseKind::Glob(n) = node.clause.kind else {
                        unreachable!()
                    };
                    let resolved_module =
                        this.resolve_external_module_name(n.module.id, n.module.val);
                    let exported_symbols = visit(this, resolved_module, visited);
                    extend_export_symbols(
                        this,
                        &mut nested_symbols,
                        exported_symbols,
                        Some(&mut lookup_table),
                        Some(node),
                    );
                }
            }

            extend_export_symbols(this, &mut symbols, Some(nested_symbols), None, None);
            Some(symbols)
        }

        let mut visited = fx_hashset_with_capacity(32);
        let symbols = visit(self, Some(module_symbol), &mut visited);
        // TODO: type_only_export_start_map

        if let Some(symbols) = symbols {
            self.alloc(symbols)
        } else {
            self.empty_symbols()
        }
    }

    pub(super) fn get_resolved_members_or_exports_of_symbol(
        &mut self,
        symbol: SymbolID,
        kind: MemberOrExportsResolutionKind,
    ) -> &'cx SymbolTable {
        let get_kind = |this: &mut Self| {
            let links = this.get_symbol_links(symbol);
            match kind {
                MemberOrExportsResolutionKind::ResolvedExports => links.get_resolved_exports(),
                MemberOrExportsResolutionKind::ResolvedMembers => links.get_resolved_members(),
            }
        };
        if let Some(resolved) = get_kind(self) {
            return resolved;
        }
        let is_static = matches!(kind, MemberOrExportsResolutionKind::ResolvedExports);
        let flags = self.symbol(symbol).flags;
        let early_symbols = if !is_static {
            self.members_of_symbol(symbol)
        } else if flags.intersects(SymbolFlags::MODULE) {
            self.get_exports_of_module_worker(symbol)
        } else {
            let exports = self.exports_of_symbol(symbol);
            if exports.0.is_empty() {
                self.empty_symbols()
            } else {
                exports
            }
        };
        let links = self.get_mut_symbol_links(symbol);
        match kind {
            MemberOrExportsResolutionKind::ResolvedExports => {
                links.set_resolved_exports(early_symbols)
            }
            MemberOrExportsResolutionKind::ResolvedMembers => {
                links.set_resolved_members(early_symbols)
            }
        }
        let s = self.symbol(symbol);
        let decls: thin_vec::ThinVec<_> = s.decls.clone().unwrap_or_default();
        let mut late_symbols = SymbolTable::new(decls.len());
        for decl in decls {
            let n = self.p().node(decl);
            use bolt_ts_ast::Node::*;
            match n {
                InterfaceDecl(n) => handle_members_for_label_symbol(
                    self,
                    symbol,
                    early_symbols,
                    &mut late_symbols,
                    n.members,
                    is_static,
                ),
                ClassDecl(n) => handle_members_for_label_symbol(
                    self,
                    symbol,
                    early_symbols,
                    &mut late_symbols,
                    n.elems.elems,
                    is_static,
                ),
                ClassExpr(n) => handle_members_for_label_symbol(
                    self,
                    symbol,
                    early_symbols,
                    &mut late_symbols,
                    n.elems.elems,
                    is_static,
                ),
                ObjectLitTy(n) => handle_members_for_label_symbol(
                    self,
                    symbol,
                    early_symbols,
                    &mut late_symbols,
                    n.members,
                    is_static,
                ),
                _ => (),
            };
        }

        let combined = if early_symbols.0.is_empty() {
            self.alloc(late_symbols)
        } else if late_symbols.0.is_empty() {
            early_symbols
        } else {
            let mut combined = SymbolTable::new(late_symbols.0.len() + early_symbols.0.len());
            self.merge_symbol_table_owner(&mut combined, early_symbols, false);
            self.merge_symbol_table_owner(&mut combined, &late_symbols, false);
            self.alloc(combined)
        };
        let links = self.get_mut_symbol_links(symbol);
        match kind {
            MemberOrExportsResolutionKind::ResolvedExports => {
                links.override_resolved_exports(combined)
            }
            MemberOrExportsResolutionKind::ResolvedMembers => {
                links.override_resolved_members(combined)
            }
        }
        combined
    }

    fn merge_symbol_table_owner(
        &mut self,
        target: &mut SymbolTable,
        source: &SymbolTable,
        unidirectional: bool,
    ) {
        for (id, source_symbol) in &source.0 {
            let target_symbol = target.0.get(id).copied();
            let merged = if let Some(target_symbol) = target_symbol {
                self.merge_symbol(target_symbol, *source_symbol, unidirectional)
            } else {
                let symbols = &self.get_symbols(source_symbol.module());
                self.merged_symbols
                    .get_merged_symbol(*source_symbol, symbols)
            };
            // TODO: parent
            target.0.insert(*id, merged);
        }
    }

    pub(super) fn resolve_symbol_by_ident(&self, ident: &'cx bolt_ts_ast::Ident) -> SymbolID {
        let id = ident.id;
        self.get_resolve_results()[id.module().as_usize()]
            .final_res
            .get(&id)
            .copied()
            .unwrap_or_else(|| {
                let n = self.p().node(id);
                let Some(node) = n.as_ident() else {
                    unreachable!("final_res not found for {:#?}", n);
                };
                let name = self.atoms().get(node.name);
                let span = self.p().node(id).span();
                let module = self.module_arena().get_path(span.module).display();
                panic!(
                    "The resolution of `{name}({module}:{}:{})` is not found.",
                    span.lo, span.hi
                );
            })
    }

    pub(super) fn resolve_entity_name(
        &mut self,
        name: &'cx bolt_ts_ast::EntityName<'cx>,
        meaning: SymbolFlags,
        dont_resolve_alias: bool,
    ) -> SymbolID {
        use bolt_ts_ast::EntityNameKind::*;
        let symbol;
        match name.kind {
            Ident(n) => {
                let id = self.resolve_symbol_by_ident(n);
                symbol = self.get_merged_symbol(id);
                if symbol == Symbol::ERR || dont_resolve_alias {
                    return symbol;
                }
            }
            Qualified(n) => {
                let ns = self.resolve_entity_name(n.left, SymbolFlags::NAMESPACE, false);
                if ns == Symbol::ERR {
                    return Symbol::ERR;
                }
                let exports = self.get_exports_of_symbol(ns);
                symbol = self
                    .get_symbol(exports, SymbolName::Atom(n.right.name), meaning)
                    .unwrap_or(Symbol::ERR);
            }
        }
        let flags = self.symbol(symbol).flags;
        if flags.intersects(meaning) {
            symbol
        } else if flags.intersects(SymbolFlags::ALIAS) {
            self.resolve_alias(symbol)
        } else {
            Symbol::ERR
        }
    }

    pub(super) fn get_symbol(
        &mut self,
        symbols: &'cx SymbolTable,
        name: SymbolName,
        meaning: SymbolFlags,
    ) -> Option<SymbolID> {
        if !meaning.is_empty() {
            if let Some(symbol) = symbols.0.get(&name).copied() {
                let symbol = self.get_merged_symbol(symbol);
                let flags = symbol_of_resolve_results(self.get_resolve_results(), symbol).flags;
                if flags.intersects(meaning) {
                    return Some(symbol);
                } else if flags.intersects(SymbolFlags::ALIAS) {
                    let target_flags = self.get_symbol_flags(symbol, false);
                    if target_flags.intersects(meaning) {
                        return Some(symbol);
                    }
                }
            }
        }
        None
    }

    pub(super) fn get_symbol_flags(
        &mut self,
        mut symbol: SymbolID,
        exclude_ty_only_meaning: bool,
    ) -> SymbolFlags {
        let mut seen_symbols = fx_hashset_with_capacity(32);
        let mut symbol_flags = self.symbol(symbol).flags;
        let mut flags = symbol_flags;
        while symbol_flags.intersects(SymbolFlags::ALIAS) {
            let target = self.resolve_alias(symbol);
            let target = self.get_export_symbol_of_value_symbol_if_exported(target);
            if target == Symbol::ERR {
                return SymbolFlags::all();
            } else if target == symbol || seen_symbols.contains(&target) {
                break;
            }
            let t = self.symbol(target);
            let t_flags = t.flags;
            if t_flags.intersects(SymbolFlags::ALIAS) {
                seen_symbols.insert(target);
            }

            flags |= t_flags;
            symbol = target;
            symbol_flags = t_flags;
        }
        flags
    }

    pub(super) fn get_export_symbol_of_value_symbol_if_exported(
        &mut self,
        symbol: SymbolID,
    ) -> SymbolID {
        // TODO: get merged symbol
        let s = symbol_of_resolve_results(self.get_resolve_results(), symbol);
        if s.flags.intersects(SymbolFlags::VALUE) {
            s.export_symbol.unwrap_or(symbol)
        } else {
            symbol
        }
    }

    fn has_late_bindable_name(&mut self, id: bolt_ts_ast::NodeID) -> bool {
        let Some(name) = self.p().get_name_of_decl(id) else {
            return false;
        };
        self.is_late_bindable_name(&name)
    }

    fn is_late_bindable_name(&mut self, name: &bolt_ts_ast::DeclarationName<'cx>) -> bool {
        name.is_late_bindable_ast() && {
            let ty = if let bolt_ts_ast::DeclarationName::Computed(n) = name {
                self.check_computed_prop_name(n)
            } else {
                todo!("element access")
            };
            ty.useable_as_prop_name()
        }
    }

    pub(super) fn has_late_bindable_index_signature(&mut self, id: bolt_ts_ast::NodeID) -> bool {
        let Some(name) = self.p().get_name_of_decl(id) else {
            return false;
        };
        self.is_late_bindable_index_signature(&name)
    }

    fn is_late_bindable_index_signature(
        &mut self,
        name: &bolt_ts_ast::DeclarationName<'cx>,
    ) -> bool {
        name.is_late_bindable_ast() && {
            let ty = if let bolt_ts_ast::DeclarationName::Computed(n) = name {
                self.check_computed_prop_name(n)
            } else {
                todo!("element access")
            };
            self.is_ty_usable_as_index_signature(ty)
        }
    }

    fn is_ty_usable_as_index_signature(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.is_type_assignable_to(ty, self.string_number_symbol_ty())
    }

    fn add_decl_to_late_bound_symbol(
        &mut self,
        symbol: SymbolID,
        decl: bolt_ts_ast::NodeID,
        flags: SymbolFlags,
    ) {
        assert!(self.get_check_flags(symbol).intersects(CheckFlags::LATE));
        let decl_s = self.get_symbol_of_decl(decl);
        let s = self
            .binder
            .bind_results
            .last_mut()
            .unwrap()
            .symbols
            .get_mut(symbol);
        s.flags |= flags;
        if s.decls.is_none() {
            s.decls = Some([decl].into());
        }
        // TODO: is_replace_by_method
        if flags.intersects(SymbolFlags::VALUE)
            && s.value_decl
                .is_none_or(|d| self.p.node(d).is_same_kind(&self.p.node(decl)))
        {
            s.value_decl = Some(decl);
        }
        assert_ne!(decl_s.module(), bolt_ts_span::ModuleID::TRANSIENT);
        self.symbol_links
            .entry(decl_s)
            .or_default()
            .set_late_symbol(symbol);
    }

    fn late_bind_member(
        &mut self,
        parent: SymbolID,
        early_symbols: &'cx SymbolTable,
        late_symbols: &mut SymbolTable,
        decl: bolt_ts_ast::NodeID,
    ) -> SymbolID {
        if let Some(s) = self.get_node_links(decl).get_resolved_symbol() {
            return s;
        }
        let s = self.get_symbol_of_decl(decl);
        self.get_mut_node_links(decl).set_resolved_symbol(s);
        // TODO: binary expression
        let decl_name = self.p.node(decl).name().unwrap();
        let ty = match decl_name {
            bolt_ts_ast::DeclarationName::Computed(n) => self.check_computed_prop_name(n),
            // TODO: element access
            _ => unreachable!("decl_name: {:#?}", decl_name),
        };
        if ty.useable_as_prop_name() {
            let member_name = self.get_prop_name_from_ty(ty).unwrap();
            let symbol_flags = self.binder.symbol(s).flags;
            use std::collections::hash_map::Entry;
            let late_symbol = match late_symbols.0.entry(member_name) {
                Entry::Occupied(occ) => *occ.get(),
                Entry::Vacant(vac) => {
                    // TODO: decls and value_declaration
                    let links = SymbolLinks::default()
                        .with_check_flags(CheckFlags::LATE)
                        .with_name_ty(ty);
                    let s = self.create_transient_symbol(
                        member_name,
                        SymbolFlags::TRANSIENT,
                        links,
                        None,
                        None,
                    );
                    vac.insert(s);
                    s
                }
            };
            // TODO: !self.symbol(parent).flags().intersection(SymbolFlags::Class)
            self.add_decl_to_late_bound_symbol(late_symbol, decl, symbol_flags);
            self.get_mut_node_links(decl)
                .override_resolved_symbol(late_symbol);
            late_symbol
        } else {
            s
        }
    }

    fn clone_symbol(
        &mut self,
        symbol: SymbolID,
        get_links: impl FnOnce(&Symbol) -> SymbolLinks<'cx>,
    ) -> SymbolID {
        let s = self.symbol(symbol);
        let links = get_links(s);
        let target = self.create_transient_symbol(
            s.name,
            s.flags | SymbolFlags::TRANSIENT,
            links,
            s.decls.clone(),
            s.value_decl,
        );
        let symbols = if symbol.module() == bolt_ts_span::ModuleID::TRANSIENT {
            &mut self.binder.bind_results.last_mut().unwrap().symbols
        } else {
            &mut self.binder.bind_results[symbol.module().as_usize()].symbols
        };
        self.merged_symbols
            .record_merged_symbol(target, symbol, symbols);
        target
    }

    fn late_bind_index_signature(
        &mut self,
        early_symbols: &'cx SymbolTable,
        late_symbols: &mut SymbolTable,
        decl: bolt_ts_ast::NodeID,
    ) {
        let late_symbol = match late_symbols.0.get(&SymbolName::Index).copied() {
            Some(index_symbol) => index_symbol,
            None => {
                let late_symbol = match early_symbols.0.get(&SymbolName::Index).copied() {
                    Some(s) => self.clone_symbol(s, |_| {
                        SymbolLinks::default().with_check_flags(CheckFlags::LATE)
                    }),
                    None => self.create_transient_symbol(
                        SymbolName::Index,
                        SymbolFlags::TRANSIENT,
                        SymbolLinks::default().with_check_flags(CheckFlags::LATE),
                        None,
                        None,
                    ),
                };
                late_symbols.0.insert(SymbolName::Index, late_symbol);
                late_symbol
            }
        };
        let s = self.get_mut_transient_symbols().get_mut(late_symbol);
        if s.decls.is_none() {
            s.decls = Some([decl].into());
        }
        // TODO: is_replace_by_method
    }
}

fn handle_members_for_label_symbol<'cx>(
    this: &mut TyChecker<'cx>,
    symbol: SymbolID,
    early_symbols: &'cx SymbolTable,
    late_symbols: &mut SymbolTable,
    members: &[&'cx impl ir::MembersOfDecl],
    is_static: bool,
) {
    for member in members {
        if is_static == member.has_static_modifier() {
            let id = member.id();
            if this.has_late_bindable_name(id) {
                this.late_bind_member(symbol, early_symbols, late_symbols, id);
            } else if this.has_late_bindable_index_signature(id) {
                this.late_bind_index_signature(early_symbols, late_symbols, id);
            }
        }
    }
}

fn get_target_of_ns_import<'cx>(
    this: &mut TyChecker<'cx>,
    n: &'cx bolt_ts_ast::NsImport<'cx>,
    dont_resolve_alias: bool,
) -> Option<SymbolID> {
    let p = this.p();
    let p_id = p.parent(n.id).unwrap();
    let p_id = p.parent(p_id).unwrap();
    let import_decl = p.node(p_id).expect_import_decl();

    // TODO: resolve_es_module;
    this.resolve_external_module_name(import_decl.module.id, import_decl.module.val)
}

fn get_target_of_import_named_spec(
    this: &mut TyChecker<'_>,
    node: bolt_ts_ast::NodeID,
    dont_recur_resolve: bool,
) -> Option<SymbolID> {
    let p = this.p();
    let root = if p.node(node).is_binding() {
        p.get_root_decl(node)
    } else {
        let p_id = p.parent(node).unwrap();

        p.parent(p_id).unwrap()
    };

    get_external_module_member(this, root, node, dont_recur_resolve)
}

fn get_external_module_member(
    this: &mut TyChecker<'_>,
    node: bolt_ts_ast::NodeID,
    spec: bolt_ts_ast::NodeID,
    dont_recur_resolve: bool,
) -> Option<SymbolID> {
    let module_spec = match this.p().node(node) {
        bolt_ts_ast::Node::ImportDecl(n) => n.module,
        bolt_ts_ast::Node::SpecsExport(n) => n.module.unwrap(),
        _ => todo!("node: {:#?}", this.p().node(node)),
    };
    let module_symbol = this.resolve_external_module_name(module_spec.id, module_spec.val);
    // TODO: target_symbol
    let target_symbol = module_symbol;
    if let Some(target_symbol) = target_symbol {
        let name = match this.p().node(spec) {
            bolt_ts_ast::Node::ImportNamedSpec(n) => match n.prop_name.kind {
                bolt_ts_ast::ModuleExportNameKind::Ident(ident) => ident.name,
                bolt_ts_ast::ModuleExportNameKind::StringLit(_) => todo!(),
            },
            bolt_ts_ast::Node::ShorthandSpec(n) => n.name.name,
            bolt_ts_ast::Node::ExportNamedSpec(n) => match n.prop_name.kind {
                bolt_ts_ast::ModuleExportNameKind::Ident(n) => n.name,
                bolt_ts_ast::ModuleExportNameKind::StringLit(_) => todo!(),
            },
            _ => todo!(),
        };
        let symbol_name = SymbolName::Atom(name);
        let symbol_from_module =
            this.get_export_of_module(target_symbol, symbol_name, dont_recur_resolve);
        if symbol_from_module.is_none() {
            error_no_module_member_symbol(this, module_symbol.unwrap(), module_spec.val, spec);
        }
        symbol_from_module
    } else {
        None
    }
}

fn error_no_module_member_symbol(
    this: &mut TyChecker<'_>,
    module_symbol: SymbolID,
    module_name: bolt_ts_atom::AtomId,
    spec_name_id: bolt_ts_ast::NodeID,
) {
    let spec_name = this
        .p()
        .node(spec_name_id)
        .import_export_spec_name()
        .unwrap();
    report_non_exported_member(this, spec_name_id, spec_name, module_symbol, module_name);
}

fn report_non_exported_member(
    this: &mut TyChecker<'_>,
    spec_name_id: bolt_ts_ast::NodeID,
    spec_name: bolt_ts_atom::AtomId,
    module_symbol: SymbolID,
    module_name: bolt_ts_atom::AtomId,
) {
    let s = symbol_of_resolve_results(this.get_resolve_results(), module_symbol);
    let local_symbol = if let Some(value_decl) = s.value_decl {
        this.locals(value_decl.module())
            .get(&value_decl)
            .unwrap()
            .0
            .get(&SymbolName::Atom(spec_name))
            .copied()
    } else {
        None
    };
    if let Some(local_symbol) = local_symbol {
        let decl = s.exports().and_then(|exports| {
            exports.0.values().find_map(|export| {
                // TODO: use `get_symbol_if_same_reference`
                let s = symbol_of_resolve_results(this.get_resolve_results(), *export);
                if s.flags == SymbolFlags::ALIAS {
                    let decl = s.decls.as_ref().unwrap()[0];
                    if let Some(spec) = this.p().node(decl).as_export_named_spec() {
                        match spec.prop_name.kind {
                            bolt_ts_ast::ModuleExportNameKind::Ident(ident)
                                if ident.name == spec_name =>
                            {
                                return Some(decl);
                            }
                            _ => (),
                        }
                    }
                }
                None
            })
        });
        let symbol_span = this
            .p()
            .node(decls_of_symbol(local_symbol, this).as_ref().unwrap()[0])
            .ident_name()
            .unwrap()
            .span;
        let error: crate::Diag = if let Some(decl) = decl {
            let mut helper = vec![];
            helper.push(
                errors::ModuleADeclaresBLocallyButItIsExportedAsCHelperKind::NameIsDeclaredHere(
                    errors::NameIsDeclaredHere {
                        span: symbol_span,
                        name: this.atoms().get(spec_name).to_string(),
                    },
                ),
            );
            let target_name = match this.p().node(decl) {
                bolt_ts_ast::Node::ExportNamedSpec(n) => match n.name.kind {
                    bolt_ts_ast::ModuleExportNameKind::Ident(ident) => ident.name,
                    bolt_ts_ast::ModuleExportNameKind::StringLit(lit) => lit.val,
                },
                _ => unreachable!(),
            };
            helper.push(
                errors::ModuleADeclaresBLocallyButItIsExportedAsCHelperKind::ExportedAliasHere(
                    errors::ExportedAliasHere {
                        span: this.p().node(decl).span(),
                        name: this.atoms().get(spec_name).to_string(),
                    },
                ),
            );
            let error = errors::ModuleADeclaresBLocallyButItIsExportedAsC {
                span: this.p().node(spec_name_id).span(),
                module_name: this.atoms().get(module_name).to_string(),
                symbol_name: this.atoms().get(spec_name).to_string(),
                target_name: this.atoms().get(target_name).to_string(),
                related: helper,
            };
            Box::new(error)
        } else {
            Box::new(errors::ModuleADeclaresBLocallyButItIsNotExported {
                span: this.p().node(spec_name_id).span(),
                module_name: this.atoms().get(module_name).to_string(),
                symbol_name: this.atoms().get(spec_name).to_string(),
                related: [errors::NameIsDeclaredHere {
                    span: symbol_span,
                    name: this.atoms().get(spec_name).to_string(),
                }],
            })
        };
        this.push_error(error);
    } else {
        use bolt_ts_ast::Node::*;
        let span = match this.p().node(spec_name_id) {
            ShorthandSpec(n) => n.span,
            ExportNamedSpec(n) => n.prop_name.span(),
            _ => unreachable!(),
        };
        let error = errors::ModuleXHasNoExportedMemberY {
            span,
            module: this.atoms().get(module_name).to_string(),
            member: this.atoms().get(spec_name).to_string(),
        };
        this.push_error(Box::new(error));
    }
}

fn get_target_of_export_spec(
    this: &mut TyChecker<'_>,
    node: bolt_ts_ast::NodeID,
    meaning: SymbolFlags,
    dont_resolve_alias: bool,
) -> Option<SymbolID> {
    let n = this.p().node(node);
    let spec_name = n.import_export_spec_name().unwrap();
    if spec_name == keyword::KW_DEFAULT {
        let spec = this.p().get_module_spec_for_import_or_export(node);
        let module_symbol =
            spec.and_then(|spec| this.resolve_external_module_name(spec.id, spec.val));
        if let Some(module_symbol) = module_symbol {
            return get_target_of_module_default(this, module_symbol, node, dont_resolve_alias);
        }
    }

    let resolved = match n {
        bolt_ts_ast::Node::ShorthandSpec(n) => {
            let p_id = this.p().parent(node).unwrap();
            let p = this.p().node(p_id).expect_specs_export();
            if p.module.is_some() {
                get_external_module_member(this, p_id, node, dont_resolve_alias)
            } else {
                this.get_resolve_results()[n.id.module().as_usize()]
                    .final_res
                    // COMMENT: pay attention to `n.name.id`, `n.id` refers to itself, but `n.name.id` refers to the result in early_resolve.
                    .get(&n.name.id)
                    .copied()
            }
        }
        bolt_ts_ast::Node::ExportNamedSpec(n) => match n.prop_name.kind {
            bolt_ts_ast::ModuleExportNameKind::Ident(ident) => {
                let p = this.p().parent(node).unwrap();
                let p = this.p().node(p).expect_specs_export();
                if p.module.is_some() {
                    todo!()
                } else {
                    this.get_resolve_results()[n.id.module().as_usize()]
                        .final_res
                        .get(&n.id)
                        .copied()
                }
            }
            bolt_ts_ast::ModuleExportNameKind::StringLit(_) => todo!(),
        },
        _ => unreachable!(),
    };

    resolved
}

fn get_target_of_export_assignment<'cx>(
    this: &mut TyChecker<'cx>,
    node: &'cx bolt_ts_ast::ExportAssign<'cx>, // TODO: binary expr
    dont_resolve_alias: bool,
) -> Option<SymbolID> {
    let expr = node.expr;

    // mark_symbol_of_alias_decl_if_ty_only
    get_target_of_alias_like_expr(this, expr, dont_resolve_alias)
}

fn get_target_of_alias_like_expr<'cx>(
    this: &mut TyChecker<'cx>,
    expr: &'cx bolt_ts_ast::Expr<'cx>,
    dont_resolve_alias: bool,
) -> Option<SymbolID> {
    use bolt_ts_ast::ExprKind;
    if let ExprKind::Class(n) = expr.kind {
        // TODO: check_expression?
        return Some(this.get_resolve_results()[n.id.module().as_usize()].final_res[&n.id]);
    }
    if !matches!(expr.kind, ExprKind::Ident(_)) && !expr.is_entity_name_expr() {
        return None;
    };
    let symbol;
    match expr.kind {
        bolt_ts_ast::ExprKind::Ident(n) => {
            let id = this.resolve_symbol_by_ident(n);
            symbol = this.get_merged_symbol(id);
            if symbol == Symbol::ERR || dont_resolve_alias {
                return Some(symbol);
            }
        }
        _ => todo!(),
    }
    let flags = this.symbol(symbol).flags;
    const MEANING: SymbolFlags = SymbolFlags::VALUE
        .union(SymbolFlags::TYPE)
        .union(SymbolFlags::NAMESPACE);
    if flags.intersects(MEANING) {
        Some(symbol)
    } else if flags.intersects(SymbolFlags::ALIAS) {
        Some(this.resolve_alias(symbol))
    } else {
        None
    }
    // TODO: resolved_symbol
}

fn get_target_of_import_clause<'cx>(
    this: &mut TyChecker<'cx>,
    node: &'cx bolt_ts_ast::ImportClause<'cx>,
    dont_recur_alias: bool,
) -> Option<SymbolID> {
    let parent = this.p().parent(node.id).unwrap();
    let parent = this.p().node(parent).expect_import_decl();
    let module_symbol = resolve_external_module_name(this.mg(), parent.module.id, this.p());
    module_symbol.and_then(|module_symbol| {
        get_target_of_module_default(this, module_symbol, node.id, dont_recur_alias)
    })
}

fn get_target_of_module_default(
    this: &mut TyChecker<'_>,
    module_symbol: SymbolID,
    node: bolt_ts_ast::NodeID,
    dont_resolve_alias: bool,
) -> Option<SymbolID> {
    let ms = binder_symbol(this, module_symbol);
    let export_default_symbol = if ms.is_shorthand_ambient_module(this.p()) {
        Some(module_symbol)
    } else {
        resolve_export_by_name(
            this,
            module_symbol,
            SymbolName::ExportDefault,
            node,
            dont_resolve_alias,
        )
    };
    if export_default_symbol.is_none() {
        let module_spec = this.p().get_module_spec_for_import_or_export(node).unwrap();
        error_no_module_member_symbol(this, module_symbol, module_spec.val, node);
    }
    // TODO: mark_symbol_of_alias_decl_if_ty_only
    export_default_symbol
}

fn resolve_export_by_name(
    this: &mut TyChecker<'_>,
    module_symbol: SymbolID,
    name: SymbolName,
    node: bolt_ts_ast::NodeID,
    dont_resolve_alias: bool,
) -> Option<SymbolID> {
    let ms = binder_symbol(this, module_symbol);
    // TODO: export=
    let export_symbol = ms
        .exports()
        .and_then(|exports| exports.0.get(&name).copied());

    // TODO: mark_symbol_of_alias_decl_if_ty_only
    export_symbol.map(|export_symbol| this.resolve_symbol(export_symbol, dont_resolve_alias))
}

impl<'cx> SymbolInfo<'cx> for super::TyChecker<'cx> {
    fn arena(&self) -> &'cx bolt_ts_arena::bumpalo::Bump {
        self.arena
    }
    fn empty_symbols(&self) -> &'cx SymbolTable {
        self.empty_symbols
    }
    fn module_arena(&self) -> &bolt_ts_span::ModuleArena {
        self.module_arena
    }
    fn mg(&self) -> &crate::graph::ModuleGraph {
        self.mg
    }
    fn p(&self) -> &crate::parser::Parser<'cx> {
        self.p
    }
    fn atoms(&self) -> &bolt_ts_atom::AtomMap<'cx> {
        self.atoms
    }
    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error })
    }
    fn get_symbol_links_map(&mut self) -> &FxHashMap<SymbolID, SymbolLinks<'cx>> {
        &self.symbol_links
    }
    fn get_mut_symbol_links_map(&mut self) -> &mut FxHashMap<SymbolID, SymbolLinks<'cx>> {
        &mut self.symbol_links
    }
    fn get_transient_symbols(&self) -> &Symbols {
        &self.binder.bind_results.last().unwrap().symbols
    }
    fn get_mut_transient_symbols(&mut self) -> &mut Symbols {
        &mut self.binder.bind_results.last_mut().unwrap().symbols
    }
    fn get_resolve_results(&self) -> &Vec<ResolveResult> {
        &self.binder.bind_results
    }
    fn get_merged_symbols(&self) -> &MergedSymbols {
        self.merged_symbols
    }
    fn get_transient_symbol_links_map(&self) -> &Vec<SymbolLinks<'cx>> {
        &self.transient_symbol_links
    }
    fn get_mut_transient_symbol_links_map(&mut self) -> &mut Vec<SymbolLinks<'cx>> {
        &mut self.transient_symbol_links
    }
}

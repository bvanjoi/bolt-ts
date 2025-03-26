use bolt_ts_utils::{fx_hashmap_with_capacity, fx_hashset_with_capacity};
use rustc_hash::{FxHashMap, FxHashSet};

use super::resolve_structured_member::MemberOrExportsResolutionKind;
use super::transient_symbol::CheckSymbol;
use super::{SymbolLinks, TransientSymbols, errors};
use crate::bind::{ResolveResult, Symbol, SymbolFlags, SymbolID, SymbolName, SymbolTable};

fn symbol_of_resolve_results(
    resolve_results: &Vec<ResolveResult>,
    symbol: SymbolID,
) -> &crate::bind::Symbol {
    let idx = symbol.module().as_usize();
    debug_assert!(idx < resolve_results.len());
    unsafe { resolve_results.get_unchecked(idx).symbols.get(symbol) }
}

fn decls_of_symbol<'cx>(
    symbol: SymbolID,
    this: &impl SymbolInfo<'cx>,
) -> &thin_vec::ThinVec<bolt_ts_ast::NodeID> {
    let s = symbol_of_resolve_results(this.get_resolve_results(), symbol);
    &s.decls
}

pub trait SymbolInfo<'cx>: Sized {
    fn arena(&self) -> &'cx bumpalo::Bump;
    fn empty_symbols(&self) -> &'cx SymbolTable;
    fn mg(&self) -> &crate::graph::ModuleGraph;
    fn p(&self) -> &crate::parser::Parser<'cx>;
    fn atoms(&self) -> &bolt_ts_atom::AtomMap<'cx>;
    fn push_error(&mut self, error: crate::Diag);

    fn get_mut_symbol_links_map(&mut self) -> &mut FxHashMap<SymbolID, SymbolLinks<'cx>>;
    fn get_transient_symbols(&self) -> &TransientSymbols<'cx>;
    fn get_mut_transient_symbols(&mut self) -> &mut TransientSymbols<'cx>;
    fn get_resolve_results(&self) -> &Vec<ResolveResult>;

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
            &self.get_transient_symbols().get(symbol).unwrap().links
        } else {
            self.get_mut_symbol_links_map().entry(symbol).or_default()
        }
    }

    fn get_mut_symbol_links(&mut self, symbol: SymbolID) -> &mut SymbolLinks<'cx> {
        if symbol.module() == bolt_ts_span::ModuleID::TRANSIENT {
            &mut self
                .get_mut_transient_symbols()
                .get_mut(symbol)
                .unwrap()
                .links
        } else {
            self.get_mut_symbol_links_map().get_mut(&symbol).unwrap()
        }
    }

    fn symbol(&self, symbol: SymbolID) -> CheckSymbol<'cx, '_> {
        if symbol.module() == bolt_ts_span::ModuleID::TRANSIENT {
            let symbol = self.get_transient_symbols().get(symbol).unwrap();
            CheckSymbol::Transient(symbol)
        } else {
            let symbol = symbol_of_resolve_results(self.get_resolve_results(), symbol);
            CheckSymbol::Normal(symbol)
        }
    }

    fn members_of_symbol(&mut self, symbol: SymbolID) -> &'cx SymbolTable {
        if let Some(m) = self.get_symbol_links(symbol).get_members() {
            m
        } else {
            let members = symbol_of_resolve_results(self.get_resolve_results(), symbol)
                .members()
                .clone();
            let members = self.alloc(members);
            self.get_mut_symbol_links(symbol).set_members(members);
            members
        }
    }

    fn exports_of_symbol(&mut self, symbol: SymbolID) -> &'cx SymbolTable {
        if let Some(e) = self.get_symbol_links(symbol).get_exports() {
            e
        } else {
            let exports = symbol_of_resolve_results(self.get_resolve_results(), symbol)
                .exports()
                .clone();
            let exports = self.alloc(exports);
            self.get_mut_symbol_links(symbol).set_exports(exports);
            exports
        }
    }

    fn resolve_external_module_name(
        &mut self,
        module_spec_id: bolt_ts_ast::NodeID,
        _module_spec: bolt_ts_atom::AtomId,
    ) -> Option<SymbolID> {
        crate::graph::resolve_external_module_name(self.mg(), module_spec_id, self.p())
    }

    fn is_non_local_alias(&self, symbol: SymbolID, excludes: Option<SymbolFlags>) -> bool {
        const DEFAULT: SymbolFlags = SymbolFlags::VALUE
            .union(SymbolFlags::TYPE)
            .union(SymbolFlags::NAMESPACE);
        let excludes = excludes.unwrap_or(DEFAULT);
        let flags = self.symbol(symbol).flags();
        (flags.intersection(SymbolFlags::ALIAS | excludes) == SymbolFlags::ALIAS)
            || (flags.intersects(SymbolFlags::ALIAS.union(SymbolFlags::ASSIGNMENT)))
    }

    fn resolve_alias(&mut self, symbol: SymbolID) -> SymbolID {
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
        let node = s.get_decl_of_alias_symbol(self.p()).unwrap();
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

    fn get_target_of_alias_decl(
        &mut self,
        node: bolt_ts_ast::NodeID,
        dont_recur_resolve: bool,
    ) -> Option<SymbolID> {
        let p = self.p();
        use bolt_ts_ast::Node::*;
        match p.node(node) {
            ImportNamedSpec(_) => get_target_of_import_named_spec(self, node, dont_recur_resolve),
            ShorthandSpec(_) if p.node(p.parent(node).unwrap()).is_import_clause() => {
                get_target_of_import_named_spec(self, node, dont_recur_resolve)
            }
            ShorthandSpec(_) => {
                assert!(p.node(p.parent(node).unwrap()).is_specs_export());
                const MEANING: SymbolFlags = SymbolFlags::VALUE
                    .union(SymbolFlags::TYPE)
                    .union(SymbolFlags::NAMESPACE);
                get_target_of_export_spec(self, node, MEANING, dont_recur_resolve)
            }
            NsImport(n) => get_target_of_ns_import(self, n, dont_recur_resolve),
            _ => todo!(),
        }
    }

    fn resolve_symbol(&mut self, symbol: SymbolID, dont_resolve_alias: bool) -> SymbolID {
        if !dont_resolve_alias && self.is_non_local_alias(symbol, None) {
            self.resolve_alias(symbol)
        } else {
            symbol
        }
    }

    fn get_export_of_module(
        &mut self,
        symbol: SymbolID,
        name: SymbolName,
        dont_resolve_alias: bool,
    ) -> Option<SymbolID> {
        let s = symbol_of_resolve_results(self.get_resolve_results(), symbol);
        if s.flags.intersects(SymbolFlags::MODULE) {
            let export_symbol = s.exports().0.get(&name).copied();
            let resolved = export_symbol
                .map(|export_symbol| self.resolve_symbol(export_symbol, dont_resolve_alias));
            resolved
        } else {
            None
        }
    }

    fn get_exports_of_module_worker(&mut self, module_symbol: SymbolID) -> &'cx SymbolTable {
        struct ExportCollisionTracker<'cx> {
            spec: bolt_ts_atom::AtomId,
            exports_with_duplicated: thin_vec::ThinVec<&'cx bolt_ts_ast::ExportDecl<'cx>>,
        }
        struct ExportCollisionTrackerTable<'cx>(FxHashMap<SymbolName, ExportCollisionTracker<'cx>>);

        fn extend_export_symbols<'cx>(
            this: &mut impl SymbolInfo<'cx>,
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

        fn visit<'cx>(
            this: &mut impl SymbolInfo<'cx>,
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
            for decl in decls_of_symbol(export_starts, this).clone() {
                let node = this.p().node(decl).expect_export_decl();
                let bolt_ts_ast::ExportClauseKind::Glob(n) = node.clause.kind else {
                    unreachable!()
                };
                let resolved_module = this.resolve_external_module_name(n.module.id, n.module.val);
                let exported_symbols = visit(this, resolved_module, visited);
                extend_export_symbols(
                    this,
                    &mut nested_symbols,
                    exported_symbols,
                    Some(&mut lookup_table),
                    Some(node),
                );
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

    fn get_resolved_members_or_exports_of_symbol(
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
        let set_links = |this: &mut Self, symbols: &'cx SymbolTable| {
            let links = this.get_mut_symbol_links(symbol);
            match kind {
                MemberOrExportsResolutionKind::ResolvedExports => {
                    links.set_resolved_exports(symbols)
                }
                MemberOrExportsResolutionKind::ResolvedMembers => {
                    links.set_resolved_members(symbols)
                }
            }
        };
        let is_static = matches!(kind, MemberOrExportsResolutionKind::ResolvedExports);
        let flags = self.symbol(symbol).flags();
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
        set_links(self, early_symbols);
        // TODO: late symbols;
        early_symbols
    }
}

fn get_target_of_ns_import<'cx>(
    this: &mut impl SymbolInfo<'cx>,
    n: &'cx bolt_ts_ast::NsImport<'cx>,
    dont_resolve_alias: bool,
) -> Option<SymbolID> {
    let p = this.p();
    let p_id = p.parent(n.id).unwrap();
    let p_id = p.parent(p_id).unwrap();
    let import_decl = p.node(p_id).expect_import_decl();
    let immediate =
        this.resolve_external_module_name(import_decl.module.id, import_decl.module.val);
    // TODO: resolve_es_module;
    let resolved = immediate;
    resolved
}
fn get_target_of_import_named_spec<'cx>(
    this: &mut impl SymbolInfo<'cx>,
    node: bolt_ts_ast::NodeID,
    dont_recur_resolve: bool,
) -> Option<SymbolID> {
    let p = this.p();
    let root = if p.node(node).is_binding() {
        p.get_root_decl(node)
    } else {
        let p_id = p.parent(node).unwrap();
        let p_id = p.parent(p_id).unwrap();
        p_id
    };
    let resolved = get_external_module_member(this, root, node, dont_recur_resolve);
    resolved
}

fn get_external_module_member<'cx>(
    this: &mut impl SymbolInfo<'cx>,
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
        let symbol_name = SymbolName::Normal(name);
        let symbol_from_module =
            this.get_export_of_module(target_symbol, symbol_name, dont_recur_resolve);
        if symbol_from_module.is_none() {
            error_no_module_member_symbol(
                this,
                module_symbol.unwrap(),
                module_spec.val,
                target_symbol,
                node,
                spec,
            );
        }
        symbol_from_module
    } else {
        None
    }
}

fn error_no_module_member_symbol<'cx>(
    this: &mut impl SymbolInfo<'cx>,
    module_symbol: SymbolID,
    module_name: bolt_ts_atom::AtomId,
    target_symbol: SymbolID,
    node: bolt_ts_ast::NodeID,
    name: bolt_ts_ast::NodeID,
) {
    let decl_name = match this.p().node(name) {
        bolt_ts_ast::Node::ImportNamedSpec(n) => match n.prop_name.kind {
            bolt_ts_ast::ModuleExportNameKind::Ident(ident) => ident.name,
            bolt_ts_ast::ModuleExportNameKind::StringLit(lit) => lit.val,
        },
        bolt_ts_ast::Node::ShorthandSpec(n) => n.name.name,
        _ => unreachable!(),
    };
    report_non_exported_member(this, node, name, decl_name, module_symbol, module_name);
}

fn report_non_exported_member<'cx>(
    this: &mut impl SymbolInfo<'cx>,
    node: bolt_ts_ast::NodeID,
    name: bolt_ts_ast::NodeID,
    decl_name: bolt_ts_atom::AtomId,
    module_symbol: SymbolID,
    module_name: bolt_ts_atom::AtomId,
) {
    let s = symbol_of_resolve_results(this.get_resolve_results(), module_symbol);
    let local_symbol = if let Some(value_decl) = s.value_decl {
        this.locals(value_decl.module())
            .get(&value_decl)
            .unwrap()
            .0
            .get(&SymbolName::Normal(decl_name))
            .copied()
    } else {
        None
    };
    if let Some(local_symbol) = local_symbol {
        let decl = s.exports().0.values().find_map(|export| {
            // TODO: use `get_symbol_if_same_reference`
            let s = symbol_of_resolve_results(this.get_resolve_results(), *export);
            if s.flags == SymbolFlags::ALIAS {
                let decl = s.decls[0];
                if let Some(spec) = this.p().node(decl).as_export_named_spec() {
                    match spec.prop_name.kind {
                        bolt_ts_ast::ModuleExportNameKind::Ident(ident)
                            if ident.name == decl_name =>
                        {
                            return Some(decl);
                        }
                        _ => (),
                    }
                }
            }
            None
        });
        let symbol_span = this
            .p()
            .node(decls_of_symbol(local_symbol, this)[0])
            .ident_name()
            .unwrap()
            .span;
        let error: crate::Diag = if let Some(decl) = decl {
            let mut helper = vec![];
            helper.push(
                errors::ModuleADeclaresBLocallyButItIsExportedAsCHelperKind::NameIsDeclaredHere(
                    errors::NameIsDeclaredHere {
                        span: symbol_span,
                        name: this.atoms().get(decl_name).to_string(),
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
                        name: this.atoms().get(decl_name).to_string(),
                    },
                ),
            );
            let error = errors::ModuleADeclaresBLocallyButItIsExportedAsC {
                span: this.p().node(name).span(),
                module_name: this.atoms().get(module_name).to_string(),
                symbol_name: this.atoms().get(decl_name).to_string(),
                target_name: this.atoms().get(target_name).to_string(),
                related: helper,
            };
            Box::new(error)
        } else {
            Box::new(errors::ModuleADeclaresBLocallyButItIsNotExported {
                span: this.p().node(name).span(),
                module_name: this.atoms().get(module_name).to_string(),
                symbol_name: this.atoms().get(decl_name).to_string(),
                related: [errors::NameIsDeclaredHere {
                    span: symbol_span,
                    name: this.atoms().get(decl_name).to_string(),
                }],
            })
        };
        this.push_error(error);
    } else {
        let error = errors::ModuleXHasNoExportedMemberY {
            span: this.p().node(name).span(),
            module: this.atoms().get(module_name).to_string(),
            member: this.atoms().get(decl_name).to_string(),
        };
        this.push_error(Box::new(error));
    }
}

fn get_target_of_export_spec<'cx>(
    this: &mut impl SymbolInfo<'cx>,
    node: bolt_ts_ast::NodeID,
    meaning: SymbolFlags,
    dont_recur_resolve: bool,
) -> Option<SymbolID> {
    let n = this.p().node(node);
    let name = match n {
        bolt_ts_ast::Node::ShorthandSpec(n) => n.name.name,
        bolt_ts_ast::Node::ExportNamedSpec(n) => match n.prop_name.kind {
            bolt_ts_ast::ModuleExportNameKind::Ident(n) => n.name,
            bolt_ts_ast::ModuleExportNameKind::StringLit(_) => todo!(),
        },
        _ => unreachable!(),
    };

    // TODO: default
    let resolved = match n {
        bolt_ts_ast::Node::ShorthandSpec(n) => {
            let p_id = this.p().parent(node).unwrap();
            let p = this.p().node(p_id).expect_specs_export();
            if let Some(_) = p.module {
                get_external_module_member(this, p_id, node, dont_recur_resolve)
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
                if let Some(_) = p.module {
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

impl<'cx> SymbolInfo<'cx> for super::TyChecker<'cx> {
    fn arena(&self) -> &'cx bumpalo::Bump {
        self.arena
    }
    fn empty_symbols(&self) -> &'cx SymbolTable {
        self.empty_symbols
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
    fn get_mut_symbol_links_map(&mut self) -> &mut FxHashMap<SymbolID, SymbolLinks<'cx>> {
        &mut self.symbol_links
    }
    fn get_transient_symbols(&self) -> &TransientSymbols<'cx> {
        &self.transient_symbols
    }
    fn get_mut_transient_symbols(&mut self) -> &mut TransientSymbols<'cx> {
        &mut self.transient_symbols
    }
    fn get_resolve_results(&self) -> &Vec<ResolveResult> {
        &self.binder.bind_results
    }
}

use bolt_ts_span::ModuleArena;
use rustc_hash::FxHashMap;

use crate::bind::set_value_declaration;
use crate::bind::{
    MergeSymbol, MergedSymbols, ResolveResult, SymbolFlags, SymbolID, SymbolName, SymbolTable,
};
use crate::check::symbol_info::SymbolInfo;
use crate::graph::resolve_external_module_name;
use crate::parser::Parser;

struct MergeModuleAugmentation<'p, 'cx> {
    pub p: &'p Parser<'cx>,
    pub bind_list: Vec<ResolveResult>,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
}

impl MergeModuleAugmentation<'_, '_> {
    fn symbol_of_decl(&self, id: bolt_ts_ast::NodeID) -> SymbolID {
        self.bind_list[id.module().as_usize()].final_res[&id]
    }
}

impl<'p, 'cx> MergeSymbol<'cx> for MergeModuleAugmentation<'p, 'cx> {
    fn get_parse_result(&self, module: bolt_ts_span::ModuleID) -> &crate::parser::ParseResult<'cx> {
        self.p.get(module)
    }
    fn get_symbols(&self, module: bolt_ts_span::ModuleID) -> &crate::bind::Symbols {
        &self.bind_list[module.as_usize()].symbols
    }
    fn get_mut_symbols(&mut self, module: bolt_ts_span::ModuleID) -> &mut crate::bind::Symbols {
        &mut self.bind_list[module.as_usize()].symbols
    }
    fn get_merged_symbols(&self) -> &MergedSymbols {
        &self.merged_symbols
    }
    fn get_mut_merged_symbols(&mut self) -> &mut MergedSymbols {
        &mut self.merged_symbols
    }
    fn get_global_symbols(&self) -> &SymbolTable {
        &self.global_symbols
    }
    fn get_mut_global_symbols(&mut self) -> &mut SymbolTable {
        &mut self.global_symbols
    }
    fn get_locals(&self, container: bolt_ts_ast::NodeID) -> &SymbolTable {
        self.bind_list[container.module().as_usize()]
            .locals
            .get(&container)
            .unwrap()
    }
    fn get_mut_locals(&mut self, container: bolt_ts_ast::NodeID) -> &mut SymbolTable {
        self.bind_list[container.module().as_usize()]
            .locals
            .get_mut(&container)
            .unwrap()
    }
    fn set_value_declaration(&mut self, symbol: SymbolID, node: bolt_ts_ast::NodeID) {
        let symbols = &mut self.bind_list[symbol.module().as_usize()].symbols;
        let p = &self.p.map[symbol.module().as_usize()];
        set_value_declaration(symbol, symbols, node, p);
    }
    fn record_merged_symbol(&mut self, target: SymbolID, source: SymbolID) {
        let symbols = &mut self.bind_list[source.module().as_usize()].symbols;
        self.merged_symbols
            .record_merged_symbol(target, source, symbols);
    }
}

pub(crate) struct MergeModuleAugmentationResult {
    pub bind_list: Vec<ResolveResult>,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
}

pub(crate) fn merge_module_augmentation_list_for_global<'cx>(
    parser: &Parser<'cx>,
    bind_list: Vec<ResolveResult>,
    module_arena: &ModuleArena,
    global_symbols: SymbolTable,
    merged_symbols: MergedSymbols,
) -> MergeModuleAugmentationResult {
    let mut c = MergeModuleAugmentation {
        p: parser,
        bind_list,
        merged_symbols,
        global_symbols,
    };
    for (m, p) in module_arena.modules().iter().zip(parser.map.iter()) {
        assert!(std::ptr::addr_eq(parser.get(m.id), p));
        for augmentation in p.module_augmentations.iter() {
            let ns_id = p.parent(*augmentation).unwrap();
            let ns = p.node(ns_id).expect_namespace_decl();
            if !ns.is_global_argument {
                continue;
            }
            let ns_id = ns.id;
            let ns_symbol = c.symbol_of_decl(ns_id);
            let ns_s = c.get_symbol(ns_symbol);
            if ns_s.decls.first().is_none_or(|decl| !ns_id.eq(decl)) {
                assert!(ns_s.decls.len() > 1);
                continue;
            }
            let target = MergeModuleAugmentation::global_loc();
            let source = MergeModuleAugmentation::exports_loc(ns_symbol);
            c.merge_symbol_table(target, source, false);
        }
    }

    MergeModuleAugmentationResult {
        bind_list: c.bind_list,
        merged_symbols: c.merged_symbols,
        global_symbols: c.global_symbols,
    }
}

impl MergeModuleAugmentationForNonGlobal<'_> {
    pub fn symbol_of_decl(&self, id: bolt_ts_ast::NodeID) -> SymbolID {
        self.bind_list[id.module().as_usize()].final_res[&id]
    }
}
impl<'cx> MergeSymbol<'cx> for MergeModuleAugmentationForNonGlobal<'cx> {
    fn get_parse_result(&self, module: bolt_ts_span::ModuleID) -> &crate::parser::ParseResult<'cx> {
        self.p.get(module)
    }
    fn get_symbols(&self, module: bolt_ts_span::ModuleID) -> &crate::bind::Symbols {
        &self.bind_list[module.as_usize()].symbols
    }
    fn get_mut_symbols(&mut self, module: bolt_ts_span::ModuleID) -> &mut crate::bind::Symbols {
        &mut self.bind_list[module.as_usize()].symbols
    }
    fn get_merged_symbols(&self) -> &MergedSymbols {
        &self.merged_symbols
    }
    fn get_mut_merged_symbols(&mut self) -> &mut MergedSymbols {
        &mut self.merged_symbols
    }
    fn get_global_symbols(&self) -> &SymbolTable {
        &self.global_symbols
    }
    fn get_mut_global_symbols(&mut self) -> &mut SymbolTable {
        &mut self.global_symbols
    }
    fn get_locals(&self, container: bolt_ts_ast::NodeID) -> &SymbolTable {
        self.bind_list[container.module().as_usize()]
            .locals
            .get(&container)
            .unwrap()
    }
    fn get_mut_locals(&mut self, container: bolt_ts_ast::NodeID) -> &mut SymbolTable {
        self.bind_list[container.module().as_usize()]
            .locals
            .get_mut(&container)
            .unwrap()
    }
    fn set_value_declaration(&mut self, symbol: SymbolID, node: bolt_ts_ast::NodeID) {
        let symbols = &mut self.bind_list[symbol.module().as_usize()].symbols;
        let p = &self.p.map[symbol.module().as_usize()];
        set_value_declaration(symbol, symbols, node, p);
    }
    fn record_merged_symbol(&mut self, target: SymbolID, source: SymbolID) {
        let symbols = &mut self.bind_list[source.module().as_usize()].symbols;
        self.merged_symbols
            .record_merged_symbol(target, source, symbols);
    }
}

pub(crate) struct MergeModuleAugmentationForNonGlobalResult<'cx> {
    pub binder: crate::bind::Binder,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub symbol_links: FxHashMap<SymbolID, super::SymbolLinks<'cx>>,
    pub transient_symbols: super::TransientSymbols<'cx>,
    pub atoms: bolt_ts_atom::AtomMap<'cx>,
}

pub(crate) struct MergeModuleAugmentationForNonGlobal<'cx> {
    pub p: &'cx Parser<'cx>,
    pub ty_arena: &'cx bumpalo::Bump,
    pub bind_list: Vec<ResolveResult>,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
    pub mg: &'cx crate::graph::ModuleGraph,
    pub atoms: bolt_ts_atom::AtomMap<'cx>,
    pub empty_symbols: &'cx SymbolTable,
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub symbol_links: FxHashMap<SymbolID, super::SymbolLinks<'cx>>,
    pub transient_symbols: super::TransientSymbols<'cx>,
}

pub(crate) fn merge_module_augmentation_list_for_non_global<'cx>(
    mut c: MergeModuleAugmentationForNonGlobal<'cx>,
    module_arena: &ModuleArena,
) -> MergeModuleAugmentationForNonGlobalResult<'cx> {
    for (m, p) in module_arena.modules().iter().zip(c.p.map.iter()) {
        assert!(std::ptr::addr_eq(c.p.get(m.id), p));
        for augmentation in p.module_augmentations.iter() {
            let ns_id = p.parent(*augmentation).unwrap();
            let ns = p.node(ns_id).expect_namespace_decl();
            if ns.is_global_argument {
                continue;
            }
            merge_module_augmentation_for_non_global(&mut c, *augmentation, ns);
        }
    }

    MergeModuleAugmentationForNonGlobalResult {
        binder: crate::bind::Binder::new(c.bind_list),
        merged_symbols: c.merged_symbols,
        global_symbols: c.global_symbols,
        diags: c.diags,
        symbol_links: c.symbol_links,
        transient_symbols: c.transient_symbols,
        atoms: c.atoms,
    }
}

fn merge_module_augmentation_for_non_global<'cx>(
    c: &mut MergeModuleAugmentationForNonGlobal<'cx>,
    module_name: bolt_ts_ast::NodeID,
    module_augmentation: &'cx bolt_ts_ast::NsDecl<'cx>,
) {
    assert!(!module_augmentation.is_global_argument);
    let ns_id = module_augmentation.id;
    let ns_symbol = c.symbol_of_decl(ns_id);
    let ns_s = c.get_symbol(ns_symbol);
    if ns_s.decls.first().is_none_or(|decl| !ns_id.eq(decl)) {
        assert!(ns_s.decls.len() > 1);
        return;
    }
    let ns_s = c.get_symbol(ns_symbol);
    let Some(main_module) = resolve_external_module_name(c.mg, module_name, c.p) else {
        return;
    };
    // TODO: resolve_external_module_symbol
    let main_module_s = c.get_symbol(main_module);
    if main_module_s.flags.intersects(SymbolFlags::NAMESPACE) {
        if main_module_s
            .exports()
            .0
            .contains_key(&SymbolName::ExportStar)
            && !ns_s.exports().0.is_empty()
        {
            let resolved_exports = c.get_resolved_members_or_exports_of_symbol(
                main_module,
                super::resolve_structured_member::MemberOrExportsResolutionKind::ResolvedExports,
            );
            let filtered = c
                .get_symbol(ns_symbol)
                .exports()
                .0
                .iter()
                .filter_map(|(k, v)| {
                    if resolved_exports.0.contains_key(k)
                        && !c.get_symbol(main_module).exports().0.contains_key(k)
                    {
                        Some((*k, *v))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            for (k, v) in filtered {
                c.merge_symbol(resolved_exports.0[&k], v, false);
            }
        }
        c.merge_symbol(main_module, ns_symbol, false);
    }
}

impl<'cx> super::symbol_info::SymbolInfo<'cx> for MergeModuleAugmentationForNonGlobal<'cx> {
    fn arena(&self) -> &'cx bumpalo::Bump {
        &self.ty_arena
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
        &self.atoms
    }

    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error });
    }

    fn get_mut_symbol_links_map(
        &mut self,
    ) -> &mut rustc_hash::FxHashMap<SymbolID, super::SymbolLinks<'cx>> {
        &mut self.symbol_links
    }

    fn get_transient_symbols(&self) -> &super::TransientSymbols<'cx> {
        &self.transient_symbols
    }

    fn get_mut_transient_symbols(&mut self) -> &mut super::TransientSymbols<'cx> {
        &mut self.transient_symbols
    }

    fn get_resolve_results(&self) -> &Vec<ResolveResult> {
        &self.bind_list
    }
}

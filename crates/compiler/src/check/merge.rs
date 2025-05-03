use bolt_ts_span::ModuleArena;

use crate::bind::set_value_declaration;
use crate::bind::{
    MergeSymbol, MergedSymbols, ResolveResult, SymbolFlags, SymbolID, SymbolName, SymbolTable,
};
use crate::graph::resolve_external_module_name;
use crate::parser::Parser;

struct MergeModuleAugmentation<'p, 'cx> {
    pub p: &'p Parser<'cx>,
    pub atoms: &'p bolt_ts_atom::AtomMap<'cx>,
    pub bind_list: Vec<ResolveResult>,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
}

impl MergeModuleAugmentation<'_, '_> {
    fn symbol_of_decl(&self, id: bolt_ts_ast::NodeID) -> SymbolID {
        self.bind_list[id.module().as_usize()].final_res[&id]
    }
}

impl<'cx> MergeSymbol<'cx> for MergeModuleAugmentation<'_, 'cx> {
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
        set_value_declaration(symbol, symbols, node, &self.p.map);
    }
    fn record_merged_symbol(&mut self, target: SymbolID, source: SymbolID) {
        let symbols = &mut self.bind_list[source.module().as_usize()].symbols;
        self.merged_symbols
            .record_merged_symbol(target, source, symbols);
    }

    fn atom(&self, atom: bolt_ts_atom::AtomId) -> &str {
        self.atoms.get(atom)
    }
}

pub(crate) struct MergeModuleAugmentationResult {
    pub bind_list: Vec<ResolveResult>,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
}

pub(crate) fn merge_module_augmentation_list_for_global(
    parser: &Parser,
    atoms: &bolt_ts_atom::AtomMap,
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
        atoms,
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
            let decls = ns_s.decls.as_ref().unwrap();
            if decls.first().is_none_or(|decl| !ns_id.eq(decl)) {
                assert!(decls.len() > 1);
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

impl<'cx> MergeSymbol<'cx> for super::TyChecker<'cx> {
    fn get_parse_result(&self, module: bolt_ts_span::ModuleID) -> &crate::parser::ParseResult<'cx> {
        self.p.get(module)
    }
    fn get_symbols(&self, module: bolt_ts_span::ModuleID) -> &crate::bind::Symbols {
        if module == bolt_ts_span::ModuleID::TRANSIENT {
            &self.binder.bind_results.last().unwrap().symbols
        } else {
            &self.binder.bind_results[module.as_usize()].symbols
        }
    }
    fn get_mut_symbols(&mut self, module: bolt_ts_span::ModuleID) -> &mut crate::bind::Symbols {
        if module == bolt_ts_span::ModuleID::TRANSIENT {
            &mut self.binder.bind_results.last_mut().unwrap().symbols
        } else {
            &mut self.binder.bind_results[module.as_usize()].symbols
        }
    }
    fn get_merged_symbols(&self) -> &MergedSymbols {
        self.merged_symbols
    }
    fn get_mut_merged_symbols(&mut self) -> &mut MergedSymbols {
        self.merged_symbols
    }
    fn get_global_symbols(&self) -> &SymbolTable {
        self.global_symbols
    }
    fn get_mut_global_symbols(&mut self) -> &mut SymbolTable {
        self.global_symbols
    }
    fn get_locals(&self, container: bolt_ts_ast::NodeID) -> &SymbolTable {
        self.binder.bind_results[container.module().as_usize()]
            .locals
            .get(&container)
            .unwrap()
    }
    fn get_mut_locals(&mut self, container: bolt_ts_ast::NodeID) -> &mut SymbolTable {
        self.binder.bind_results[container.module().as_usize()]
            .locals
            .get_mut(&container)
            .unwrap()
    }
    fn set_value_declaration(&mut self, symbol: SymbolID, node: bolt_ts_ast::NodeID) {
        let symbols = &mut self.binder.bind_results[symbol.module().as_usize()].symbols;
        set_value_declaration(symbol, symbols, node, &self.p.map);
    }
    fn record_merged_symbol(&mut self, target: SymbolID, source: SymbolID) {
        let symbols = &mut self.binder.bind_results[source.module().as_usize()].symbols;
        self.merged_symbols
            .record_merged_symbol(target, source, symbols);
    }

    fn atom(&self, atom: bolt_ts_atom::AtomId) -> &str {
        self.atoms.get(atom)
    }
}

impl<'cx> super::TyChecker<'cx> {
    pub(super) fn merge_module_augmentation_list_for_non_global(&mut self) {
        for (m, p) in self.module_arena.modules().iter().zip(self.p.map.iter()) {
            assert!(std::ptr::addr_eq(self.p.get(m.id), p));
            for augmentation in p.module_augmentations.iter() {
                let ns_id = p.parent(*augmentation).unwrap();
                let ns = p.node(ns_id).expect_namespace_decl();
                if ns.is_global_argument {
                    continue;
                }
                self.merge_module_augmentation_for_non_global(*augmentation, ns);
            }
        }
    }

    fn merge_module_augmentation_for_non_global(
        &mut self,
        module_name: bolt_ts_ast::NodeID,
        module_augmentation: &'cx bolt_ts_ast::NsDecl<'cx>,
    ) {
        assert!(!module_augmentation.is_global_argument);
        let ns_id = module_augmentation.id;
        let ns_symbol = self.get_symbol_of_decl(ns_id);
        let ns_s = MergeSymbol::get_symbol(self, ns_symbol);
        let decls = ns_s.decls.as_ref().unwrap();
        if decls.first().is_none_or(|decl| !ns_id.eq(decl)) {
            assert!(decls.len() > 1);
            return;
        }
        let Some(main_module) = resolve_external_module_name(self.mg, module_name, self.p) else {
            return;
        };
        // TODO: resolve_external_module_symbol
        let main_module_s = MergeSymbol::get_symbol(self, main_module);
        if main_module_s.flags.intersects(SymbolFlags::NAMESPACE) {
            if main_module_s
                .exports()
                .is_some_and(|table| table.0.contains_key(&SymbolName::ExportStar))
                && ns_s.exports().is_some_and(|t| !t.0.is_empty())
            {
                let resolved_exports = self.get_resolved_members_or_exports_of_symbol(
                main_module,
                super::resolve_structured_member::MemberOrExportsResolutionKind::ResolvedExports,
            );
                let filtered = MergeSymbol::get_symbol(self, ns_symbol)
                    .exports()
                    .unwrap()
                    .0
                    .iter()
                    .filter_map(|(k, v)| {
                        if resolved_exports.0.contains_key(k)
                            && !MergeSymbol::get_symbol(self, main_module)
                                .exports()
                                .unwrap()
                                .0
                                .contains_key(k)
                        {
                            Some((*k, *v))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                for (k, v) in filtered {
                    self.merge_symbol(resolved_exports.0[&k], v, false);
                }
            }
            self.merge_symbol(main_module, ns_symbol, false);
        }
    }
}

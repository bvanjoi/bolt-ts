use bolt_ts_span::ModuleArena;

use super::create::set_value_declaration;
use super::symbol::SymbolTable;
use super::{BinderResult, SymbolFlags, SymbolID};
use crate::parser::Parser;

pub struct MergedSymbols(Vec<SymbolID>);

impl MergedSymbols {
    pub fn get_merged_symbol(&self, id: SymbolID, symbols: &super::Symbols) -> SymbolID {
        let s = symbols.get(id);
        if let Some(merged_id) = s.merged_id {
            if let Some(merged) = self.0.get(merged_id).copied() {
                return merged;
            }
        }
        id
    }

    fn record_merged_symbol(
        &mut self,
        target: SymbolID,
        source: SymbolID,
        symbols: &mut super::Symbols,
    ) {
        let s = symbols.get_mut(source);
        if let Some(merged_id) = s.merged_id {
            self.0[merged_id] = target;
        } else {
            let next_merged_id = self.0.len();
            s.merged_id = Some(next_merged_id);
            self.0.push(target);
        }
    }
}

struct MergeSymbolContainer<'p, 'cx> {
    p: &'p Parser<'cx>,
    bind_list: Vec<BinderResult<'cx>>,
    merged_symbols: MergedSymbols,
    global_symbols: SymbolTable,
}

#[derive(Clone, Copy)]
enum SymbolTableLocation {
    Global,
    Members { symbol: SymbolID },
    Exports { symbol: SymbolID },
    Locals { container: bolt_ts_ast::NodeID },
}

impl<'p, 'cx> MergeSymbolContainer<'p, 'cx> {
    fn get_mut_symbol_table_by_location(&mut self, loc: SymbolTableLocation) -> &mut SymbolTable {
        match loc {
            SymbolTableLocation::Global => &mut self.global_symbols,
            SymbolTableLocation::Members { symbol } => {
                &mut self.bind_list[symbol.module().as_usize()]
                    .symbols
                    .get_mut(symbol)
                    .members
            }
            SymbolTableLocation::Exports { symbol } => {
                &mut self.bind_list[symbol.module().as_usize()]
                    .symbols
                    .get_mut(symbol)
                    .exports
            }
            SymbolTableLocation::Locals { container } => self.bind_list
                [container.module().as_usize()]
            .locals
            .get_mut(&container)
            .unwrap(),
        }
    }

    fn get_symbol_table_by_location(&self, loc: SymbolTableLocation) -> &SymbolTable {
        match loc {
            SymbolTableLocation::Global => &self.global_symbols,
            SymbolTableLocation::Members { symbol } => {
                &self.bind_list[symbol.module().as_usize()]
                    .symbols
                    .get(symbol)
                    .members
            }
            SymbolTableLocation::Exports { symbol } => {
                &self.bind_list[symbol.module().as_usize()]
                    .symbols
                    .get(symbol)
                    .exports
            }
            SymbolTableLocation::Locals { container } => &self.bind_list
                [container.module().as_usize()]
            .locals
            .get(&container)
            .as_ref()
            .unwrap_or_else(|| {
                panic!(
                    "local symbol table of {} not found",
                    self.p.node(container).span()
                )
            }),
        }
    }

    fn merge_symbol_table(
        &mut self,
        target: SymbolTableLocation,
        source: SymbolTableLocation,
        unidirectional: bool,
    ) {
        // TODO: can we delete clone?
        for (id, source_symbol) in self.get_symbol_table_by_location(source).0.clone() {
            let target_symbol = self
                .get_symbol_table_by_location(target)
                .0
                .get(&id)
                .copied();
            let merged = if let Some(target_symbol) = target_symbol {
                self.merge_symbol(target_symbol, source_symbol, unidirectional)
            } else {
                let symbols = &self.bind_list[source_symbol.module().as_usize()].symbols;
                self.merged_symbols
                    .get_merged_symbol(source_symbol, symbols)
            };
            // TODO: parent
            self.get_mut_symbol_table_by_location(target)
                .0
                .insert(id, merged);
        }
    }

    fn symbol(&self, symbol_id: SymbolID) -> &super::Symbol {
        self.bind_list[symbol_id.module().as_usize()]
            .symbols
            .get(symbol_id)
    }

    fn symbol_mut(&mut self, symbol_id: SymbolID) -> &mut super::Symbol {
        self.bind_list[symbol_id.module().as_usize()]
            .symbols
            .get_mut(symbol_id)
    }

    fn merge_symbol(
        &mut self,
        target: SymbolID,
        source: SymbolID,
        unidirectional: bool,
    ) -> SymbolID {
        let s = self.symbol(source);
        let t = self.symbol(target);
        let t_flags = t.flags;
        let s_flags = s.flags;
        let s_value_decl = s.value_decl;

        if !t_flags.intersects(s_flags.get_excluded())
            || s_flags.union(t_flags).intersects(SymbolFlags::ASSIGNMENT)
        {
            if source == target {
                return target;
            }
            if t_flags.intersects(SymbolFlags::TRANSIENT) {
                todo!()
            }
            // if !t.flags.intersects(SymbolFlags::TRANSIENT) {

            // }
            if s_flags.intersects(SymbolFlags::VALUE_MODULE)
                && t_flags.intersects(SymbolFlags::VALUE_MODULE)
                && t.const_enum_only_module.is_some_and(|t| t)
                && s.const_enum_only_module.is_none_or(|t| !t)
            {
                self.symbol_mut(target).const_enum_only_module = Some(false);
            }
            self.symbol_mut(target).flags |= s_flags;
            if let Some(s_value_decl) = s_value_decl {
                let symbols = &mut self.bind_list[target.module().as_usize()].symbols;
                let p = &self.p.map[target.module().as_usize()];
                set_value_declaration(target, symbols, s_value_decl, p);
            }
            let t = SymbolTableLocation::Members { symbol: target };
            let s = SymbolTableLocation::Members { symbol: source };
            self.merge_symbol_table(t, s, unidirectional);

            let t = SymbolTableLocation::Exports { symbol: target };
            let s = SymbolTableLocation::Exports { symbol: source };
            // TODO: parent
            self.merge_symbol_table(t, s, unidirectional);

            if !unidirectional {
                let symbols = &mut self.bind_list[source.module().as_usize()].symbols;
                self.merged_symbols
                    .record_merged_symbol(target, source, symbols);
            }
        } else if t.flags.intersects(SymbolFlags::NAMESPACE_MODULE) {
            // todo: if target != global_this_symbol_module
        } else {
            // todo: report merge symbol error;
        }

        target
    }
}

pub(crate) struct MergeGlobalSymbolResult<'cx> {
    pub(crate) bind_list: Vec<BinderResult<'cx>>,
    pub(crate) merged_symbols: MergedSymbols,
    pub(crate) global_symbols: SymbolTable,
}

pub(crate) fn merge_global_symbol<'cx>(
    parser: &Parser<'cx>,
    bind_list: Vec<BinderResult<'cx>>,
    module_arena: &ModuleArena,
) -> MergeGlobalSymbolResult<'cx> {
    let global_symbols = SymbolTable::new(bind_list.len() * 32);
    let merged_symbols = MergedSymbols(Vec::with_capacity(bind_list.len() * 64));
    let mut c = MergeSymbolContainer {
        p: parser,
        bind_list,
        merged_symbols,
        global_symbols,
    };

    for (m, p) in module_arena.modules().iter().zip(parser.map.iter()) {
        assert!(std::ptr::addr_eq(&parser.map[m.id.as_usize()], p));
        if !p.is_external_or_commonjs_module() {
            let target = SymbolTableLocation::Global;
            let source = SymbolTableLocation::Locals {
                container: bolt_ts_ast::NodeID::root(m.id),
            };
            c.merge_symbol_table(target, source, false);
        }
    }

    MergeGlobalSymbolResult {
        bind_list: c.bind_list,
        merged_symbols: c.merged_symbols,
        global_symbols: c.global_symbols,
    }
}

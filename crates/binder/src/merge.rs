use bolt_ts_span::ModuleArena;

use super::create::set_value_declaration;
use super::symbol::SymbolTable;
use super::{BinderResult, SymbolFlags, SymbolID};
use bolt_ts_parser::Parser;

pub struct MergedSymbols(Vec<SymbolID>);

impl MergedSymbols {
    pub fn get_merged_symbol(&self, id: SymbolID, symbols: &super::Symbols) -> SymbolID {
        let s = symbols.get(id);
        if let Some(merged_id) = s.merged_id
            && let Some(merged) = self.0.get(merged_id as usize).copied()
        {
            merged
        } else {
            id
        }
    }

    pub fn record_merged_symbol(
        &mut self,
        target: SymbolID,
        source: SymbolID,
        symbols: &mut super::Symbols,
    ) {
        let s = symbols.get_mut(source);
        if let Some(merged_id) = s.merged_id {
            self.0[merged_id as usize] = target;
        } else {
            let next_merged_id = self.0.len();
            s.merged_id = Some(next_merged_id as u32);
            self.0.push(target);
        }
    }
}

struct MergeGlobalSymbol<'p, 'cx> {
    pub p: &'p Parser<'cx>,
    pub atom: &'p bolt_ts_atom::AtomIntern,
    pub bind_list: Vec<BinderResult<'cx>>,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
}

impl<'cx> MergeSymbol<'cx> for MergeGlobalSymbol<'_, 'cx> {
    fn get_symbols(&self, module: bolt_ts_span::ModuleID) -> &super::Symbols {
        &self.bind_list[module.as_usize()].symbols
    }
    fn get_mut_symbols(&mut self, module: bolt_ts_span::ModuleID) -> &mut super::Symbols {
        &mut self.bind_list[module.as_usize()].symbols
    }
    fn get_merged_symbols(&self) -> &MergedSymbols {
        &self.merged_symbols
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
    fn atom(&self, atom: bolt_ts_atom::Atom) -> &str {
        self.atom.get(atom)
    }
}

#[derive(Clone, Copy)]
pub enum SymbolTableLocation {
    Global,
    Members { symbol: SymbolID },
    Exports { symbol: SymbolID },
    Locals { container: bolt_ts_ast::NodeID },
}

pub trait MergeSymbol<'cx> {
    fn atom(&self, atom: bolt_ts_atom::Atom) -> &str;

    fn global_loc() -> SymbolTableLocation {
        SymbolTableLocation::Global
    }
    fn members_loc(symbol: SymbolID) -> SymbolTableLocation {
        SymbolTableLocation::Members { symbol }
    }
    fn exports_loc(symbol: SymbolID) -> SymbolTableLocation {
        SymbolTableLocation::Exports { symbol }
    }
    fn locals_loc(container: bolt_ts_ast::NodeID) -> SymbolTableLocation {
        SymbolTableLocation::Locals { container }
    }

    fn get_symbols(&self, module: bolt_ts_span::ModuleID) -> &super::Symbols;
    fn get_mut_symbols(&mut self, module: bolt_ts_span::ModuleID) -> &mut super::Symbols;

    fn get_merged_symbols(&self) -> &MergedSymbols;

    fn get_global_symbols(&self) -> &SymbolTable;
    fn get_mut_global_symbols(&mut self) -> &mut SymbolTable;

    fn get_locals(&self, container: bolt_ts_ast::NodeID) -> &SymbolTable;
    fn get_mut_locals(&mut self, container: bolt_ts_ast::NodeID) -> &mut SymbolTable;

    fn set_value_declaration(&mut self, symbol: SymbolID, node: bolt_ts_ast::NodeID);
    fn record_merged_symbol(&mut self, target: SymbolID, source: SymbolID);

    fn get_symbol(&self, symbol: SymbolID) -> &super::Symbol {
        self.get_symbols(symbol.module()).get(symbol)
    }
    fn get_mut_symbol(&mut self, symbol: SymbolID) -> &mut super::Symbol {
        self.get_mut_symbols(symbol.module()).get_mut(symbol)
    }

    fn get_symbol_table_by_location(&self, loc: SymbolTableLocation) -> &SymbolTable {
        match loc {
            SymbolTableLocation::Global => self.get_global_symbols(),
            SymbolTableLocation::Members { symbol } => self.get_symbol(symbol).members().unwrap(),
            SymbolTableLocation::Exports { symbol } => self.get_symbol(symbol).exports().unwrap(),
            SymbolTableLocation::Locals { container } => self.get_locals(container),
        }
    }

    fn get_mut_symbol_table_by_location(&mut self, loc: SymbolTableLocation) -> &mut SymbolTable {
        match loc {
            SymbolTableLocation::Global => self.get_mut_global_symbols(),
            SymbolTableLocation::Members { symbol } => {
                self.get_mut_symbol(symbol).members.as_mut().unwrap()
            }
            SymbolTableLocation::Exports { symbol } => {
                self.get_mut_symbol(symbol).exports.as_mut().unwrap()
            }
            SymbolTableLocation::Locals { container } => self.get_mut_locals(container),
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
                let symbols = &self.get_symbols(source_symbol.module());
                self.get_merged_symbols()
                    .get_merged_symbol(source_symbol, symbols)
            };
            // TODO: parent
            self.get_mut_symbol_table_by_location(target)
                .0
                .insert(id, merged);
        }
    }

    fn merge_symbol(
        &mut self,
        target: SymbolID,
        source: SymbolID,
        unidirectional: bool,
    ) -> SymbolID {
        let s = self.get_symbol(source);
        let t = self.get_symbol(target);
        let t_flags = t.flags;
        let s_flags = s.flags;
        let s_value_decl = s.value_decl;
        let s_members_is_some = s.members.is_some();
        let t_members_is_none = t.members.is_none();
        let s_exports_is_some = s.exports.is_some();
        let t_exports_is_none = t.exports.is_none();
        let t_const_enum_only_module = t.const_enum_only_module;
        let s_const_enum_only_module = s.const_enum_only_module;

        if !t_flags.intersects(s_flags.get_excluded())
            || s_flags.union(t_flags).intersects(SymbolFlags::ASSIGNMENT)
        {
            if source == target {
                return target;
            }
            if !t_flags.intersects(SymbolFlags::TRANSIENT) {
                // TODO:
            }
            if let Some(old_decls) = s.decls.clone() {
                match self.get_mut_symbol(target).decls.as_mut() {
                    Some(decls) => {
                        decls.extend(old_decls);
                    }
                    None => {
                        self.get_mut_symbol(target).decls = Some(old_decls);
                    }
                }
            }
            if s_flags.intersects(SymbolFlags::VALUE_MODULE)
                && t_flags.intersects(SymbolFlags::VALUE_MODULE)
                && t_const_enum_only_module.is_some_and(|t| t)
                && s_const_enum_only_module.is_none_or(|t| !t)
            {
                self.get_mut_symbol(target).const_enum_only_module = Some(false);
            }
            self.get_mut_symbol(target).flags |= s_flags;
            if let Some(s_value_decl) = s_value_decl {
                self.set_value_declaration(target, s_value_decl);
            }

            if s_members_is_some {
                let s = SymbolTableLocation::Members { symbol: source };
                let t = SymbolTableLocation::Members { symbol: target };
                if t_members_is_none {
                    self.get_mut_symbol(target).members = Some(SymbolTable::new(32));
                }
                self.merge_symbol_table(t, s, unidirectional);
            }

            if s_exports_is_some {
                let s = SymbolTableLocation::Exports { symbol: source };
                let t = SymbolTableLocation::Exports { symbol: target };
                if t_exports_is_none {
                    self.get_mut_symbol(target).exports = Some(SymbolTable::new(32));
                }
                self.merge_symbol_table(t, s, unidirectional);
            }

            if !unidirectional {
                self.record_merged_symbol(target, source);
            }
        } else if t.flags.intersects(SymbolFlags::NAMESPACE_MODULE) {
            // todo: if target != global_this_symbol_module
        } else {
            // todo: report merge symbol error;
        }

        target
    }
}

pub struct MergeGlobalSymbolResult<'cx> {
    pub bind_list: Vec<BinderResult<'cx>>,
    pub merged_symbols: MergedSymbols,
    pub global_symbols: SymbolTable,
}

pub fn merge_global_symbol<'cx>(
    parser: &Parser<'cx>,
    atom: &bolt_ts_atom::AtomIntern,
    bind_list: Vec<BinderResult<'cx>>,
    module_arena: &ModuleArena,
) -> MergeGlobalSymbolResult<'cx> {
    let global_symbols = SymbolTable::new(bind_list.len() * 32);
    let merged_symbols = MergedSymbols(Vec::with_capacity(bind_list.len() * 64));

    let mut c = MergeGlobalSymbol {
        p: parser,
        bind_list,
        merged_symbols,
        global_symbols,
        atom,
    };

    for (m, p) in module_arena.modules().iter().zip(parser.map.iter()) {
        assert!(std::ptr::addr_eq(parser.get(m.id()), p));

        if !p.is_external_or_commonjs_module() {
            let target = SymbolTableLocation::Global;
            let container = parser.get(m.id()).root().id;
            let source = SymbolTableLocation::Locals { container };
            c.merge_symbol_table(target, source, false);
        }
    }

    MergeGlobalSymbolResult {
        bind_list: c.bind_list,
        merged_symbols: c.merged_symbols,
        global_symbols: c.global_symbols,
    }
}

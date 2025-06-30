use super::symbol::{SymbolFlags, SymbolTableLocation};
use super::{BinderState, Symbol, SymbolID, SymbolName, Symbols, errors};
use crate::bind::SymbolTable;
use crate::parser::ParseResult;

use bolt_ts_ast as ast;

pub(crate) fn set_value_declaration(
    symbol: SymbolID,
    symbols: &mut Symbols,
    node: ast::NodeID,
    p: &[ParseResult],
) {
    let s = symbols.get_mut(symbol);
    // TODO: ambient declaration
    if s.value_decl.is_none_or(|value_decl| {
        let v = p[value_decl.module().as_usize()].node(value_decl);
        let other = p[node.module().as_usize()].node(node);
        !v.is_same_kind(&other) && v.is_effective_module_decl()
    }) {
        s.value_decl = Some(node);
    }
}

pub(crate) fn set_value_declaration_in_same_module(
    symbol: SymbolID,
    symbols: &mut Symbols,
    node: ast::NodeID,
    p: &ParseResult,
) {
    assert_eq!(node.module(), symbol.module());
    let s = symbols.get_mut(symbol);
    // TODO: ambient declaration
    if s.value_decl.is_none_or(|value_decl| {
        let v = p.node(value_decl);
        !v.is_same_kind(&p.node(node)) && v.is_effective_module_decl()
    }) {
        s.value_decl = Some(node);
    }
}

impl BinderState<'_, '_, '_> {
    pub(super) fn create_final_res(&mut self, id: ast::NodeID, symbol: SymbolID) {
        let prev = self.final_res.insert(id, symbol);
        assert!(prev.is_none(), "prev: {:#?}", prev.unwrap());
    }

    pub(super) fn declare_symbol(
        &mut self,
        name: Option<SymbolName>,
        location: SymbolTableLocation,
        parent: Option<SymbolID>,
        node: ast::NodeID,
        includes: SymbolFlags,
        excludes: SymbolFlags,
        is_replaceable_by_method: bool,
        is_computed_name: bool,
    ) -> SymbolID {
        let symbol;
        let is_default_export = {
            let n = self.p.node(node);
            n.has_syntactic_modifier(ast::ModifierKind::Default.into())
                || n.as_export_named_spec()
                    .is_some_and(|spec| spec.name.is_default())
        };
        let name = if is_computed_name {
            Some(SymbolName::Computed)
        } else if is_default_export {
            Some(SymbolName::ExportDefault)
        } else {
            // TODO: get_decl_name
            name
        };
        if let Some(name) = name {
            let old = self
                .get_symbol_table_by_location(location)
                .and_then(|t| t.0.get(&name))
                .copied();
            if includes.intersects(SymbolFlags::CLASSIFIABLE) {
                // todo!()
            }
            if let Some(old) = old {
                if is_replaceable_by_method
                    && self
                        .symbols
                        .get(old)
                        .is_replaceable_by_method
                        .is_none_or(|r| !r)
                {
                    return old;
                }

                symbol = old;
                let old_symbol = self.symbols.get(old);
                if old_symbol.flags.intersects(excludes) {
                    if old_symbol.is_replaceable_by_method.is_some_and(|r| r) {
                        todo!()
                    } else if !(includes.intersects(SymbolFlags::VARIABLE)
                        && old_symbol.flags.intersects(SymbolFlags::ASSIGNMENT))
                    {
                        let old_decl_id = self.symbols.get(old).opt_decl().unwrap();
                        let old_decl = self.p.node(old_decl_id);
                        let error = errors::DuplicateIdentifier {
                            span: self.p.node(node).ident_name().unwrap().span,
                            name: self.atoms.get(name.expect_atom()).to_string(),
                            original_span: old_decl
                                .ident_name()
                                .map(|name| name.span)
                                .unwrap_or(old_decl.span()),
                        };
                        self.push_error(Box::new(error));
                    }
                }
            } else {
                symbol = self.create_symbol(name, includes);
                if let Some(table) = self.get_symbol_table_by_location(location) {
                    let prev = table.0.insert(name, symbol);
                    assert!(prev.is_none());
                } else {
                    let mut table = SymbolTable::new(32);
                    table.0.insert(name, symbol);
                    self.init_symbol_table_by_location(location, table);
                }

                if is_replaceable_by_method {
                    self.symbols.get_mut(symbol).is_replaceable_by_method = Some(true);
                }
            }
        } else {
            todo!("missing symbol")
        }

        self.add_declaration_to_symbol(symbol, node, includes);
        // TODO: parent
        symbol
    }

    pub(super) fn add_declaration_to_symbol(
        &mut self,
        symbol: SymbolID,
        node: ast::NodeID,
        flags: SymbolFlags,
    ) {
        let s = self.symbols.get_mut(symbol);
        let has_exports = s.exports.is_some();
        let has_members = s.members.is_some();
        s.flags |= flags;
        if let Some(decls) = s.decls.as_mut() {
            decls.push(node);
        } else {
            let mut decls = thin_vec::ThinVec::with_capacity(4);
            decls.push(node);
            s.decls = Some(decls);
        }

        if s.const_enum_only_module.is_some_and(|y| y)
            && s.flags.intersects(
                SymbolFlags::FUNCTION
                    .union(SymbolFlags::CLASS)
                    .union(SymbolFlags::REGULAR_ENUM),
            )
        {
            s.const_enum_only_module = Some(false);
        }

        if !has_exports
            && flags.intersects(
                SymbolFlags::CLASS
                    .union(SymbolFlags::ENUM)
                    .union(SymbolFlags::MODULE)
                    .union(SymbolFlags::VARIABLE),
            )
        {
            self.symbols.get_mut(symbol).exports = Some(SymbolTable::new(32));
        }
        if !has_members
            && flags.intersects(
                SymbolFlags::CLASS
                    .union(SymbolFlags::INTERFACE)
                    .union(SymbolFlags::TYPE_LITERAL)
                    .union(SymbolFlags::OBJECT_LITERAL),
            )
        {
            self.symbols.get_mut(symbol).members = Some(SymbolTable::new(32));
        }

        if flags.intersects(SymbolFlags::VALUE) {
            self.set_value_declaration(symbol, node);
        }
    }

    pub(super) fn set_value_declaration(&mut self, symbol: SymbolID, node: ast::NodeID) {
        set_value_declaration_in_same_module(symbol, &mut self.symbols, node, self.p);
    }

    pub(super) fn create_symbol(&mut self, name: SymbolName, flags: SymbolFlags) -> SymbolID {
        let s = Symbol::new(name, flags);
        self.symbols.insert(s)
    }

    fn get_symbol_table_by_location(
        &mut self,
        location: SymbolTableLocation,
    ) -> Option<&mut SymbolTable> {
        fn inner<'a>(
            this: &'a mut BinderState,
            container: ast::NodeID,
            is_export: bool,
        ) -> Option<&'a mut SymbolTable> {
            let container = this.final_res.get(&container).copied()?;
            let container = this.symbols.get_mut(container);
            if is_export {
                container.exports.as_mut()
            } else {
                container.members.as_mut()
            }
        }
        use super::symbol::SymbolTableLocationKind;
        match location.kind {
            SymbolTableLocationKind::SymbolMember => inner(self, location.container, false),
            SymbolTableLocationKind::SymbolExports => inner(self, location.container, true),
            SymbolTableLocationKind::ContainerLocals => {
                assert!(self.p.node(location.container).has_locals());
                self.locals.get_mut(&location.container)
            }
        }
    }

    fn init_symbol_table_by_location(&mut self, location: SymbolTableLocation, table: SymbolTable) {
        fn inner(
            this: &mut BinderState,
            container: ast::NodeID,
            is_export: bool,
            table: SymbolTable,
        ) {
            let Some(container) = this.final_res.get(&container).copied() else {
                return;
            };
            let container = this.symbols.get_mut(container);
            let prev = if is_export {
                container.exports.replace(table)
            } else {
                container.members.replace(table)
            };
            assert!(prev.is_none());
        }
        use super::symbol::SymbolTableLocationKind;
        match location.kind {
            SymbolTableLocationKind::SymbolMember => inner(self, location.container, false, table),
            SymbolTableLocationKind::SymbolExports => inner(self, location.container, true, table),
            SymbolTableLocationKind::ContainerLocals => {
                assert!(self.p.node(location.container).has_locals());
                let prev = self.locals.insert(location.container, table);
                assert!(prev.is_none());
            }
        }
    }
}

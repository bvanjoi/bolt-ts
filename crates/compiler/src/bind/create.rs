use rustc_hash::FxHashMap;

use super::symbol::{SymbolFlags, SymbolTableLocation};
use super::{BinderState, ModuleInstanceState, Symbol, SymbolID, SymbolName, Symbols, errors};
use crate::bind::SymbolTable;
use crate::ir;
use crate::parser::ParseResult;

use bolt_ts_ast as ast;
use bolt_ts_utils::fx_hashmap_with_capacity;

pub(super) fn set_value_declaration(
    symbol: SymbolID,
    symbols: &mut Symbols,
    node: ast::NodeID,
    p: &ParseResult,
) {
    let s = symbols.get_mut(symbol);
    // TODO: ambient declaration
    if s.value_decl.is_none_or(|value_decl| {
        let v = p.node(value_decl);
        !v.is_same_kind(&p.node(node)) && v.is_effective_module_decl()
    }) {
        s.value_decl = Some(node);
    }
}

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn create_final_res(&mut self, id: ast::NodeID, symbol: SymbolID) {
        let prev = self.final_res.insert(id, symbol);
        assert!(prev.is_none(), "prev: {:#?}", prev.unwrap());
    }

    pub(super) fn declare_symbol_with_ns(
        &mut self,
        name: SymbolName,
        container: ast::NodeID,
        is_export: bool,
        ns: &ast::NsDecl<'cx>,
    ) -> SymbolID {
        let key = (self.scope_id, name);
        let state = self.p.get_module_instance_state(ns, None);
        let instantiated = state != ModuleInstanceState::NonInstantiated;
        let includes = if instantiated {
            SymbolFlags::VALUE_MODULE
        } else {
            SymbolFlags::NAMESPACE_MODULE
        };
        let excludes = if instantiated {
            SymbolFlags::VALUE_MODULE_EXCLUDES
        } else {
            SymbolFlags::NAMESPACE_MODULE_EXCLUDES
        };
        let loc = self.temp_local(container, is_export);
        let id = self.declare_symbol_and_add_to_symbol_table(
            container, name, ns.id, loc, includes, excludes,
        );
        id
    }

    pub(super) fn declare_symbol(
        &mut self,
        name: Option<SymbolName>,
        table: SymbolTableLocation,
        parent: Option<SymbolID>,
        node: ast::NodeID,
        includes: SymbolFlags,
        excludes: SymbolFlags,
        is_replaceable_by_method: bool,
        is_computed_name: bool,
    ) -> SymbolID {
        let symbol;
        if let Some(name) = name {
            let old = self
                .get_symbol_table_by_location(table)
                .and_then(|t| t.get(&name))
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
                if let Some(table) = self.get_symbol_table_by_location(table) {
                    let prev = table.insert(name, symbol);
                    assert!(prev.is_none());
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
        s.flags |= flags;
        s.decls.push(node);

        // if flags.intersects(
        //     SymbolFlags::CLASS | SymbolFlags::ENUM | SymbolFlags::MODULE | SymbolFlags::VARIABLE,
        // ) && m.exports.is_none()
        // {
        //     m.exports = Some(Default::default());
        // }
        // if flags.intersects(
        //     SymbolFlags::CLASS
        //         | SymbolFlags::INTERFACE
        //         | SymbolFlags::TYPE_LITERAL
        //         | SymbolFlags::OBJECT_LITERAL,
        // ) && m.members.is_none()
        // {
        //     m.members = Some(Default::default());
        // }

        if s.const_enum_only_module.is_some_and(|y| y)
            && s.flags
                .intersects(SymbolFlags::FUNCTION | SymbolFlags::CLASS | SymbolFlags::REGULAR_ENUM)
        {
            s.const_enum_only_module = Some(false);
        }

        if flags.intersects(SymbolFlags::VALUE) {
            self.set_value_declaration(symbol, node);
        }
    }

    fn set_value_declaration(&mut self, symbol: SymbolID, node: ast::NodeID) {
        set_value_declaration(symbol, &mut self.symbols, node, &self.p);
    }

    pub(super) fn create_symbol(&mut self, name: SymbolName, flags: SymbolFlags) -> SymbolID {
        let s = Symbol {
            name,
            flags,
            decls: Default::default(),
            value_decl: None,
            members: Default::default(),
            exports: Default::default(),
            parent: None,
            const_enum_only_module: None,
            is_replaceable_by_method: None,
            merged_id: None,
            export_symbol: None,
        };
        self.symbols.insert(s)
    }

    fn opt_members(
        &mut self,
        container: ast::NodeID,
        is_export: bool,
    ) -> Option<&mut FxHashMap<super::SymbolName, super::SymbolID>> {
        let container = self.final_res.get(&container).copied()?;
        let container = self.symbols.get_mut(container);
        let map = if is_export {
            &mut container.exports.0
        } else {
            &mut container.members.0
        };
        Some(map)
    }

    fn get_symbol_table_by_location(
        &mut self,
        location: SymbolTableLocation,
    ) -> Option<&mut FxHashMap<SymbolName, SymbolID>> {
        use super::symbol::SymbolTableLocationKind;
        match location.kind {
            SymbolTableLocationKind::SymbolMember => self.opt_members(location.container, false),
            SymbolTableLocationKind::SymbolExports => self.opt_members(location.container, true),
            SymbolTableLocationKind::ContainerLocals => {
                assert!(self.p.node(location.container).has_locals());
                Some(
                    &mut self
                        .locals
                        .entry(location.container)
                        .or_insert_with(|| SymbolTable(fx_hashmap_with_capacity(64)))
                        .0,
                )
            }
        }
    }

    pub(super) fn create_fn_expr_symbol(
        &mut self,
        f: &impl ir::FnExprLike<'cx>,
        decl: ast::NodeID,
    ) {
        let name = f.name().map(SymbolName::Normal).unwrap_or(SymbolName::Fn);
        let symbol = self.bind_anonymous_decl(f.id(), SymbolFlags::FUNCTION, name);
        self.create_final_res(decl, symbol);
    }

    pub(super) fn create_fn_ty_symbol(&mut self, id: ast::NodeID, symbol_name: SymbolName) {
        let symbol = self.create_symbol(symbol_name, SymbolFlags::SIGNATURE);
        self.add_declaration_to_symbol(symbol, id, SymbolFlags::SIGNATURE);

        let ty_lit_symbol = self.create_symbol(symbol_name, SymbolFlags::TYPE_LITERAL);
        self.add_declaration_to_symbol(ty_lit_symbol, id, SymbolFlags::TYPE_LITERAL);
        self.symbols
            .get_mut(ty_lit_symbol)
            .members
            .0
            .insert(symbol_name, symbol);
        self.create_final_res(id, ty_lit_symbol);
    }

    pub(super) fn create_object_member_symbol(
        &mut self,
        container: ast::NodeID,
        name: SymbolName,
        member: ast::NodeID,
        is_optional: bool,
    ) -> SymbolID {
        let flags = SymbolFlags::PROPERTY
            | if is_optional {
                SymbolFlags::OPTIONAL
            } else {
                SymbolFlags::empty()
            };

        let loc = SymbolTableLocation::members(container);
        let symbol = self.declare_symbol_and_add_to_symbol_table(
            container,
            name,
            member,
            loc,
            flags,
            SymbolFlags::PROPERTY_EXCLUDES,
        );
        self.create_final_res(member, symbol);
        symbol
    }
}

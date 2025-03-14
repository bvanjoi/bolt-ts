use bolt_ts_atom::AtomId;

use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

use super::symbol::{
    BlockContainerSymbol, FnSymbol, MergedSymbol, ObjectSymbol, SymbolFlags, SymbolTableLocation,
};
use super::{
    BinderState, ModuleInstanceState, Symbol, SymbolFnKind, SymbolID, SymbolKind, SymbolName,
    errors,
};
use crate::ir;

use bolt_ts_ast as ast;
use bolt_ts_utils::fx_hashmap_with_capacity;

impl<'cx> BinderState<'cx, '_, '_> {
    pub(super) fn create_final_res(&mut self, id: ast::NodeID, symbol: SymbolID) {
        let prev = self.final_res.insert(id, symbol);
        assert!(prev.is_none(), "prev: {:#?}", prev.unwrap());
    }

    pub(super) fn create_var_symbol(
        &mut self,
        name: AtomId,
        flags: SymbolFlags,
        kind: SymbolKind,
        exclude: SymbolFlags,
    ) -> SymbolID {
        let name = SymbolName::Normal(name);
        self.declare_symbol(name, flags, kind, exclude)
    }

    pub(super) fn declare_symbol(
        &mut self,
        name: SymbolName,
        flags: SymbolFlags,
        kind: SymbolKind,
        exclude: SymbolFlags,
    ) -> SymbolID {
        let key = (self.scope_id, name);
        if name.as_atom().is_some() {
            if let Some(id) = self.res.get(&key).copied() {
                let prev = self.symbols.get_mut(id);
                if flags.intersects(SymbolFlags::TYPE_PARAMETER)
                    && kind.opt_decl().is_some_and(|p| {
                        self.p
                            .parent(p)
                            .is_some_and(|n| self.p.node(n).is_infer_ty())
                    })
                {
                    let id = self.symbols.insert(Symbol::new(name, flags, kind));
                    // dont insert into resolution because infer type param had been stored into locals.
                    return id;
                } else if flags.intersects(SymbolFlags::ALIAS)
                    || prev.flags.intersects(SymbolFlags::ALIAS)
                {
                    let id = self.symbols.insert(Symbol::new(name, flags, kind));
                    let _prev = self.res.insert(key, id);
                    return id;
                }
                if flags == SymbolFlags::FUNCTION_SCOPED_VARIABLE {
                    prev.flags |= flags;
                    let prev = &mut prev.kind;
                    prev.0 = kind;
                } else if matches!(prev.kind.0, SymbolKind::Err) {
                    prev.flags |= flags;
                    let prev = &mut prev.kind;
                    assert!(prev.1.is_some());
                    prev.0 = kind;
                } else if prev.flags.intersects(exclude) || prev.flags == SymbolFlags::PROPERTY {
                    let n = self.atoms.get(name.expect_atom());
                    let span = |kind: &SymbolKind| {
                        let id = match kind {
                            SymbolKind::Err
                            | SymbolKind::BlockContainer { .. }
                            | SymbolKind::FunctionScopedVar { .. } => unreachable!(),
                            SymbolKind::BlockScopedVar { .. } => todo!(),
                            _ => todo!("name: {n:#?}, kind: {kind:#?}"),
                        };
                        self.p.node(id).ident_name().unwrap().span
                    };

                    let error_span = span(&kind);

                    let error = errors::DuplicateIdentifier {
                        span: error_span,
                        name: n.to_string(),
                        original_span: span(&prev.kind.0),
                    };
                    self.push_error(Box::new(error));

                    if flags.intersects(SymbolFlags::PROPERTY) {
                        let id = self.symbols.insert(Symbol::new(name, flags, kind));
                        self.res.insert(key, id);
                        return id;
                    }
                }
                return id;
            }
        }
        let id = self.symbols.insert(Symbol::new(name, flags, kind));
        self.res.insert(key, id);
        id
    }

    pub(super) fn declare_symbol_with_interface(
        &mut self,
        name: SymbolName,
        container: ast::NodeID,
        is_export: bool,
        node: ast::NodeID,
    ) -> SymbolID {
        let key = (self.scope_id, name);
        let location = SymbolTableLocation::Local {
            container,
            is_export,
        };
        let flags = SymbolFlags::INTERFACE;
        let id = self._declare_symbol(
            Some(name),
            location,
            None,
            node,
            flags,
            SymbolFlags::INTERFACE_EXCLUDES,
            false,
            false,
        );
        self.res.insert(key, id);
        id
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
        let id = self.declare_symbol_and_add_to_symbol_table(
            container,
            name,
            None,
            ns.id,
            is_export,
            Some(includes),
            Some(excludes),
        );
        self.res.insert(key, id);
        id
    }

    pub(super) fn _declare_symbol(
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
                .and_then(|t| t.get(&name).copied());
            if includes.intersects(SymbolFlags::CLASSIFIABLE) {
                // todo!()
            }
            if let Some(old) = old {
                // ==== TODO: delete
                symbol = self.create_symbol(name, includes);
                if let Some(table) = self.get_symbol_table_by_location(table) {
                    table.insert(name, symbol);
                }

                if is_replaceable_by_method {
                    todo!()
                } else if self.symbols.get(old).flags.intersects(excludes) {
                    let error = errors::DuplicateIdentifier {
                        span: self.p.node(node).ident_name().unwrap().span,
                        name: self.atoms.get(name.expect_atom()).to_string(),
                        original_span: self
                            .p
                            .node(self.symbols.get(old).opt_decl().unwrap())
                            .ident_name()
                            .unwrap()
                            .span,
                    };
                    self.push_error(Box::new(error));
                }
            } else {
                symbol = self.create_symbol(name, includes);
                if let Some(table) = self.get_symbol_table_by_location(table) {
                    let prev = table.insert(name, symbol);
                    assert!(prev.is_none());
                }

                if is_replaceable_by_method {}
            }
        } else {
            todo!()
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
        let m = s.kind.1.as_mut().unwrap();
        m.decls.push(node);

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

        if m.const_enum_only_module.is_some_and(|y| y)
            && s.flags
                .intersects(SymbolFlags::FUNCTION | SymbolFlags::CLASS | SymbolFlags::REGULAR_ENUM)
        {
            m.const_enum_only_module = Some(false);
        }

        if flags.intersects(SymbolFlags::VALUE) {
            self.set_value_declaration(symbol, node);
        }
    }

    fn set_value_declaration(&mut self, symbol: SymbolID, node: ast::NodeID) {
        let s = self.symbols.get_mut(symbol);
        let m = s.kind.1.as_mut().unwrap();
        // TODO: ambient declaration
        if m.value_decl.is_none_or(|value_decl| {
            let v = self.p.node(value_decl);
            !v.is_same_kind(&self.p.node(node)) && v.is_effective_module_decl()
        }) {
            m.value_decl = Some(node);
        }
    }

    pub(super) fn create_symbol(&mut self, name: SymbolName, flags: SymbolFlags) -> SymbolID {
        let s = MergedSymbol {
            decls: Default::default(),
            value_decl: None,
            members: Default::default(),
            exports: Default::default(),
            parent: None,
            const_enum_only_module: Default::default(),
        };
        let s = Symbol::new_symbol(name, flags, s);
        let id = self.symbols.insert(s);
        id
    }

    fn get_symbol_table_by_location(
        &mut self,
        location: SymbolTableLocation,
    ) -> Option<&mut FxHashMap<SymbolName, SymbolID>> {
        match location {
            SymbolTableLocation::Symbol { .. } => todo!(),
            SymbolTableLocation::Local {
                container,
                is_export,
            } => self.opt_members(container, is_export),
        }
    }

    // fn get_mut_symbol_table_by_location(
    //     &mut self,
    //     location: SymbolTableLocation,
    // ) -> &mut FxHashMap<SymbolName, SymbolID> {
    //     &mut Default::default()
    // }

    pub(super) fn create_fn_expr_symbol(
        &mut self,
        f: &impl ir::FnExprLike<'cx>,
        decl: ast::NodeID,
    ) {
        let name = f.name().map(SymbolName::Normal).unwrap_or(SymbolName::Fn);
        let symbol = self.declare_symbol(
            name,
            SymbolFlags::FUNCTION,
            SymbolKind::Fn(FnSymbol {
                kind: SymbolFnKind::FnExpr,
                decls: thin_vec::thin_vec![decl],
            }),
            SymbolFlags::empty(),
        );
        self.create_final_res(decl, symbol);
    }

    pub(super) fn create_interface_symbol(
        &mut self,
        id: ast::NodeID,
        name: AtomId,
        container: ast::NodeID,
        is_export: bool,
    ) -> SymbolID {
        let symbol =
            self.declare_symbol_with_interface(SymbolName::Normal(name), container, is_export, id);
        self.create_final_res(id, symbol);
        symbol
    }

    pub(super) fn create_fn_symbol(
        &mut self,
        container: ast::NodeID,
        decl: &'cx ast::FnDecl<'cx>,
    ) -> SymbolID {
        let ele_name = SymbolName::Normal(decl.name.name);
        self.create_fn_decl_like_symbol(container, decl, ele_name, SymbolFnKind::FnDecl, false)
    }

    pub(super) fn create_fn_ty_symbol(&mut self, id: ast::NodeID, symbol_name: SymbolName) {
        let symbol = self.declare_symbol(
            symbol_name,
            SymbolFlags::SIGNATURE,
            SymbolKind::Fn(FnSymbol {
                kind: SymbolFnKind::Call,
                decls: thin_vec::thin_vec![id],
            }),
            SymbolFlags::empty(),
        );

        let members = FxHashMap::from_iter([(symbol_name, symbol)]);
        self.create_object_lit_ty_symbol(id, members);
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

        let symbol = self.declare_symbol_and_add_to_symbol_table(
            container,
            name,
            None,
            member,
            false,
            Some(flags),
            Some(SymbolFlags::PROPERTY_EXCLUDES),
        );
        self.create_final_res(member, symbol);
        symbol
    }

    pub(super) fn create_object_lit_ty_symbol(
        &mut self,
        node_id: ast::NodeID,
        members: FxHashMap<SymbolName, SymbolID>,
    ) -> SymbolID {
        let id = self.symbols.insert(Symbol::new(
            SymbolName::Type,
            SymbolFlags::TYPE_LITERAL,
            SymbolKind::TyLit(super::TyLitSymbol {
                decl: node_id,
                members,
            }),
        ));
        self.create_final_res(node_id, id);
        id
    }

    pub(super) fn create_object_lit_symbol(
        &mut self,
        node_id: ast::NodeID,
        members: FxHashMap<SymbolName, SymbolID>,
    ) -> SymbolID {
        let id = self.symbols.insert(Symbol::new(
            SymbolName::Object,
            SymbolFlags::OBJECT_LITERAL,
            SymbolKind::Object(ObjectSymbol {
                decl: node_id,
                members,
            }),
        ));
        self.create_final_res(node_id, id);
        id
    }

    pub(super) fn create_block_container_symbol(&mut self, node_id: ast::NodeID) -> SymbolID {
        let symbol = self.declare_symbol(
            SymbolName::Container,
            SymbolFlags::VALUE_MODULE,
            SymbolKind::BlockContainer(BlockContainerSymbol {
                locals: fx_hashmap_with_capacity(32),
                exports: fx_hashmap_with_capacity(32),
            }),
            SymbolFlags::empty(),
        );
        self.create_final_res(node_id, symbol);
        symbol
    }
}

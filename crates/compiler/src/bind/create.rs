use bolt_ts_atom::AtomId;

use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

use super::symbol::{
    BlockContainerSymbol, FnSymbol, InterfaceSymbol, NsSymbol, ObjectSymbol, PropSymbol,
    SymbolFlags,
};
use super::{BinderState, Symbol, SymbolFnKind, SymbolID, SymbolKind, SymbolName, errors};
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
                    assert!(prev.1.is_some() || prev.2.is_some());
                    prev.0 = kind;
                } else if prev.flags.intersects(exclude) || prev.flags == SymbolFlags::PROPERTY {
                    let n = self.atoms.get(name.expect_atom());
                    let span = |kind: &SymbolKind| {
                        let id = match kind {
                            SymbolKind::Class(c) => c.decl,
                            SymbolKind::Prop(p) => p.decl,
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
        flags: SymbolFlags,
        i: InterfaceSymbol,
    ) -> SymbolID {
        let key = (self.scope_id, name);
        // if name.as_atom().is_some() {
        //     if let Some(id) = self.res.get(&key) {
        //         let prev = self.symbols.get_mut(*id);
        //         prev.flags |= flags;
        //         let prev = &mut prev.kind;
        //         if !matches!(prev.0, SymbolKind::Err) {
        //             // todo: symbol merge
        //         }
        //         assert!(prev.1.is_none());
        //         prev.1 = Some(i);
        //         return *id;
        //     }
        // }
        if let Some(old_id) = self.res.get(&key) {
            let old = self.symbols.get_mut(*old_id);
            if let SymbolKind::FunctionScopedVar(_) = &old.kind.0 {
                old.flags |= flags;
                old.kind.1 = Some(i);
                return *old_id;
            }
        }
        let id = self.symbols.insert(Symbol::new_interface(name, flags, i));
        self.res.insert(key, id);
        id
    }

    pub(super) fn declare_symbol_with_ns(
        &mut self,
        name: SymbolName,
        flags: SymbolFlags,
        i: NsSymbol,
    ) -> SymbolID {
        let key = (self.scope_id, name);
        // if name.as_atom().is_some() {
        //     if let Some(id) = self.res.get(&key) {
        //         let prev = self.symbols.get_mut(*id);
        //         prev.flags |= flags;
        //         let prev = &mut prev.kind;
        //         if !matches!(prev.0, SymbolKind::Err) {
        //             todo!("error handler")
        //         }
        //         // assert!(prev.2.is_none());
        //         prev.2 = Some(i);
        //         return *id;
        //     }
        // }
        let id = self.symbols.insert(Symbol::new_ns(name, flags, i));
        let prev = self.res.insert(key, id);
        id
    }

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
        members: FxHashMap<SymbolName, SymbolID>,
    ) -> SymbolID {
        let symbol = self.declare_symbol_with_interface(
            SymbolName::Normal(name),
            SymbolFlags::INTERFACE,
            InterfaceSymbol {
                decls: thin_vec::thin_vec![id],
                members,
            },
        );
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

        let symbol = self.declare_symbol(
            name,
            flags,
            SymbolKind::Prop(PropSymbol { decl: member }),
            SymbolFlags::empty(),
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

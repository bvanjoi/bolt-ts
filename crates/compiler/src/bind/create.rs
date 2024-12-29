use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

use super::symbol::{
    BlockContainerSymbol, FnSymbol, InterfaceSymbol, NsSymbol, ObjectSymbol, PropSymbol,
    SymbolFlags,
};
use super::{errors, BinderState, Symbol, SymbolFnKind, SymbolID, SymbolKind, SymbolName};
use crate::ast;
use crate::atoms::AtomId;
use crate::utils::fx_hashmap_with_capacity;

impl<'cx> BinderState<'cx> {
    pub(super) fn create_final_res(&mut self, id: ast::NodeID, symbol: SymbolID) {
        let prev = self.final_res.insert(id, symbol);
        assert!(prev.is_none());
    }

    fn next_symbol_id(&mut self) -> SymbolID {
        let id = self.symbol_id;
        self.symbol_id = self.symbol_id.next();
        id
    }

    pub(super) fn create_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>, kind: SymbolKind<'cx>) {
        let flags = if matches!(kind, SymbolKind::BlockScopedVar { .. }) {
            SymbolFlags::BLOCK_SCOPED_VARIABLE
        } else {
            SymbolFlags::FUNCTION_SCOPED_VARIABLE
        };
        let symbol = self.create_var_symbol(decl.binding.name, flags, kind);
        self.create_final_res(decl.id, symbol);
    }

    pub(super) fn create_var_symbol(
        &mut self,
        name: AtomId,
        flags: SymbolFlags,
        kind: SymbolKind<'cx>,
    ) -> SymbolID {
        let name = SymbolName::Normal(name);
        self.create_symbol(name, flags, kind)
    }

    pub(super) fn create_symbol(
        &mut self,
        name: SymbolName,
        flags: SymbolFlags,
        kind: SymbolKind<'cx>,
    ) -> SymbolID {
        let key = (self.scope_id, name);
        if name.as_atom().is_some() {
            if let Some(id) = self.res.get(&key).copied() {
                let prev = self.symbols.get_mut(id);
                if flags == SymbolFlags::FUNCTION_SCOPED_VARIABLE {
                    prev.flags |= flags;
                    let prev = &mut prev.kind;
                    prev.0 = kind;
                } else if matches!(prev.kind.0, SymbolKind::Err) {
                    prev.flags |= flags;
                    let prev = &mut prev.kind;
                    assert!(prev.1.is_some());
                    prev.0 = kind;
                } else {
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
                    self.push_error(error_span.module, error.into());

                    if flags.intersects(SymbolFlags::PROPERTY) {
                        let id = self.next_symbol_id();
                        self.symbols.insert(id, Symbol::new(name, flags, kind));
                        let prev = self.res.insert(key, id);
                        return id;
                    }
                }
                return id;
            }
        }
        let id = self.next_symbol_id();
        self.symbols.insert(id, Symbol::new(name, flags, kind));
        let prev = self.res.insert(key, id);
        id
    }

    pub(super) fn create_symbol_with_interface(
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
        let id = self.next_symbol_id();
        self.symbols
            .insert(id, Symbol::new_interface(name, flags, i));
        let prev = self.res.insert(key, id);
        id
    }

    pub(super) fn create_symbol_with_ns(
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
        let id = self.next_symbol_id();
        self.symbols.insert(id, Symbol::new_ns(name, flags, i));
        let prev = self.res.insert(key, id);
        id
    }

    pub(super) fn create_fn_expr_symbol(&mut self, id: ast::NodeID) {
        let symbol = self.create_symbol(
            SymbolName::Fn,
            SymbolFlags::FUNCTION,
            SymbolKind::Fn(FnSymbol {
                kind: SymbolFnKind::FnExpr,
                decls: thin_vec::thin_vec![id],
            }),
        );
        self.final_res.insert(id, symbol);
    }

    pub(super) fn create_interface_symbol(
        &mut self,
        id: ast::NodeID,
        name: AtomId,
        members: FxHashMap<SymbolName, SymbolID>,
    ) -> SymbolID {
        let symbol = self.create_symbol_with_interface(
            SymbolName::Normal(name),
            SymbolFlags::INTERFACE,
            InterfaceSymbol { decl: id, members },
        );
        self.final_res.insert(id, symbol);
        symbol
    }

    pub(super) fn create_fn_symbol(&mut self, container: ast::NodeID, decl: &'cx ast::FnDecl<'cx>) {
        let ele_name = SymbolName::Normal(decl.name.name);
        self.create_fn_decl_like_symbol(container, decl, ele_name, SymbolFnKind::FnDecl);
    }

    pub(super) fn create_object_member_symbol(
        &mut self,
        name: SymbolName,
        member: ast::NodeID,
    ) -> SymbolID {
        let symbol = self.create_symbol(
            name,
            SymbolFlags::PROPERTY,
            SymbolKind::Prop(PropSymbol { decl: member }),
        );
        self.create_final_res(member, symbol);
        symbol
    }

    pub(super) fn create_object_lit_symbol(
        &mut self,
        node_id: ast::NodeID,
        members: FxHashMap<SymbolName, SymbolID>,
    ) -> SymbolID {
        let id = self.next_symbol_id();
        self.symbols.insert(
            id,
            Symbol::new(
                SymbolName::Object,
                SymbolFlags::OBJECT_LITERAL,
                SymbolKind::Object(ObjectSymbol {
                    decl: node_id,
                    members,
                }),
            ),
        );
        self.create_final_res(node_id, id);
        id
    }

    pub(super) fn create_block_container_symbol(&mut self, node_id: ast::NodeID) -> SymbolID {
        let symbol = self.create_symbol(
            SymbolName::Container,
            SymbolFlags::VALUE_MODULE,
            SymbolKind::BlockContainer(BlockContainerSymbol {
                locals: fx_hashmap_with_capacity(32),
            }),
        );
        self.create_final_res(node_id, symbol);
        symbol
    }
}

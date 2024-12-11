use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

use super::symbol::{FnSymbol, InterfaceSymbol, ObjectSymbol, PropSymbol, SymbolFlags};
use super::{BinderState, Symbol, SymbolFnKind, SymbolID, SymbolKind, SymbolName};
use crate::ast;
use crate::atoms::AtomId;

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

    pub(super) fn create_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>, kind: SymbolKind) {
        let flags = if matches!(kind, SymbolKind::BlockScopedVar) {
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
        kind: SymbolKind,
    ) -> SymbolID {
        let name = SymbolName::Normal(name);
        self.create_symbol(name, flags, kind)
    }

    pub(super) fn create_symbol(
        &mut self,
        name: SymbolName,
        flags: SymbolFlags,
        kind: SymbolKind,
    ) -> SymbolID {
        // use super::SymbolKind::*;
        let id = self.next_symbol_id();
        // let is_blocked_scope_var = matches!(kind, BlockScopedVar);
        self.symbols.insert(id, Symbol::new(name, flags, kind));
        let prev = self.res.insert((self.scope_id, name), id);
        // if !is_blocked_scope_var {
        //     // assert!(prev.is_none(), "`{name:#?}` is a duplicate symbol ");
        // }
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
    ) {
        let symbol = self.create_symbol(
            SymbolName::Normal(name),
            SymbolFlags::INTERFACE,
            SymbolKind::Interface(InterfaceSymbol { decl: id, members }),
        );
        self.final_res.insert(id, symbol);
    }

    pub(super) fn create_fn_symbol(&mut self, container: ast::NodeID, f: &'cx ast::FnDecl) {
        let Some(container_symbol_id) = self.final_res.get(&container).copied() else {
            unreachable!()
        };
        let SymbolKind::BlockContainer { locals, .. } =
            &mut self.symbols.get_mut(container_symbol_id).kind
        else {
            unreachable!()
        };

        let name = SymbolName::Normal(f.name.name);
        if let Some(s) = locals.get(&name).copied() {
            let symbol = self.symbols.get_mut(s);
            match &mut symbol.kind {
                SymbolKind::Fn(FnSymbol { decls, kind }) => {
                    assert!(*kind == super::SymbolFnKind::FnDecl);
                    assert!(!decls.is_empty());
                    decls.push(f.id)
                }
                _ => unreachable!(),
            }
            self.create_final_res(f.id, s);
        } else {
            let symbol = self.create_var_symbol(
                f.name.name,
                SymbolFlags::FUNCTION,
                SymbolKind::Fn(FnSymbol {
                    kind: super::SymbolFnKind::FnDecl,
                    decls: thin_vec![f.id],
                }),
            );
            self.create_final_res(f.id, symbol);
            let SymbolKind::BlockContainer { locals, .. } =
                &mut self.symbols.get_mut(container_symbol_id).kind
            else {
                unreachable!()
            };
            let prev = locals.insert(name, symbol);
            assert!(prev.is_none())
        }
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

    pub(super) fn create_block_container_symbol(&mut self, node_id: ast::NodeID) {
        let symbol = self.create_symbol(
            SymbolName::Container,
            SymbolFlags::VALUE_MODULE,
            SymbolKind::BlockContainer {
                locals: FxHashMap::default(),
            },
        );
        self.create_final_res(node_id, symbol);
    }
}

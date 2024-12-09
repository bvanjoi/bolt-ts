use super::{BinderState, ClassSymbol, SymbolID, SymbolKind, SymbolName};
use crate::{ast, ir};
use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

pub(super) trait ClassLike<'cx>: ir::ClassLike<'cx> {
    fn create_symbol(&'cx self, binder: &mut BinderState<'cx>) -> SymbolID;
}

impl<'cx> ClassLike<'cx> for ast::ClassDecl<'cx> {
    fn create_symbol(&'cx self, binder: &mut BinderState<'cx>) -> SymbolID {
        binder.create_class_decl(self)
    }
}

impl<'cx> ClassLike<'cx> for ast::ClassExpr<'cx> {
    fn create_symbol(&'cx self, binder: &mut BinderState<'cx>) -> SymbolID {
        binder.create_class_expr(self)
    }
}

impl<'cx> BinderState<'cx> {
    fn create_class_expr(&mut self, expr: &'cx ast::ClassExpr<'cx>) -> SymbolID {
        let name = SymbolName::ClassExpr;
        let symbol = self.create_symbol(
            name,
            SymbolKind::Class(ClassSymbol {
                decl: expr.id,
                members: FxHashMap::default(),
            }),
        );
        self.create_final_res(expr.id, symbol);
        symbol
    }
    fn create_class_decl(&mut self, decl: &'cx ast::ClassDecl<'cx>) -> SymbolID {
        let symbol = self.create_var_symbol(
            decl.name.name,
            SymbolKind::Class(ClassSymbol {
                decl: decl.id,
                members: FxHashMap::default(),
            }),
        );
        self.create_final_res(decl.id, symbol);
        symbol
    }

    fn create_class_prop_ele(
        &mut self,
        decl_id: ast::NodeID,
        ele: &'cx ast::ClassPropEle<'cx>,
    ) -> SymbolID {
        let name = Self::prop_name(ele.name);
        let symbol = self.create_symbol(name, SymbolKind::Property { decl: ele.id });
        let Some(class_symbol_id) = self.final_res.get(&decl_id).copied() else {
            unreachable!()
        };
        let SymbolKind::Class(ClassSymbol { members, .. }) =
            &mut self.symbols.get_mut(class_symbol_id).kind
        else {
            unreachable!()
        };
        members.insert(name, symbol);
        self.create_final_res(ele.id, symbol);
        symbol
    }

    fn create_class_fn_like_ele(
        &mut self,
        decl_id: ast::NodeID,
        ele_name: SymbolName,
        ele_id: ast::NodeID,
        ele_kind: super::SymbolFnKind,
    ) {
        let Some(class_symbol_id) = self.final_res.get(&decl_id).copied() else {
            unreachable!()
        };
        let SymbolKind::Class(ClassSymbol { members, .. }) =
            &mut self.symbols.get_mut(class_symbol_id).kind
        else {
            unreachable!()
        };
        if let Some(s) = members.get(&ele_name).copied() {
            let symbol = self.symbols.get_mut(s);
            match &mut symbol.kind {
                SymbolKind::Function { decls, kind } => {
                    assert!(*kind == ele_kind);
                    assert!(!decls.is_empty());
                    decls.push(ele_id)
                }
                _ => unreachable!(),
            };
            self.create_final_res(ele_id, s);
        } else {
            let symbol = self.create_symbol(
                ele_name,
                SymbolKind::Function {
                    kind: ele_kind,
                    decls: thin_vec![ele_id],
                },
            );
            self.create_final_res(ele_id, symbol);
            let SymbolKind::Class(ClassSymbol { members, .. }) =
                &mut self.symbols.get_mut(class_symbol_id).kind
            else {
                unreachable!()
            };
            let prev = members.insert(ele_name, symbol);
            assert!(prev.is_none())
        }
    }

    fn create_class_method_ele(
        &mut self,
        decl_id: ast::NodeID,
        ele: &'cx ast::ClassMethodEle<'cx>,
    ) {
        let ele_name = Self::prop_name(ele.name);
        self.create_class_fn_like_ele(decl_id, ele_name, ele.id, super::SymbolFnKind::Method);
    }

    fn create_class_ctor(&mut self, decl_id: ast::NodeID, ctor: &'cx ast::ClassCtor<'cx>) {
        self.create_class_fn_like_ele(
            decl_id,
            SymbolName::Constructor,
            ctor.id,
            super::SymbolFnKind::Ctor,
        );
    }

    fn bind_class_ctor(&mut self, decl_id: ast::NodeID, ele: &'cx ast::ClassCtor<'cx>) {
        self.create_class_ctor(decl_id, ele);
        self.bind_params(ele.params);
        if let Some(body) = ele.body {
            self.bind_block_stmt(body);
        }
    }

    fn bind_class_method_ele(&mut self, decl_id: ast::NodeID, ele: &'cx ast::ClassMethodEle<'cx>) {
        self.create_class_method_ele(decl_id, ele);
        self.bind_params(ele.params);
        if let Some(body) = ele.body {
            self.bind_block_stmt(body);
        }
    }

    fn bind_class_prop_ele(&mut self, decl_id: ast::NodeID, ele: &'cx ast::ClassPropEle<'cx>) {
        self.create_class_prop_ele(decl_id, ele);
        if let Some(ty) = ele.ty {
            self.bind_ty(ty);
        }
        if let Some(init) = ele.init {
            self.bind_expr(init);
        }
    }

    pub(super) fn bind_class_like(&mut self, class: &'cx impl ClassLike<'cx>) {
        self.connect(class.id());
        let class_symbol = class.create_symbol(self);

        if let Some(extends) = class.extends() {
            self.bind_expr(extends.expr);
        }

        if let Some(implements) = class.implements() {
            for ty in implements.tys {
                self.bind_ty(ty);
            }
        }

        let old = self.scope_id;
        self.scope_id = self.new_scope();
        for ele in class.elems().elems {
            match ele.kind {
                ast::ClassEleKind::Prop(n) => self.bind_class_prop_ele(class.id(), n),
                ast::ClassEleKind::Method(n) => self.bind_class_method_ele(class.id(), n),
                ast::ClassEleKind::Ctor(n) => self.bind_class_ctor(class.id(), n),
                ast::ClassEleKind::IndexSig(n) => {
                    let name = SymbolName::Index;
                    let symbol = self.bind_index_sig(n);
                    self.create_final_res(n.id, symbol);
                    let SymbolKind::Class(ClassSymbol { members, .. }) =
                        &mut self.symbols.get_mut(class_symbol).kind
                    else {
                        unreachable!("{:#?}", self.symbols.get(symbol))
                    };
                    let prev = members.insert(name, symbol);
                    // FIXME: multiple index sig
                    assert!(prev.is_none())
                }
                ast::ClassEleKind::Getter(_) => {}
                ast::ClassEleKind::Setter(_) => {}
            }
        }
        self.scope_id = old;
    }
}

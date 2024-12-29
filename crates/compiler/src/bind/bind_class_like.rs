use super::symbol::{PropSymbol, SymbolFlags};
use super::{BinderState, ClassSymbol, SymbolID, SymbolKind, SymbolName};
use crate::{ast, ir};
use rustc_hash::FxHashMap;

impl<'cx> BinderState<'cx> {
    fn create_class_symbol(&mut self, c: &impl ir::ClassLike<'cx>) -> SymbolID {
        let name = c
            .name()
            .map_or(SymbolName::ClassExpr, |name| SymbolName::Normal(name.name));
        let id = c.id();
        let symbol = self.create_symbol(
            name,
            SymbolFlags::CLASS,
            SymbolKind::Class(ClassSymbol {
                decl: id,
                members: FxHashMap::default(),
            }),
        );
        self.create_final_res(id, symbol);
        symbol
    }

    fn create_class_prop_ele(
        &mut self,
        decl_id: ast::NodeID,
        ele: &'cx ast::ClassPropEle<'cx>,
    ) -> SymbolID {
        let name = Self::prop_name(ele.name);
        let symbol = self.create_symbol(
            name,
            SymbolFlags::PROPERTY,
            SymbolKind::Prop(PropSymbol { decl: ele.id }),
        );
        let class_symbol_id = self.final_res[&decl_id];
        let SymbolKind::Class(ClassSymbol { members, .. }) =
            &mut self.symbols.get_mut(class_symbol_id).kind.0
        else {
            unreachable!()
        };
        members.insert(name, symbol);
        self.create_final_res(ele.id, symbol);
        symbol
    }

    fn bind_class_ctor(&mut self, container: ast::NodeID, ctor: &'cx ast::ClassCtor<'cx>) {
        self.create_fn_decl_like_symbol(
            container,
            ctor,
            SymbolName::Constructor,
            super::SymbolFnKind::Ctor,
        );
        self.bind_params(ctor.params);
        if let Some(body) = ctor.body {
            self.bind_block_stmt(body);
        }
    }

    fn bind_class_method_ele(
        &mut self,
        container: ast::NodeID,
        ele: &'cx ast::ClassMethodEle<'cx>,
    ) {
        self.create_fn_decl_like_symbol(
            container,
            ele,
            Self::prop_name(ele.name),
            super::SymbolFnKind::Method,
        );
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

    pub(super) fn bind_class_like(&mut self, class: &'cx impl ir::ClassLike<'cx>, is_expr: bool) {
        self.connect(class.id());
        let old_old = self.scope_id;
        if is_expr {
            self.scope_id = self.new_scope();
        }

        self.create_class_symbol(class);

        let old = self.scope_id;
        self.scope_id = self.new_scope();

        if let Some(ty_params) = class.ty_params() {
            self.bind_ty_params(ty_params);
        }

        if let Some(extends) = class.extends() {
            self.bind_expr(extends.expr);
        }

        if let Some(implements) = class.implements() {
            for ty in implements.list {
                self.bind_refer_ty(ty);
            }
        }

        for ele in class.elems().elems {
            match ele.kind {
                ast::ClassEleKind::Prop(n) => self.bind_class_prop_ele(class.id(), n),
                ast::ClassEleKind::Method(n) => self.bind_class_method_ele(class.id(), n),
                ast::ClassEleKind::Ctor(n) => self.bind_class_ctor(class.id(), n),
                ast::ClassEleKind::IndexSig(n) => {
                    self.bind_index_sig(class.id(), n);
                }
                ast::ClassEleKind::Getter(_) => {}
                ast::ClassEleKind::Setter(_) => {}
            }
        }
        self.scope_id = old;

        if is_expr {
            self.scope_id = old_old;
        }
    }
}

use super::symbol::{PropSymbol, SymbolFlags, SymbolKind};
use super::{prop_name, BinderState, ClassSymbol, SymbolID, SymbolName};
use crate::ast::ModifierKind;
use crate::{ast, ir};

use bolt_ts_utils::fx_hashmap_with_capacity;

impl<'cx> BinderState<'cx> {
    fn create_class_symbol(&mut self, c: &impl ir::ClassLike<'cx>) -> SymbolID {
        let name = c
            .name()
            .map_or(SymbolName::ClassExpr, |name| SymbolName::Normal(name.name));
        let id = c.id();
        let cap = c.elems().elems.len();
        let symbol = self.declare_symbol(
            name,
            SymbolFlags::CLASS,
            SymbolKind::Class(ClassSymbol {
                decl: id,
                members: fx_hashmap_with_capacity(cap),
                exports: fx_hashmap_with_capacity(cap),
            }),
            SymbolFlags::CLASS_EXCLUDES,
        );
        self.create_final_res(id, symbol);
        symbol
    }

    fn create_class_prop_ele(
        &mut self,
        decl_id: ast::NodeID,
        ele_name: SymbolName,
        ele_id: ast::NodeID,
        ele_modifiers: Option<&ast::Modifiers>,
    ) -> SymbolID {
        let symbol = self.declare_symbol(
            ele_name,
            SymbolFlags::PROPERTY,
            SymbolKind::Prop(PropSymbol { decl: ele_id }),
            SymbolFlags::PROPERTY_EXCLUDES,
        );
        let SymbolKind::Class(ClassSymbol {
            members, exports, ..
        }) = &mut self.symbols.get_mut(self.final_res[&decl_id]).kind.0
        else {
            unreachable!()
        };
        if ele_modifiers.is_some_and(|mods| mods.flags.contains(ModifierKind::Static)) {
            exports.insert(ele_name, symbol);
        } else {
            members.insert(ele_name, symbol);
        }
        symbol
    }

    fn bind_class_ctor(&mut self, container: ast::NodeID, ctor: &'cx ast::ClassCtor<'cx>) {
        self.create_fn_decl_like_symbol(
            container,
            ctor,
            SymbolName::Constructor,
            super::SymbolFnKind::Ctor,
            false,
        );
        for param in ctor.params {
            self.bind_param(param);

            if param.dotdotdot.is_none()
                && param
                    .modifiers
                    .is_some_and(|ms| ms.flags.contains(ModifierKind::Public))
            {
                self.create_class_prop_ele(
                    container,
                    SymbolName::Ele(param.name.name),
                    param.id,
                    param.modifiers,
                );
            }
        }
        if let Some(body) = ctor.body {
            self.bind_block_stmt(body);
        }
    }

    fn bind_class_method_ele(
        &mut self,
        container: ast::NodeID,
        ele: &'cx ast::ClassMethodElem<'cx>,
    ) {
        let is_static = ele
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ModifierKind::Static));
        self.create_fn_decl_like_symbol(
            container,
            ele,
            prop_name(ele.name),
            super::SymbolFnKind::Method,
            is_static,
        );
        if let Some(ty_params) = ele.ty_params {
            self.bind_ty_params(ty_params);
        }
        self.bind_params(ele.params);
        if let Some(body) = ele.body {
            self.bind_block_stmt(body);
        }
        if let Some(ty) = ele.ty {
            self.bind_ty(ty);
        }
    }

    fn bind_class_prop_ele(&mut self, decl_id: ast::NodeID, ele: &'cx ast::ClassPropElem<'cx>) {
        let symbol =
            self.create_class_prop_ele(decl_id, prop_name(ele.name), ele.id, ele.modifiers);
        self.create_final_res(ele.id, symbol);
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
            self.bind_entity_name(extends.name);
            if let Some(ty_args) = extends.ty_args {
                self.bind_tys(ty_args.list);
            }
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

use super::symbol::{SymbolFlags, SymbolTableLocation};
use super::{BinderState, SymbolID, SymbolName, prop_name};
use crate::ir;

use bolt_ts_ast as ast;
use bolt_ts_ast::ModifierKind;

impl<'cx> BinderState<'cx, '_, '_> {
    fn create_class_symbol(
        &mut self,
        c: &impl ir::ClassLike<'cx>,
        is_expr: bool,
        container: Option<ast::NodeID>,
    ) -> SymbolID {
        let name = c
            .name()
            .map_or(SymbolName::ClassExpr, |name| SymbolName::Normal(name.name));
        let id = c.id();
        let symbol = if let Some(container) = container {
            assert!(!is_expr);
            let is_export = c
                .modifiers()
                .is_some_and(|mods| mods.flags.contains(ModifierKind::Export));
            self.bind_block_scoped_decl(
                id,
                name,
                is_export,
                container,
                SymbolFlags::CLASS,
                SymbolFlags::CLASS_EXCLUDES,
            )
        } else {
            self.bind_anonymous_decl(id, SymbolFlags::CLASS, name)
        };
        self.create_final_res(id, symbol);
        symbol
    }

    fn create_class_prop_ele(
        &mut self,
        container: ast::NodeID,
        ele_name: SymbolName,
        ele_id: ast::NodeID,
        ele_modifiers: Option<&ast::Modifiers>,
    ) -> SymbolID {
        let is_export = ele_modifiers.is_some_and(|mods| mods.flags.contains(ModifierKind::Static));
        let loc = if is_export {
            SymbolTableLocation::exports(container)
        } else {
            SymbolTableLocation::members(container)
        };
        self.declare_symbol_and_add_to_symbol_table(
            container,
            ele_name,
            ele_id,
            loc,
            SymbolFlags::PROPERTY | SymbolFlags::ASSIGNMENT,
            SymbolFlags::empty(),
        )
    }

    fn bind_class_ctor(&mut self, container: ast::NodeID, ctor: &'cx ast::ClassCtor<'cx>) {
        self.create_fn_decl_like_symbol(
            container,
            ctor,
            SymbolName::Constructor,
            SymbolFlags::CONSTRUCTOR,
            SymbolFlags::empty(),
            false,
        );
        for param in ctor.params {
            self.bind_param(ctor.id, param);

            if param.dotdotdot.is_none()
                && param
                    .modifiers
                    .is_some_and(|ms| ms.flags.contains(ModifierKind::Public))
            {
                match param.name.kind {
                    bolt_ts_ast::BindingKind::Ident(ident) => self.create_class_prop_ele(
                        container,
                        SymbolName::Ele(ident.name),
                        param.id,
                        param.modifiers,
                    ),
                    bolt_ts_ast::BindingKind::ObjectPat(_) => todo!(),
                    bolt_ts_ast::BindingKind::ArrayPat(_) => todo!(),
                };
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
            SymbolFlags::METHOD,
            SymbolFlags::METHOD_EXCLUDES,
            is_static,
        );
        if let Some(ty_params) = ele.ty_params {
            self.bind_ty_params(ele.id, ty_params);
        }
        self.bind_params(ele.id, ele.params);
        if let Some(ty) = ele.ty {
            self.bind_ty(ty);
        }
        if let Some(body) = ele.body {
            self.bind_block_stmt(body);
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

    pub(super) fn bind_prop_or_method_or_access(
        &mut self,
        container: ast::NodeID,
        decl_id: ast::NodeID,
        name: SymbolName,
        is_export: bool,
        includes: SymbolFlags,
        excludes: SymbolFlags,
    ) -> SymbolID {
        let loc = if is_export {
            SymbolTableLocation::exports(container)
        } else {
            SymbolTableLocation::members(container)
        };
        // TODO: has dynamic name
        self.declare_symbol_and_add_to_symbol_table(
            container, name, decl_id, loc, includes, excludes,
        )
    }

    pub(super) fn bind_class_like(
        &mut self,
        container: Option<ast::NodeID>,
        class: &'cx impl ir::ClassLike<'cx>,
        is_expr: bool,
    ) {
        self.create_class_symbol(class, is_expr, container);

        if let Some(ty_params) = class.ty_params() {
            self.bind_ty_params(class.id(), ty_params);
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
                    let is_static = n
                        .modifiers
                        .is_some_and(|ms| ms.flags.contains(ModifierKind::Static));
                    self.bind_index_sig(class.id(), n, is_static);
                }
                ast::ClassEleKind::Getter(n) => {
                    let is_static = n
                        .modifiers
                        .is_some_and(|ms| ms.flags.contains(ModifierKind::Static));
                    let symbol = self.bind_prop_or_method_or_access(
                        class.id(),
                        n.id,
                        prop_name(n.name),
                        is_static,
                        SymbolFlags::GET_ACCESSOR,
                        SymbolFlags::GET_ACCESSOR_EXCLUDES,
                    );
                    self.create_final_res(n.id, symbol);
                    if let Some(ty) = n.ty {
                        self.bind_ty(ty);
                    }
                    if let Some(body) = n.body {
                        self.bind_block_stmt(body);
                    }
                }
                ast::ClassEleKind::Setter(n) => {
                    let is_static = n
                        .modifiers
                        .is_some_and(|ms| ms.flags.contains(ModifierKind::Static));
                    let symbol = self.bind_prop_or_method_or_access(
                        class.id(),
                        n.id,
                        prop_name(n.name),
                        is_static,
                        SymbolFlags::SET_ACCESSOR,
                        SymbolFlags::SET_ACCESSOR_EXCLUDES,
                    );
                    self.create_final_res(n.id, symbol);

                    assert!(n.params.len() == 1);
                    self.bind_params(n.id, n.params);
                    if let Some(body) = n.body {
                        self.bind_block_stmt(body);
                    }
                }
            }
        }
    }
}

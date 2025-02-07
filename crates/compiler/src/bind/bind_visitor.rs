use super::symbol;
use super::symbol::FunctionScopedVarSymbol;
use super::symbol::GetterSetterSymbol;
use super::symbol::IndexSymbol;
use super::symbol::{SymbolFlags, SymbolFnKind, SymbolKind};
use super::symbol::{SymbolID, SymbolName, Symbols};
use super::BinderState;
use super::ScopeID;

use bolt_ts_atom::AtomMap;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;
use thin_vec::thin_vec;

use crate::ast::ModifierKind;
use crate::ast::{self};
use crate::bind::prop_name;
use crate::bind::symbol::AliasSymbol;
use crate::parser::Parser;

impl<'cx> BinderState<'cx> {
    pub fn new(atoms: &'cx AtomMap, parser: &'cx Parser<'cx>, module_id: ModuleID) -> Self {
        let symbols = Symbols::new(module_id);
        BinderState {
            atoms,
            p: parser,
            scope_id: ScopeID::root(module_id),
            scope_id_parent_map: Vec::with_capacity(512),
            res: fx_hashmap_with_capacity(128),
            final_res: fx_hashmap_with_capacity(256),
            node_id_to_scope_id: fx_hashmap_with_capacity(32),
            symbols,
            diags: Vec::new(),
        }
    }

    pub(super) fn push_error(&mut self, error: crate::Diag) {
        let diag = bolt_ts_errors::Diag { inner: error };
        self.diags.push(diag);
    }

    pub(super) fn connect(&mut self, node_id: ast::NodeID) {
        let prev = self.node_id_to_scope_id.insert(node_id, self.scope_id);
        assert!(prev.is_none());
    }

    pub(super) fn new_scope(&mut self) -> ScopeID {
        let next = ScopeID {
            module: self.scope_id.module,
            index: self.scope_id_parent_map.len() as u32,
        };
        self.scope_id_parent_map.push(Some(self.scope_id));
        next
    }

    pub fn bind_program(&mut self, root: &'cx ast::Program) {
        assert_eq!(self.scope_id.index_as_u32(), 0);
        assert!(self.scope_id_parent_map.is_empty());
        self.scope_id_parent_map.push(None);
        self.connect(root.id);
        let id = self.create_block_container_symbol(root.id);
        assert_eq!(id.index_as_u32(), 1);
        for stmt in root.stmts {
            self.bind_stmt(root.id, stmt)
        }
    }

    fn bind_stmt(&mut self, container: ast::NodeID, stmt: &'cx ast::Stmt) {
        use ast::StmtKind::*;
        match stmt.kind {
            Empty(_) => (),
            Var(var) => self.bind_var_stmt(container, var),
            Expr(expr) => self.bind_expr(expr),
            Fn(f) => self.bind_fn_decl(container, f),
            If(stmt) => {
                self.bind_expr(stmt.expr);
                self.bind_stmt(container, stmt.then);
                if let Some(alt) = stmt.else_then {
                    self.bind_stmt(container, alt)
                }
            }
            Block(block) => self.bind_block_stmt(block),
            Return(ret) => {
                if let Some(expr) = ret.expr {
                    self.bind_expr(expr)
                }
            }
            Class(class) => self.bind_class_like(Some(container), class, false),
            Interface(interface) => self.bind_interface_decl(container, interface),
            Type(t) => self.bind_type_decl(t),
            Namespace(ns) => self.bind_ns_decl(container, ns),
            Throw(t) => {
                self.bind_expr(t.expr);
            }
            Enum(_) => {}
            Import(_) => {}
            Export(decl) => self.bind_export_decl(container, decl),
            For(n) => {
                if let Some(init) = &n.init {
                    self.bind_for_init(init);
                }
                if let Some(cond) = n.cond {
                    self.bind_expr(cond);
                }
                if let Some(update) = n.incr {
                    self.bind_expr(update);
                }
                self.bind_stmt(container, n.body);
            }
            ForOf(n) => {
                self.bind_for_init(&n.init);
                self.bind_expr(n.expr);
                self.bind_stmt(container, n.body);
            }
            ForIn(n) => {
                self.bind_for_init(&n.init);
                self.bind_expr(n.expr);
                self.bind_stmt(container, n.body);
            }
            Break(_) | Continue(_) => {}
            Try(n) => {
                self.bind_try_stmt(container, n);
            }
            While(n) => {}
            Do(n) => {}
        }
    }

    fn bind_try_stmt(&mut self, container: ast::NodeID, stmt: &'cx ast::TryStmt<'cx>) {
        self.bind_block_stmt(stmt.try_block);
        if let Some(catch) = stmt.catch_clause {
            let old = self.scope_id;
            self.scope_id = self.new_scope();
            if let Some(var) = catch.var {
                self.bind_var_binding(
                    None,
                    false,
                    var.binding,
                    ast::VarKind::Var,
                    var.id,
                    SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                    SymbolFlags::FUNCTION_SCOPED_VARIABLE_EXCLUDES,
                );
            }
            self.bind_block_stmt(catch.block);
            self.scope_id = old;
        }
        if let Some(finally) = stmt.finally_block {
            self.bind_block_stmt(finally);
        }
    }

    fn bind_for_init(&mut self, init: &ast::ForInitKind<'cx>) {
        use ast::ForInitKind::*;
        match init {
            Var((kind, var)) => self.bind_var_decls(None, var, *kind, false),
            Expr(expr) => self.bind_expr(expr),
        }
    }

    fn bind_spec_export(&mut self, container: ast::NodeID, spec: &'cx ast::ExportSpec<'cx>) {
        use ast::ExportSpecKind::*;
        let (name, symbol) = match spec.kind {
            Shorthand(spec) => {
                let name = spec.name.name;
                let name = SymbolName::Normal(name);
                let symbol = self.declare_symbol(
                    name,
                    SymbolFlags::ALIAS,
                    SymbolKind::Alias(AliasSymbol {
                        decl: spec.id,
                        source: name,
                        target: name,
                    }),
                    SymbolFlags::empty(),
                );
                self.create_final_res(spec.id, symbol);
                (name, symbol)
            }
            Named(named) => {
                use ast::ModuleExportNameKind::*;
                let n = |name: &ast::ModuleExportName| match name.kind {
                    Ident(ident) => SymbolName::Normal(ident.name),
                    StringLit(lit) => SymbolName::Normal(lit.val),
                };

                let name = n(named.name);
                let symbol = self.declare_symbol(
                    name,
                    SymbolFlags::ALIAS,
                    SymbolKind::Alias(AliasSymbol {
                        decl: named.id,
                        source: n(named.prop_name),
                        target: name,
                    }),
                    SymbolFlags::empty(),
                );
                self.create_final_res(named.id, symbol);
                (name, symbol)
            }
        };
        if let SymbolKind::BlockContainer(c) =
            &mut self.symbols.get_mut(self.final_res[&container]).kind.0
        {
            let prev = c.exports.insert(name, symbol);
            assert!(prev.is_none());
        } else {
            unreachable!()
        };
    }

    fn bind_export_decl(&mut self, container: ast::NodeID, decl: &'cx ast::ExportDecl<'cx>) {
        use ast::ExportClauseKind::*;
        match decl.clause.kind {
            Glob(glob_export) => todo!(),
            Ns(ns_export) => todo!(),
            Specs(n) => {
                for spec in n.list {
                    self.bind_spec_export(container, spec);
                }
            }
        }
    }

    fn bind_ns_decl(&mut self, container: ast::NodeID, ns: &'cx ast::NsDecl<'cx>) {
        let name = match ns.name {
            ast::ModuleName::Ident(ident) => ident.name,
            ast::ModuleName::StringLit(_) => {
                if let Some(block) = ns.block {
                    self.bind_block_stmt(block);
                }
                return;
            }
        };
        let flags = if ns.block.is_some_and(|block| block.stmts.is_empty()) {
            SymbolFlags::NAMESPACE_MODULE
        } else {
            SymbolFlags::VALUE_MODULE
        };
        let symbol = self.declare_symbol_with_ns(
            SymbolName::Normal(name),
            flags,
            super::symbol::NsSymbol {
                decls: thin_vec::thin_vec![ns.id],
                exports: fx_hashmap_with_capacity(16),
                members: fx_hashmap_with_capacity(16),
            },
        );
        self.create_final_res(ns.id, symbol);

        let is_export = ns
            .modifiers
            .is_some_and(|mods| mods.flags.contains(ModifierKind::Export));
        let name = SymbolName::Normal(name);
        self.members(container, is_export).insert(name, symbol);

        let container = self.final_res[&container];
        if let SymbolKind::BlockContainer(c) = &mut self.symbols.get_mut(container).kind.0 {
            c.locals.insert(name, symbol);
        }
        if let Some(block) = ns.block {
            self.bind_block_stmt_with_container(ns.id, block);
        }
    }

    fn bind_type_decl(&mut self, t: &'cx ast::TypeDecl<'cx>) {
        let symbol = self.create_var_symbol(
            t.name.name,
            SymbolFlags::TYPE_ALIAS,
            SymbolKind::TyAlias(symbol::TyAliasSymbol { decl: t.id }),
            SymbolFlags::TYPE_ALIAS_EXCLUDES,
        );
        self.create_final_res(t.id, symbol);
        let old = self.scope_id;
        self.scope_id = self.new_scope();
        if let Some(ty_params) = t.ty_params {
            self.bind_ty_params(ty_params);
        }
        self.bind_ty(t.ty);
        self.scope_id = old;
    }

    pub(super) fn bind_ty_params(&mut self, ty_params: ast::TyParams<'cx>) {
        for ty_param in ty_params {
            self.bind_ty_param(ty_param)
        }
    }

    fn bind_ty_param(&mut self, ty_param: &'cx ast::TyParam<'cx>) {
        let symbol = self.create_var_symbol(
            ty_param.name.name,
            SymbolFlags::TYPE_PARAMETER,
            SymbolKind::TyParam(symbol::TyParamSymbol { decl: ty_param.id }),
            SymbolFlags::empty(),
        );
        self.create_final_res(ty_param.id, symbol);

        if let Some(constraint) = ty_param.constraint {
            self.bind_ty(constraint);
        }
        if let Some(default) = ty_param.default {
            self.bind_ty(default);
        }
    }

    pub(super) fn bind_index_sig(
        &mut self,
        container: ast::NodeID,
        index: &'cx ast::IndexSigDecl<'cx>,
        is_export: bool,
    ) -> SymbolID {
        let name = SymbolName::Index;
        let symbol = self.declare_symbol(
            name,
            SymbolFlags::SIGNATURE,
            SymbolKind::Index(IndexSymbol { decl: index.id }),
            SymbolFlags::empty(),
        );
        self.create_final_res(index.id, symbol);
        let container = self.final_res[&container];
        let s = self.symbols.get_mut(container);
        if let Some(i) = &mut s.kind.1 {
            let prev = i.members.insert(name, symbol);
            // FIXME: multiple index sig
            assert!(prev.is_none());
        } else if let SymbolKind::Class(c) = &mut s.kind.0 {
            if is_export {
                let prev = c.exports.insert(name, symbol);
                // FIXME: multiple index sig
                assert!(prev.is_none());
            } else {
                let prev = c.members.insert(name, symbol);
                // FIXME: multiple index sig
                assert!(prev.is_none());
            }
        } else if let SymbolKind::TyLit(o) = &mut s.kind.0 {
            let prev = o.members.insert(name, symbol);
            // FIXME: multiple index sig
            assert!(prev.is_none());
        } else if let SymbolKind::Object(_) = &mut s.kind.0 {
            unreachable!("object lit should not have index sig");
        } else {
            todo!()
        }

        self.bind_params(index.params);
        self.bind_ty(index.ty);

        symbol
    }

    fn insert_getter_setter_symbol(
        &mut self,
        container: ast::NodeID,
        name: &'cx ast::PropName<'cx>,
        decl: ast::NodeID,
        flags: SymbolFlags,
        exclude_flags: SymbolFlags,
        is_export: bool,
    ) {
        let name = prop_name(name);
        if let Some(name) = self.members(container, is_export).get(&name).copied() {
            if self.symbols.get(name).flags.intersects(exclude_flags) {
                todo!("duplicate identifier");
            } else if let SymbolKind::GetterSetter(symbol) = &mut self.symbols.get_mut(name).kind.0
            {
                if flags.intersects(SymbolFlags::GET_ACCESSOR) {
                    symbol.getter_decl = Some(decl);
                    assert!(symbol.setter_decl.is_some());
                } else {
                    symbol.setter_decl = Some(decl);
                    assert!(symbol.getter_decl.is_some());
                }
            } else {
                unreachable!()
            }
        } else {
            let symbol = if flags.intersects(SymbolFlags::GET_ACCESSOR) {
                self.declare_symbol(
                    name,
                    SymbolFlags::GET_ACCESSOR,
                    SymbolKind::GetterSetter(GetterSetterSymbol {
                        getter_decl: Some(decl),
                        setter_decl: None,
                    }),
                    SymbolFlags::GET_ACCESSOR_EXCLUDES,
                )
            } else {
                self.declare_symbol(
                    name,
                    SymbolFlags::SET_ACCESSOR,
                    SymbolKind::GetterSetter(GetterSetterSymbol {
                        getter_decl: None,
                        setter_decl: Some(decl),
                    }),
                    SymbolFlags::SET_ACCESSOR_EXCLUDES,
                )
            };
            self.create_final_res(decl, symbol);
            let prev = self.members(container, is_export).insert(name, symbol);
            assert!(prev.is_none());
        }
    }

    pub(super) fn bind_get_access(
        &mut self,
        container: ast::NodeID,
        getter: &'cx ast::GetterDecl<'cx>,
        is_export: bool,
    ) {
        self.insert_getter_setter_symbol(
            container,
            getter.name,
            getter.id,
            SymbolFlags::GET_ACCESSOR,
            SymbolFlags::GET_ACCESSOR_EXCLUDES,
            is_export,
        );
    }

    pub(super) fn bind_set_access(
        &mut self,
        container: ast::NodeID,
        setter: &'cx ast::SetterDecl<'cx>,
        is_export: bool,
    ) {
        self.insert_getter_setter_symbol(
            container,
            setter.name,
            setter.id,
            SymbolFlags::SET_ACCESSOR,
            SymbolFlags::SET_ACCESSOR_EXCLUDES,
            is_export,
        );
    }

    fn bind_object_ty_member(&mut self, container: ast::NodeID, m: &'cx ast::ObjectTyMember<'cx>) {
        use ast::ObjectTyMemberKind::*;
        match m.kind {
            Prop(m) => {
                if let Some(ty) = m.ty {
                    self.bind_ty(ty);
                }
                let name = prop_name(m.name);
                let symbol = self.create_object_member_symbol(name, m.id, m.question.is_some());
                let s = self.final_res[&container];
                let s = self.symbols.get_mut(s);
                if let Some(i) = &mut s.kind.1 {
                    if let std::collections::hash_map::Entry::Vacant(e) = i.members.entry(name) {
                        e.insert(symbol);
                    } else {
                        // TODO: prev
                    };
                } else if let SymbolKind::TyLit(o) = &mut s.kind.0 {
                    let prev = o.members.insert(name, symbol);
                    assert!(prev.is_none());
                } else {
                    todo!()
                }
            }
            Method(m) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();

                if let Some(ty_params) = m.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(m.params);
                if let Some(ty) = m.ty {
                    self.bind_ty(ty);
                }

                let name = prop_name(m.name);
                self.create_fn_decl_like_symbol(container, m, name, SymbolFnKind::Method, false);
                self.scope_id = old;
            }
            CallSig(call) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();

                if let Some(ty_params) = call.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(call.params);
                if let Some(ty) = call.ty {
                    self.bind_ty(ty);
                }

                let name = SymbolName::Call;
                self.create_fn_decl_like_symbol(container, call, name, SymbolFnKind::Call, false);
                self.scope_id = old;
            }
            IndexSig(index) => {
                self.bind_index_sig(container, index, false);
            }
            CtorSig(decl) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                if let Some(ty_params) = decl.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(decl.params);
                if let Some(ty) = decl.ty {
                    self.bind_ty(ty);
                }
                self.create_fn_decl_like_symbol(
                    container,
                    decl,
                    SymbolName::New,
                    SymbolFnKind::Ctor,
                    false,
                );
                self.scope_id = old;
            }
        }
    }

    fn bind_interface_decl(&mut self, container: ast::NodeID, i: &'cx ast::InterfaceDecl<'cx>) {
        let id = self.create_interface_symbol(i.id, i.name.name, Default::default());
        let is_export = i
            .modifiers
            .is_some_and(|ms| ms.flags.intersects(ModifierKind::Export));
        let name = SymbolName::Normal(i.name.name);
        let members = self.members(container, is_export);
        let _prev = members.insert(name, id);
        // assert!(prev.is_none());

        let old = self.scope_id;
        self.scope_id = self.new_scope();

        if let Some(ty_params) = i.ty_params {
            self.bind_ty_params(ty_params);
        }
        if let Some(extends) = i.extends {
            for ty in extends.list {
                self.bind_refer_ty(ty);
            }
        }

        for m in i.members {
            self.bind_object_ty_member(i.id, m)
        }
        self.scope_id = old;
    }

    pub(super) fn bind_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();

        self.create_block_container_symbol(block.id);

        for stmt in block.stmts {
            self.bind_stmt(block.id, stmt)
        }

        self.scope_id = old;
    }

    pub(super) fn bind_block_stmt_with_container(
        &mut self,
        container: ast::NodeID,
        block: &'cx ast::BlockStmt<'cx>,
    ) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();

        for stmt in block.stmts {
            self.bind_stmt(container, stmt)
        }

        self.scope_id = old;
    }

    fn bind_ident(&mut self, ident: &'cx ast::Ident) {
        self.connect(ident.id)
    }

    pub(super) fn bind_expr(&mut self, expr: &'cx ast::Expr) {
        use ast::ExprKind::*;
        match expr.kind {
            Ident(ident) => self.bind_ident(ident),
            Call(call) => self.bind_call_like(call),
            New(new) => self.bind_call_like(new),
            Bin(bin) => {
                self.bind_expr(bin.left);
                self.bind_expr(bin.right);
            }
            Assign(assign) => {
                self.bind_expr(assign.left);
                self.bind_expr(assign.right);
            }
            ObjectLit(lit) => self.bind_object_lit(lit),
            ArrayLit(lit) => self.bind_array_lit(lit),
            Cond(cond) => self.bind_cond_expr(cond),
            Paren(paren) => self.bind_expr(paren.expr),
            ArrowFn(f) => self.bind_arrow_fn_expr(f),
            Fn(f) => self.bind_fn_expr(f),
            Class(class) => self.bind_class_like(None, class, true),
            PrefixUnary(unary) => self.bind_expr(unary.expr),
            PostfixUnary(unary) => self.bind_expr(unary.expr),
            PropAccess(node) => {
                self.bind_expr(node.expr);
            }
            EleAccess(node) => {
                self.bind_expr(node.expr);
                self.bind_expr(node.arg);
            }
            Typeof(node) => {
                self.bind_expr(node.expr);
            }
            Void(node) => {
                self.bind_expr(node.expr);
            }
            _ => (),
        }
    }

    fn bind_fn_expr(&mut self, f: &'cx ast::FnExpr<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();

        self.create_fn_expr_symbol(f, f.id);

        for param in f.params {
            self.bind_param(param);
        }
        self.bind_block_stmt(f.body);
        self.scope_id = old;
    }

    fn bind_arrow_fn_expr(&mut self, f: &'cx ast::ArrowFnExpr<'cx>) {
        self.create_fn_expr_symbol(f, f.id);

        let old = self.scope_id;
        self.scope_id = self.new_scope();
        self.bind_params(f.params);
        use ast::ArrowFnExprBody::*;
        match f.body {
            Block(block) => self.bind_block_stmt(block),
            Expr(expr) => self.bind_expr(expr),
        }
        self.scope_id = old;
    }

    fn bind_cond_expr(&mut self, cond: &'cx ast::CondExpr<'cx>) {
        self.bind_expr(cond.cond);
        self.bind_expr(cond.when_true);
        self.bind_expr(cond.when_false);
    }

    fn bind_array_lit(&mut self, lit: &'cx ast::ArrayLit<'cx>) {
        for expr in lit.elems {
            self.bind_expr(expr);
        }
    }

    fn bind_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();
        let members = lit
            .members
            .iter()
            .map(|member| {
                use ast::ObjectMemberKind::*;
                match member.kind {
                    Shorthand(n) => {
                        self.bind_ident(n.name);
                        let name = SymbolName::Ele(n.name.name);
                        let symbol = self.create_object_member_symbol(name, n.id, false);
                        (name, symbol)
                    }
                    Prop(n) => {
                        let name = prop_name(n.name);
                        let symbol = self.create_object_member_symbol(name, n.id, false);
                        self.bind_expr(n.value);
                        (name, symbol)
                    }
                    Method(n) => {
                        let name = prop_name(n.name);
                        let symbol = self.create_object_member_symbol(name, n.id, false);

                        let old = self.scope_id;
                        self.scope_id = self.new_scope();
                        if let Some(ty_params) = n.ty_params {
                            self.bind_ty_params(ty_params);
                        }
                        self.bind_params(n.params);
                        if let Some(ty) = n.ty {
                            self.bind_ty(ty);
                        }
                        self.bind_block_stmt(n.body);
                        self.scope_id = old;

                        (name, symbol)
                    }
                }
            })
            .collect();
        self.scope_id = old;
        self.create_object_lit_symbol(lit.id, members);
    }

    fn bind_var_stmt(&mut self, container: ast::NodeID, var: &'cx ast::VarStmt) {
        self.connect(var.id);
        let is_export = var
            .modifiers
            .is_some_and(|mods| mods.flags.contains(ModifierKind::Export));
        self.bind_var_decls(Some(container), var.list, var.kind, is_export);
    }

    pub(super) fn bind_entity_name(&mut self, name: &'cx ast::EntityName) {
        match name.kind {
            ast::EntityNameKind::Ident(ident) => self.bind_ident(ident),
            ast::EntityNameKind::Qualified(q) => {
                self.bind_entity_name(q.left);
                self.bind_ident(q.right);
            }
        }
    }

    pub(super) fn bind_refer_ty(&mut self, refer: &'cx ast::ReferTy) {
        self.bind_entity_name(refer.name);
        if let Some(ty_args) = refer.ty_args {
            for ty_arg in ty_args.list {
                self.bind_ty(ty_arg);
            }
        }
    }

    pub(super) fn bind_tys(&mut self, tys: &'cx [&'cx ast::Ty<'cx>]) {
        for ty in tys {
            self.bind_ty(ty);
        }
    }

    pub(super) fn bind_ty(&mut self, ty: &'cx ast::Ty) {
        use ast::TyKind::*;
        match ty.kind {
            Array(array) => self.bind_array_ty(array),
            Tuple(tup) => {
                for ty in tup.tys {
                    self.bind_ty(ty);
                }
            }
            ObjectLit(lit) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                let symbol = self.create_object_lit_ty_symbol(lit.id, Default::default());
                for m in lit.members {
                    self.bind_object_ty_member(lit.id, m);
                }
                self.scope_id = old;
            }
            Refer(refer) => self.bind_refer_ty(refer),
            Cond(cond) => {
                self.bind_ty(cond.check_ty);
                self.bind_ty(cond.extends_ty);
                self.bind_ty(cond.true_ty);
                self.bind_ty(cond.false_ty);
            }
            IndexedAccess(indexed) => {
                self.bind_ty(indexed.ty);
                self.bind_ty(indexed.index_ty);
            }
            Rest(rest) => {
                self.bind_ty(rest.ty);
            }
            Fn(f) => {
                self.create_fn_ty_symbol(f.id, SymbolName::Call);
                if let Some(ty_params) = f.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(f.params);
                self.bind_ty(f.ty);
            }
            Ctor(n) => {
                self.create_fn_ty_symbol(n.id, SymbolName::New);
                self.bind_params(n.params);
                self.bind_ty(n.ty);
            }
            Lit(_) => {}
            Union(u) => {
                for ty in u.tys {
                    self.bind_ty(ty);
                }
            }
            Intersection(i) => {
                for ty in i.tys {
                    self.bind_ty(ty);
                }
            }
            Typeof(n) => {
                self.bind_entity_name(n.name);
            }
            Mapped(n) => {}
            TyOp(n) => {
                self.bind_ty(n.ty);
            }
            Pred(n) => {
                self.bind_ident(n.name);
                self.bind_ty(n.ty);
            }
        }
    }

    fn bind_array_ty(&mut self, array: &'cx ast::ArrayTy) {
        self.bind_ty(array.ele)
    }

    fn bind_var_decls(
        &mut self,
        container: Option<ast::NodeID>,
        decls: ast::VarDecls<'cx>,
        kind: ast::VarKind,
        is_export: bool,
    ) {
        for decl in decls {
            self.bind_var_decl(container, decl, kind, is_export);
        }
    }

    fn bind_var_binding(
        &mut self,
        container: Option<ast::NodeID>,
        is_export: bool,
        binding: &'cx ast::Binding<'cx>,
        kind: ast::VarKind,
        var_decl: ast::NodeID,
        include_flags: SymbolFlags,
        exclude_flags: SymbolFlags,
    ) {
        let symbol_kind = || {
            if kind == ast::VarKind::Let || kind == ast::VarKind::Const {
                SymbolKind::BlockScopedVar { decl: var_decl }
            } else {
                SymbolKind::FunctionScopedVar(FunctionScopedVarSymbol { decl: var_decl })
            }
        };
        use ast::Binding::*;
        match binding {
            Ident(ident) => {
                self.connect(ident.id);
                let symbol =
                    self.create_var_symbol(ident.name, include_flags, symbol_kind(), exclude_flags);
                self.create_final_res(ident.id, symbol);
                if let Some(container) = container {
                    let members = self.members(container, is_export);
                    members.insert(SymbolName::Normal(ident.name), symbol);
                }
            }
            ObjectPat(object) => {
                for elem in object.elems {
                    use ast::ObjectBindingName::*;
                    match elem.name {
                        Shorthand(ident) => {
                            self.connect(ident.id);
                            let symbol = self.create_var_symbol(
                                ident.name,
                                include_flags,
                                symbol_kind(),
                                exclude_flags,
                            );
                            self.create_final_res(ident.id, symbol);
                        }
                        Prop { name, .. } => {
                            self.bind_var_binding(
                                None,
                                false,
                                name,
                                kind,
                                var_decl,
                                include_flags,
                                exclude_flags,
                            );
                        }
                    }
                }
            }
        }
    }

    fn bind_var_decl(
        &mut self,
        container: Option<ast::NodeID>,
        decl: &'cx ast::VarDecl<'cx>,
        kind: ast::VarKind,
        is_export: bool,
    ) {
        let (include_flags, exclude_flags) =
            if kind == ast::VarKind::Let || kind == ast::VarKind::Const {
                (
                    SymbolFlags::BLOCK_SCOPED_VARIABLE,
                    SymbolFlags::BLOCK_SCOPED_VARIABLE_EXCLUDES,
                )
            } else {
                (
                    SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                    SymbolFlags::FUNCTION_SCOPED_VARIABLE_EXCLUDES,
                )
            };
        self.bind_var_binding(
            container,
            is_export,
            decl.binding,
            kind,
            decl.id,
            include_flags,
            exclude_flags,
        );
        if let Some(ty) = decl.ty {
            self.bind_ty(ty);
        }
        if let Some(init) = decl.init {
            self.bind_expr(init);
        }
    }

    pub(super) fn bind_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.bind_param(param);
        }
    }

    pub(super) fn bind_param(&mut self, param: &'cx ast::ParamDecl) {
        let symbol = self.create_var_symbol(
            param.name.name,
            SymbolFlags::FUNCTION_SCOPED_VARIABLE,
            SymbolKind::FunctionScopedVar(FunctionScopedVarSymbol { decl: param.id }),
            SymbolFlags::empty(),
        );
        self.create_final_res(param.id, symbol);
        if let Some(ty) = param.ty {
            self.bind_ty(ty);
        }
        if let Some(init) = param.init {
            self.bind_expr(init);
        }
    }

    fn bind_fn_decl(&mut self, container: ast::NodeID, f: &'cx ast::FnDecl<'cx>) {
        self.connect(f.id);
        self.create_fn_symbol(container, f);

        if let Some(ty_params) = f.ty_params {
            self.bind_ty_params(ty_params);
        }

        let old = self.scope_id;
        self.scope_id = self.new_scope();
        self.bind_params(f.params);
        if let Some(body) = f.body {
            self.bind_block_stmt(body);
        }
        if let Some(ty) = f.ty {
            self.bind_ty(ty);
        }
        self.scope_id = old;
    }
}

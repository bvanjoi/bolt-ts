use super::BinderState;
use super::FlowNodes;
use super::ScopeID;
use super::container_flags::ContainerFlags;
use super::container_flags::GetContainerFlags;
use super::flow::FlowFlags;
use super::flow::FlowID;
use super::flow::FlowNodeKind;
use super::symbol;
use super::symbol::FunctionScopedVarSymbol;
use super::symbol::GetterSetterSymbol;
use super::symbol::IndexSymbol;
use super::symbol::{SymbolFlags, SymbolFnKind, SymbolKind};
use super::symbol::{SymbolID, SymbolName, Symbols};

use bolt_ts_ast::ModifierKind;
use bolt_ts_ast::NodeFlags;
use bolt_ts_atom::AtomMap;
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;
use thin_vec::thin_vec;

use crate::bind::Symbol;
use crate::bind::prop_name;
use crate::bind::symbol::AliasSymbol;
use crate::parser::ParseResult;
use crate::parser::is_left_hand_side_expr_kind;
use bolt_ts_ast as ast;

impl<'cx, 'atoms, 'parser> BinderState<'cx, 'atoms, 'parser> {
    pub fn new(
        atoms: &'atoms AtomMap<'cx>,
        parser: &'parser mut ParseResult<'cx>,
        root: &'cx ast::Program<'cx>,
        module_id: ModuleID,
        options: &NormalizedTsConfig,
    ) -> Self {
        let symbols = Symbols::new(module_id);
        let mut flow_nodes = FlowNodes::new(module_id);
        let unreachable_flow_node = flow_nodes.create_flow_unreachable();
        let report_unreachable_flow_node = flow_nodes.create_flow_unreachable();

        let in_strict_mode = !root.is_declaration || *options.compiler_options().always_strict();

        BinderState {
            atoms,
            p: parser,
            scope_id: ScopeID::root(module_id),
            scope_id_parent_map: Vec::with_capacity(512),
            res: fx_hashmap_with_capacity(512),
            final_res: fx_hashmap_with_capacity(512),
            node_id_to_scope_id: fx_hashmap_with_capacity(512),
            container_chain: fx_hashmap_with_capacity(128),
            locals: fx_hashmap_with_capacity(128),
            symbols,
            diags: Vec::new(),

            flow_nodes,
            in_strict_mode,
            seen_this_keyword: false,
            emit_flags: bolt_ts_ast::NodeFlags::empty(),
            current_flow: None,
            current_break_target: None,
            current_continue_target: None,
            current_return_target: None,
            current_exception_target: None,
            current_true_target: None,
            current_false_target: None,
            unreachable_flow_node,
            report_unreachable_flow_node,
            has_flow_effects: false,
            in_return_position: false,
            has_explicit_return: false,

            container: None,
            this_parent_container: None,
            block_scope_container: None,
            last_container: None,
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
        self.current_flow = Some(self.flow_nodes.create_start(None));
        let id = self.create_block_container_symbol(root.id);
        assert_eq!(id, SymbolID::container(id.module()));
        for stmt in root.stmts {
            self.bind_stmt(root.id, stmt)
        }
    }

    fn bind_stmt(&mut self, container: ast::NodeID, stmt: &'cx ast::Stmt) {
        use bolt_ts_ast::StmtKind::*;

        match stmt.kind {
            Empty(_) => (),
            Var(var) => self.bind_var_stmt(container, var),
            Expr(expr) => self.bind_expr(expr),
            Fn(f) => self.bind_fn_decl(container, f),
            If(n) => self.bind_if_stmt(container, n),
            Block(block) => self.bind_block_stmt(block),
            Return(ret) => self.bind_ret_or_throw(ret.expr, true),
            Throw(t) => self.bind_ret_or_throw(Some(t.expr), false),
            Class(class) => self.bind_class_like(Some(container), class, false),
            Interface(interface) => self.bind_interface_decl(container, interface),
            Type(t) => self.bind_type_decl(t),
            Namespace(ns) => self.bind_ns_decl(container, ns),
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
            While(_) => {}
            Do(_) => {}
            Debugger(_) => {}
        }
    }

    fn bind_if_stmt(&mut self, container: ast::NodeID, n: &'cx ast::IfStmt<'cx>) {
        let then_label = self.flow_nodes.create_branch_label();
        let else_label = self.flow_nodes.create_branch_label();
        let post_if_label = self.flow_nodes.create_branch_label();
        self.bind_cond(Some(n.expr), then_label, else_label);
        self.current_flow = Some(self.finish_flow_label(then_label));
        self.bind_stmt(container, n.then);
        self.flow_nodes
            .add_antecedent(post_if_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(else_label));

        if let Some(alt) = n.else_then {
            self.bind_stmt(container, alt)
        }
        self.flow_nodes
            .add_antecedent(post_if_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(post_if_label));
    }

    fn bind_ret_or_throw(&mut self, expr: Option<&'cx ast::Expr<'cx>>, is_ret: bool) {
        let saved_in_return_position = self.in_return_position;
        self.in_return_position = true;
        if let Some(expr) = expr {
            self.bind_expr(expr);
        }
        self.in_return_position = saved_in_return_position;
        if is_ret {
            // TODO:
        }
        self.current_flow = Some(self.unreachable_flow_node);
        self.has_flow_effects = true;
    }

    fn bind_try_stmt(&mut self, _container: ast::NodeID, stmt: &'cx ast::TryStmt<'cx>) {
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
        use bolt_ts_ast::ForInitKind::*;
        match init {
            Var((kind, var)) => self.bind_var_decls(None, var, *kind, false),
            Expr(expr) => self.bind_expr(expr),
        }
    }

    fn bind_spec_export(&mut self, container: ast::NodeID, spec: &'cx ast::ExportSpec<'cx>) {
        use bolt_ts_ast::ExportSpecKind::*;
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
                use bolt_ts_ast::ModuleExportNameKind::*;
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
        use bolt_ts_ast::ExportClauseKind::*;
        match decl.clause.kind {
            Glob(_) => todo!(),
            Ns(_) => todo!(),
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
                    let container_flags = block.get_container_flags(self.p);
                    self.bind_container(block.id, container_flags, |this| {
                        this.create_block_container_symbol(block.id);
                        for stmt in block.stmts {
                            this.bind_stmt(block.id, stmt)
                        }
                    });
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
            .is_some_and(|mods| mods.flags.contains(ast::ModifierKind::Export));
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

        let parent = self.p.parent(ty_param.id).unwrap();
        let p = self.p.node(parent);
        if let Some(infer_ty) = p.as_infer_ty() {
            assert!(ty_param.default.is_none());
            // get infer ty container
            let extends_ty = self.p.find_ancestor(infer_ty.id, |n| {
                let n_id = n.id();
                let Some(p) = self.p.parent(n_id) else {
                    return None;
                };
                if let Some(cond) = self.p.node(p).as_cond_ty() {
                    if cond.extends_ty.id() == n_id {
                        return Some(true);
                    }
                }
                None
            });
            let container = extends_ty.map(|extends_ty| {
                let p = self.p.parent(extends_ty).unwrap();
                let n = self.p.node(p);
                assert!(n.is_cond_ty());
                p
            });
            if let Some(container) = container {
                let name = SymbolName::Normal(ty_param.name.name);
                self.insert_into_locals(container, name, symbol);
            }
        }
    }

    fn insert_into_locals(&mut self, container: ast::NodeID, name: SymbolName, symbol: SymbolID) {
        assert!(self.p.node(container).has_locals());
        let locals = self
            .locals
            .entry(container)
            .or_insert_with(|| fx_hashmap_with_capacity(64));
        locals.insert(name, symbol);
    }

    pub(super) fn bind_index_sig(
        &mut self,
        container: ast::NodeID,
        index: &'cx ast::IndexSigDecl<'cx>,
        is_export: bool,
    ) {
        let name = SymbolName::Index;
        let insert = |this: &mut Self, id: SymbolID, decl: ast::NodeID| {
            let s = this.symbols.get_mut(id);
            let SymbolKind::Index(index) = &mut s.kind.0 else {
                unreachable!()
            };
            index.decls.push(decl);
        };
        let add = |this: &mut Self, decl: ast::NodeID| -> SymbolID {
            let symbol = this.declare_symbol(
                name,
                SymbolFlags::SIGNATURE,
                SymbolKind::Index(IndexSymbol {
                    decls: thin_vec::thin_vec![decl],
                }),
                SymbolFlags::empty(),
            );
            this.create_final_res(index.id, symbol);
            symbol
        };
        let container = self.final_res[&container];
        let s = self.symbols.get(container);
        if let Some(i) = &s.kind.1 {
            if let Some(id) = i.members.get(&name).copied() {
                insert(self, id, index.id);
            } else {
                let id = add(self, index.id);
                let i = self.symbols.get_mut(container).kind.1.as_mut().unwrap();
                i.members.insert(name, id);
            }
        } else if let SymbolKind::Class(c) = &s.kind.0 {
            if is_export {
                if let Some(id) = c.exports.get(&name).copied() {
                    insert(self, id, index.id);
                } else {
                    let id = add(self, index.id);
                    let SymbolKind::Class(c) = &mut self.symbols.get_mut(container).kind.0 else {
                        unreachable!()
                    };
                    c.exports.insert(name, id);
                }
            } else if let Some(id) = c.members.get(&name).copied() {
                insert(self, id, index.id);
            } else {
                let id = add(self, index.id);
                let SymbolKind::Class(c) = &mut self.symbols.get_mut(container).kind.0 else {
                    unreachable!()
                };
                c.members.insert(name, id);
            }
        } else if let SymbolKind::TyLit(o) = &s.kind.0 {
            if let Some(id) = o.members.get(&name).copied() {
                insert(self, id, index.id);
            } else {
                let id = add(self, index.id);
                let SymbolKind::TyLit(o) = &mut self.symbols.get_mut(container).kind.0 else {
                    unreachable!()
                };
                o.members.insert(name, id);
            }
        } else if let SymbolKind::Object(_) = s.kind.0 {
            unreachable!("object lit should not have index sig");
        } else {
            todo!()
        }

        self.bind_params(index.params);
        self.bind_ty(index.ty);
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
            self.create_final_res(decl, name);
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
        use bolt_ts_ast::ObjectTyMemberKind::*;
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
            Setter(n) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                self.bind_params(n.params);
                self.bind_set_access(container, n, false);
                self.scope_id = old;
            }
            Getter(n) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                self.bind_get_access(container, n, false);
                self.scope_id = old;
            }
        }
    }

    fn bind_interface_decl(&mut self, container: ast::NodeID, i: &'cx ast::InterfaceDecl<'cx>) {
        let id = self.create_interface_symbol(i.id, i.name.name, Default::default());
        let is_export = i
            .modifiers
            .is_some_and(|ms| ms.flags.intersects(ast::ModifierKind::Export));
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

    fn create_locals_for_container(&mut self, container: ast::NodeID) {
        assert!(self.p.node(container).has_locals());
        let prev = self.locals.insert(container, fx_hashmap_with_capacity(64));
        assert!(prev.is_none());
    }

    fn delete_locals_for_container(&mut self, container: ast::NodeID) {
        assert!(self.p.node(container).has_locals());
        self.locals.remove(&container);
    }

    fn add_to_container_chain(&mut self, next: ast::NodeID) {
        assert!(self.p.node(next).has_locals());
        if let Some(last_container) = self.last_container {
            // same as `last_container.next_container = next`
            let prev = self.container_chain.insert(last_container, next);
            assert!(prev.is_none());
        }
        self.last_container = Some(next);
    }

    fn bind_container(
        &mut self,
        node: ast::NodeID,
        container_flags: ContainerFlags,
        bind_children: impl FnOnce(&mut Self),
    ) {
        let old = self.scope_id;
        let save_container = self.container;
        let save_this_parent_container = self.this_parent_container;
        let save_block_scope_container = self.block_scope_container;
        let save_in_return_position = self.in_return_position;

        self.scope_id = self.new_scope();

        let n = self.p.node(node);
        if n.as_arrow_fn_expr()
            .is_some_and(|n| !matches!(n.body, ast::ArrowFnExprBody::Block(_)))
        {
            self.in_return_position = true;
        }

        if container_flags.intersects(ContainerFlags::IS_CONTAINER) {
            if !n.is_arrow_fn_expr() {
                self.this_parent_container = self.container;
            }
            self.block_scope_container = Some(node);
            self.container = self.block_scope_container;
            if container_flags.intersects(ContainerFlags::HAS_LOCALS) {
                self.create_locals_for_container(node);
                self.add_to_container_chain(node);
            }
        } else if container_flags.intersects(ContainerFlags::IS_BLOCK_SCOPED_CONTAINER) {
            self.block_scope_container = Some(node);
            if container_flags.intersects(ContainerFlags::HAS_LOCALS) {
                self.delete_locals_for_container(node);
            }
        }

        if container_flags.intersects(ContainerFlags::IS_CONTROL_FLOW_CONTAINER) {
            let save_current_flow = self.current_flow;
            let save_break_target = self.current_break_target;
            let save_continue_target = self.current_continue_target;
            let save_return_target = self.current_return_target;
            let save_exception_target = self.current_exception_target;
            // TODO: active_label_list
            let save_has_explicit_return = self.has_explicit_return;
            let is_immediately_invoked = (container_flags
                .intersects(ContainerFlags::IS_FUNCTION_EXPRESSION)
                && !n.has_syntactic_modifier(ModifierKind::Async.into())
                && !n.is_fn_like_and_has_asterisk()
                && self.p.get_immediately_invoked_fn_expr(node).is_some())
                || self.p.node(node).is_class_static_block_decl();

            if !is_immediately_invoked {
                let flow_node = container_flags.intersects(ContainerFlags::IS_FUNCTION_EXPRESSION | ContainerFlags::IS_OBJECT_LITERAL_OR_CLASS_EXPRESSION_METHOD_OR_ACCESSOR).then_some(node);
                self.current_flow = Some(self.flow_nodes.create_start(flow_node));
            }
            self.current_return_target = if is_immediately_invoked || n.is_class_ctor() {
                Some(self.flow_nodes.create_branch_label())
            } else {
                None
            };
            self.current_exception_target = None;
            self.current_break_target = None;
            self.current_continue_target = None;
            self.has_explicit_return = false;
            self.p.node_flags_map.update(node, |flags| {
                *flags &= !NodeFlags::REACHABILITY_AND_EMIT_FLAGS
            });
            // TODO: unreachable case

            bind_children(self);

            if let Some(current_return_target) = self.current_return_target {
                self.flow_nodes
                    .add_antecedent(current_return_target, self.current_flow.unwrap());
                self.current_flow = Some(self.finish_flow_label(current_return_target));
            }

            if !is_immediately_invoked {
                self.current_flow = save_current_flow;
            }
            self.current_break_target = save_break_target;
            self.current_continue_target = save_continue_target;
            self.current_return_target = save_return_target;
            self.current_exception_target = save_exception_target;
            // TODO: active_label_list
            self.has_explicit_return = save_has_explicit_return;
        } else if container_flags.intersects(ContainerFlags::IS_INTERFACE) {
            self.seen_this_keyword = false;
            bind_children(self);
            assert!(n.is_ident());
            // TODO: node flags
        } else {
            // TODO: delete `saved_current_flow`
            let saved_current_flow = self.current_flow;
            bind_children(self);
            // TODO: delete `saved_current_flow`
            self.current_flow = saved_current_flow;
        }

        self.in_return_position = save_in_return_position;
        self.container = save_container;
        self.this_parent_container = save_this_parent_container;
        self.block_scope_container = save_block_scope_container;
        self.scope_id = old;
    }

    pub(super) fn bind_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        let container_flags = block.get_container_flags(self.p);
        self.bind_container(block.id, container_flags, |this| {
            this.create_block_container_symbol(block.id);
            for stmt in block.stmts {
                this.bind_stmt(block.id, stmt)
            }
        });
    }

    pub(super) fn bind_block_stmt_with_container(
        &mut self,
        container: ast::NodeID,
        block: &'cx ast::ModuleBlock<'cx>,
    ) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();

        for stmt in block.stmts {
            self.bind_stmt(container, stmt)
        }

        self.scope_id = old;
    }

    fn bind_ident(&mut self, ident: &'cx ast::Ident) {
        if let Some(flow) = self.current_flow {
            self.flow_nodes.insert_container_map(ident.id, flow);
        }
        self.connect(ident.id)
    }

    pub(super) fn bind_expr(&mut self, expr: &ast::Expr<'cx>) {
        use bolt_ts_ast::ExprKind::*;
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
            As(node) => {
                self.bind_expr(node.expr);
                if !node.ty.is_const_ty_refer() {
                    self.bind_ty(node.ty);
                }
            }
            Template(node) => {
                for item in node.spans {
                    self.bind_expr(item.expr);
                }
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
        if let Some(ty_params) = f.ty_params {
            self.bind_ty_params(ty_params);
        }
        self.bind_params(f.params);
        if let Some(ty) = f.ty {
            self.bind_ty(ty);
        }
        use bolt_ts_ast::ArrowFnExprBody::*;
        match f.body {
            Block(block) => self.bind_block_stmt(block),
            Expr(expr) => self.bind_expr(expr),
        }
        self.scope_id = old;
    }

    pub(super) fn finish_flow_label(&mut self, id: FlowID) -> FlowID {
        let node = self.flow_nodes.get_flow_node(id);
        let FlowNodeKind::Label(label) = &node.kind else {
            unreachable!()
        };
        let Some(antecedents) = &label.antecedent else {
            return self.unreachable_flow_node;
        };
        if antecedents.len() == 1 {
            antecedents[0]
        } else {
            id
        }
    }

    fn bind_cond_expr(&mut self, cond: &'cx ast::CondExpr<'cx>) {
        let true_label = self.flow_nodes.create_branch_label();
        let false_label = self.flow_nodes.create_branch_label();
        let post_expression_label = self.flow_nodes.create_branch_label();

        let saved_current_flow = self.current_flow;
        let saved_has_flow_effects = self.has_flow_effects;

        self.has_flow_effects = false;
        self.bind_cond(Some(cond.cond), true_label, false_label);

        self.current_flow = Some(self.finish_flow_label(true_label));
        let when_true_flow = self.current_flow.unwrap();
        self.bind_expr(cond.when_true);
        self.flow_nodes
            .add_antecedent(post_expression_label, when_true_flow);

        self.current_flow = Some(self.finish_flow_label(false_label));
        let when_false_flow = self.current_flow.unwrap();
        if self.in_return_position {
            self.flow_nodes
                .insert_cond_expr_flow(cond, when_true_flow, when_false_flow);
        }
        self.bind_expr(cond.when_false);
        self.flow_nodes
            .add_antecedent(post_expression_label, when_false_flow);

        self.current_flow = if self.has_flow_effects {
            Some(self.finish_flow_label(post_expression_label))
        } else {
            saved_current_flow
        };
        if !self.has_flow_effects {
            self.has_flow_effects = saved_has_flow_effects;
        }
    }

    fn do_with_cond_branch<T>(
        &mut self,
        f: impl FnOnce(&mut Self, T),
        value: T,
        true_target: FlowID,
        false_target: FlowID,
    ) {
        let saved_true_target = self.current_true_target;
        let saved_false_target = self.current_false_target;

        self.current_true_target = Some(true_target);
        self.current_false_target = Some(false_target);
        f(self, value);
        self.current_true_target = saved_true_target;
        self.current_false_target = saved_false_target;
    }

    fn bind_cond(
        &mut self,
        node: Option<&'cx ast::Expr<'cx>>,
        true_target: FlowID,
        false_target: FlowID,
    ) {
        self.do_with_cond_branch(
            |this, value| {
                if let Some(node) = value {
                    this.bind_expr(node);
                }
            },
            node,
            true_target,
            false_target,
        );
        let should_add_antecedent = node
            .is_none_or(|node| !node.kind.is_logical_assignment() && !node.kind.is_logical_expr());
        if should_add_antecedent {
            let t = self.create_flow_condition(
                FlowFlags::TRUE_CONDITION,
                self.current_flow.unwrap(),
                node,
            );
            self.flow_nodes.add_antecedent(true_target, t);
            let f = self.create_flow_condition(
                FlowFlags::FALSE_CONDITION,
                self.current_flow.unwrap(),
                node,
            );
            self.flow_nodes.add_antecedent(false_target, f);
        }
    }

    fn bind_array_lit(&mut self, lit: &'cx ast::ArrayLit<'cx>) {
        for expr in lit.elems {
            self.bind_expr(expr);
        }
    }

    fn bind_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) {
        self.create_object_lit_symbol(lit.id, fx_hashmap_with_capacity(lit.members.len()));
        let old = self.scope_id;
        self.scope_id = self.new_scope();

        for member in lit.members {
            use bolt_ts_ast::ObjectMemberKind::*;
            match member.kind {
                Shorthand(n) => {
                    self.bind_ident(n.name);
                    let name = SymbolName::Ele(n.name.name);
                    let symbol = self.create_object_member_symbol(name, n.id, false);
                    self.members(lit.id, false).insert(name, symbol);
                }
                Prop(n) => {
                    let name = prop_name(n.name);
                    let symbol = self.create_object_member_symbol(name, n.id, false);
                    self.members(lit.id, false).insert(name, symbol);
                    self.bind_expr(n.value);
                }
                Method(n) => {
                    let name = prop_name(n.name);
                    self.create_fn_decl_like_symbol(lit.id, n, name, SymbolFnKind::Method, false);
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
                }
                SpreadAssignment(n) => {
                    self.bind_expr(n.expr);
                }
            }
        }
        self.scope_id = old;
    }

    fn bind_var_stmt(&mut self, container: ast::NodeID, var: &'cx ast::VarStmt) {
        self.connect(var.id);
        let is_export = var
            .modifiers
            .is_some_and(|mods| mods.flags.contains(ast::ModifierKind::Export));
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
        use bolt_ts_ast::TyKind::*;
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
                self.create_object_lit_ty_symbol(lit.id, Default::default());
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
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                self.bind_ty(n.ty);
                self.scope_id = old;
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
            Mapped(n) => {
                let symbol = self.symbols.insert(Symbol::new(
                    SymbolName::Type,
                    SymbolFlags::TYPE_LITERAL,
                    SymbolKind::TyMapped { decl: n.id },
                ));
                self.final_res.insert(n.id, symbol);
                assert!(n.ty_param.default.is_none());
                assert!(n.ty_param.constraint.is_some());
                self.bind_ty_param(n.ty_param);
                if let Some(named_ty) = n.name_ty {
                    self.bind_ty(named_ty);
                }
                if let Some(ty) = n.ty {
                    self.bind_ty(ty);
                }
            }
            TyOp(n) => {
                self.bind_ty(n.ty);
            }
            Pred(n) => {
                self.bind_ident(n.name);
                self.bind_ty(n.ty);
            }
            Paren(n) => {
                self.bind_ty(n.ty);
            }
            Infer(n) => {
                self.bind_ty_param(n.ty_param);
            }
            Nullable(n) => {
                self.bind_ty(n.ty);
            }
            Intrinsic(_) => {}
            NamedTuple(n) => {
                self.bind_ident(n.name);
                self.bind_ty(n.ty);
            }
            TemplateLit(n) => {
                for item in n.spans {
                    self.bind_ty(item.ty);
                }
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
        use bolt_ts_ast::BindingKind::*;
        match binding.kind {
            Ident(ident) => {
                self.connect(ident.id);
                let symbol =
                    self.create_var_symbol(ident.name, include_flags, symbol_kind(), exclude_flags);
                self.create_final_res(ident.id, symbol);
                if let Some(container) = container {
                    let name = SymbolName::Normal(ident.name);
                    self.declare_symbol_and_add_to_symbol_table(
                        container, name, symbol, binding.id, is_export,
                    );
                }
            }
            ObjectPat(object) => {
                for elem in object.elems {
                    use bolt_ts_ast::ObjectBindingName::*;
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
            ArrayPat(_) => todo!(),
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
            self.bind_init_var_flow(decl.id);
        }
    }

    fn bind_init_var_flow(&mut self, id: ast::NodeID) {
        self.current_flow = Some(self.create_flow_assign(self.current_flow.unwrap(), id))
    }

    pub(super) fn bind_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.bind_param(param);
        }
    }

    pub(super) fn bind_param(&mut self, param: &'cx ast::ParamDecl) {
        self.bind_var_binding(
            None,
            false,
            param.name,
            ast::VarKind::Var,
            param.id,
            SymbolFlags::FUNCTION_SCOPED_VARIABLE,
            SymbolFlags::empty(),
        );
        if let Some(ty) = param.ty {
            self.bind_ty(ty);
        }
        if let Some(init) = param.init {
            self.bind_expr(init);
        }
    }

    fn bind_fn_decl(&mut self, container: ast::NodeID, f: &'cx ast::FnDecl<'cx>) {
        self.connect(f.id);
        let symbol = self.create_fn_symbol(container, f);
        let name = SymbolName::Normal(f.name.name);
        let is_export = f
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ModifierKind::Export));
        self.declare_symbol_and_add_to_symbol_table(container, name, symbol, f.id, is_export);

        let old_old = self.scope_id;
        self.scope_id = self.new_scope();

        if let Some(ty_params) = f.ty_params {
            self.bind_ty_params(ty_params);
        }

        self.scope_id = self.new_scope();

        self.bind_params(f.params);
        if let Some(ty) = f.ty {
            self.bind_ty(ty);
        }
        if let Some(body) = f.body {
            self.bind_block_stmt(body);
        }
        self.scope_id = old_old;
    }

    pub(super) fn declare_symbol_and_add_to_symbol_table(
        &mut self,
        container: ast::NodeID,
        name: SymbolName,
        symbol: SymbolID,
        current: ast::NodeID,
        // TODO: delete `is_export`
        is_export: bool,
    ) {
        let c = self.p.node(container);
        use ast::Node::*;
        match c {
            NamespaceDecl(_) => self.declare_module_member(container, name, symbol, current),
            _ => {
                // TODO: handle more case:
                if is_export {
                    let members = self.members(container, true);
                    members.insert(name, symbol);
                } else {
                    let members = self.members(container, false);
                    members.insert(name, symbol);
                }
            }
        }
    }

    fn declare_module_member(
        &mut self,
        container: ast::NodeID,
        name: SymbolName,
        symbol: SymbolID,
        current: ast::NodeID,
    ) {
        assert!(self.p.node(container).is_namespace_decl());
        let has_export_modifier = self
            .p
            .get_combined_modifier_flags(current)
            .intersects(ModifierKind::Export);
        if has_export_modifier
            || self
                .p
                .node_flags_map
                .get(container)
                .intersects(bolt_ts_ast::NodeFlags::EXPORT_CONTEXT)
        {
            let members = self.members(container, true);
            members.insert(name, symbol);
        } else {
            let members = self.members(container, false);
            members.insert(name, symbol);
        }
    }

    fn check_contextual_ident(&mut self, ident: ast::NodeID) {
        // TODO:
    }

    fn bind(&mut self, node: ast::NodeID) {
        let save_in_strict_mode = self.in_strict_mode;
        self._bind(node);
        self.in_strict_mode = save_in_strict_mode;
    }

    fn _bind(&mut self, node: ast::NodeID) {
        let n = self.p.node(node);
        use ast::Node::*;
        match n {
            Ident(_) => {
                // TODO: identifier with NodeFlags.IdentifierIsInJSDocNamespace
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_container_map(node, flow);
                }
                self.check_contextual_ident(node);
            }
            ThisExpr(_) => {
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_container_map(node, flow);
                }
                self.check_contextual_ident(node);
            }
            QualifiedName(_) => {
                if let Some(flow) = self.current_flow {
                    if self.p.is_part_of_ty_query(node) {
                        self.flow_nodes.insert_container_map(node, flow);
                    }
                }
            }
            // TODO: meta
            SuperExpr(_) => {
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_container_map(node, flow);
                } else {
                    self.flow_nodes.reset_container_map(node);
                }
            }
            // TODO: private
            PropAccessExpr(p) => {
                if let Some(flow) = self.current_flow {
                    if self.is_narrowable_reference(p.expr) {
                        self.flow_nodes.insert_container_map(node, flow);
                    }
                }
                // TODO: is_special_prop_decl
                // TODO: js
            }
            EleAccessExpr(e) => {
                if let Some(flow) = self.current_flow {
                    if self.ele_access_is_narrowable_reference(e) {
                        self.flow_nodes.insert_container_map(node, flow);
                    }
                }
                // TODO: is_special_prop_decl
                // TODO: js
            }
            BinExpr(_) => {
                // TODO: special_kind
            }
            CatchClause(_) => {
                // TODO: self.check_strict_mode_catch_clause()
            }
            // TODO: delete expr
            PostfixUnaryExpr(_) => {
                // TODO: check
            }
            PrefixUnaryExpr(_) => {
                // TODO: check
            }
            // TODO: with stmt
            // TODO: label
            // TODO: this type
            _ => {}
        }
    }

    fn ele_access_is_narrowable_reference(&self, n: &ast::EleAccessExpr) -> bool {
        (n.arg.is_string_or_number_lit_like() || n.arg.is_prop_access_entity_name_expr())
            && self.is_narrowable_reference(n.expr)
    }

    fn is_narrowable_reference(&self, expr: &ast::Expr<'_>) -> bool {
        use ast::ExprKind::*;
        match expr.kind {
            // TODO: metaProperty
            Ident(_) | This(_) | Super(_) => true,
            PropAccess(n) => self.is_narrowable_reference(n.expr),
            Paren(n) => self.is_narrowable_reference(n.expr),
            NonNull(n) => self.is_narrowable_reference(n.expr),
            EleAccess(n) => self.ele_access_is_narrowable_reference(n),
            Bin(_) => {
                // TODO: n.op.kind == Comma
                false
            }
            Assign(n) => is_left_hand_side_expr_kind(n.left),
            _ => false,
        }
    }
}

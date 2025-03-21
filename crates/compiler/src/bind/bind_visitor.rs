use super::BinderState;
use super::FlowNodes;
use super::container_flags::ContainerFlags;
use super::container_flags::container_flags_for_node;
use super::flow::FlowFlags;
use super::flow::FlowID;
use super::flow::FlowNodeKind;
use super::symbol::SymbolFlags;
use super::symbol::SymbolTableLocation;
use super::symbol::{SymbolID, SymbolName, Symbols};

use bolt_ts_ast as ast;
use bolt_ts_ast::NodeFlags;
use bolt_ts_atom::AtomMap;
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;

use crate::bind::SymbolTable;
use crate::bind::prop_name;
use crate::ir;
use crate::parser::ParseResult;
use crate::parser::is_left_hand_side_expr_kind;

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
            final_res: fx_hashmap_with_capacity(512),
            container_chain: fx_hashmap_with_capacity(128),
            locals: fx_hashmap_with_capacity(128),
            symbols,
            diags: Vec::new(),

            flow_nodes,
            in_strict_mode,
            in_assignment_pattern: false,
            seen_this_keyword: false,
            emit_flags: bolt_ts_ast::NodeFlags::empty(),
            parent: None,
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
            has_explicit_ret: false,
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

    fn bind_if_stmt(&mut self, n: &'cx ast::IfStmt<'cx>) {
        let then_label = self.flow_nodes.create_branch_label();
        let else_label = self.flow_nodes.create_branch_label();
        let post_if_label = self.flow_nodes.create_branch_label();
        self.bind_cond(Some(n.expr), then_label, else_label);
        self.current_flow = Some(self.finish_flow_label(then_label));
        self.bind(n.then.id());
        self.flow_nodes
            .add_antecedent(post_if_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(else_label));

        if let Some(alt) = n.else_then {
            self.bind(alt.id());
        }
        self.flow_nodes
            .add_antecedent(post_if_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(post_if_label));
    }

    fn bind_try_stmt(&mut self, stmt: &'cx ast::TryStmt<'cx>) {
        self.bind(stmt.try_block.id);
        if let Some(catch) = stmt.catch_clause {
            self.bind(catch.id);
        }
        if let Some(finally) = stmt.finally_block {
            self.bind(finally.id);
        }
    }

    fn bind_export_clause(&mut self, clause: &'cx ast::ExportClause<'cx>) {
        use bolt_ts_ast::ExportClauseKind::*;
        match clause.kind {
            Glob(_) => todo!(),
            Ns(_) => todo!(),
            Specs(n) => {
                for spec in n.list {
                    self.bind_export_spec(spec);
                }
            }
        }
    }

    fn bind_ns_decl(&mut self, ns: &'cx ast::NsDecl<'cx>) {
        let name = match ns.name {
            ast::ModuleName::Ident(ident) => SymbolName::Normal(ident.name),
            ast::ModuleName::StringLit(lit) => SymbolName::Normal(lit.val),
        };

        let s = if ns.is_ambient() {
            self.declare_symbol_and_add_to_symbol_table(
                name,
                ns.id,
                None,
                SymbolFlags::VALUE_MODULE,
                SymbolFlags::VALUE_MODULE_EXCLUDES,
            )
        } else {
            self.declare_symbol_with_ns(name, ns)
        };
        self.create_final_res(ns.id, s);
    }

    pub(super) fn bind_block_scoped_decl(
        &mut self,
        node: ast::NodeID,
        name: SymbolName,
        includes: SymbolFlags,
        exclude_flags: SymbolFlags,
    ) -> SymbolID {
        let block_container = self.block_scope_container.unwrap();
        let c = self.p.node(block_container);
        match c {
            ast::Node::NamespaceDecl(_) => {
                self.declare_module_member(block_container, name, node, includes, exclude_flags)
            }
            _ => {
                let loc = SymbolTableLocation::locals(block_container);
                self.declare_symbol(
                    Some(name),
                    loc,
                    None,
                    node,
                    includes,
                    exclude_flags,
                    false,
                    false,
                )
            }
        }
    }

    pub(super) fn temp_local(
        &self,
        container: ast::NodeID,
        is_export: bool,
    ) -> SymbolTableLocation {
        if is_export {
            SymbolTableLocation::exports(container)
        } else if self.p.node(container).has_locals() {
            SymbolTableLocation::locals(container)
        } else {
            SymbolTableLocation::members(container)
        }
    }

    fn bind_type_decl(&mut self, t: &'cx ast::TypeDecl<'cx>) {
        let name = SymbolName::Normal(t.name.name);
        let symbol = self.bind_block_scoped_decl(
            t.id,
            name,
            SymbolFlags::TYPE_ALIAS,
            SymbolFlags::TYPE_ALIAS_EXCLUDES,
        );
        self.create_final_res(t.id, symbol);
    }

    pub(super) fn bind_ty_params(&mut self, ty_params: ast::TyParams<'cx>) {
        for ty_param in ty_params {
            self.bind(ty_param.id);
        }
    }

    fn bind_ty_param(&mut self, ty_param: &'cx ast::TyParam<'cx>) {
        let name = SymbolName::Normal(ty_param.name.name);
        let parent = self.p.parent(ty_param.id).unwrap();
        // TODO: is_js_doc_template_tag
        let s = if let Some(infer_ty) = self.p.node(parent).as_infer_ty() {
            assert!(ty_param.default.is_none());
            let extends_ty = self.p.find_ancestor(infer_ty.id, |n| {
                let n_id = n.id();
                let p = self.p.parent(n_id)?;
                if let Some(cond) = self.p.node(p).as_cond_ty() {
                    if cond.extends_ty.id() == n_id {
                        return Some(true);
                    }
                }
                None
            });
            let cond_container = extends_ty.map(|extends_ty| {
                let p = self.p.parent(extends_ty).unwrap();
                let n = self.p.node(p);
                assert!(n.is_cond_ty());
                p
            });
            if let Some(cond_container) = cond_container {
                let loc = SymbolTableLocation::locals(cond_container);
                self.declare_symbol(
                    Some(name),
                    loc,
                    None,
                    ty_param.id,
                    SymbolFlags::TYPE_PARAMETER,
                    SymbolFlags::TYPE_ALIAS_EXCLUDES,
                    false,
                    false,
                )
            } else {
                self.bind_anonymous_decl(ty_param.id, SymbolFlags::TYPE_PARAMETER, name)
            }
        } else {
            assert!(!self.p.node(self.container.unwrap()).is_infer_ty());
            self.declare_symbol_and_add_to_symbol_table(
                name,
                ty_param.id,
                None,
                SymbolFlags::TYPE_PARAMETER,
                SymbolFlags::TYPE_PARAMETER_EXCLUDES,
            )
        };
        self.create_final_res(ty_param.id, s);
    }

    fn bind_object_ty_member(&mut self, m: &'cx ast::ObjectTyMember<'cx>) {
        use bolt_ts_ast::ObjectTyMemberKind::*;
        match m.kind {
            Prop(n) => self.bind(n.id),
            Method(n) => self.bind(n.id),
            IndexSig(n) => self.bind(n.id),
            Getter(n) => self.bind(n.id),
            Setter(n) => self.bind(n.id),
            CallSig(n) => self.bind(n.id),
            CtorSig(n) => self.bind(n.id),
        }
    }

    fn bind_interface_decl(&mut self, i: &'cx ast::InterfaceDecl<'cx>) {
        let name = SymbolName::Normal(i.name.name);
        let symbol = self.bind_block_scoped_decl(
            i.id,
            name,
            SymbolFlags::INTERFACE,
            SymbolFlags::INTERFACE_EXCLUDES,
        );
        self.create_final_res(i.id, symbol);
    }

    fn create_locals_for_container(&mut self, container: ast::NodeID) {
        assert!(self.p.node(container).has_locals());
        let prev = self.locals.insert(container, SymbolTable::default());
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

    fn bind_container(&mut self, node: ast::NodeID, container_flags: ContainerFlags) {
        let save_container = self.container;
        let save_this_parent_container = self.this_parent_container;
        let save_block_scope_container = self.block_scope_container;
        let save_in_return_position = self.in_return_position;

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
                && !n.has_syntactic_modifier(ast::ModifierKind::Async.into())
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
                *flags &= !ast::NodeFlags::REACHABILITY_AND_EMIT_FLAGS
            });
            // TODO: unreachable case

            self.bind_children(node);

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
            self.bind_children(node);
            assert!(!n.is_ident());
            // TODO: node flags
        } else {
            // TODO: delete `saved_current_flow`
            // let saved_current_flow = self.current_flow;
            self.bind_children(node);
            // TODO: delete `saved_current_flow`
            // self.current_flow = saved_current_flow;
        }

        self.in_return_position = save_in_return_position;
        self.container = save_container;
        self.this_parent_container = save_this_parent_container;
        self.block_scope_container = save_block_scope_container;
    }

    pub(super) fn bind_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        for stmt in block.stmts {
            self.bind(stmt.id())
        }
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

    fn bind_cond_expr_flow(&mut self, cond: &'cx ast::CondExpr<'cx>) {
        let true_label = self.flow_nodes.create_branch_label();
        let false_label = self.flow_nodes.create_branch_label();
        let post_expression_label = self.flow_nodes.create_branch_label();

        let saved_current_flow = self.current_flow;
        let saved_has_flow_effects = self.has_flow_effects;

        self.has_flow_effects = false;
        self.bind_cond(Some(cond.cond), true_label, false_label);

        self.current_flow = Some(self.finish_flow_label(true_label));
        let when_true_flow = self.current_flow.unwrap();
        self.bind(cond.when_true.id());
        self.flow_nodes
            .add_antecedent(post_expression_label, when_true_flow);

        self.current_flow = Some(self.finish_flow_label(false_label));
        let when_false_flow = self.current_flow.unwrap();
        if self.in_return_position {
            self.flow_nodes
                .insert_cond_expr_flow(cond, when_true_flow, when_false_flow);
        }
        self.bind(cond.when_false.id());
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
                    this.bind(node.id());
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

    pub(super) fn bind_entity_name(&mut self, name: &'cx ast::EntityName<'cx>) {
        use bolt_ts_ast::EntityNameKind::*;
        match name.kind {
            Ident(n) => self.bind(n.id),
            Qualified(q) => {
                self.bind(q.left.id());
                self.bind(q.right.id);
            }
        }
    }

    pub(super) fn bind_tys(&mut self, tys: &'cx [&'cx ast::Ty<'cx>]) {
        for ty in tys {
            self.bind(ty.id());
        }
    }

    fn bind_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.bind(param.id);
        }
    }

    fn bind_fn_decl(&mut self, f: &'cx ast::FnDecl<'cx>) {
        // if self.in_strict_mode {
        // } else {
        let ele_name = SymbolName::Normal(f.name.name);
        let symbol = self.declare_symbol_and_add_to_symbol_table(
            ele_name,
            f.id,
            None,
            SymbolFlags::FUNCTION,
            SymbolFlags::FUNCTION_EXCLUDES,
        );
        self.create_final_res(f.id, symbol);
        // }
    }

    pub(super) fn declare_symbol_and_add_to_symbol_table(
        &mut self,
        name: SymbolName,
        current: ast::NodeID,
        loc: Option<SymbolTableLocation>,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> SymbolID {
        let container = self.container.unwrap();
        let c = self.p.node(container);
        use ast::Node::*;
        match c {
            NamespaceDecl(_) => {
                self.declare_module_member(container, name, current, symbol_flags, symbol_excludes)
            }
            Program(_) => self.declare_source_file_member(
                container,
                name,
                current,
                symbol_flags,
                symbol_excludes,
            ),
            ClassExpr(_) | ClassDecl(_) => {
                self.declare_class_member(name, current, symbol_flags, symbol_excludes)
            }
            EnumDecl(_) => {
                let loc = SymbolTableLocation::exports(container);
                self.declare_symbol(
                    Some(name),
                    loc,
                    None,
                    current,
                    symbol_flags,
                    symbol_excludes,
                    false,
                    false,
                )
            }
            ObjectLitTy(_) | ObjectLit(_) | InterfaceDecl(_) => self.declare_symbol(
                Some(name),
                SymbolTableLocation::members(container),
                None,
                current,
                symbol_flags,
                symbol_excludes,
                false,
                false,
            ),
            FnTy(_)
            | ClassCtor(_)
            | CallSigDecl(_)
            | CtorSigDecl(_)
            | IndexSigDecl(_)
            | ClassMethodElem(_)
            | ObjectMethodMember(_)
            | MethodSignature(_)
            | CtorTy(_)
            | GetterDecl(_)
            | SetterDecl(_)
            | FnDecl(_)
            | FnExpr(_)
            | ArrowFnExpr(_)
            | TypeDecl(_)
            | MappedTy(_) => {
                assert!(
                    c.has_locals(),
                    "container({:?}) should have locals, but it doesn't",
                    c.span()
                );
                let table = SymbolTableLocation::locals(container);
                self.declare_symbol(
                    Some(name),
                    table,
                    None,
                    current,
                    symbol_flags,
                    symbol_excludes,
                    false,
                    false,
                )
            }
            _ => {
                // TODO: remove
                if let Some(loc) = loc {
                    self.declare_symbol(
                        Some(name),
                        loc,
                        None,
                        current,
                        symbol_flags,
                        symbol_excludes,
                        false,
                        false,
                    )
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn declare_class_member(
        &mut self,
        name: SymbolName,
        node: ast::NodeID,
        include: SymbolFlags,
        excludes: SymbolFlags,
    ) -> SymbolID {
        let container = self.container.unwrap();
        let loc = if self.p.node(node).is_static() {
            SymbolTableLocation::exports(container)
        } else {
            SymbolTableLocation::members(container)
        };
        self.declare_symbol(Some(name), loc, None, node, include, excludes, false, false)
    }

    fn declare_source_file_member(
        &mut self,
        container: ast::NodeID,
        name: SymbolName,
        current: ast::NodeID,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> SymbolID {
        assert!(self.p.node(container).is_program());
        // TODO: if is_external_module
        self.declare_module_member(container, name, current, symbol_flags, symbol_excludes)
    }

    fn declare_module_member(
        &mut self,
        container: ast::NodeID,
        name: SymbolName,
        current: ast::NodeID,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> SymbolID {
        let has_export_modifier = self
            .p
            .get_combined_modifier_flags(current)
            .intersects(ast::ModifierKind::Export);
        if symbol_flags.intersects(SymbolFlags::ALIAS) {
            let n = self.p.node(current);
            if n.is_export_named_spec() || n.is_shorthand_spec() {
                let table = SymbolTableLocation::exports(container);
                return self.declare_symbol(
                    Some(name),
                    table,
                    None,
                    current,
                    symbol_flags,
                    symbol_excludes,
                    false,
                    false,
                );
            }
        }

        if !self.p.node(current).is_ambient_module()
            && (has_export_modifier
                || self
                    .p
                    .node_flags(container)
                    .intersects(NodeFlags::EXPORT_CONTEXT))
        {
            let export_kind = if symbol_flags.intersects(SymbolFlags::VALUE) {
                SymbolFlags::EXPORT_VALUE
            } else {
                SymbolFlags::empty()
            };
            let table = SymbolTableLocation::locals(container);
            let local = self.declare_symbol(
                Some(name),
                table,
                None,
                current,
                export_kind,
                symbol_excludes,
                false,
                false,
            );
            let table = SymbolTableLocation::exports(container);
            let export_symbol = self.declare_symbol(
                Some(name),
                table,
                None,
                current,
                symbol_flags,
                symbol_excludes,
                false,
                false,
            );
            self.symbols.get_mut(local).export_symbol = Some(export_symbol);
            // TODO: node.local_symbol = local;
            // TODO: return local
            export_symbol
        } else {
            let loc = SymbolTableLocation::locals(container);
            self.declare_symbol(
                Some(name),
                loc,
                None,
                current,
                symbol_flags,
                symbol_excludes,
                false,
                false,
            )
        }
    }

    fn check_contextual_ident(&mut self, ident: ast::NodeID) {
        // TODO:
    }

    fn bind_modifiers(&mut self, mods: &'cx ast::Modifiers<'cx>) {
        for m in mods.list {
            self.bind(m.id);
        }
    }

    fn bind_prop_name(&mut self, name: &'cx ast::PropName<'cx>) {
        use bolt_ts_ast::PropNameKind::*;
        match name.kind {
            Ident(n) => self.bind(n.id),
            StringLit { raw: n, .. } => self.bind(n.id),
            NumLit(n) => self.bind(n.id),
            Computed(n) => self.bind(n.id),
        }
    }

    fn bind_module_export_name(&mut self, name: &'cx ast::ModuleExportName<'cx>) {
        use bolt_ts_ast::ModuleExportNameKind::*;
        match name.kind {
            Ident(n) => self.bind(n.id),
            StringLit(n) => self.bind(n.id),
        }
    }

    fn bind_export_spec(&mut self, n: &'cx ast::ExportSpec<'cx>) {
        use bolt_ts_ast::ExportSpecKind::*;
        match n.kind {
            Shorthand(n) => self.bind(n.id),
            Named(n) => self.bind(n.id),
        }
    }

    fn bind_binding(&mut self, n: &'cx ast::Binding<'cx>) {
        use bolt_ts_ast::BindingKind::*;
        match n.kind {
            Ident(n) => self.bind(n.id),
            ObjectPat(n) => {
                for elem in n.elems {
                    self.bind(elem.id);
                }
            }
            ArrayPat(_) => {
                // TODO:
            }
        }
    }

    fn bind_class_elem(&mut self, n: &'cx ast::ClassElem<'cx>) {
        use ast::ClassEleKind::*;
        match n.kind {
            Ctor(n) => self.bind(n.id),
            Prop(n) => self.bind(n.id),
            Method(n) => self.bind(n.id),
            IndexSig(n) => self.bind(n.id),
            Getter(n) => self.bind(n.id),
            Setter(n) => self.bind(n.id),
        }
    }

    fn bind_children(&mut self, node: ast::NodeID) {
        use ast::Node::*;
        let save_in_assignment_pattern = self.in_assignment_pattern;
        self.in_assignment_pattern = true;

        let n = self.p.node(node);
        // if n.ge_first_stmt_and_le_last_stmt() && (n.is_ret_stmt() || )

        match n {
            WhileStmt(n) => self.bind_while_stmt(n),
            DoStmt(n) => self.bind_do_stmt(n),
            ForStmt(n) => self.bind_for_stmt(n),
            ForInStmt(n) => self.bind_for_in_or_for_of_stmt(n),
            ForOfStmt(n) => self.bind_for_in_or_for_of_stmt(n),
            IfStmt(n) => self.bind_if_stmt(n),
            RetStmt(n) => self.bind_ret_or_throw(n),
            ThrowStmt(n) => self.bind_ret_or_throw(n),
            BreakStmt(n) => self.bind_break_or_continue_stmt(n),
            ContinueStmt(n) => self.bind_break_or_continue_stmt(n),
            TryStmt(n) => self.bind_try_stmt(n),
            ExprStmt(n) => self.bind_expr_stmt(n),
            PrefixUnaryExpr(n) => self.bind_prefix_unary_expr_flow(n),
            PostfixUnaryExpr(n) => self.bind_postfix_unary_expr_flow(n),
            BinExpr(n) => {
                // TODO; is_destructing_assignment
                self.bind_bin_expr_flow(n)
            }
            CondExpr(n) => self.bind_cond_expr_flow(n),
            VarDecl(n) => self.bind_var_decl_flow(n),
            PropAccessExpr(n) => self.bind_access_expr_flow(n),
            EleAccessExpr(n) => self.bind_access_expr_flow(n),
            CallExpr(n) => self.bind_call_expr_flow(n),
            NonNullExpr(n) => self.bind_non_null_expr_flow(n),
            Program(n) => {
                for stmt in n.stmts {
                    self.bind(stmt.id());
                }
            }
            BlockStmt(ast::BlockStmt { stmts, .. })
            | ModuleBlock(ast::ModuleBlock { stmts, .. }) => {
                for stmt in *stmts {
                    self.bind(stmt.id());
                }
            }
            ObjectBindingElem(n) => {
                self.bind_object_binding_elem_flow(n);
            }
            ParamDecl(n) => {
                self.bind_param_flow(n);
            }
            ObjectLit(n) => {
                self.in_assignment_pattern = save_in_assignment_pattern;
                for member in n.members {
                    self.bind(member.id());
                }
            }
            ArrayLit(n) => {
                self.in_assignment_pattern = save_in_assignment_pattern;
                for elem in n.elems {
                    self.bind(elem.id());
                }
            }
            VarStmt(n) => {
                for item in n.list {
                    self.bind(item.id);
                }
            }
            FnDecl(n) => {
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                if let Some(body) = n.body {
                    self.bind_block_stmt(body);
                }
            }
            EmptyStmt(_) => {}
            ClassDecl(n) => {
                self.bind(n.name.id);
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                if let Some(extends) = n.extends {
                    self.bind(extends.id);
                }
                if let Some(implements) = n.implements {
                    for elem in implements.list {
                        self.bind(elem.id);
                    }
                }
                for elem in n.elems.elems {
                    self.bind_class_elem(elem);
                }
            }
            NamespaceDecl(n) => {
                use ast::ModuleName::*;
                match n.name {
                    Ident(n) => self.bind(n.id),
                    StringLit(n) => self.bind(n.id),
                }
                if let Some(block) = n.block {
                    self.bind(block.id);
                }
            }
            ClassCtor(n) => {
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ret) = n.ret {
                    self.bind(ret.id());
                }
                if let Some(body) = n.body {
                    self.bind_block_stmt(body);
                }
            }
            ClassPropElem(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
                self.bind_prop_name(n.name);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                if let Some(init) = n.init {
                    self.bind(init.id());
                }
            }
            ClassMethodElem(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
                self.bind_prop_name(n.name);
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                if let Some(body) = n.body {
                    self.bind_block_stmt(body);
                }
            }
            GetterDecl(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
                self.bind_prop_name(n.name);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                if let Some(body) = n.body {
                    self.bind_block_stmt(body);
                }
            }
            SetterDecl(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
                self.bind_prop_name(n.name);
                self.bind_params(n.params);
                if let Some(body) = n.body {
                    self.bind(body.id);
                }
            }
            InterfaceDecl(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
                self.bind(n.name.id);
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                if let Some(extends) = n.extends {
                    self.bind(extends.id);
                }
                for m in n.members {
                    self.bind_object_ty_member(m);
                }
            }
            TypeDecl(n) => {
                self.bind(n.name.id);
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind(n.ty.id());
            }
            InterfaceExtendsClause(n) => {
                for ty in n.list {
                    self.bind(ty.id);
                }
            }
            ClassExtendsClause(n) => {
                self.bind_entity_name(n.name);
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
            }
            ClassImplementsClause(n) => {
                for ty in n.list {
                    self.bind(ty.id);
                }
            }
            NsImport(n) => {
                self.bind(n.name.id);
            }
            NsExport(n) => {
                self.bind_module_export_name(n.name);
                self.bind(n.module.id);
            }
            GlobExport(n) => {
                self.bind(n.module.id);
            }
            SpecsExport(n) => {
                for spec in n.list {
                    self.bind_export_spec(spec);
                }
                if let Some(module) = n.module {
                    self.bind(module.id);
                }
            }
            ImportNamedSpec(n) => {
                self.bind_module_export_name(n.prop_name);
                self.bind(n.name.id);
            }
            ImportClause(n) => {
                if let Some(ident) = n.ident {
                    self.bind(ident.id);
                }
                if let Some(kind) = n.kind {
                    use bolt_ts_ast::ImportClauseKind::*;
                    match kind {
                        Ns(n) => self.bind(n.id),
                        Specs(n) => {
                            for spec in n {
                                use bolt_ts_ast::ImportSpecKind::*;
                                match spec.kind {
                                    Shorthand(n) => self.bind(n.id),
                                    Named(n) => self.bind(n.id),
                                }
                            }
                        }
                    }
                }
            }
            ImportDecl(n) => {
                self.bind(n.clause.id);
                self.bind(n.module.id);
            }
            ExportDecl(n) => {
                self.bind_export_clause(n.clause);
            }
            CatchClause(n) => {
                if let Some(var) = n.var {
                    self.bind(var.id);
                }
                self.bind(n.block.id);
            }
            ObjectPat(n) => {
                for elem in n.elems {
                    self.bind(elem.id);
                }
            }
            ArrayPat(_) => {
                // TODO:
            }
            NullLit(_) | StringLit(_) | NumLit(_) | Ident(_) | ThisExpr(_) | BigIntLit(_)
            | BoolLit(_) => {}
            Binding(n) => self.bind_binding(n),
            OmitExpr(_) => {}
            ParenExpr(n) => {
                self.bind(n.expr.id());
            }
            EnumDecl(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
                self.bind(n.name.id);
                for member in n.members {
                    self.bind(member.id);
                }
            }
            EnumMember(n) => {
                self.bind_prop_name(n.name);
                if let Some(init) = n.init {
                    self.bind(init.id());
                }
            }
            ObjectShorthandMember(n) => {
                self.bind(n.name.id);
            }
            ObjectPropMember(n) => {
                self.bind_prop_name(n.name);
                self.bind(n.value.id());
            }
            ObjectMethodMember(n) => {
                self.bind_prop_name(n.name);
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                self.bind(n.body.id);
            }
            SpreadAssignment(n) => {
                self.bind(n.expr.id());
            }
            FnExpr(n) => {
                if let Some(name) = n.name {
                    self.bind(name.id);
                }
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                self.bind(n.body.id);
            }
            ClassExpr(n) => {
                if let Some(name) = n.name {
                    self.bind(name.id);
                }
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                if let Some(extends) = n.extends {
                    self.bind(extends.id);
                }
                if let Some(implements) = n.implements {
                    for ty in implements.list {
                        self.bind(ty.id);
                    }
                }
                for elem in n.elems.elems {
                    self.bind_class_elem(elem);
                }
            }
            NewExpr(n) => {
                self.bind(n.expr.id());
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
                if let Some(args) = n.args {
                    for arg in args {
                        self.bind(arg.id());
                    }
                }
            }
            AssignExpr(n) => {
                self.bind(n.left.id());
                self.bind(n.right.id());
            }
            ArrowFnExpr(n) => {
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                use ast::ArrowFnExprBody::*;
                match n.body {
                    Block(n) => self.bind_block_stmt(n),
                    Expr(n) => self.bind(n.id()),
                }
            }
            TypeofExpr(n) => {
                self.bind(n.expr.id());
            }
            VoidExpr(n) => {
                self.bind(n.expr.id());
            }
            SuperExpr(n) => {}
            QualifiedName(n) => {
                self.bind(n.left.id());
                self.bind(n.right.id);
            }
            AsExpr(n) => {
                self.bind(n.expr.id());
                self.bind(n.ty.id());
            }
            TyAssertionExpr(n) => {
                self.bind(n.expr.id());
                self.bind(n.ty.id());
            }
            SatisfiesExpr(n) => {
                self.bind(n.expr.id());
                self.bind(n.ty.id());
            }
            TemplateExpr(n) => {
                self.bind(n.head.id);
                for span in n.spans {
                    self.bind(span.id);
                }
            }
            TemplateHead(_) => {}
            TemplateSpan(n) => {
                self.bind(n.expr.id());
            }
            ComputedPropName(n) => {
                self.bind(n.expr.id());
            }
            LitTy(_) => {}
            ReferTy(n) => {
                self.bind_entity_name(n.name);
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
            }
            ArrayTy(n) => {
                self.bind(n.ele.id());
            }
            IndexedAccessTy(n) => {
                self.bind(n.ty.id());
                self.bind(n.index_ty.id());
            }
            FnTy(n) => {
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                self.bind(n.ty.id());
            }
            CtorTy(n) => {
                if let Some(modifiers) = n.modifiers {
                    self.bind_modifiers(modifiers);
                }
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                self.bind(n.ty.id());
            }
            ObjectLitTy(n) => {
                for m in n.members {
                    self.bind_object_ty_member(m);
                }
            }
            TyParam(n) => {
                self.bind(n.name.id);
                if let Some(constraint) = n.constraint {
                    self.bind(constraint.id());
                }
                if let Some(default) = n.default {
                    self.bind(default.id());
                }
            }
            IndexSigDecl(n) => {
                if let Some(modifiers) = n.modifiers {
                    self.bind_modifiers(modifiers);
                }
                self.bind_params(n.params);
                self.bind(n.ty.id());
            }
            CallSigDecl(n) => {
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
            }
            CtorSigDecl(n) => {
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
            }
            PropSignature(n) => {
                if let Some(modifiers) = n.modifiers {
                    self.bind_modifiers(modifiers);
                }
                self.bind_prop_name(n.name);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
            }
            MethodSignature(n) => {
                self.bind_prop_name(n.name);
                if let Some(ty_params) = n.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(n.params);
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
            }
            RestTy(n) => {
                self.bind(n.ty.id());
            }
            NamedTupleTy(n) => {
                self.bind(n.name.id);
                self.bind(n.ty.id());
            }
            TupleTy(n) => {
                for ty in n.tys {
                    self.bind(ty.id());
                }
            }
            CondTy(n) => {
                self.bind(n.check_ty.id());
                self.bind(n.extends_ty.id());
                self.bind(n.true_ty.id());
                self.bind(n.false_ty.id());
            }
            IntersectionTy(n) => {
                for ty in n.tys {
                    self.bind(ty.id());
                }
            }
            UnionTy(n) => {
                for ty in n.tys {
                    self.bind(ty.id());
                }
            }
            TypeofTy(n) => {
                self.bind_entity_name(n.name);
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
            }
            MappedTy(n) => {
                self.bind(n.ty_param.id);
                if let Some(name_ty) = n.name_ty {
                    self.bind(name_ty.id());
                }
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
                for m in n.members {
                    self.bind_object_ty_member(m);
                }
            }
            TyOp(n) => {
                self.bind(n.ty.id());
            }
            PredTy(n) => {
                self.bind(n.name.id);
                self.bind(n.ty.id());
            }
            ParenTy(n) => {
                self.bind(n.ty.id());
            }
            InferTy(n) => {
                self.bind(n.ty_param.id);
            }
            IntrinsicTy(_) => {}
            NullableTy(n) => {
                self.bind(n.ty.id());
            }
            TemplateLitTy(n) => {
                self.bind(n.head.id);
                for span in n.spans {
                    self.bind(span.id);
                }
            }
            TemplateSpanTy(n) => {
                self.bind(n.ty.id());
            }
            Modifier(_) => {}
            ShorthandSpec(n) => self.bind(n.name.id),
            ExportNamedSpec(n) => {
                self.bind_module_export_name(n.prop_name);
                self.bind_module_export_name(n.name);
            }
            DebuggerStmt(_) => {}
        }
        // TODO: bind_js_doc
        self.in_assignment_pattern = save_in_assignment_pattern;
    }

    fn bind_param_flow(&mut self, n: &ast::ParamDecl) {
        self.bind(n.name.id);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(init) = n.init {
            self.bind(init.id());
        }
    }

    fn bind_object_binding_elem_flow(&mut self, n: &ast::ObjectBindingElem<'cx>) {
        match n.name {
            ast::ObjectBindingName::Shorthand(ident) => {
                self.bind(ident.id);
            }
            ast::ObjectBindingName::Prop { prop_name, name } => {
                self.bind(prop_name.id());
                self.bind(name.id);
            }
        }
        if let Some(init) = n.init {
            self.bind(init.id());
        }
    }

    fn bind_non_null_expr_flow(&mut self, n: &ast::NonNullExpr<'cx>) {
        // TODO: is_optional_chain
        self.bind(n.expr.id());
    }

    fn bind_call_expr_flow(&mut self, n: &ast::CallExpr<'cx>) {
        // TODO: is_optional_chain
        let expr = bolt_ts_ast::Expr::skip_parens(n.expr);
        if matches!(expr.kind, ast::ExprKind::Fn(_) | ast::ExprKind::ArrowFn(_)) {
            if let Some(ty_args) = n.ty_args {
                for ty_arg in ty_args.list {
                    self.bind(ty_arg.id());
                }
            }
            for arg in n.args {
                self.bind(arg.id());
            }
            self.bind(n.expr.id());
        } else {
            if let Some(ty_args) = n.ty_args {
                for ty_arg in ty_args.list {
                    self.bind(ty_arg.id());
                }
            }
            for arg in n.args {
                self.bind(arg.id());
            }
            self.bind(n.expr.id());
            if matches!(n.expr.kind, ast::ExprKind::Super(_)) {
                // self.current_flow = self.create_flow_call
            }
        }
    }

    fn bind_var_decl_flow(&mut self, n: &ast::VarDecl<'cx>) {
        self.bind(n.binding.id);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(init) = n.init {
            self.bind(init.id());
        }

        use bolt_ts_ast::Node::*;
        if n.init.is_some()
            || matches!(
                self.p.node(self.p.parent(n.id).unwrap()),
                ForInStmt(_) | ForOfStmt(_)
            )
        {
            // self.bind(n.init.unwrap().id());
        }
    }

    fn bind_bin_expr_flow(&mut self, n: &ast::BinExpr<'cx>) {
        // TODO: more case
        self.bind(n.left.id());
        self.bind(n.right.id());
    }

    fn bind_postfix_unary_expr_flow(&mut self, n: &ast::PostfixUnaryExpr<'cx>) {
        self.bind(n.expr.id());
    }

    fn bind_prefix_unary_expr_flow(&mut self, n: &ast::PrefixUnaryExpr<'cx>) {
        self.bind(n.expr.id());
    }

    fn bind_expr_stmt(&mut self, n: &ast::ExprStmt<'cx>) {
        self.bind(n.expr.id());
        // TODO: maybe_bind_expr_flow_if_call
    }

    fn bind_while_stmt(&mut self, n: &ast::WhileStmt<'cx>) {}
    fn bind_do_stmt(&mut self, n: &ast::DoStmt<'cx>) {}
    fn bind_for_stmt(&mut self, n: &ast::ForStmt<'cx>) {
        if let Some(init) = &n.init {
            use ast::ForInitKind::*;
            match init {
                Var((_, list)) => {
                    for item in *list {
                        self.bind(item.id);
                    }
                }
                Expr(expr) => self.bind(expr.id()),
            }
        }
        if let Some(cond) = n.cond {
            // TODO: bind_cond
            self.bind(cond.id());
        }
        // TODO: delete this?
        if let Some(update) = n.incr {
            self.bind(update.id());
        }

        self.bind_iterative_stmt(n.body);
    }

    pub(super) fn bind_iterative_stmt(&mut self, n: &'cx ast::Stmt<'cx>) {
        self.bind(n.id());
    }

    pub(super) fn bind(&mut self, node: ast::NodeID) {
        let save_in_strict_mode = self.in_strict_mode;
        // TODO: set parent

        self._bind(node);

        let save_parent = self.parent;
        self.parent = Some(node);
        let container_flags = container_flags_for_node(self.p, node);
        if container_flags.is_empty() {
            self.bind_children(node);
        } else {
            self.bind_container(node, container_flags);
        }
        self.parent = save_parent;

        self.in_strict_mode = save_in_strict_mode;
    }

    fn bind_fn_expr(&mut self, f: &impl ir::FnExprLike<'cx>) {
        let name = f.name().map(SymbolName::Normal).unwrap_or(SymbolName::Fn);
        let id = f.id();
        let symbol = self.bind_anonymous_decl(id, SymbolFlags::FUNCTION, name);
        self.create_final_res(id, symbol);
    }

    fn bind_object_lit(&mut self, n: &ast::ObjectLit<'cx>) -> SymbolID {
        let s = self.bind_anonymous_decl(n.id, SymbolFlags::OBJECT_LITERAL, SymbolName::Object);
        self.create_final_res(n.id, s);
        s
    }

    fn bind_anonymous_ty(&mut self, id: ast::NodeID) -> SymbolID {
        let s = self.bind_anonymous_decl(id, SymbolFlags::TYPE_LITERAL, SymbolName::Type);
        self.create_final_res(id, s);
        s
    }

    fn bind_fn_or_ctor_ty(&mut self, id: ast::NodeID, symbol_name: SymbolName) {
        let symbol = self.create_symbol(symbol_name, SymbolFlags::SIGNATURE);
        self.add_declaration_to_symbol(symbol, id, SymbolFlags::SIGNATURE);

        let ty_lit_symbol = self.create_symbol(symbol_name, SymbolFlags::TYPE_LITERAL);
        self.add_declaration_to_symbol(ty_lit_symbol, id, SymbolFlags::TYPE_LITERAL);
        self.symbols
            .get_mut(ty_lit_symbol)
            .members
            .0
            .insert(symbol_name, symbol);
        self.create_final_res(id, ty_lit_symbol);
    }

    fn bind_var_decl(&mut self, n: &ast::VarDecl<'cx>) {
        if self.in_strict_mode {
            // TODO:
        }

        let name = match n.binding.kind {
            bolt_ts_ast::BindingKind::Ident(name) => name,
            _ => return,
        };
        let symbol = self.bind_var(n.id, name.name);
        // TODO: use var.id
        self.create_final_res(name.id, symbol);
    }

    fn bind_object_binding_ele(&mut self, n: &ast::ObjectBindingElem<'cx>) {
        if self.in_strict_mode {
            // TODO:
        }

        use ast::ObjectBindingName::*;
        let name = match n.name {
            Shorthand(name) => name,
            Prop { .. } => return,
        };
        let symbol = self.bind_var(n.id, name.name);
        // TODO: use binding.id
        self.create_final_res(name.id, symbol);
    }

    fn bind_var(&mut self, id: ast::NodeID, name: bolt_ts_atom::AtomId) -> SymbolID {
        let name = SymbolName::Normal(name);
        let symbol = if self.p.is_block_or_catch_scoped(id) {
            self.bind_block_scoped_decl(
                id,
                name,
                SymbolFlags::BLOCK_SCOPED_VARIABLE,
                SymbolFlags::BLOCK_SCOPED_VARIABLE_EXCLUDES,
            )
        } else if self.p.is_part_of_param_decl(id) {
            self.declare_symbol_and_add_to_symbol_table(
                name,
                id,
                None,
                SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                SymbolFlags::PARAMETER_EXCLUDES,
            )
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                name,
                id,
                None,
                SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                SymbolFlags::FUNCTION_SCOPED_VARIABLE_EXCLUDES,
            )
        };
        symbol
    }

    fn bind_param_decl(&mut self, n: &ast::ParamDecl<'cx>) {
        if self.in_strict_mode && !self.p.node_flags(n.id).intersects(NodeFlags::AMBIENT) {
            // TODO: check
        }
        match n.name.kind {
            bolt_ts_ast::BindingKind::Ident(ident) => {
                let name = SymbolName::Normal(ident.name);
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    n.id,
                    None,
                    SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                    SymbolFlags::PARAMETER_EXCLUDES,
                );
                // TODO: use `n.id`
                self.create_final_res(ident.id, symbol);
            }
            bolt_ts_ast::BindingKind::ObjectPat(_) => {
                // self.bind_anonymous_decl(n.id, flags, name)
                todo!()
            }
            bolt_ts_ast::BindingKind::ArrayPat(_) => todo!(),
        }

        // TODO: is_param_property_decl
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
            TyParam(n) => {
                self.bind_ty_param(n);
            }
            ParamDecl(n) => self.bind_param_decl(n),
            VarDecl(n) => {
                self.bind_var_decl(n);
            }
            ObjectBindingElem(n) => {
                // self.flow_nodes
                //     .insert_container_map(node, self.current_flow.unwrap());
                self.bind_object_binding_ele(n);
            }
            PropSignature(ast::PropSignature { name, question, .. })
            | ClassPropElem(ast::ClassPropElem { name, question, .. }) => {
                // TODO: is_auto_accessor
                let name = prop_name(name);
                let includes = SymbolFlags::PROPERTY
                    | if question.is_some() {
                        SymbolFlags::OPTIONAL
                    } else {
                        SymbolFlags::empty()
                    };
                let symbol = self.bind_prop_or_method_or_access(
                    node,
                    name,
                    includes,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            ObjectPropMember(n) => {
                let name = prop_name(n.name);
                let symbol = self.bind_prop_or_method_or_access(
                    node,
                    name,
                    SymbolFlags::PROPERTY,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            ObjectShorthandMember(n) => {
                let name = SymbolName::Ele(n.name.name);
                let symbol = self.bind_prop_or_method_or_access(
                    node,
                    name,
                    SymbolFlags::PROPERTY,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            EnumMember(m) => {
                let name = prop_name(m.name);
                let symbol = self.bind_prop_or_method_or_access(
                    node,
                    name,
                    SymbolFlags::ENUM_MEMBER,
                    SymbolFlags::ENUM_MEMBER_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            CallSigDecl(_) | CtorSigDecl(_) | IndexSigDecl(_) => {
                let name = if n.is_call_sig_decl() {
                    SymbolName::Call
                } else if n.is_ctor_sig_decl() {
                    SymbolName::New
                } else if n.is_index_sig_decl() {
                    SymbolName::Index
                } else {
                    unreachable!()
                };
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    node,
                    None,
                    SymbolFlags::SIGNATURE,
                    SymbolFlags::empty(),
                );
                self.create_final_res(node, symbol);
            }
            MethodSignature(node) => {
                let includes = SymbolFlags::METHOD
                    | if node.question.is_some() {
                        SymbolFlags::OPTIONAL
                    } else {
                        SymbolFlags::empty()
                    };
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    prop_name(node.name),
                    includes,
                    SymbolFlags::METHOD_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            ClassMethodElem(node) => {
                // TODO: is_optional
                let includes = SymbolFlags::METHOD;
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    prop_name(node.name),
                    includes,
                    SymbolFlags::METHOD_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            ObjectMethodMember(node) => {
                // TODO: is_optional
                let includes = SymbolFlags::METHOD;
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    prop_name(node.name),
                    includes,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            FnDecl(node) => {
                self.bind_fn_decl(node);
            }
            ClassCtor(node) => {
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    SymbolName::Constructor,
                    node.id,
                    None,
                    SymbolFlags::CONSTRUCTOR,
                    SymbolFlags::empty(),
                );
                self.create_final_res(node.id, symbol);
            }
            GetterDecl(node) => {
                let name = prop_name(node.name);
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    name,
                    SymbolFlags::GET_ACCESSOR,
                    SymbolFlags::GET_ACCESSOR_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            SetterDecl(node) => {
                let name = prop_name(node.name);
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    name,
                    SymbolFlags::SET_ACCESSOR,
                    SymbolFlags::SET_ACCESSOR_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            FnTy(_) => {
                self.bind_fn_or_ctor_ty(node, SymbolName::Call);
            }
            CtorTy(_) => {
                self.bind_fn_or_ctor_ty(node, SymbolName::New);
            }
            ObjectLitTy(_) | MappedTy(_) => {
                self.bind_anonymous_ty(node);
            }
            ObjectLit(n) => {
                self.bind_object_lit(n);
            }
            FnExpr(n) => {
                self.bind_fn_expr(n);
            }
            ArrowFnExpr(n) => {
                self.bind_fn_expr(n);
            }
            CallExpr(_) => {}
            ClassExpr(node) => {
                self.in_strict_mode = true;
                self.bind_class_like_decl(node, true);
            }
            ClassDecl(node) => {
                self.in_strict_mode = true;
                self.bind_class_like_decl(node, false);
            }
            InterfaceDecl(node) => self.bind_interface_decl(node),
            TypeDecl(node) => self.bind_type_decl(node),
            EnumDecl(node) => self.bind_enum_decl(node),
            NamespaceDecl(node) => self.bind_ns_decl(node),
            // import/export shorthand spec
            ShorthandSpec(node) => {
                let name = SymbolName::Normal(node.name.name);
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    node.id,
                    None,
                    SymbolFlags::ALIAS,
                    SymbolFlags::ALIAS_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            ExportNamedSpec(node) => {
                let n = |name: &ast::ModuleExportName| {
                    use bolt_ts_ast::ModuleExportNameKind::*;
                    match name.kind {
                        Ident(ident) => SymbolName::Normal(ident.name),
                        StringLit(lit) => SymbolName::Normal(lit.val),
                    }
                };
                let name = n(node.name);
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    node.id,
                    None,
                    SymbolFlags::ALIAS,
                    SymbolFlags::ALIAS_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            ImportClause(node) => self.bind_import_clause(node),
            ExportDecl(node) => self.bind_export_decl(node),
            Program(node) => {
                // TODO: `update_strict_module_statement_list`
                self.bind_source_file_if_external_module(node);
            }
            BlockStmt(_)
                if self
                    .p
                    .node(self.p.parent(node).unwrap())
                    .is_fn_like_or_class_static_block_decl() => {}
            BlockStmt(_) | ModuleBlock(_) => {
                // TODO: `update_strict_module_statement_list`
            }
            _ => {}
        }
    }

    fn bind_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) {
        // TODO: is const
        let name = SymbolName::Normal(node.name.name);
        let symbol = self.bind_block_scoped_decl(
            node.id,
            name,
            SymbolFlags::REGULAR_ENUM,
            SymbolFlags::REGULAR_ENUM_EXCLUDES,
        );
        self.final_res.insert(node.id, symbol);
    }

    fn bind_import_clause(&mut self, node: &'cx ast::ImportClause<'cx>) {}

    fn bind_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) {
        let container = self.container.unwrap();
        // match node.clause {
        //     ast
        // }
    }

    fn bind_source_file_if_external_module(&mut self, node: &'cx ast::Program<'cx>) {
        // TODO: if is_external_module
        let s = self.bind_anonymous_decl(
            node.id,
            SymbolFlags::VALUE_MODULE,
            SymbolName::Normal(node.filepath),
        );
        assert_eq!(s, SymbolID::container(node.id.module()));
        self.final_res.insert(node.id, s);
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

    pub(super) fn bind_anonymous_decl(
        &mut self,
        node: ast::NodeID,
        flags: SymbolFlags,
        name: SymbolName,
    ) -> SymbolID {
        let symbol = self.create_symbol(name, flags);
        if flags.intersects(SymbolFlags::ENUM_MEMBER | SymbolFlags::CLASS_MEMBER) {
            // self.symbols.get_mut(symbol).parent = container.symbol
        }
        self.add_declaration_to_symbol(symbol, node, flags);
        symbol
    }
}

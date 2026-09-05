use super::BinderState;
use super::container_flags::container_flags_for_node;
use super::create::DeclareSymbolProperty;
use super::flow::FlowArrayMutationNode;
use super::flow::FlowFlags;
use super::flow::FlowID;
use super::flow::FlowNodeKind;
use super::symbol::SymbolFlags;
use super::symbol::SymbolTableLocation;
use super::symbol::{SymbolID, SymbolName};

use bolt_ts_ast as ast;
use bolt_ts_ast::BinOpKind;
use bolt_ts_ast::NodeFlags;
use bolt_ts_ast::keyword::is_push_or_unshift;
use bolt_ts_ast::r#trait::VarLike;

impl<'cx, 'atoms, 'parser> BinderState<'cx, 'atoms, 'parser> {
    fn bind_getter_decl_children(&mut self, n: &'cx ast::GetterDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        self.bind_prop_name(n.name);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(body) = n.body {
            self.bind_block_stmt_children(body);
        }
    }

    fn bind_setter_decl_children(&mut self, n: &'cx ast::SetterDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        self.bind_prop_name(n.name);
        self.bind_params(n.params);
        if let Some(body) = n.body {
            self.bind_block_stmt_children(body);
        }
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
        if let Some(else_then) = n.else_then {
            self.bind(else_then.id())
        }
        self.flow_nodes
            .add_antecedent(post_if_label, self.current_flow.unwrap());

        self.current_flow = Some(self.finish_flow_label(post_if_label));
    }

    fn bind_try_stmt(&mut self, stmt: &'cx ast::TryStmt<'cx>) {
        let save_return_target = self.current_return_target;
        let save_exception_target = self.current_exception_target;
        let normal_exit_label = self.flow_nodes.create_branch_label();
        let return_label = self.flow_nodes.create_branch_label();
        let mut exception_label = self.flow_nodes.create_branch_label();
        if stmt.finally_block.is_some() {
            self.current_return_target = Some(return_label);
        }
        self.flow_nodes
            .add_antecedent(exception_label, self.current_flow.unwrap());
        self.current_exception_target = Some(exception_label);
        self.bind(stmt.try_block.id);
        self.flow_nodes
            .add_antecedent(normal_exit_label, self.current_flow.unwrap());
        if let Some(catch) = stmt.catch_clause {
            self.current_flow = Some(self.finish_flow_label(exception_label));
            exception_label = self.flow_nodes.create_branch_label();
            self.flow_nodes
                .add_antecedent(normal_exit_label, self.current_flow.unwrap());
            self.current_exception_target = Some(exception_label);
            self.bind(catch.id);
            self.flow_nodes
                .add_antecedent(normal_exit_label, self.current_flow.unwrap());
        }
        self.current_return_target = save_return_target;
        self.current_exception_target = save_exception_target;
        if let Some(finally) = stmt.finally_block {
            let finally_label = self.flow_nodes.create_branch_label();
            let mut antecedents = vec![];
            if let Some(list) = self.flow_nodes.antecedent_of_label(normal_exit_label) {
                antecedents.extend_from_slice(list);
            }
            if let Some(list) = self.flow_nodes.antecedent_of_label(exception_label) {
                antecedents.extend_from_slice(list);
            }
            if let Some(list) = self.flow_nodes.antecedent_of_label(return_label) {
                antecedents.extend_from_slice(list);
            }
            let FlowNodeKind::Label(f) = &mut self.flow_nodes.get_mut_flow_node(finally_label).kind
            else {
                unreachable!()
            };
            f.antecedent = Some(antecedents);
            self.current_flow = Some(finally_label);
            self.bind(finally.id);
            if self
                .flow_nodes
                .get_flow_node(self.current_flow.unwrap())
                .flags
                .contains(FlowFlags::UNREACHABLE)
            {
                self.current_flow = Some(self.unreachable_flow_node);
            } else {
                if let Some(current_return_target) = self.current_return_target
                    && let Some(antecedents) = self.flow_nodes.antecedent_of_label(return_label)
                {
                    let antecedents = antecedents.to_vec();

                    let reduced_label = self.flow_nodes.create_reduced_label(
                        finally_label,
                        antecedents,
                        self.current_flow.unwrap(),
                    );
                    self.flow_nodes
                        .add_antecedent(current_return_target, reduced_label);
                }

                if let Some(current_exception_target) = self.current_exception_target
                    && let Some(antecedents) = self.flow_nodes.antecedent_of_label(exception_label)
                {
                    let antecedents = antecedents.to_vec();
                    let reduced_label = self.flow_nodes.create_reduced_label(
                        finally_label,
                        antecedents,
                        self.current_flow.unwrap(),
                    );
                    self.flow_nodes
                        .add_antecedent(current_exception_target, reduced_label);
                }
                self.current_flow = Some(
                    if let Some(antecedents) =
                        self.flow_nodes.antecedent_of_label(normal_exit_label)
                    {
                        let antecedents = antecedents.to_vec();
                        self.flow_nodes.create_reduced_label(
                            finally_label,
                            antecedents,
                            self.current_flow.unwrap(),
                        )
                    } else {
                        self.unreachable_flow_node
                    },
                );
            }
        } else {
            self.current_flow = Some(self.finish_flow_label(normal_exit_label));
        }
    }

    fn bind_export_clause(&mut self, clause: &'cx ast::ExportClause<'cx>) {
        use bolt_ts_ast::ExportClauseKind::*;
        match clause.kind {
            Glob(n) => self.bind(n.module.id),
            Ns(_) => todo!(),
            Specs(n) => self.bind(n.id),
        }
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
            ast::Node::NestedModuleDecl(_) | ast::Node::BlockModuleDecl(_) => {
                self.declare_module_member(name, node, includes, exclude_flags)
            }
            ast::Node::Program(_) if self.p.is_external_or_commonjs_module() => {
                self.declare_module_member(name, node, includes, exclude_flags)
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
                    DeclareSymbolProperty::empty(),
                )
            }
        }
    }

    fn bind_type_parameters(&mut self, ty_params: Option<ast::TyParams<'cx>>) {
        if let Some(ty_params) = ty_params {
            for ty_param in ty_params {
                self.bind(ty_param.id);
            }
        }
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

    fn bind_block_stmt_children(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        self.bind(block.id);
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
            .add_antecedent(post_expression_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(false_label));
        let when_false_flow = self.current_flow.unwrap();

        if self.in_return_position {
            self.flow_nodes
                .insert_cond_expr_flow(cond, when_true_flow, when_false_flow);
        }
        self.bind(cond.when_false.id());
        self.flow_nodes
            .add_antecedent(post_expression_label, self.current_flow.unwrap());

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
        let should_add_antecedent = node.is_none_or(|node| {
            !(node.kind.is_logical_assignment() || node.kind.is_logical_expr() || {
                let nq = self.node_query();
                let node_id = node.id();
                nq.is_optional_chain(node_id) && nq.is_outermost_optional_chain(node_id)
            })
        });
        if should_add_antecedent {
            let t = self.create_flow_condition(
                FlowFlags::TRUE_CONDITION,
                self.current_flow.unwrap(),
                node.map(|n| n.id()),
            );
            self.flow_nodes.add_antecedent(true_target, t);
            let f = self.create_flow_condition(
                FlowFlags::FALSE_CONDITION,
                self.current_flow.unwrap(),
                node.map(|n| n.id()),
            );
            self.flow_nodes.add_antecedent(false_target, f);
        }
    }

    pub(super) fn bind_entity_name(&mut self, name: &'cx ast::EntityName<'cx>) {
        use bolt_ts_ast::EntityNameKind::*;
        match name.kind {
            Ident(n) => self.bind(n.id),
            Qualified(q) => {
                self.bind(q.id);
            }
        }
    }

    fn bind_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.bind(param.id);
        }
    }

    pub(super) fn declare_symbol_and_add_to_symbol_table_for_fn_like_container(
        &mut self,
        name: SymbolName,
        current: ast::NodeID,
        container: ast::NodeID,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> SymbolID {
        use ast::Node::*;
        debug_assert_eq!(self.container, Some(container));
        debug_assert!(matches!(
            self.p.node(container),
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
                | TypeAliasDecl(_)
                | MappedTy(_)
                | ClassStaticBlockDecl(_)
        ));
        debug_assert!(
            self.p.node(container).has_locals(),
            "container({:?}) should have locals, but it doesn't",
            self.p.node(container).span()
        );
        let table = SymbolTableLocation::locals(container);
        let container_symbol = self.final_res[&container];
        self.declare_symbol(
            Some(name),
            table,
            Some(container_symbol),
            current,
            symbol_flags,
            symbol_excludes,
            DeclareSymbolProperty::empty(),
        )
    }

    pub(super) fn declare_symbol_and_add_to_symbol_table(
        &mut self,
        name: SymbolName,
        current: ast::NodeID,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> SymbolID {
        let container = self.container.unwrap();
        let c = self.p.node(container);
        use ast::Node::*;
        match c {
            NestedModuleDecl(_) | BlockModuleDecl(_) => {
                self.declare_module_member(name, current, symbol_flags, symbol_excludes)
            }
            Program(_) => {
                self.declare_source_file_member(name, current, symbol_flags, symbol_excludes)
            }
            ClassExpr(_) | ClassDecl(_) => {
                self.declare_class_member(name, current, symbol_flags, symbol_excludes)
            }
            EnumDecl(_) => {
                let loc = SymbolTableLocation::exports(container);
                let parent = self.final_res[&container];
                self.declare_symbol(
                    Some(name),
                    loc,
                    Some(parent),
                    current,
                    symbol_flags,
                    symbol_excludes,
                    DeclareSymbolProperty::empty(),
                )
            }
            ObjectLitTy(_) | ObjectLit(_) | InterfaceDecl(_) => {
                let parent = self.final_res[&container];
                self.declare_symbol(
                    Some(name),
                    SymbolTableLocation::members(container),
                    Some(parent),
                    current,
                    symbol_flags,
                    symbol_excludes,
                    DeclareSymbolProperty::empty(),
                )
            }
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
            | TypeAliasDecl(_)
            | MappedTy(_)
            | ClassStaticBlockDecl(_) => self
                .declare_symbol_and_add_to_symbol_table_for_fn_like_container(
                    name,
                    current,
                    container,
                    symbol_flags,
                    symbol_excludes,
                ),
            _ => unreachable!(),
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
        let container_symbol = self.final_res[&container];
        let loc = if self.p.node(node).is_static() {
            SymbolTableLocation::exports(container)
        } else {
            SymbolTableLocation::members(container)
        };
        self.declare_symbol(
            Some(name),
            loc,
            Some(container_symbol),
            node,
            include,
            excludes,
            DeclareSymbolProperty::empty(),
        )
    }

    fn declare_source_file_member(
        &mut self,
        name: SymbolName,
        current: ast::NodeID,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> SymbolID {
        let container = self.container.unwrap();
        assert!(self.p.node(container).is_program());
        if self.p.is_external_or_commonjs_module() {
            self.declare_module_member(name, current, symbol_flags, symbol_excludes)
        } else {
            let table = SymbolTableLocation::locals(container);
            self.declare_symbol(
                Some(name),
                table,
                None,
                current,
                symbol_flags,
                symbol_excludes,
                DeclareSymbolProperty::empty(),
            )
        }
    }

    fn declare_module_member(
        &mut self,
        name: SymbolName,
        current: ast::NodeID,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> SymbolID {
        let container = self.container.unwrap();
        let has_export_modifier = self
            .node_query()
            .get_combined_modifier_flags(current)
            .contains(ast::ModifierFlags::EXPORT); // TODO: js
        if symbol_flags.contains(SymbolFlags::ALIAS) {
            let n = self.p.node(current);
            let (loc, parent) = if n.is_export_named_spec()
                || n.is_export_shorthand_spec()
                || (n.is_import_equals_decl() && has_export_modifier)
            {
                let table = SymbolTableLocation::exports(container);
                let parent = self.final_res[&container];
                (table, Some(parent))
            } else {
                assert!(self.p.node(container).has_locals());
                let table = SymbolTableLocation::locals(container);
                (table, None)
            };
            return self.declare_symbol(
                Some(name),
                loc,
                parent,
                current,
                symbol_flags,
                symbol_excludes,
                DeclareSymbolProperty::empty(),
            );
        }

        let current_node = self.p.node(current);
        if !current_node.is_ambient_module()
            && (has_export_modifier
                || self
                    .p
                    .node_flags(container)
                    .contains(NodeFlags::EXPORT_CONTEXT))
        {
            if !self.p.node(container).has_locals()
                || !self.locals.contains_key(&container)
                || (current_node.has_syntactic_modifier(ast::ModifierFlags::DEFAULT)
                    && current_node.ident_name().is_none())
            {
                let table = SymbolTableLocation::exports(container);
                return self.declare_symbol(
                    Some(SymbolName::ExportDefault),
                    table,
                    Some(self.final_res[&container]),
                    current,
                    symbol_flags,
                    symbol_excludes,
                    DeclareSymbolProperty::empty(),
                );
            }
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
                DeclareSymbolProperty::empty(),
            );
            let table = SymbolTableLocation::exports(container);
            let export_symbol = self.declare_symbol(
                Some(name),
                table,
                Some(self.final_res[&container]),
                current,
                symbol_flags,
                symbol_excludes,
                DeclareSymbolProperty::empty(),
            );
            self.symbols.get_mut(local).export_symbol = Some(export_symbol);
            let prev = self.local_symbols.insert(current.index_as_u32(), local);
            debug_assert!(prev.is_none());
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
                DeclareSymbolProperty::empty(),
            )
        }
    }

    fn bind_modifiers(&mut self, mods: &'cx ast::Modifiers<'cx>) {
        for m in mods.list {
            self.bind(m.id());
        }
    }

    fn bind_prop_name(&mut self, name: &'cx ast::PropName<'cx>) {
        use bolt_ts_ast::PropNameKind::*;
        match name.kind {
            Ident(n) => self.bind(n.id),
            PrivateIdent(n) => self.bind(n.id),
            StringLit { raw: n, .. } => self.bind(n.id),
            NumLit(n) => self.bind(n.id),
            Computed(n) => self.bind(n.id),
            BigIntLit(n) => self.bind(n.id),
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
            ObjectPat(n) => self.bind(n.id),
            ArrayPat(n) => self.bind(n.id),
        }
    }

    fn bind_class_elem(&mut self, n: &'cx ast::ClassElem<'cx>) {
        self.bind(n.id());
    }

    fn bind_stmts_under(&mut self, parent: ast::NodeID, stmts: ast::Stmts<'cx>) {
        self.block_parent_stack.push(parent);
        for stmt in stmts {
            self.bind(stmt.id());
        }
        self.block_parent_stack.pop();
    }

    pub(super) fn bind_children(&mut self, node: ast::NodeID) {
        let save_in_assignment_pattern = self.in_assignment_pattern;
        self.in_assignment_pattern = true;

        if self
            .current_flow
            .is_some_and(|f| f == self.unreachable_flow_node)
        {
            self.bind_children_worker_in_unreachable_flow(node);
            self.in_assignment_pattern = save_in_assignment_pattern;
            return;
        }

        // if n.ge_first_stmt_and_le_last_stmt() && (n.is_ret_stmt() || )

        self.bind_children_worker(node, save_in_assignment_pattern);
        // TODO: bind_js_doc
        self.in_assignment_pattern = save_in_assignment_pattern;
    }

    fn bind_object_pat_children(&mut self, node: &'cx ast::ObjectPat<'cx>) {
        for elem in node.elems {
            self.bind(elem.id);
        }
    }

    fn bind_object_binding_name(&mut self, node: &'cx ast::ObjectBindingName<'cx>) {
        match node {
            ast::ObjectBindingName::Shorthand(ident) => {
                self.bind(ident.id);
            }
            ast::ObjectBindingName::Prop { prop_name, name } => {
                self.bind(prop_name.id());
                self.bind_binding(name);
            }
        }
    }

    fn bind_object_binding_elem_children(&mut self, node: &'cx ast::ObjectBindingElem<'cx>) {
        self.bind_object_binding_name(node.name);
        if let Some(init) = node.init() {
            self.bind(init.id());
        }
    }

    fn bind_array_pat_children(&mut self, node: &'cx ast::ArrayPat<'cx>) {
        for elem in node.elems {
            match elem.kind {
                ast::ArrayBindingElemKind::Omit(e) => {
                    self.bind(e.id);
                }
                ast::ArrayBindingElemKind::Binding(e) => {
                    self.bind(e.id);
                }
            }
        }
    }

    fn bind_enum_member_children(&mut self, node: &'cx ast::EnumMember<'cx>) {
        match node.name {
            ast::EnumMemberNameKind::Ident(ident) => self.bind(ident.id),
            ast::EnumMemberNameKind::StringLit { raw, .. } => self.bind(raw.id),
        }
        if let Some(init) = node.init {
            self.bind(init.id());
        }
    }

    fn bind_object_shorthand_member_children(
        &mut self,
        node: &'cx ast::ObjectShorthandMember<'cx>,
    ) {
        self.bind(node.name.id);
        if let Some(init) = node.object_assignment_initializer {
            self.bind(init.id());
        }
    }

    fn bind_object_prop_assignment_children(&mut self, node: &'cx ast::ObjectPropAssignment<'cx>) {
        self.bind_prop_name(node.name);
        self.bind(node.init.id());
    }

    fn bind_object_method_member_children(&mut self, node: &'cx ast::ObjectMethodMember<'cx>) {
        self.bind_prop_name(node.name);
        self.bind_type_parameters(node.ty_params);
        self.bind_params(node.params);
        if let Some(ty) = node.ty {
            self.bind(ty.id());
        }
        self.bind(node.body.id);
    }

    fn bind_spread_assignment_children(&mut self, n: &'cx ast::SpreadAssignment<'cx>) {
        self.bind(n.expr.id());
    }

    fn bind_spread_element_children(&mut self, n: &'cx ast::SpreadElement<'cx>) {
        self.bind(n.expr.id());
    }

    fn bind_template_span_children(&mut self, n: &'cx ast::TemplateSpan<'cx>) {
        self.bind(n.expr.id());
    }

    fn bind_default_clause_children(&mut self, n: &'cx ast::DefaultClause<'cx>) {
        for stmt in n.stmts {
            self.bind(stmt.id());
        }
    }

    fn bind_case_or_default_clause_children(&mut self, clause: &ast::CaseOrDefaultClause<'cx>) {
        match clause {
            ast::CaseOrDefaultClause::Case(c) => self.bind(c.id),
            ast::CaseOrDefaultClause::Default(d) => {
                self.bind(d.id);
            }
        }
    }

    fn bind_var_stmt_children(&mut self, n: &'cx ast::VarStmt<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        for item in n.list {
            self.bind(item.id);
        }
    }

    fn bind_fn_declaration_children(&mut self, n: &'cx ast::FnDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        if let Some(name) = n.name {
            self.bind(name.id);
        }
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(body) = n.body {
            self.bind_block_stmt_children(body);
        }
    }

    fn bind_class_declaration_children(&mut self, n: &'cx ast::ClassDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        if let Some(name) = n.name {
            self.bind(name.id);
        }
        self.bind_type_parameters(n.ty_params);
        if let Some(extends) = n.extends {
            self.bind(extends.id);
        }
        if let Some(implements) = n.implements {
            for elem in implements.list {
                self.bind(elem.id);
            }
        }
        for elem in n.elems.list {
            self.bind_class_elem(elem);
        }
    }

    fn bind_interface_decl_children(&mut self, n: &'cx ast::InterfaceDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        self.bind(n.name.id);
        self.bind_type_parameters(n.ty_params);
        if let Some(extends) = n.extends {
            self.bind(extends.id);
        }
        for m in n.members {
            self.bind_object_ty_member(m);
        }
    }

    fn bind_type_alias_decl_children(&mut self, n: &'cx ast::TypeAliasDecl<'cx>) {
        self.bind(n.name.id);
        self.bind_type_parameters(n.ty_params);
        self.bind(n.ty.id());
    }

    fn bind_interface_extends_clause_children(&mut self, n: &'cx ast::InterfaceExtendsClause<'cx>) {
        for ty in n.list {
            self.bind(ty.id);
        }
    }

    fn bind_class_implements_clause_children(&mut self, n: &'cx ast::ClassImplementsClause<'cx>) {
        for ty in n.list {
            self.bind(ty.id);
        }
    }

    fn bind_enum_decl_children(&mut self, n: &'cx ast::EnumDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        self.bind(n.name.id);
        for member in n.members {
            self.bind(member.id);
        }
    }

    fn bind_import_decl_children(&mut self, n: &'cx ast::ImportDecl<'cx>) {
        if let Some(clause) = n.clause {
            self.bind(clause.id);
        }
        self.bind(n.module.id);
    }

    fn bind_import_equals_decl_children(&mut self, n: &'cx ast::ImportEqualsDecl<'cx>) {
        self.bind(n.name.id);
        match n.module_reference {
            ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                self.bind(n.id());
            }
            ast::ModuleReferenceKind::EntityName(n) => self.bind_entity_name(n),
        }
    }

    fn bind_catch_clause_children(&mut self, n: &'cx ast::CatchClause<'cx>) {
        if let Some(var) = n.var {
            self.bind(var.id);
        }
        self.bind(n.block.id);
    }

    fn bind_labeled_stmt_children(&mut self, n: &'cx ast::LabeledStmt<'cx>) {
        self.bind(n.label.id);
        self.bind(n.stmt.id());
    }

    fn bind_fn_expr_children(&mut self, n: &'cx ast::FnExpr<'cx>) {
        if let Some(name) = n.name {
            self.bind(name.id);
        }
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        self.bind(n.body.id);
    }

    fn bind_class_expr_children(&mut self, n: &'cx ast::ClassExpr<'cx>) {
        if let Some(name) = n.name {
            self.bind(name.id);
        }
        self.bind_type_parameters(n.ty_params);
        if let Some(extends) = n.extends {
            self.bind(extends.id);
        }
        if let Some(implements) = n.implements {
            for ty in implements.list {
                self.bind(ty.id);
            }
        }
        for elem in n.elems.list {
            self.bind_class_elem(elem);
        }
    }

    fn bind_new_expr_children(&mut self, n: &'cx ast::NewExpr<'cx>) {
        self.bind(n.expr.id());
        self.bind_type_arguments(n.ty_args);
        if let Some(args) = n.args {
            for arg in args {
                self.bind(arg.id());
            }
        }
    }

    fn bind_arrow_fn_expr_children(&mut self, n: &'cx ast::ArrowFnExpr<'cx>) {
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        use ast::ArrowFnExprBody::*;
        match n.body {
            Block(n) => self.bind_block_stmt_children(n),
            Expr(n) => self.bind(n.id()),
        }
    }

    fn bind_yield_expr_children(&mut self, n: &'cx ast::YieldExpr<'cx>) {
        if let Some(expr) = n.expr {
            self.bind(expr.id());
        }
    }

    fn bind_as_expr_children(&mut self, n: &'cx ast::AsExpr<'cx>) {
        self.bind(n.expr.id());
        self.bind(n.ty.id());
    }

    fn bind_ty_assertion_expr_children(&mut self, n: &'cx ast::TyAssertion<'cx>) {
        self.bind(n.expr.id());
        self.bind(n.ty.id());
    }

    fn bind_satisfies_expr_children(&mut self, n: &'cx ast::SatisfiesExpr<'cx>) {
        self.bind(n.expr.id());
        self.bind(n.ty.id());
    }

    fn bind_template_expr_children(&mut self, n: &'cx ast::TemplateExpr<'cx>) {
        self.bind(n.head.id);
        for span in n.spans {
            self.bind(span.id);
        }
    }

    fn bind_tagged_template_expr_children(&mut self, n: &'cx ast::TaggedTemplateExpr<'cx>) {
        self.bind(n.tag.id());
        self.bind_type_arguments(n.ty_args);
        self.bind(n.tpl.id());
    }

    fn bind_expr_with_ty_args_children(&mut self, n: &'cx ast::ExprWithTyArgs<'cx>) {
        self.bind(n.expr.id());
        self.bind_type_arguments(n.ty_args);
    }

    fn bind_qualified_name_children(&mut self, n: &'cx ast::QualifiedName<'cx>) {
        self.bind(n.left.id());
        self.bind(n.right.id);
    }

    fn bind_refer_ty_children(&mut self, n: &'cx ast::ReferTy<'cx>) {
        self.bind_entity_name(n.name);
        self.bind_type_arguments(n.ty_args);
    }

    fn bind_import_type_children(&mut self, n: &'cx ast::ImportType<'cx>) {
        self.bind(n.argument.id());
        self.bind_type_arguments(n.type_arguments);
        if let Some(qualifier) = n.qualifier {
            self.bind_entity_name(qualifier);
        }
    }

    fn bind_indexed_access_ty_children(&mut self, n: &'cx ast::IndexedAccessTy<'cx>) {
        self.bind(n.ty.id());
        self.bind(n.index_ty.id());
    }

    fn bind_fn_ty_children(&mut self, n: &'cx ast::FnTy<'cx>) {
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        self.bind(n.ty.id());
    }

    fn bind_ctor_ty_children(&mut self, n: &'cx ast::CtorTy<'cx>) {
        if let Some(modifiers) = n.modifiers {
            self.bind_modifiers(modifiers);
        }
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        self.bind(n.ty.id());
    }

    fn bind_object_lit_ty_children(&mut self, n: &'cx ast::ObjectLitTy<'cx>) {
        for m in n.members {
            self.bind_object_ty_member(m);
        }
    }

    fn bind_ty_param_children(&mut self, n: &'cx ast::TyParam<'cx>) {
        self.bind(n.name.id);
        if let Some(constraint) = n.constraint {
            self.bind(constraint.id());
        }
        if let Some(default) = n.default {
            self.bind(default.id());
        }
    }

    fn bind_index_sig_decl_children(&mut self, n: &'cx ast::IndexSigDecl<'cx>) {
        if let Some(modifiers) = n.modifiers {
            self.bind_modifiers(modifiers);
        }
        self.bind_binding(n.key);
        self.bind(n.key_ty.id());
        self.bind(n.ty.id());
    }

    fn bind_call_sig_decl_children(&mut self, n: &'cx ast::CallSigDecl<'cx>) {
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
    }

    fn bind_ctor_sig_decl_children(&mut self, n: &'cx ast::CtorSigDecl<'cx>) {
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
    }

    fn bind_prop_signature_children(&mut self, n: &'cx ast::PropSignature<'cx>) {
        if let Some(modifiers) = n.modifiers {
            self.bind_modifiers(modifiers);
        }
        self.bind_prop_name(n.name);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
    }

    fn bind_method_signature_children(&mut self, n: &'cx ast::MethodSignature<'cx>) {
        self.bind_prop_name(n.name);
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
    }

    fn bind_named_tuple_ty_children(&mut self, n: &'cx ast::NamedTupleTy<'cx>) {
        self.bind(n.name.id);
        self.bind(n.ty.id());
    }

    fn bind_tuple_ty_children(&mut self, n: &'cx ast::TupleTy<'cx>) {
        for ty in n.tys {
            self.bind(ty.id());
        }
    }

    fn bind_cond_ty_children(&mut self, n: &'cx ast::CondTy<'cx>) {
        self.bind(n.check_ty.id());
        self.bind(n.extends_ty.id());
        self.bind(n.true_ty.id());
        self.bind(n.false_ty.id());
    }

    fn bind_intersection_ty_children(&mut self, n: &'cx ast::IntersectionTy<'cx>) {
        for ty in n.tys {
            self.bind(ty.id());
        }
    }

    fn bind_union_ty_children(&mut self, n: &'cx ast::UnionTy<'cx>) {
        for ty in n.tys {
            self.bind(ty.id());
        }
    }

    fn bind_typeof_ty_children(&mut self, n: &'cx ast::TypeofTy<'cx>) {
        self.bind_entity_name(n.name);
        self.bind_type_arguments(n.ty_args);
    }

    fn bind_mapped_ty_children(&mut self, n: &'cx ast::MappedTy<'cx>) {
        self.bind(n.ty_param.id);
        if let Some(name_ty) = n.name_ty {
            self.bind(name_ty.id());
        }
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
    }

    fn bind_pred_ty_children(&mut self, n: &'cx ast::PredTy<'cx>) {
        match n.name {
            ast::PredTyName::Ident(n) => self.bind(n.id),
            ast::PredTyName::This(n) => self.bind(n.id),
        }
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
    }

    fn bind_template_lit_ty_children(&mut self, n: &'cx ast::TemplateLitTy<'cx>) {
        self.bind(n.head.id);
        for span in n.spans {
            self.bind(span.id);
        }
    }

    fn bind_jsx_ns_name_children(&mut self, n: &'cx ast::JsxNsName<'cx>) {
        self.bind(n.ns.id);
        self.bind(n.name.id);
    }

    fn bind_jsx_named_attr_children(&mut self, n: &'cx ast::JsxNamedAttr<'cx>) {
        self.bind(n.name.id());
        if let Some(attr_value) = n.init {
            self.bind(attr_value.id());
        }
    }

    fn bind_jsx_expr_children(&mut self, n: &'cx ast::JsxExpr<'cx>) {
        if let Some(e) = n.expr {
            self.bind(e.id());
        }
    }

    fn bind_jsx_opening_elem_children(&mut self, n: &'cx ast::JsxOpeningElem<'cx>) {
        self.bind(n.tag_name.id());
        self.bind_type_arguments(n.ty_args);
        for attr in n.attrs {
            self.bind(attr.id());
        }
    }

    fn bind_jsx_self_closing_elem_children(&mut self, n: &'cx ast::JsxSelfClosingElem<'cx>) {
        self.bind(n.tag_name.id());
        self.bind_type_arguments(n.ty_args);
        for attr in n.attrs {
            self.bind(attr.id());
        }
    }

    fn bind_jsx_frag_children(&mut self, n: &'cx ast::JsxFrag<'cx>) {
        self.bind(n.opening_frag.id);
        for child in n.children {
            self.bind(child.id());
        }
        self.bind(n.closing_frag.id);
    }

    fn bind_jsx_elem_children(&mut self, n: &'cx ast::JsxElem<'cx>) {
        self.bind(n.opening_elem.id);
        for child in n.children {
            self.bind(child.id());
        }
        self.bind(n.closing_elem.id);
    }

    fn bind_property_access_expr_children(&mut self, n: &'cx ast::PropAccessExpr<'cx>) {
        self.bind(n.expr.id());
        self.bind(n.name.id);
    }

    fn bind_element_access_expr_children(&mut self, n: &'cx ast::EleAccessExpr<'cx>) {
        self.bind(n.expr.id());
        self.bind(n.arg.id());
    }

    fn bind_children_worker_in_unreachable_flow(&mut self, node: ast::NodeID) {
        use ast::Node::*;
        let n = self.p.node(node);
        match n {
            Program(_) => unreachable!(),
            Modifier(_) => {}
            VarDecl(n) => self.bind_variable_declaration(n),
            ParamDecl(n) => self.bind_parameter_declaration(n),
            ClassExtendsClause(n) => self.bind_class_extends_clause_children(n),
            ImportShorthandSpec(n) => self.bind_import_shorthand_spec_children(n),
            ExportShorthandSpec(n) => self.bind_export_shorthand_spec_children(n),
            NsImport(n) => self.bind_ns_import_children(n),
            NsExport(n) => self.bind_ns_export_children(n),
            GlobExport(n) => self.bind_glob_export_children(n),
            SpecsExport(n) => self.bind_specs_export_children(n),
            ExportNamedSpec(n) => self.bind_export_named_spec_children(n),
            ImportNamedSpec(n) => self.bind_import_named_spec_children(n),
            ImportClause(n) => self.bind_import_clause_children(n),
            ObjectPat(n) => self.bind_object_pat_children(n),
            ObjectBindingElem(n) => self.bind_object_binding_elem_children(n),
            ArrayPat(n) => self.bind_array_pat_children(n),
            ArrayBinding(n) => {
                self.bind_binding(n.name);
                if let Some(init) = n.init {
                    self.bind(init.id());
                }
            }
            EnumMember(n) => self.bind_enum_member_children(n),
            ObjectShorthandMember(n) => self.bind_object_shorthand_member_children(n),
            ObjectPropAssignment(n) => self.bind_object_prop_assignment_children(n),
            ObjectMethodMember(n) => self.bind_object_method_member_children(n),
            SpreadAssignment(n) => self.bind_spread_assignment_children(n),
            SpreadElement(n) => self.bind_spread_element_children(n),
            TemplateHead(_) => {}
            TemplateSpan(n) => self.bind_template_span_children(n),
            CaseClause(n) => {
                self.bind(n.expr.id());
                for stmt in n.stmts {
                    self.bind(stmt.id());
                }
            }
            DefaultClause(n) => self.bind_default_clause_children(n),
            CaseBlock(n) => {
                for clause in n.clauses {
                    self.bind_case_or_default_clause_children(clause);
                }
            }
            VarStmt(n) => self.bind_var_stmt_children(n),
            FnDecl(n) => self.bind_fn_declaration_children(n),
            IfStmt(n) => {
                self.bind(n.expr.id());
                self.bind(n.then.id());
                if let Some(else_then) = n.else_then {
                    self.bind(else_then.id());
                }
            }
            RetStmt(n) => {
                if let Some(expr) = n.expr {
                    self.bind(expr.id());
                }
            }
            EmptyStmt(_) => {}
            ClassDecl(n) => self.bind_class_declaration_children(n),
            ClassCtor(n) => self.bind_class_ctor_children(n),
            ClassPropElem(n) => self.bind_class_prop_elem_children(n),
            ClassMethodElem(n) => self.bind_class_method_elem_children(n),
            ClassSemiElem(_) => {}
            ClassStaticBlockDecl(n) => self.bind_class_static_block_decl_children(n),
            NestedModuleDecl(n) => self.bind_nested_module_decl_children(n),
            BlockModuleDecl(n) => self.bind_block_module_decl_children(n),
            GetterDecl(n) => self.bind_getter_decl_children(n),
            SetterDecl(n) => self.bind_setter_decl_children(n),
            InterfaceDecl(n) => self.bind_interface_decl_children(n),
            TypeAliasDecl(n) => self.bind_type_alias_decl_children(n),
            InterfaceExtendsClause(n) => self.bind_interface_extends_clause_children(n),
            ClassImplementsClause(n) => self.bind_class_implements_clause_children(n),
            BlockStmt(n) => self.bind_stmts_under(node, n.stmts),
            ModuleBlock(n) => self.bind_stmts_under(node, n.stmts),
            ThrowStmt(n) => {
                self.bind(n.expr.id());
            }
            EnumDecl(n) => self.bind_enum_decl_children(n),
            ImportDecl(n) => self.bind_import_decl_children(n),
            ImportEqualsDecl(n) => self.bind_import_equals_decl_children(n),
            ExternalModuleReference(n) => {
                self.bind(n.module_spec().id);
            }
            ExportDecl(n) => {
                self.bind_export_clause(n.clause);
            }
            ExportAssign(n) => {
                self.bind(n.expr.id());
            }
            ForStmt(n) => {
                if let Some(init) = &n.init {
                    use ast::ForInitKind::*;
                    match init {
                        Var(list) => {
                            for item in *list {
                                self.bind(item.id);
                            }
                        }
                        Expr(expr) => self.bind(expr.id()),
                    }
                }
                if let Some(cond) = n.cond {
                    self.bind(cond.id());
                }
                self.bind(n.body.id());
                if let Some(update) = n.incr {
                    self.bind(update.id());
                }
            }
            ForInStmt(n) => {
                self.bind(n.expr.id());
                use ast::ForInitKind::*;
                match n.init {
                    Var(list) => {
                        for item in list {
                            self.bind(item.id);
                        }
                    }
                    Expr(expr) => self.bind(expr.id()),
                }
                self.bind(n.body.id());
            }
            ForOfStmt(n) => {
                self.bind(n.expr.id());
                use ast::ForInitKind::*;
                match n.init {
                    Var(list) => {
                        for item in list {
                            self.bind(item.id);
                        }
                    }
                    Expr(expr) => self.bind(expr.id()),
                }
                self.bind(n.body.id());
            }
            WhileStmt(n) => {
                self.bind(n.expr.id());
                self.bind(n.stmt.id());
            }
            DoWhileStmt(n) => {
                self.bind(n.stmt.id());
                self.bind(n.expr.id());
            }
            BreakStmt(n) => {
                if let Some(label) = n.label {
                    self.bind(label.id);
                }
            }
            ContinueStmt(n) => {
                if let Some(label) = n.label {
                    self.bind(label.id);
                }
            }
            TryStmt(n) => {
                self.bind(n.try_block.id);
                if let Some(catch) = n.catch_clause {
                    self.bind(catch.id);
                }
                if let Some(finally) = n.finally_block {
                    self.bind(finally.id);
                }
            }
            CatchClause(n) => self.bind_catch_clause_children(n),
            LabeledStmt(n) => self.bind_labeled_stmt_children(n),
            SwitchStmt(n) => {
                self.bind(n.expr.id());
                self.bind(n.case_block.id);
            }
            ExprStmt(n) => {
                self.bind(n.expr.id());
            }
            BinExpr(n) => {
                self.bind(n.left.id());
                self.bind(n.right.id());
            }
            OmitExpr(_) | DebuggerStmt(_) => {}
            ParenExpr(n) => {
                self.bind(n.expr.id());
            }
            CondExpr(n) => {
                self.bind(n.cond.id());
                self.bind(n.when_true.id());
                self.bind(n.when_false.id());
            }
            CallExpr(n) => {
                self.bind(n.expr.id());
                self.bind_type_arguments(n.ty_args);
                for arg in n.args {
                    self.bind(arg.id());
                }
            }
            FnExpr(n) => self.bind_fn_expr_children(n),
            ClassExpr(n) => self.bind_class_expr_children(n),
            NewExpr(n) => self.bind_new_expr_children(n),
            AssignExpr(n) => {
                self.bind(n.left.id());
                self.bind(n.right.id());
            }
            ArrowFnExpr(n) => self.bind_arrow_fn_expr_children(n),
            PrefixUnaryExpr(n) => {
                self.bind(n.expr.id());
            }
            PostfixUnaryExpr(n) => {
                self.bind(n.expr.id());
            }
            PropAccessExpr(n) => self.bind_property_access_expr_children(n),
            EleAccessExpr(n) => self.bind_element_access_expr_children(n),
            ThisExpr(_) => {}
            TypeofExpr(n) => {
                self.bind(n.expr.id());
            }
            VoidExpr(n) => {
                self.bind(n.expr.id());
            }
            AwaitExpr(n) => {
                self.bind(n.expr.id());
            }
            YieldExpr(n) => self.bind_yield_expr_children(n),
            SuperExpr(_) => {}
            AsExpr(n) => self.bind_as_expr_children(n),
            TyAssertionExpr(n) => self.bind_ty_assertion_expr_children(n),
            SatisfiesExpr(n) => self.bind_satisfies_expr_children(n),
            NonNullExpr(n) => {
                self.bind(n.expr.id());
            }
            TemplateExpr(n) => self.bind_template_expr_children(n),
            TaggedTemplateExpr(n) => self.bind_tagged_template_expr_children(n),
            DeleteExpr(n) => {
                self.bind(n.expr.id());
            }
            ImportExpression(_) => {}
            NumLit(_)
            | BigIntLit(_)
            | BoolLit(_)
            | NullLit(_)
            | RegExpLit(_)
            | StringLit(_)
            | NoSubstitutionTemplateLit(_)
            | Ident(_)
            | PrivateIdent(_)
            | LitTy(_)
            | IntrinsicTy(_)
            | ThisTy(_)
            | JsxText(_)
            | JsxOpeningFrag(_)
            | JsxClosingFrag(_) => {}
            ArrayLit(n) => {
                for elem in n.elems {
                    self.bind(elem.id());
                }
            }
            ObjectLit(n) => {
                for member in n.members {
                    self.bind(member.id());
                }
            }
            ComputedPropName(n) => {
                self.bind(n.expr.id());
            }
            ExprWithTyArgs(n) => self.bind_expr_with_ty_args_children(n),
            NewMetaProperty(n) => {
                self.bind(n.name.id);
            }
            ReferTy(n) => self.bind_refer_ty_children(n),
            ArrayTy(n) => {
                self.bind(n.ele.id());
            }
            ImportType(n) => self.bind_import_type_children(n),
            IndexedAccessTy(n) => self.bind_indexed_access_ty_children(n),
            FnTy(n) => self.bind_fn_ty_children(n),
            CtorTy(n) => self.bind_ctor_ty_children(n),
            ObjectLitTy(n) => self.bind_object_lit_ty_children(n),
            TyParam(n) => self.bind_ty_param_children(n),
            IndexSigDecl(n) => self.bind_index_sig_decl_children(n),
            CallSigDecl(n) => self.bind_call_sig_decl_children(n),
            CtorSigDecl(n) => self.bind_ctor_sig_decl_children(n),
            PropSignature(n) => self.bind_prop_signature_children(n),
            MethodSignature(n) => self.bind_method_signature_children(n),
            RestTy(n) => {
                self.bind(n.ty.id());
            }
            OptionalTy(n) => {
                self.bind(n.ty.id());
            }
            NamedTupleTy(n) => self.bind_named_tuple_ty_children(n),
            TupleTy(n) => self.bind_tuple_ty_children(n),
            CondTy(n) => self.bind_cond_ty_children(n),
            IntersectionTy(n) => self.bind_intersection_ty_children(n),
            UnionTy(n) => self.bind_union_ty_children(n),
            TypeofTy(n) => self.bind_typeof_ty_children(n),
            MappedTy(n) => self.bind_mapped_ty_children(n),
            TyOp(n) => {
                self.bind(n.ty.id());
            }
            PredTy(n) => self.bind_pred_ty_children(n),
            ParenTy(n) => {
                self.bind(n.ty.id());
            }
            InferTy(n) => {
                self.bind(n.ty_param.id);
            }
            NullableTy(n) => {
                self.bind(n.ty.id());
            }
            TemplateLitTy(n) => self.bind_template_lit_ty_children(n),
            TemplateSpanTy(n) => {
                self.bind(n.ty.id());
            }
            QualifiedName(n) => self.bind_qualified_name_children(n),
            JsxSpreadAttr(n) => {
                self.bind(n.expr.id());
            }
            JsxNsName(n) => self.bind_jsx_ns_name_children(n),
            JsxNamedAttr(n) => self.bind_jsx_named_attr_children(n),
            JsxExpr(n) => self.bind_jsx_expr_children(n),
            JsxOpeningElem(n) => self.bind_jsx_opening_elem_children(n),
            JsxClosingElem(n) => {
                self.bind(n.tag_name.id());
            }
            JsxSelfClosingElem(n) => self.bind_jsx_self_closing_elem_children(n),
            JsxFrag(n) => self.bind_jsx_frag_children(n),
            JsxElem(n) => self.bind_jsx_elem_children(n),
        }
    }

    fn bind_class_extends_clause_children(&mut self, n: &'cx ast::ClassExtendsClause<'cx>) {
        self.bind(n.expr_with_ty_args.id);
    }

    fn bind_import_shorthand_spec_children(&mut self, n: &'cx ast::ImportShorthandSpec<'cx>) {
        self.bind(n.name.id);
    }

    fn bind_export_shorthand_spec_children(&mut self, n: &'cx ast::ExportShorthandSpec<'cx>) {
        self.bind(n.name.id);
    }

    fn bind_ns_import_children(&mut self, n: &'cx ast::NsImport<'cx>) {
        self.bind(n.name.id);
    }

    fn bind_ns_export_children(&mut self, n: &'cx ast::NsExport<'cx>) {
        self.bind_module_export_name(n.name);
        self.bind(n.module.id);
    }

    fn bind_glob_export_children(&mut self, n: &'cx ast::GlobExport<'cx>) {
        self.bind(n.module.id);
    }

    fn bind_specs_export_children(&mut self, n: &'cx ast::SpecsExport<'cx>) {
        for spec in n.list {
            self.bind_export_spec(spec);
        }
        if let Some(module) = n.module {
            self.bind(module.id);
        }
    }

    fn bind_import_named_spec_children(&mut self, n: &'cx ast::ImportNamedSpec<'cx>) {
        self.bind_module_export_name(n.prop_name);
        self.bind(n.name.id);
    }

    fn bind_export_named_spec_children(&mut self, n: &'cx ast::ExportNamedSpec<'cx>) {
        self.bind_module_export_name(n.prop_name);
        self.bind_module_export_name(n.name);
    }

    fn bind_import_clause_children(&mut self, n: &'cx ast::ImportClause<'cx>) {
        if let Some(name) = n.name {
            self.bind(name.id);
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

    fn bind_nested_module_decl_children(&mut self, n: &'cx ast::NestedModuleDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        self.bind(n.name.id);
        match n.block {
            ast::NestedModuleBlock::Nested(n) => self.bind(n.id),
            ast::NestedModuleBlock::Block(n) => self.bind(n.id),
        }
    }

    fn bind_block_module_decl_children(&mut self, n: &'cx ast::BlockModuleDecl<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        match n.name {
            ast::ModuleName::Ident(n) => self.bind(n.id),
            ast::ModuleName::StringLit(n) => self.bind(n.id),
        }
        if let Some(block) = n.block {
            self.bind(block.id);
        }
    }

    fn bind_class_static_block_decl_children(&mut self, n: &'cx ast::ClassStaticBlockDecl<'cx>) {
        self.bind_block_stmt_children(n.body);
    }

    fn bind_class_prop_elem_children(&mut self, n: &'cx ast::ClassPropElem<'cx>) {
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

    fn bind_class_method_elem_children(&mut self, n: &'cx ast::ClassMethodElem<'cx>) {
        if let Some(mods) = n.modifiers {
            self.bind_modifiers(mods);
        }
        self.bind_prop_name(n.name);
        self.bind_type_parameters(n.ty_params);
        self.bind_params(n.params);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(body) = n.body {
            self.bind_block_stmt_children(body);
        }
    }

    fn bind_class_ctor_children(&mut self, n: &'cx ast::ClassCtor<'cx>) {
        self.bind_params(n.params);
        if let Some(ret) = n.ret {
            self.bind(ret.id());
        }
        if let Some(body) = n.body {
            self.bind_block_stmt_children(body);
        }
    }

    fn bind_children_worker(&mut self, node: ast::NodeID, save_in_assignment_pattern: bool) {
        use ast::Node::*;
        let n = self.p.node(node);
        match n {
            VarDecl(n) => self.bind_var_decl_flow(n),
            WhileStmt(n) => self.bind_while_stmt(n),
            DoWhileStmt(n) => self.bind_do_stmt(n),
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
            BinExpr(n) => self.bind_bin_expr_flow(n),
            CondExpr(n) => self.bind_cond_expr_flow(n),
            PropAccessExpr(n) => {
                let node_id = n.id;
                if self.node_query().is_optional_chain(node_id) {
                    self.bind_optional_chain_flow(node_id);
                } else {
                    self.bind_property_access_expr_children(n);
                }
            }
            EleAccessExpr(n) => {
                let node_id = n.id;
                if self.node_query().is_optional_chain(node_id) {
                    self.bind_optional_chain_flow(node_id);
                } else {
                    self.bind_element_access_expr_children(n);
                }
            }
            CallExpr(n) => self.bind_call_expr_flow(n),
            ImportExpression(_) => {}
            NonNullExpr(n) => self.bind_non_null_expr_flow(n),
            Program(n) => {
                self.bind_stmts_under(node, n.stmts());
            }
            BlockStmt(ast::BlockStmt { stmts, .. })
            | ModuleBlock(ast::ModuleBlock { stmts, .. }) => {
                self.bind_stmts_under(node, stmts);
            }
            ObjectBindingElem(n) => {
                self.bind_object_binding_elem_flow(n);
            }
            ArrayBinding(n) => {
                self.bind_array_binding_flow(n);
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
            VarStmt(n) => self.bind_var_stmt_children(n),
            FnDecl(n) => self.bind_fn_declaration_children(n),
            EmptyStmt(_) => {}
            ClassDecl(n) => self.bind_class_declaration_children(n),
            NestedModuleDecl(n) => self.bind_nested_module_decl_children(n),
            BlockModuleDecl(n) => self.bind_block_module_decl_children(n),
            ClassCtor(n) => self.bind_class_ctor_children(n),
            ClassPropElem(n) => self.bind_class_prop_elem_children(n),
            ClassMethodElem(n) => self.bind_class_method_elem_children(n),
            GetterDecl(n) => self.bind_getter_decl_children(n),
            SetterDecl(n) => self.bind_setter_decl_children(n),
            InterfaceDecl(n) => self.bind_interface_decl_children(n),
            TypeAliasDecl(n) => self.bind_type_alias_decl_children(n),
            InterfaceExtendsClause(n) => self.bind_interface_extends_clause_children(n),
            ClassExtendsClause(n) => self.bind_class_extends_clause_children(n),
            ClassImplementsClause(n) => self.bind_class_implements_clause_children(n),
            NsImport(n) => self.bind_ns_import_children(n),
            NsExport(n) => self.bind_ns_export_children(n),
            GlobExport(n) => self.bind_glob_export_children(n),
            SpecsExport(n) => self.bind_specs_export_children(n),
            ImportNamedSpec(n) => self.bind_import_named_spec_children(n),
            ImportClause(n) => self.bind_import_clause_children(n),
            ImportDecl(n) => self.bind_import_decl_children(n),
            ExportDecl(n) => {
                self.bind_export_clause(n.clause);
            }
            CatchClause(n) => self.bind_catch_clause_children(n),
            ObjectPat(n) => self.bind_object_pat_children(n),
            ArrayPat(n) => self.bind_array_pat_children(n),
            OmitExpr(_) => {}
            ParenExpr(n) => {
                self.bind(n.expr.id());
            }
            EnumDecl(n) => self.bind_enum_decl_children(n),
            EnumMember(n) => self.bind_enum_member_children(n),
            ObjectShorthandMember(n) => self.bind_object_shorthand_member_children(n),
            ObjectPropAssignment(n) => {
                self.in_assignment_pattern = save_in_assignment_pattern;
                self.bind_object_prop_assignment_children(n);
            }
            ObjectMethodMember(n) => self.bind_object_method_member_children(n),
            SpreadAssignment(n) => {
                self.in_assignment_pattern = save_in_assignment_pattern;
                self.bind_spread_assignment_children(n);
            }
            FnExpr(n) => self.bind_fn_expr_children(n),
            ClassExpr(n) => self.bind_class_expr_children(n),
            NewExpr(n) => self.bind_new_expr_children(n),
            AssignExpr(n) => {
                self.bind(n.left.id());
                self.bind(n.right.id());
                if !self.is_assignment_target(n.id) {
                    self.bind_assignment_target_flow(n.left);
                    if n.op == ast::AssignOp::Eq
                        && let ast::ExprKind::EleAccess(left) = n.left.kind
                        && self.is_narrowable_operand(left.expr)
                    {
                        let f = self.create_flow_array_mutation(
                            self.current_flow.unwrap(),
                            FlowArrayMutationNode::AssignmentExpression(n),
                        );
                        self.current_flow = Some(f);
                    }
                }
            }
            ArrowFnExpr(n) => self.bind_arrow_fn_expr_children(n),
            TypeofExpr(n) => {
                self.bind(n.expr.id());
            }
            VoidExpr(n) => {
                self.bind(n.expr.id());
            }
            SuperExpr(_) => {}
            QualifiedName(n) => self.bind_qualified_name_children(n),
            AsExpr(n) => self.bind_as_expr_children(n),
            TyAssertionExpr(n) => self.bind_ty_assertion_expr_children(n),
            SatisfiesExpr(n) => self.bind_satisfies_expr_children(n),
            TemplateExpr(n) => self.bind_template_expr_children(n),
            TemplateHead(_) => {}
            TemplateSpan(n) => self.bind_template_span_children(n),
            ComputedPropName(n) => {
                self.bind(n.expr.id());
            }
            LitTy(_) => {}
            ReferTy(n) => self.bind_refer_ty_children(n),
            ArrayTy(n) => {
                self.bind(n.ele.id());
            }
            IndexedAccessTy(n) => self.bind_indexed_access_ty_children(n),
            FnTy(n) => self.bind_fn_ty_children(n),
            CtorTy(n) => self.bind_ctor_ty_children(n),
            ObjectLitTy(n) => self.bind_object_lit_ty_children(n),
            TyParam(n) => self.bind_ty_param_children(n),
            IndexSigDecl(n) => self.bind_index_sig_decl_children(n),
            CallSigDecl(n) => self.bind_call_sig_decl_children(n),
            CtorSigDecl(n) => self.bind_ctor_sig_decl_children(n),
            PropSignature(n) => self.bind_prop_signature_children(n),
            MethodSignature(n) => self.bind_method_signature_children(n),
            RestTy(n) => {
                self.bind(n.ty.id());
            }
            NamedTupleTy(n) => self.bind_named_tuple_ty_children(n),
            TupleTy(n) => self.bind_tuple_ty_children(n),
            CondTy(n) => self.bind_cond_ty_children(n),
            IntersectionTy(n) => self.bind_intersection_ty_children(n),
            UnionTy(n) => self.bind_union_ty_children(n),
            TypeofTy(n) => self.bind_typeof_ty_children(n),
            MappedTy(n) => self.bind_mapped_ty_children(n),
            TyOp(n) => {
                self.bind(n.ty.id());
            }
            PredTy(n) => self.bind_pred_ty_children(n),
            ParenTy(n) => {
                self.bind(n.ty.id());
            }
            InferTy(n) => {
                self.bind(n.ty_param.id);
            }
            NullableTy(n) => {
                self.bind(n.ty.id());
            }
            TemplateLitTy(n) => self.bind_template_lit_ty_children(n),
            TemplateSpanTy(n) => {
                self.bind(n.ty.id());
            }
            ImportShorthandSpec(n) => self.bind_import_shorthand_spec_children(n),
            ExportShorthandSpec(n) => self.bind_export_shorthand_spec_children(n),
            ExportNamedSpec(n) => self.bind_export_named_spec_children(n),
            ExportAssign(n) => {
                self.bind(n.expr.id());
            }
            ExprWithTyArgs(n) => self.bind_expr_with_ty_args_children(n),
            SpreadElement(n) => self.bind_spread_element_children(n),
            TaggedTemplateExpr(n) => self.bind_tagged_template_expr_children(n),
            LabeledStmt(n) => self.bind_labeled_stmt_children(n),
            NullLit(_)
            | StringLit(_)
            | NoSubstitutionTemplateLit(_)
            | NumLit(_)
            | Ident(_)
            | ThisExpr(_)
            | BigIntLit(_)
            | BoolLit(_)
            | RegExpLit(_)
            | IntrinsicTy(_)
            | Modifier(_)
            | DebuggerStmt(_)
            | ThisTy(_)
            | JsxText(_)
            | JsxOpeningFrag(_)
            | JsxClosingFrag(_) => {}
            JsxSpreadAttr(n) => {
                self.bind(n.expr.id());
            }
            JsxNsName(n) => self.bind_jsx_ns_name_children(n),
            JsxNamedAttr(n) => self.bind_jsx_named_attr_children(n),
            JsxExpr(n) => self.bind_jsx_expr_children(n),
            JsxOpeningElem(n) => self.bind_jsx_opening_elem_children(n),
            JsxClosingElem(n) => {
                self.bind(n.tag_name.id());
            }
            JsxSelfClosingElem(n) => self.bind_jsx_self_closing_elem_children(n),
            JsxFrag(n) => self.bind_jsx_frag_children(n),
            JsxElem(n) => self.bind_jsx_elem_children(n),
            ClassStaticBlockDecl(n) => self.bind_class_static_block_decl_children(n),
            CaseClause(n) => self.bind_case_clause_flow(n),
            DefaultClause(n) => self.bind_default_clause_children(n),
            SwitchStmt(n) => self.bind_switch_stmt_flow(n),
            CaseBlock(n) => self.bind_case_block(n),
            DeleteExpr(n) => {
                self.bind(n.expr.id());
            }
            AwaitExpr(n) => {
                self.bind(n.expr.id());
            }
            YieldExpr(n) => self.bind_yield_expr_children(n),
            PrivateIdent(_) => {
                // TODO:
            }
            ImportEqualsDecl(n) => self.bind_import_equals_decl_children(n),
            ExternalModuleReference(n) => {
                self.bind(n.module_spec().id);
            }
            ImportType(n) => self.bind_import_type_children(n),
            ClassSemiElem(_) => {}
            NewMetaProperty(n) => {
                self.bind(n.name.id);
            }
            OptionalTy(n) => {
                self.bind(n.ty.id());
            }
        }
    }

    fn bind_type_arguments(&mut self, ty_args: Option<&'cx ast::Tys<'cx>>) {
        if let Some(ty_args) = ty_args {
            for ty in ty_args.list {
                self.bind(ty.id());
            }
        }
    }

    fn bind_case_clause_flow(&mut self, n: &'cx ast::CaseClause<'cx>) {
        let saved_current_flow = self.current_flow;
        debug_assert!(self.pre_switch_case_flow.is_some());
        self.current_flow = self.pre_switch_case_flow;
        self.bind(n.expr.id());
        self.current_flow = saved_current_flow;
        for stmt in n.stmts {
            self.bind(stmt.id());
        }
    }

    fn is_assignment_target(&self, n: ast::NodeID) -> bool {
        self.node_query().is_assignment_target(n)
    }

    fn bind_destructuring_target_flow(&mut self, n: &'cx ast::Expr<'cx>) {
        if let ast::ExprKind::Assign(n) = n.kind {
            self.bind_assignment_target_flow(n.left);
        } else {
            self.bind_assignment_target_flow(n);
        }
    }

    pub(super) fn bind_assignment_target_flow(&mut self, n: &'cx ast::Expr<'cx>) {
        if self.is_narrowable_reference(n) {
            self.current_flow = Some(self.create_flow_assign(self.current_flow.unwrap(), n.id()));
        } else {
            match n.kind {
                ast::ExprKind::ArrayLit(n) => {
                    for elem in n.elems {
                        match elem.kind {
                            ast::ExprKind::SpreadElement(e) => {
                                self.bind_assignment_target_flow(e.expr);
                            }
                            _ => self.bind_destructuring_target_flow(elem),
                        }
                    }
                }
                ast::ExprKind::ObjectLit(n) => {
                    for member in n.members {
                        match member.kind {
                            ast::ObjectMemberKind::Shorthand(e) => {
                                self.current_flow =
                                    Some(self.create_flow_assign(self.current_flow.unwrap(), e.id));
                            }
                            ast::ObjectMemberKind::PropAssignment(e) => {
                                self.bind_destructuring_target_flow(e.init);
                            }
                            ast::ObjectMemberKind::SpreadAssignment(e) => {
                                self.bind_assignment_target_flow(e.expr);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn bind_case_block(&mut self, n: &'cx ast::CaseBlock<'cx>) {
        let p = self.parent_map.parent(n.id).unwrap();
        let p = self.p.node(p).expect_switch_stmt();
        let is_narrowing_switch = matches!(p.expr.kind, ast::ExprKind::BoolLit(lit) if lit.val)
            || self.is_narrowable_expression(p.expr);
        let mut fallthrough_flow = self.unreachable_flow_node;

        let mut i = 0;
        while i < n.clauses.len() {
            let clause_start = i;
            while n.clauses[i].stmts().is_empty() && i + 1 < n.clauses.len() {
                if fallthrough_flow == self.unreachable_flow_node {
                    debug_assert!(self.pre_switch_case_flow.is_some());
                    self.current_flow = self.pre_switch_case_flow;
                }
                self.bind(n.clauses[i].id());
                i += 1;
            }
            let prev_case_label = self.flow_nodes.create_branch_label();
            let antecedent = if is_narrowing_switch {
                self.create_flow_switch_clause(
                    self.pre_switch_case_flow.unwrap(),
                    p,
                    clause_start as u8,
                    (i + 1) as u8,
                )
            } else {
                self.pre_switch_case_flow.unwrap()
            };
            self.flow_nodes.add_antecedent(prev_case_label, antecedent);
            self.flow_nodes
                .add_antecedent(prev_case_label, fallthrough_flow);
            self.current_flow = Some(self.finish_flow_label(prev_case_label));
            self.bind_case_or_default_clause_children(&n.clauses[i]);
            fallthrough_flow = self.current_flow.unwrap();
            if !self
                .flow_nodes
                .get_flow_node(self.current_flow.unwrap())
                .flags
                .contains(FlowFlags::UNREACHABLE)
                && i != n.clauses.len() - 1
                && self
                    .compiler_options
                    .compiler_options()
                    .no_fallthrough_cases_in_switch()
            {
                // TODO: fallthrough_flow_node
            }
            i += 1;
        }
    }

    fn bind_switch_stmt_flow(&mut self, n: &'cx ast::SwitchStmt<'cx>) {
        let post_switch_label = self.flow_nodes.create_branch_label();
        self.bind(n.expr.id());
        let save_break_target = self.current_break_target;
        let save_pre_switch_case_flow = self.pre_switch_case_flow;
        self.current_break_target = Some(post_switch_label);
        self.pre_switch_case_flow = self.current_flow;
        self.bind(n.case_block.id);
        self.flow_nodes
            .add_antecedent(post_switch_label, self.current_flow.unwrap());
        let has_default = n
            .case_block
            .clauses
            .iter()
            .any(|clause| matches!(clause, ast::CaseOrDefaultClause::Default(_)));
        // TODO: possibly exhaustive
        if !has_default {
            let antecedent =
                self.create_flow_switch_clause(self.pre_switch_case_flow.unwrap(), n, 0, 0);
            self.flow_nodes
                .add_antecedent(post_switch_label, antecedent);
        }
        self.current_break_target = save_break_target;
        self.pre_switch_case_flow = save_pre_switch_case_flow;
        self.current_flow = Some(self.finish_flow_label(post_switch_label));
    }

    fn bind_parameter_declaration(&mut self, n: &'cx ast::ParamDecl<'cx>) {
        self.bind_binding(n.name);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(init) = n.init {
            self.bind(init.id());
        }
    }

    fn bind_param_flow(&mut self, n: &'cx ast::ParamDecl<'cx>) {
        self.bind_binding(n.name);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        self.bind_initializer(n.init);
    }

    fn bind_initializer(&mut self, init: Option<&'cx ast::Expr<'cx>>) {
        let Some(init) = init else { return };
        let entry_flow = self.current_flow;
        self.bind(init.id());
        if entry_flow == Some(self.unreachable_flow_node) || entry_flow == self.current_flow {
            return;
        }
        let exit_flow = self.flow_nodes.create_branch_label();
        self.flow_nodes
            .add_antecedent(exit_flow, entry_flow.unwrap());
        self.flow_nodes
            .add_antecedent(exit_flow, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(exit_flow));
    }

    fn bind_object_binding_elem_flow(&mut self, n: &ast::ObjectBindingElem<'cx>) {
        self.bind_object_binding_name(n.name);
        self.bind_initializer(n.init());
    }

    fn bind_array_binding_flow(&mut self, n: &ast::ArrayBinding<'cx>) {
        self.bind_binding(n.name);
        self.bind_initializer(n.init);
    }

    fn bind_non_null_expr_flow(&mut self, n: &ast::NonNullExpr<'cx>) {
        if self.node_query().is_optional_chain(n.id) {
            self.bind_optional_chain_flow(n.id);
        } else {
            self.bind(n.expr.id());
        }
    }

    fn bind_call_expr_flow(&mut self, n: &'cx ast::CallExpr<'cx>) {
        if self.node_query().is_optional_chain(n.id) {
            self.bind_optional_chain_flow(n.id);
        } else {
            let expr = bolt_ts_ast::Expr::skip_parens(n.expr);
            if matches!(expr.kind, ast::ExprKind::Fn(_) | ast::ExprKind::ArrowFn(_)) {
                self.bind_type_arguments(n.ty_args);
                for arg in n.args {
                    self.bind(arg.id());
                }
                self.bind(n.expr.id());
            } else {
                self.bind(n.expr.id());
                self.bind_type_arguments(n.ty_args);
                for arg in n.args {
                    self.bind(arg.id());
                }
                if matches!(n.expr.kind, ast::ExprKind::Super(_)) {
                    let c = self.create_flow_call(self.current_flow.unwrap(), n);
                    self.current_flow = Some(c);
                }
            }
        }

        if let ast::ExprKind::PropAccess(property_access) = n.expr.kind
            && self.is_narrowable_operand(property_access.expr)
            && is_push_or_unshift(property_access.name.name)
        {
            // TODO: only handler identifier
            let f = self.create_flow_array_mutation(
                self.current_flow.unwrap(),
                FlowArrayMutationNode::CallExpression(n),
            );
            self.current_flow = Some(f);
        }
    }

    fn bind_initialized_var_flow(&mut self, node: ast::NodeID, binding: &ast::Binding) {
        use ast::BindingKind::*;
        match binding.kind {
            Ident(_) => {
                let flow = self.create_flow_assign(self.current_flow.unwrap(), node);
                self.current_flow = Some(flow);
            }
            ObjectPat(pat) => {
                for elem in pat.elems {
                    match elem.name {
                        ast::ObjectBindingName::Shorthand(_) => {
                            let flow = self.create_flow_assign(self.current_flow.unwrap(), elem.id);
                            self.current_flow = Some(flow);
                        }
                        ast::ObjectBindingName::Prop { name, .. } => {
                            self.bind_initialized_var_flow(elem.id, name);
                        }
                    };
                }
            }
            ArrayPat(pat) => {
                for elem in pat.elems {
                    use ast::ArrayBindingElemKind::*;
                    if let Binding(b) = elem.kind {
                        self.bind_initialized_var_flow(b.id, b.name)
                    }
                }
            }
        }
    }

    fn bind_variable_declaration(&mut self, n: &'cx ast::VarDecl<'cx>) {
        self.bind_binding(n.name);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(init) = n.init {
            self.bind(init.id());
        }
    }

    fn bind_var_decl_flow(&mut self, n: &'cx ast::VarDecl<'cx>) {
        self.bind_variable_declaration(n);

        if n.init.is_some()
            || matches!(
                self.p.node(self.parent_map.parent(n.id).unwrap()),
                ast::Node::ForInStmt(_) | ast::Node::ForOfStmt(_)
            )
        {
            self.bind_initialized_var_flow(n.id, n.name);
        }
    }

    fn is_top_level_logical_expr(&self, mut n: ast::NodeID) -> bool {
        let p = &self.p;
        debug_assert!(match p.node(n) {
            ast::Node::BinExpr(bin) => bin.op.kind.is_logical_or_coalescing_op(),
            _ => p.node_flags(n).contains(ast::NodeFlags::OPTIONAL_CHAIN),
        });
        let mut parent = self.parent_map.parent(n).unwrap();
        let mut parent_node = p.node(parent);
        while parent_node.is_paren_expr()
            || parent_node
                .as_prefix_unary_expr()
                .is_some_and(|n| n.op == ast::PrefixUnaryOp::Excl)
        {
            n = parent;
            parent = self.parent_map.parent(n).unwrap();
            parent_node = p.node(parent);
        }

        debug_assert!(parent == self.parent_map.parent(n).unwrap());
        // if it's statement condition, then return false
        match parent_node {
            ast::Node::IfStmt(node) if node.expr.id() == n => return false,
            ast::Node::WhileStmt(node) if node.expr.id() == n => return false,
            ast::Node::DoWhileStmt(node) if node.expr.id() == n => return false,
            ast::Node::ForStmt(node) if node.cond.is_some_and(|cond| cond.id() == n) => {
                return false;
            }
            ast::Node::CondExpr(node) if node.cond.id() == n => return false,
            _ => {}
        }

        if parent_node
            .as_bin_expr()
            .is_some_and(|expr| expr.op.kind.is_logical_or_coalescing_op())
        {
            return false;
        }

        !(self.node_query().is_optional_chain(parent)
            && match parent_node {
                ast::Node::PropAccessExpr(node) => node.expr.id() == n,
                ast::Node::EleAccessExpr(node) => node.expr.id() == n,
                ast::Node::CallExpr(node) => node.expr.id() == n,
                ast::Node::NonNullExpr(node) => node.expr.id() == n,
                _ => unreachable!(),
            })
    }

    fn bind_bin_expr_flow(&mut self, n: &'cx ast::BinExpr<'cx>) {
        fn bind_logical_expr<'cx, const IS_LOGICAL_AND: bool>(
            this: &mut crate::BinderState<'cx, '_, '_>,
            node: &'cx ast::BinExpr<'cx>,
            true_target: FlowID,
            false_target: FlowID,
        ) {
            let pre_right_label = this.flow_nodes.create_branch_label();
            if IS_LOGICAL_AND {
                this.bind_cond(Some(node.left), pre_right_label, false_target);
            } else {
                this.bind_cond(Some(node.left), true_target, pre_right_label);
            }
            this.current_flow = Some(this.finish_flow_label(pre_right_label));
            this.bind_cond(Some(node.right), true_target, false_target);
        }

        fn bind_top_level_logical_expr<'cx, const IS_LOGICAL_AND: bool>(
            this: &mut crate::BinderState<'cx, '_, '_>,
            node: &'cx ast::BinExpr<'cx>,
        ) {
            let post_expr_label = this.flow_nodes.create_branch_label();
            let saved_current_flow = this.current_flow;
            let save_has_flow_effects = this.has_flow_effects;
            this.has_flow_effects = false;
            bind_logical_expr::<IS_LOGICAL_AND>(this, node, post_expr_label, post_expr_label);
            this.current_flow = if this.has_flow_effects {
                Some(this.finish_flow_label(post_expr_label))
            } else {
                saved_current_flow
            };
            this.has_flow_effects |= save_has_flow_effects;
        }

        let op_is_comma = match n.op.kind {
            BinOpKind::LogicalAnd => {
                if self.is_top_level_logical_expr(n.id) {
                    bind_top_level_logical_expr::<true>(self, n);
                } else {
                    bind_logical_expr::<true>(
                        self,
                        n,
                        self.current_true_target.unwrap(),
                        self.current_false_target.unwrap(),
                    );
                }
                return;
            }
            BinOpKind::LogicalOr => {
                if self.is_top_level_logical_expr(n.id) {
                    bind_top_level_logical_expr::<false>(self, n);
                } else {
                    bind_logical_expr::<false>(
                        self,
                        n,
                        self.current_true_target.unwrap(),
                        self.current_false_target.unwrap(),
                    );
                }
                return;
            }
            BinOpKind::Nullish => {
                if self.is_top_level_logical_expr(n.id) {
                    bind_top_level_logical_expr::<false>(self, n);
                } else {
                    bind_logical_expr::<false>(
                        self,
                        n,
                        self.current_true_target.unwrap(),
                        self.current_false_target.unwrap(),
                    );
                }
                return;
            }
            BinOpKind::Comma => true,
            _ => false,
        };

        let maybe_bind = |this: &mut Self, node: &'cx ast::Expr<'cx>| {
            // if node.is_bin_expr() && !node.is_destructing_assignment() {
            //     return;
            // } else {
            this.bind(node.id());
            // }
        };
        maybe_bind(self, n.left);
        if op_is_comma {
            self.maybe_bind_expr_flow_if_call(n.left);
        }
        maybe_bind(self, n.right);
        if op_is_comma {
            self.maybe_bind_expr_flow_if_call(n.right);
        }
    }

    fn bind_postfix_unary_expr_flow(&mut self, n: &ast::PostfixUnaryExpr<'cx>) {
        self.bind(n.expr.id());
    }

    fn bind_prefix_unary_expr_flow(&mut self, n: &ast::PrefixUnaryExpr<'cx>) {
        self.bind(n.expr.id());
    }

    fn bind_expr_stmt(&mut self, n: &ast::ExprStmt<'cx>) {
        self.bind(n.expr.id());
        self.maybe_bind_expr_flow_if_call(n.expr);
    }

    fn maybe_bind_expr_flow_if_call(&mut self, n: &ast::Expr<'cx>) {
        if let ast::ExprKind::Call(call) = n.kind
            && !matches!(call.expr.kind, ast::ExprKind::Super(_))
            && call.expr.is_dotted_name()
        {
            let c = self.create_flow_call(self.current_flow.unwrap(), call);
            self.current_flow = Some(c)
        }
    }

    pub(super) fn set_continue_target(&mut self, target: FlowID) -> FlowID {
        // TODO:
        target
    }

    fn bind_while_stmt(&mut self, n: &ast::WhileStmt<'cx>) {
        let pre_while_label = {
            let label = self.flow_nodes.create_loop_label();
            self.set_continue_target(label)
        };
        let pre_body_label = self.flow_nodes.create_branch_label();
        let post_while_label = self.flow_nodes.create_branch_label();
        self.flow_nodes
            .add_antecedent(pre_while_label, self.current_flow.unwrap());
        self.current_flow = Some(pre_while_label);
        self.bind_cond(Some(n.expr), pre_body_label, post_while_label);
        self.current_flow = Some(self.finish_flow_label(pre_body_label));
        self.bind_iterative_stmt(n.stmt, post_while_label, pre_while_label);
        self.flow_nodes
            .add_antecedent(pre_while_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(post_while_label));
    }

    fn bind_do_stmt(&mut self, n: &ast::DoWhileStmt<'cx>) {
        let pre_do_label = self.flow_nodes.create_loop_label();
        let pre_condition_label = {
            let t = self.flow_nodes.create_loop_label();
            self.set_continue_target(t)
        };
        let post_do_label = self.flow_nodes.create_branch_label();
        self.flow_nodes
            .add_antecedent(pre_do_label, self.current_flow.unwrap());
        self.current_flow = Some(pre_do_label);
        self.bind_iterative_stmt(n.stmt, pre_do_label, pre_condition_label);
        self.flow_nodes
            .add_antecedent(pre_condition_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(pre_condition_label));
        self.bind_cond(Some(n.expr), pre_do_label, post_do_label);
        self.current_flow = Some(self.finish_flow_label(post_do_label));
    }

    fn bind_for_stmt(&mut self, n: &ast::ForStmt<'cx>) {
        let pre_loop_label = {
            let label = self.flow_nodes.create_loop_label();
            self.set_continue_target(label)
        };
        let pre_body_label = self.flow_nodes.create_branch_label();
        let pre_incrementor_label = self.flow_nodes.create_branch_label();
        let post_loop_label = self.flow_nodes.create_branch_label();

        if let Some(init) = &n.init {
            use ast::ForInitKind::*;
            match init {
                Var(list) => {
                    for item in *list {
                        self.bind(item.id);
                    }
                }
                Expr(expr) => self.bind(expr.id()),
            }
        }

        self.flow_nodes
            .add_antecedent(pre_loop_label, self.current_flow.unwrap());

        self.current_flow = Some(pre_loop_label);
        if let Some(cond) = n.cond {
            self.bind_cond(Some(cond), pre_body_label, post_loop_label);
        }
        self.current_flow = Some(self.finish_flow_label(pre_body_label));

        self.bind_iterative_stmt(n.body, post_loop_label, pre_loop_label);
        self.flow_nodes
            .add_antecedent(pre_incrementor_label, self.current_flow.unwrap());

        self.current_flow = Some(self.finish_flow_label(pre_incrementor_label));
        // TODO: delete this?
        if let Some(update) = n.incr {
            self.bind(update.id());
        }
        self.flow_nodes
            .add_antecedent(pre_loop_label, self.current_flow.unwrap());
        self.current_flow = Some(self.finish_flow_label(post_loop_label));
    }

    pub(super) fn bind_iterative_stmt(
        &mut self,
        n: &'cx ast::Stmt<'cx>,
        break_target: FlowID,
        continue_target: FlowID,
    ) {
        let saved_break_target = self.current_break_target;
        let saved_continue_target = self.current_continue_target;
        self.current_break_target = Some(break_target);
        self.current_continue_target = Some(continue_target);
        self.bind(n.id());
        self.current_break_target = saved_break_target;
        self.current_continue_target = saved_continue_target;
    }

    pub(super) fn bind(&mut self, node: ast::NodeID) {
        let saved_in_strict_mode = self.in_strict_mode;
        if let Some(parent) = self.parent {
            self.parent_map.insert(node, parent);
        }

        self.bind_worker(node);

        let save_parent = self.parent;
        self.parent = Some(node);
        let container_flags = container_flags_for_node(self.p, &self.parent_map, node);
        if container_flags.is_empty() {
            self.bind_children(node);
        } else {
            self.bind_container(node, container_flags);
        }
        self.parent = save_parent;

        self.in_strict_mode = saved_in_strict_mode;
    }

    pub(super) fn bind_optional_chain_flow(&mut self, node: ast::NodeID) {
        if self.is_top_level_logical_expr(node) {
            let post_expr_label = self.flow_nodes.create_branch_label();
            let save_current_flow = self.current_flow;
            let save_has_flow_effects = self.has_flow_effects;
            self.bind_optional_chain(node, post_expr_label, post_expr_label);
            self.current_flow = if self.has_flow_effects {
                Some(self.finish_flow_label(post_expr_label))
            } else {
                save_current_flow
            };
            self.has_flow_effects |= save_has_flow_effects;
        } else {
            let t = self.current_true_target.unwrap();
            let f = self.current_false_target.unwrap();
            self.bind_optional_chain(node, t, f);
        }
    }

    fn bind_optional_expr(
        &mut self,
        expr: &'cx ast::Expr<'cx>,
        true_target: FlowID,
        false_target: FlowID,
    ) {
        let expr_id = expr.id();
        self.do_with_cond_branch(
            |this, node| {
                this.bind(node);
            },
            expr_id,
            true_target,
            false_target,
        );
        if !self.node_query().is_optional_chain(expr_id)
            || self.node_query().is_outermost_optional_chain(expr_id)
        {
            let t = self.create_flow_condition(
                FlowFlags::TRUE_CONDITION,
                self.current_flow.unwrap(),
                Some(expr_id),
            );
            self.flow_nodes.add_antecedent(true_target, t);
            let f = self.create_flow_condition(
                FlowFlags::FALSE_CONDITION,
                self.current_flow.unwrap(),
                Some(expr_id),
            );
            self.flow_nodes.add_antecedent(false_target, f);
        }
    }

    fn bind_optional_chain(
        &mut self,
        node: ast::NodeID,
        true_target: FlowID,
        false_target: FlowID,
    ) {
        let pre_chain_label = if self.node_query().is_optional_chain_root(node) {
            Some(self.flow_nodes.create_branch_label())
        } else {
            None
        };
        let expr = match self.p.node(node) {
            ast::Node::PropAccessExpr(n) => n.expr,
            ast::Node::EleAccessExpr(n) => n.expr,
            ast::Node::CallExpr(n) => n.expr,
            _ => unreachable!(),
        };
        self.bind_optional_expr(expr, pre_chain_label.unwrap_or(true_target), false_target);
        if let Some(pre_chain_label) = pre_chain_label {
            self.current_flow = Some(self.finish_flow_label(pre_chain_label));
        }
        self.do_with_cond_branch(
            |this, n| {
                this.bind_optional_chain_rest(n);
            },
            node,
            true_target,
            false_target,
        );
        if self.node_query().is_outermost_optional_chain(node) {
            let t = self.create_flow_condition(
                FlowFlags::TRUE_CONDITION,
                self.current_flow.unwrap(),
                Some(node),
            );
            self.flow_nodes.add_antecedent(true_target, t);
            let f = self.create_flow_condition(
                FlowFlags::FALSE_CONDITION,
                self.current_flow.unwrap(),
                Some(node),
            );
            self.flow_nodes.add_antecedent(false_target, f);
        }
    }

    fn bind_optional_chain_rest(&mut self, node: ast::NodeID) {
        match self.p.node(node) {
            ast::Node::PropAccessExpr(n) => {
                self.bind(n.name.id);
            }
            ast::Node::EleAccessExpr(n) => {
                self.bind(n.arg.id());
            }
            ast::Node::CallExpr(n) => {
                self.bind_type_arguments(n.ty_args);
                for arg in n.args {
                    self.bind(arg.id());
                }
            }
            _ => unreachable!(),
        }
    }
}

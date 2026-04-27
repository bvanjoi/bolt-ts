use super::BinderState;
use super::container_flags::container_flags_for_node;
use super::create::DeclareSymbolProperty;
use super::flow::FlowFlags;
use super::flow::FlowID;
use super::flow::FlowNodeKind;
use super::symbol::SymbolFlags;
use super::symbol::SymbolTableLocation;
use super::symbol::{SymbolID, SymbolName};

use bolt_ts_ast as ast;
use bolt_ts_ast::BinOpKind;
use bolt_ts_ast::NodeFlags;

impl<'cx, 'atoms, 'parser> BinderState<'cx, 'atoms, 'parser> {
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
        n.else_then.map(|else_then| self.bind(else_then.id()));
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
            ast::Node::ModuleDecl(_) => {
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

    pub(super) fn bind_ty_params(&mut self, ty_params: ast::TyParams<'cx>) {
        for ty_param in ty_params {
            self.bind(ty_param.id);
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

    pub(super) fn bind_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
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
        let should_add_antecedent = node.is_none_or(|node| {
            !node.kind.is_logical_assignment()
                && !node.kind.is_logical_expr()
                && !(self.node_query().is_optional_chain(node.id())
                    && self.node_query().is_outermost_optional_chain(node.id()))
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
            ModuleDecl(_) => {
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
        use ast::Node::*;
        let save_in_assignment_pattern = self.in_assignment_pattern;
        self.in_assignment_pattern = true;

        let n = self.p.node(node);
        // if n.ge_first_stmt_and_le_last_stmt() && (n.is_ret_stmt() || )

        match n {
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
            VarDecl(n) => self.bind_var_decl_flow(n),
            PropAccessExpr(n) => {
                let node_id = n.id;
                if self.node_query().is_optional_chain(node_id) {
                    self.bind_optional_chain_flow(node_id);
                } else {
                    self.bind(n.expr.id());
                    self.bind(n.name.id);
                }
            }
            EleAccessExpr(n) => {
                let node_id = n.id;
                if self.node_query().is_optional_chain(node_id) {
                    self.bind_optional_chain_flow(node_id);
                } else {
                    self.bind(n.expr.id());
                    self.bind(n.arg.id());
                }
            }
            CallExpr(n) => self.bind_call_expr_flow(n),
            NonNullExpr(n) => self.bind_non_null_expr_flow(n),
            Program(n) => {
                self.bind_stmts_under(node, n.stmts());
            }
            BlockStmt(ast::BlockStmt { stmts, .. })
            | ModuleBlock(ast::ModuleBlock { stmts, .. }) => {
                self.bind_stmts_under(node, *stmts);
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
            VarStmt(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
                for item in n.list {
                    self.bind(item.id);
                }
            }
            FnDecl(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
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
                if let Some(body) = n.body {
                    self.bind_block_stmt(body);
                }
            }
            EmptyStmt(_) => {}
            ClassDecl(n) => {
                if let Some(mods) = n.modifiers {
                    self.bind_modifiers(mods);
                }
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
                    for elem in implements.list {
                        self.bind(elem.id);
                    }
                }
                for elem in n.elems.list {
                    self.bind_class_elem(elem);
                }
            }
            ModuleDecl(n) => {
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
            TypeAliasDecl(n) => {
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
                self.bind(n.expr_with_ty_args.id);
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
            ImportDecl(n) => {
                if let Some(clause) = n.clause {
                    self.bind(clause.id);
                }
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
            ArrayPat(n) => {
                for elem in n.elems {
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
                match n.name {
                    ast::EnumMemberNameKind::Ident(ident) => self.bind(ident.id),
                    ast::EnumMemberNameKind::StringLit { raw, .. } => self.bind(raw.id),
                }
                if let Some(init) = n.init {
                    self.bind(init.id());
                }
            }
            ObjectShorthandMember(n) => {
                self.bind(n.name.id);
            }
            ObjectPropAssignment(n) => {
                self.in_assignment_pattern = save_in_assignment_pattern;
                self.bind_prop_name(n.name);
                self.bind(n.init.id());
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
                self.in_assignment_pattern = save_in_assignment_pattern;
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
                for elem in n.elems.list {
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
                if !self.is_assignment_target(n.id) {
                    self.bind_assignment_target_flow(n.left);
                }
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
            SuperExpr(_) => {}
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
                self.bind_binding(n.key);
                self.bind(n.key_ty.id());
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
                match n.name {
                    bolt_ts_ast::PredTyName::Ident(n) => self.bind(n.id),
                    bolt_ts_ast::PredTyName::This(n) => self.bind(n.id),
                }
                if let Some(ty) = n.ty {
                    self.bind(ty.id());
                }
            }
            ParenTy(n) => {
                self.bind(n.ty.id());
            }
            InferTy(n) => {
                self.bind(n.ty_param.id);
            }
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
            ImportShorthandSpec(n) => self.bind(n.name.id),
            ExportShorthandSpec(n) => self.bind(n.name.id),
            ExportNamedSpec(n) => {
                self.bind_module_export_name(n.prop_name);
                self.bind_module_export_name(n.name);
            }
            ExportAssign(n) => {
                self.bind(n.expr.id());
            }
            ExprWithTyArgs(n) => {
                self.bind(n.expr.id());
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
            }
            SpreadElement(n) => {
                self.bind(n.expr.id());
            }
            TaggedTemplateExpr(n) => {
                self.bind(n.tag.id());
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
                self.bind(n.tpl.id());
            }
            LabeledStmt(n) => {
                self.bind(n.label.id);
                self.bind(n.stmt.id());
            }
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
            JsxNsName(n) => {
                self.bind(n.ns.id);
                self.bind(n.name.id);
            }
            JsxNamedAttr(n) => {
                self.bind(n.name.id());
                if let Some(attr_value) = n.init {
                    self.bind(attr_value.id());
                }
            }
            JsxExpr(n) => {
                if let Some(e) = n.expr {
                    self.bind(e.id());
                }
            }
            JsxOpeningElem(n) => {
                self.bind(n.tag_name.id());
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
                for attr in n.attrs {
                    self.bind(attr.id());
                }
            }
            JsxClosingElem(n) => {
                self.bind(n.tag_name.id());
            }
            JsxSelfClosingElem(n) => {
                self.bind(n.tag_name.id());
                if let Some(ty_args) = n.ty_args {
                    for ty in ty_args.list {
                        self.bind(ty.id());
                    }
                }
                for attr in n.attrs {
                    self.bind(attr.id());
                }
            }
            JsxFrag(n) => {
                self.bind(n.opening_frag.id);
                for child in n.children {
                    self.bind(child.id());
                }
                self.bind(n.closing_frag.id);
            }
            JsxElem(n) => {
                self.bind(n.opening_elem.id);
                for child in n.children {
                    self.bind(child.id());
                }
                self.bind(n.closing_elem.id);
            }
            ClassStaticBlockDecl(n) => {
                self.bind(n.body.id);
            }
            CaseClause(n) => self.bind_case_clause(n),
            DefaultClause(n) => {
                for stmt in n.stmts {
                    self.bind(stmt.id());
                }
            }
            SwitchStmt(n) => self.bind_switch_stmt(n),
            CaseBlock(n) => self.bind_case_block(n),
            DeleteExpr(n) => {
                self.bind(n.expr.id());
            }
            AwaitExpr(n) => {
                self.bind(n.expr.id());
            }
            YieldExpr(n) => {
                if let Some(expr) = n.expr {
                    self.bind(expr.id());
                }
            }
            PrivateIdent(_) => {
                // TODO:
            }
            ImportEqualsDecl(n) => {
                self.bind(n.name.id);
                match n.module_reference {
                    ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                        self.bind(n.id());
                    }
                    ast::ModuleReferenceKind::EntityName(n) => self.bind_entity_name(n),
                }
            }
            ExternalModuleReference(n) => {
                self.bind(n.module_spec().id);
            }
            ClassSemiElem(_n) => {}
        }
        // TODO: bind_js_doc
        self.in_assignment_pattern = save_in_assignment_pattern;
    }

    fn bind_case_clause(&mut self, n: &'cx ast::CaseClause<'cx>) {
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

    fn bind_assignment_target_flow(&mut self, n: &'cx ast::Expr<'cx>) {
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

        for i in 0..n.clauses.len() {
            let clause_start = i;
            // while clause.stmts().is_empty() && i + 1 < n.clauses.len() {
            // TODO:
            // }
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
            match n.clauses[i] {
                ast::CaseOrDefaultClause::Case(c) => self.bind(c.id),
                ast::CaseOrDefaultClause::Default(c) => self.bind(c.id),
            }
            fallthrough_flow = self.current_flow.unwrap();
            // TODO: clause.fallthrough_flow = fallthrough_flow;
        }
    }

    fn bind_switch_stmt(&mut self, n: &'cx ast::SwitchStmt<'cx>) {
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

    fn bind_param_flow(&mut self, n: &'cx ast::ParamDecl<'cx>) {
        self.bind_binding(n.name);
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
                self.bind_binding(name);
            }
        }
        if let Some(init) = n.init {
            self.bind(init.id());
        }
    }

    fn bind_array_binding_flow(&mut self, n: &ast::ArrayBinding<'cx>) {
        self.bind_binding(n.name);
        if let Some(init) = n.init {
            self.bind(init.id());
        }
    }

    fn bind_non_null_expr_flow(&mut self, n: &ast::NonNullExpr<'cx>) {
        // TODO: is_optional_chain
        self.bind(n.expr.id());
    }

    fn bind_call_expr_flow(&mut self, n: &'cx ast::CallExpr<'cx>) {
        if self.node_query().is_optional_chain(n.id) {
            self.bind_optional_chain_flow(n.id);
        } else {
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
                self.bind(n.expr.id());
                if let Some(ty_args) = n.ty_args {
                    for ty_arg in ty_args.list {
                        self.bind(ty_arg.id());
                    }
                }
                for arg in n.args {
                    self.bind(arg.id());
                }
                if matches!(n.expr.kind, ast::ExprKind::Super(_)) {
                    let c = self.create_flow_call(self.current_flow.unwrap(), n);
                    self.current_flow = Some(c);
                }
            }
        }

        if let ast::ExprKind::PropAccess(prop_access) = n.expr.kind {
            // TODO: private
            // && let n = prop_access.name
            // && self.is_narrowable_operand(prop_access.expr)
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

    fn bind_var_decl_flow(&mut self, n: &ast::VarDecl<'cx>) {
        self.bind_binding(n.name);
        if let Some(ty) = n.ty {
            self.bind(ty.id());
        }
        if let Some(init) = n.init {
            self.bind(init.id());
        }

        use bolt_ts_ast::Node::*;
        if n.init.is_some()
            || matches!(
                self.p.node(self.parent_map.parent(n.id).unwrap()),
                ForInStmt(_) | ForOfStmt(_)
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

    fn bind_do_stmt(&mut self, _n: &ast::DoWhileStmt<'cx>) {
        // TODO:
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
            // TODO: bind_cond
            self.bind(cond.id());
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

    pub(super) fn bind_anonymous_decl(
        &mut self,
        node: ast::NodeID,
        flags: SymbolFlags,
        name: SymbolName,
    ) -> SymbolID {
        let symbol = self.create_symbol(name, flags);
        if flags.intersects(SymbolFlags::ENUM_MEMBER.union(SymbolFlags::CLASS_MEMBER)) {
            let container = self.final_res[&self.container.unwrap()];
            self.symbols.get_mut(symbol).parent = Some(container);
        }
        self.add_declaration_to_symbol(symbol, node, flags);
        symbol
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
                if let Some(ty_args) = n.ty_args {
                    for ty_arg in ty_args.list {
                        self.bind(ty_arg.id());
                    }
                }
                for arg in n.args {
                    self.bind(arg.id());
                }
            }
            _ => unreachable!(),
        }
    }
}

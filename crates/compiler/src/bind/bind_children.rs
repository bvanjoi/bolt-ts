use crate::bind::create::DeclareSymbolProperty;

use super::BinderState;
use super::NodeQuery;
use super::container_flags::container_flags_for_node;
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
                self.bind(q.id);
            }
        }
    }

    fn bind_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.bind(param.id);
        }
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
            ObjectLitTy(_) | ObjectLit(_) | InterfaceDecl(_) => self.declare_symbol(
                Some(name),
                SymbolTableLocation::members(container),
                None,
                current,
                symbol_flags,
                symbol_excludes,
                DeclareSymbolProperty::empty(),
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
            | TypeAliasDecl(_)
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
                    DeclareSymbolProperty::empty(),
                )
            }
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
        let loc = if self.p.node(node).is_static() {
            SymbolTableLocation::exports(container)
        } else {
            SymbolTableLocation::members(container)
        };
        self.declare_symbol(
            Some(name),
            loc,
            None,
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
            .intersects(ast::ModifierKind::Export);
        if symbol_flags.intersects(SymbolFlags::ALIAS) {
            let n = self.p.node(current);
            let (loc, parent) = if n.is_export_named_spec()
                || n.as_shorthand_spec().is_some_and(|_| {
                    let parent = self.parent_map.parent_unfinished(current).unwrap();
                    self.p.node(parent).is_specs_export()
                }) {
                // TODO: is_import_eq_decl && has_export_modifier
                let table = SymbolTableLocation::exports(container);
                // let parent = self.final_res[&container];
                let parent = None;
                (table, parent)
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
                    .intersects(NodeFlags::EXPORT_CONTEXT))
        {
            if !self.p.node(container).has_locals()
                || !self.locals.contains_key(&container)
                || (current_node.has_syntactic_modifier(ast::ModifierKind::Default.into())
                    && current_node.ident_name().is_none())
            {
                let table = SymbolTableLocation::exports(container);
                return self.declare_symbol(
                    None,
                    table,
                    None,
                    current,
                    symbol_flags,
                    symbol_excludes,
                    DeclareSymbolProperty::IS_DEFAULT_EXPORT,
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
                None,
                current,
                symbol_flags,
                symbol_excludes,
                DeclareSymbolProperty::empty(),
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
                DeclareSymbolProperty::empty(),
            )
        }
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
                self.bind(n.id);
            }
            ArrayPat(n) => {
                self.bind(n.id);
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

    pub(super) fn bind_children(&mut self, node: ast::NodeID) {
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
            BinExpr(n) => self.bind_bin_expr_flow(n),
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
            ArrayBindingElem(n) => {
                self.bind_array_binding_elem_flow(n);
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
                self.bind(n.name.id);
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
                for elem in n.elems.elems {
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
            ArrayPat(n) => {
                for elem in n.elems {
                    self.bind(elem.id);
                }
            }
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
            ShorthandSpec(n) => self.bind(n.name.id),
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
            JsxOpeningEle(n) => {
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
            JsxClosingEle(n) => {
                self.bind(n.tag_name.id());
            }
            JsxSelfClosingEle(n) => {
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
                self.bind(n.opening_ele.id);
                for child in n.children {
                    self.bind(child.id());
                }
                self.bind(n.closing_ele.id);
            }
            JsxEle(n) => {
                self.bind(n.opening_ele.id);
                for child in n.children {
                    self.bind(child.id());
                }
                self.bind(n.closing_ele.id);
            }
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

    fn bind_array_binding_elem_flow(&mut self, n: &ast::ArrayBindingElem<'cx>) {
        match n.kind {
            ast::ArrayBindingElemKind::Omit(e) => {
                self.bind(e.id);
            }
            ast::ArrayBindingElemKind::Binding { name, init, .. } => {
                self.bind(name.id);
                if let Some(init) = init {
                    self.bind(init.id());
                }
            }
        }
    }

    fn bind_non_null_expr_flow(&mut self, n: &ast::NonNullExpr<'cx>) {
        // TODO: is_optional_chain
        self.bind(n.expr.id());
    }

    fn bind_call_expr_flow(&mut self, n: &'cx ast::CallExpr<'cx>) {
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
                let c = self.create_flow_call(self.current_flow.unwrap(), n);
                self.current_flow = Some(c);
            }
        }

        if let ast::ExprKind::PropAccess(_) = n.expr.kind {
            // TODO:
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
                self.p
                    .node(self.parent_map.parent_unfinished(n.id).unwrap()),
                ForInStmt(_) | ForOfStmt(_)
            )
        {
            let flow = self.create_flow_assign(self.current_flow.unwrap(), n.id);
            self.current_flow = Some(flow);
        }
    }

    fn bind_bin_expr_flow(&mut self, n: &ast::BinExpr<'cx>) {
        let op_is_comma = n.op.kind == BinOpKind::Comma;
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
        if let ast::ExprKind::Call(call) = n.kind {
            if !matches!(call.expr.kind, ast::ExprKind::Super(_)) && call.expr.is_dotted_name() {
                let c = self.create_flow_call(self.current_flow.unwrap(), call);
                self.current_flow = Some(c)
            }
        }
    }

    fn bind_while_stmt(&mut self, n: &ast::WhileStmt<'cx>) {
        // TODO:
    }
    fn bind_do_stmt(&mut self, n: &ast::DoStmt<'cx>) {
        // TODO:
    }
    fn bind_for_stmt(&mut self, n: &ast::ForStmt<'cx>) {
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
        if let Some(parent) = self.parent {
            self.parent_map.insert(node, parent);
        }

        self._bind(node);

        let save_parent = self.parent;
        self.parent = Some(node);
        let container_flags = container_flags_for_node(self.p, &self.parent_map, node);
        if container_flags.is_empty() {
            self.bind_children(node);
        } else {
            self.bind_container(node, container_flags);
        }
        self.parent = save_parent;

        self.in_strict_mode = save_in_strict_mode;
    }

    pub(super) fn bind_anonymous_decl(
        &mut self,
        node: ast::NodeID,
        flags: SymbolFlags,
        name: SymbolName,
    ) -> SymbolID {
        let symbol = self.create_symbol(name, flags);
        if flags.intersects(SymbolFlags::ENUM_MEMBER.union(SymbolFlags::CLASS_MEMBER)) {
            // self.symbols.get_mut(symbol).parent = container.symbol
        }
        self.add_declaration_to_symbol(symbol, node, flags);
        symbol
    }
}

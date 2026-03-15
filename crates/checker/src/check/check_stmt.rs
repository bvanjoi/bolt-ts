use bolt_ts_ast::NodeFlags;
use bolt_ts_ast::keyword;
use bolt_ts_binder::ModuleInstanceState;
use bolt_ts_binder::Symbol;
use bolt_ts_binder::SymbolFlags;
use bolt_ts_binder::SymbolName;
use bolt_ts_ty::ObjectFlags;
use bolt_ts_ty::TypeFacts;

use super::TyChecker;
use super::ast;
use super::errors;
use super::symbol_info::SymbolInfo;
use super::ty;
use super::ty::TypeFlags;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_stmt(&mut self, stmt: &'cx ast::Stmt) {
        use bolt_ts_ast::StmtKind::*;
        match stmt.kind {
            Var(node) => self.check_var_stmt(node),
            Expr(node) => {
                self.check_expr(node.expr);
            }
            Fn(node) => self.check_fn_decl(node),
            If(node) => self.check_if_stmt(node),
            Block(node) => self.check_block(node),
            Ret(node) => self.check_ret_stmt(node),
            Class(node) => self.check_class_decl(node),
            Interface(node) => self.check_interface_decl(node),
            Module(node) => self.check_module_decl(node),
            TypeAlias(node) => self.check_type_alias_decl(node),
            For(node) => self.check_for_stmt(node),
            ForIn(node) => self.check_for_in_stmt(node),
            ForOf(node) => self.check_for_of_stmt(node),
            Import(node) => self.check_import_decl(node),
            ImportEquals(node) => {
                self.check_import_equals_decl(node);
            }
            Export(node) => self.check_export_decl(node),
            Enum(node) => self.check_enum_decl(node),
            ExportAssign(_) => {}
            Empty(_) => {}
            Throw(_) => {}
            Break(_) => {}
            Continue(_) => {}
            Try(_) => {}
            While(n) => self.check_while_stmt(n),
            Do(_) => {}
            Debugger(_) => {}
            Switch(n) => self.check_switch_stmt(n),
            Labeled(n) => self.check_stmt(n.stmt),
        };
    }

    fn check_while_stmt(&mut self, node: &'cx ast::WhileStmt<'cx>) {
        self.check_truthiness_expr(node.expr);
        self.check_stmt(node.stmt);
    }

    fn check_switch_stmt(&mut self, node: &'cx ast::SwitchStmt<'cx>) {
        use ast::CaseOrDefaultClause::*;
        let expr_ty = self.check_expr(node.expr);
        let mut first_default_clause = None;
        let mut has_duplicate_default_clause = false;

        for clause in node.case_block.clauses {
            match clause {
                Case(n) => {
                    let case_ty = self.check_expr(n.expr);
                    if !self.is_type_equality_comparable_to(expr_ty, case_ty) {
                        if !self.check_type_comparable_to(case_ty, expr_ty, Some(n.expr.id())) {
                            let error = errors::TypeXIsNotComparableToTypeY {
                                span: n.expr.span(),
                                ty1: case_ty.to_string(self),
                                ty2: expr_ty.to_string(self),
                            };
                            self.push_error(Box::new(error));
                        }
                    }
                }
                Default(n) => {
                    if !has_duplicate_default_clause {
                        match first_default_clause {
                            Some(first_default_clause) => {
                                has_duplicate_default_clause = true;
                                todo!("error handler")
                            }
                            None => {
                                first_default_clause = Some(n);
                            }
                        }
                    }
                }
            }
        }
    }

    fn check_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) {
        for member in node.members {
            self.check_enum_member(member);
        }

        self.compute_enum_member_values(node);

        let enum_symbol = self.get_symbol_of_decl(node.id);
        let s = self.binder.symbol(enum_symbol);
        let Some(first_decl) = s.get_declaration_of_kind(|n| self.p.node(n).is_enum_decl()) else {
            unreachable!()
        };
        if first_decl == node.id
            && let Some(decls) = &s.decls
        {
            // if decls.len() > 1 {
            //     todo!()
            // }

            let mut seen_enum_missing_init = false;
            let mut error_span_list = vec![];
            for decl in decls {
                let Some(enum_decl) = self.p.node(*decl).as_enum_decl() else {
                    continue;
                };
                if enum_decl.members.is_empty() {
                    continue;
                }
                let first_enum_member = enum_decl.members[0];
                if first_enum_member.init.is_none() {
                    if seen_enum_missing_init {
                        error_span_list.push(first_enum_member.name.span());
                    } else {
                        seen_enum_missing_init = true;
                    }
                }
            }

            for error_span in error_span_list {
                let error = errors::InAnEnumWithMultipleDeclarationsOnlyOneDeclarationCanOmitAnInitializerForItsFirstEnumElement {
                    span: error_span,
                };
                self.push_error(Box::new(error));
            }
        }
    }

    fn check_enum_member(&mut self, member: &'cx ast::EnumMember<'cx>) {
        if let Some(init) = member.init {
            self.check_expr(init);
        }
    }

    fn check_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) {
        let has_module_spec = node.module_spec().is_some();
        if (!has_module_spec || self.check_external_module_name(node.id))
            && let ast::ExportClauseKind::Specs(specs) = node.clause.kind
        {
            // export { a, b as c } from 'xxxx'
            // export { a, b as c }
            for spec in specs.list {
                self.check_export_spec(spec, has_module_spec);
            }
        }
    }

    fn check_export_spec(&mut self, spec: &'cx ast::ExportSpec<'cx>, has_module_spec: bool) {
        let id = spec.id();
        self.check_alias_symbol(id);

        if !has_module_spec {
            // export { a }
            // export { b as c }
            let name = match spec.kind {
                ast::ExportSpecKind::Shorthand(spec) => spec.name,
                ast::ExportSpecKind::Named(spec) => match spec.prop_name.kind {
                    ast::ModuleExportNameKind::Ident(name) => name,
                    ast::ModuleExportNameKind::StringLit(_) => {
                        return;
                    }
                },
            };
            let symbol = self.final_res(name.id);

            if symbol == Symbol::UNDEFINED
                || symbol == Symbol::GLOBAL_THIS
                || self
                    .binder
                    .symbol(symbol)
                    .decls
                    .as_ref()
                    .is_some_and(|decls| {
                        let decl = decls[0];
                        let module = decl.module();
                        let c = self.node_query(module).get_declaration_container(decl);
                        debug_assert!(c.module() == module);
                        self.p.get(module).is_global_source_file(c)
                    })
            {
                let error = errors::CannotExportXOnlyLocalDeclarationsCanBeExportedFromAModule {
                    span: name.span,
                    spec: self.atoms.get(name.name).to_string(),
                };
                self.push_error(Box::new(error));
            }
        }
    }

    fn check_external_module_name(&mut self, node: ast::NodeID) -> bool {
        let Some(module_name) = self.p.node(node).get_external_module_name() else {
            return false;
        };
        // TODO: more checks
        true
    }

    fn check_import_equals_decl(&mut self, node: &'cx ast::ImportEqualsDecl<'cx>) {
        self.check_import_binding(node.id);
    }

    fn check_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) {
        if let Some(clause) = node.clause.and_then(|c| c.kind) {
            use bolt_ts_ast::ImportClauseKind::*;
            match clause {
                Ns(n) => {
                    // import * as ns from 'xxxx'
                    self.check_import_binding(n.id);
                }
                Specs(specs) => {
                    // import { a, b as c } from 'xxxx'
                    for spec in specs {
                        use bolt_ts_ast::ImportSpecKind::*;
                        match spec.kind {
                            Shorthand(n) => {
                                // import { a } from 'xxxx'
                                self.check_import_binding(n.id);
                            }
                            Named(_) => {
                                // import { a as b } from 'xxxx'
                            }
                        }
                    }
                }
            }
        }
    }

    fn get_extract_string_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let extract_ty_alias = self.get_global_extract_symbol();
        if let Some(extract_ty_alias) = extract_ty_alias {
            let ty_args = self.alloc([ty, self.string_ty]);
            self.get_type_alias_instantiation(extract_ty_alias, ty_args, None, None)
        } else {
            self.string_ty
        }
    }

    fn get_index_ty_or_string(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let index_ty = self.get_index_ty(ty, ty::IndexFlags::empty());
        let index_ty = self.get_extract_string_ty(index_ty);
        if index_ty.flags.contains(TypeFlags::NEVER) {
            self.string_ty
        } else {
            index_ty
        }
    }

    fn check_for_of_stmt(&mut self, node: &'cx ast::ForOfStmt<'cx>) {
        match node.init {
            ast::ForInitKind::Var(var) => {
                self.check_var_decl_list(var);
            }
            ast::ForInitKind::Expr(_) => {}
        };
    }

    fn check_for_in_stmt(&mut self, node: &'cx ast::ForInStmt<'cx>) {
        let right_ty = {
            let ty = self.check_expr(node.expr);
            self.get_non_nullable_ty(ty)
        };
        match node.init {
            ast::ForInitKind::Var(_) => {
                // TODO:
            }
            ast::ForInitKind::Expr(init) => {
                let left_ty = self.check_expr(init);
                let valid_ty = self.get_index_ty_or_string(right_ty);

                if !self.is_type_assignable_to(valid_ty, left_ty) {
                    self.push_error(Box::new(
                        errors::TheLeftHandSideOfAForInStatementMustBeOfTypeStringOrAny {
                            span: init.span(),
                        },
                    ));
                }
            }
        };

        if right_ty == self.never_ty
            || !self.is_type_assignable_to_kind(
                right_ty,
                TypeFlags::NON_PRIMITIVE | TypeFlags::INSTANTIABLE,
                false,
            )
        {
            let error = errors::TheRightHandSideOfAForInStatementMustBeOfTypeAnyAnObjectTypeOrATypeParameterButHereHasType {
                span: node.expr.span(),
                ty: self.print_ty(right_ty).to_string(),
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_for_stmt(&mut self, node: &'cx ast::ForStmt<'cx>) {
        if let Some(init) = node.init {
            match init {
                ast::ForInitKind::Var(list) => self.check_var_decl_list(list),
                ast::ForInitKind::Expr(expr) => {
                    self.check_expr(expr);
                }
            }
        }
        if let Some(cond) = node.cond {
            self.check_truthiness_expr(cond);
        }

        if let Some(incr) = node.incr {
            self.check_expr(incr);
        }

        self.check_stmt(node.body);
    }

    fn check_var_stmt(&mut self, var: &'cx ast::VarStmt<'cx>) {
        self.check_var_decl_list(var.list);
    }

    fn check_var_decl_list(&mut self, list: &[&'cx ast::VarDecl<'cx>]) {
        for decl in list {
            self.check_var_decl(decl);
        }
    }

    fn check_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>) {
        self.check_var_like_decl(decl);
    }

    fn check_fn_decl(&mut self, f: &'cx ast::FnDecl<'cx>) {
        self.check_fn_like_decl(f);
    }

    fn check_if_stmt(&mut self, i: &'cx ast::IfStmt) {
        self.check_expr(i.expr);
        self.check_stmt(i.then);
        if let Some(else_then) = i.else_then {
            self.check_stmt(else_then);
        }
    }

    pub(super) fn check_block(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        for item in block.stmts {
            self.check_stmt(item);
        }
    }

    fn is_instantiate_module(&self, node: &'cx ast::ModuleDecl<'cx>) -> bool {
        let state = self
            .node_query(node.id.module())
            .get_module_instance_state(node, None);
        state == ModuleInstanceState::Instantiated
    }

    fn get_first_non_ambient_class_or_fn_decl(
        &self,
        s: &bolt_ts_binder::Symbol,
    ) -> Option<ast::NodeID> {
        let decls = s.decls.as_ref()?;
        for decl in decls {
            let n = self.p.node(*decl);
            if (n.is_class_decl() || n.as_fn_decl().is_some_and(|f| f.body.is_some()))
                && !self.p.node_flags(*decl).intersects(NodeFlags::AMBIENT)
            {
                return Some(*decl);
            }
        }

        None
    }

    fn check_module_decl(&mut self, ns: &'cx ast::ModuleDecl<'cx>) {
        if let Some(block) = ns.block {
            for item in block.stmts {
                self.check_stmt(item);
            }
        }

        let in_ambient_context = self.p.node_flags(ns.id).contains(NodeFlags::AMBIENT);

        let is_global_augmentation = ns.is_global_scope_argument();
        let is_ambient_external_module = ns.is_ambient();
        if is_ambient_external_module {
            let p = self.parent(ns.id).unwrap();
            if self.p.get(p.module()).is_global_source_file(p) {
                if is_global_augmentation {
                    let error = errors::AugmentationsForTheGlobalScopeCanOnlyBeDirectlyNestedInExternalModulesOrAmbientModuleDeclarations {
                        span: ns.name.span()
                    };
                    self.push_error(Box::new(error));
                } else if let ast::ModuleName::StringLit(lit) = ns.name {
                    let module_name = self.atoms.get(lit.val);
                    if bolt_ts_path::is_external_module_relative(module_name) {
                        let error =
                            errors::AmbientModuleDeclarationCannotSpecifyRelativeModuleName {
                                span: lit.span,
                            };
                        self.push_error(Box::new(error));
                    }
                }
            }
        }

        let symbol = self.get_symbol_of_decl(ns.id);
        let s = self.symbol(symbol);
        if s.flags.intersects(SymbolFlags::VALUE_MODULE)
            && !in_ambient_context
            && self.is_instantiate_module(ns)
        {
            if s.decls.as_ref().is_some_and(|decls| decls.len() > 1) {
                if let Some(first_none_ambient_class_or_fn) =
                    self.get_first_non_ambient_class_or_fn_decl(s)
                {
                    if ns.span.lo() < self.p.node(first_none_ambient_class_or_fn).span().lo() {
                        let error = errors::ANamespaceDeclarationCannotBeLocatedPriorToAClassOrFunctionWithWhichItIsMerged {
                            span: ns.name.span(),
                        };
                        self.push_error(Box::new(error));
                    }
                }
            }
        }
    }

    pub(super) fn is_awaited_ty_instantiation(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.flags.contains(TypeFlags::CONDITIONAL) {
            let Some(awaited) = self.get_global_awaited_symbol() else {
                return false;
            };
            ty.alias_symbol() == Some(awaited)
                && ty.alias_ty_arguments().is_some_and(|args| args.len() == 1)
        } else {
            false
        }
    }

    fn is_thenable_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        let base_constraint = self.get_base_constraint_or_ty(ty);
        if self.all_types_assignable_to_kind(
            base_constraint,
            TypeFlags::PRIMITIVE.union(TypeFlags::NEVER),
            false,
        ) {
            return false;
        };
        let Some(then_fn) = self.get_ty_of_prop_of_ty(ty, SymbolName::Atom(keyword::IDENT_THEN))
        else {
            return false;
        };
        let t = self.get_ty_with_facts(then_fn, TypeFacts::NE_UNDEFINED_OR_NULL);
        !self.get_signatures_of_type(t, ty::SigKind::Call).is_empty()
    }

    fn is_awaited_ty_needed(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if self.is_type_any(ty) || self.is_awaited_ty_instantiation(ty) {
            return false;
        }
        if self.is_generic_object_ty(ty) {
            let base_constraint = self.get_base_constraint_of_ty(ty);
            if let Some(base_constraint) = base_constraint
                && (base_constraint.flags.contains(TypeFlags::ANY_OR_UNKNOWN)
                    || self.is_empty_object_ty(base_constraint)
                    || self.some_type(base_constraint, |this, t| this.is_thenable_ty(t)))
            {
                return true;
            } else if ty.maybe_type_of_kind(TypeFlags::TYPE_VARIABLE) {
                return true;
            }
        }

        false
    }

    fn try_create_awaited_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        let awaited_symbol = self.get_global_awaited_symbol()?;
        let ty_arguments = vec![self.unwrap_awaited_ty(ty)];
        let ty_arguments = self.alloc(ty_arguments);
        Some(self.get_type_alias_instantiation(awaited_symbol, ty_arguments, None, None))
    }

    fn create_awaited_ty_if_needed(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.is_awaited_ty_needed(ty) {
            self.try_create_awaited_ty(ty).unwrap_or(ty)
        } else {
            debug_assert!(
                self.is_awaited_ty_instantiation(ty)
                    || self.get_promised_ty_of_promise(ty).is_none()
            );
            ty
        }
    }

    pub(super) fn get_awaited_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        let awaited_ty = self.get_awaited_ty_no_alias(ty);
        awaited_ty.map(|awaited_ty| self.create_awaited_ty_if_needed(awaited_ty))
    }

    pub(super) fn get_awaited_ty_no_alias(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.is_type_any(ty) || self.is_awaited_ty_instantiation(ty) {
            return Some(ty);
        }

        let id = ty.promise_or_awaitable_ty_links();
        if let Some(id) = id
            && let Some(cached) = self.promise_or_awaitable_links_arena[id].get_awaited_ty_of_ty()
        {
            return Some(cached);
        }

        if let Some(u) = ty.kind.as_union() {
            if self.awaited_ty_stack.contains(&ty) {
                todo!("error handler")
            }

            self.awaited_ty_stack.push(ty);
            // TODO: error node
            let mapped = self
                .map_union_ty(ty, u, |this, t| this.get_awaited_ty_no_alias(t), false)
                .unwrap();
            self.awaited_ty_stack.pop();

            self.promise_or_awaitable_links_arena[u.promise_or_awaitable_links]
                .set_awaited_ty_of_ty(mapped);
            return Some(mapped);
        }

        if self.is_awaited_ty_needed(ty) {
            if let Some(id) = id {
                self.promise_or_awaitable_links_arena[id].set_awaited_ty_of_ty(ty);
            }
            return Some(ty);
        }

        let promised_ty = self.get_promised_ty_of_promise(ty);
        if let Some(promised_ty) = promised_ty {
            if ty == promised_ty || self.awaited_ty_stack.contains(&promised_ty) {
                // TODO: error
                return None;
            }
            self.awaited_ty_stack.push(ty);
            let awaited_ty = self.get_awaited_ty_no_alias(promised_ty);
            self.awaited_ty_stack.pop();

            match awaited_ty {
                Some(ty) => {
                    if let Some(id) = id {
                        self.promise_or_awaitable_links_arena[id].set_awaited_ty_of_ty(ty);
                    }
                    return Some(ty);
                }
                None => return None,
            }
        }

        if self.is_thenable_ty(ty) {
            // TODO: error node
            return None;
        }
        if let Some(id) = id {
            self.promise_or_awaitable_links_arena[id].set_awaited_ty_of_ty(ty);
        }
        return Some(ty);
    }

    pub(super) fn is_reference_to_ty(
        &self,
        ty: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        ty.get_object_flags().contains(ObjectFlags::REFERENCE) && {
            // TODO: Tuple?
            let Some(object_ty) = ty.kind.as_object() else {
                unreachable!()
            };
            match object_ty.kind {
                ty::ObjectTyKind::Interface(_) => true,
                ty::ObjectTyKind::Reference(t) => t.target == target,
                _ => unreachable!(),
            }
        }
    }

    pub(super) fn get_promised_ty_of_promise(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.is_type_any(ty) {
            return None;
        }

        let id = ty.promise_or_awaitable_ty_links();

        if let Some(id) = id
            && let Some(cached) =
                self.promise_or_awaitable_links_arena[id].get_promised_ty_of_promise()
        {
            return Some(cached);
        }

        let promise_ty = self.get_global_promise_ty::<false>();
        if self.is_reference_to_ty(ty, promise_ty) {
            let promised_ty_of_promise = self.get_ty_arguments(ty)[0];
            if let Some(id) = id {
                self.promise_or_awaitable_links_arena[id]
                    .set_promised_ty_of_promise(promised_ty_of_promise);
            }
            return Some(promised_ty_of_promise);
        }
        let base_ctor_or_ty = self.get_base_constraint_or_ty(ty);
        if self.all_types_assignable_to_kind(
            base_ctor_or_ty,
            TypeFlags::PRIMITIVE.union(TypeFlags::NEVER),
            false,
        ) {
            return None;
        }

        let then_fn = self.get_ty_of_prop_of_ty(ty, SymbolName::Atom(keyword::IDENT_THEN));
        if then_fn.is_some_and(|ty| self.is_type_any(ty)) {
            return None;
        }

        let then_sigs = if let Some(then_fn) = then_fn {
            self.get_signatures_of_type(then_fn, ty::SigKind::Call)
        } else {
            self.empty_array()
        };
        if then_sigs.is_empty() {
            // TODO: error
            return None;
        }
        let mut this_ty_for_error = None;
        let mut candidates = vec![];
        for then_sig in then_sigs {
            let this_ty = self.get_this_ty_of_sig(then_sig);
            if let Some(this_ty) = this_ty
                && this_ty != self.void_ty
                && !self.is_type_related_to(ty, this_ty, super::relation::RelationKind::Subtype)
            {
                this_ty_for_error = Some(this_ty);
            } else {
                candidates.push(then_sig);
            }
        }
        if candidates.is_empty() {
            let Some(this_ty_for_error) = this_ty_for_error else {
                unreachable!()
            };
            // TODO: error
            return None;
        }

        let onfulfilled_param_ty = {
            let tys = candidates
                .iter()
                .map(|sig| self.get_ty_of_first_param_of_sig(sig))
                .collect::<Vec<_>>();
            let ty = self.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None);
            self.get_ty_with_facts(ty, TypeFacts::NE_UNDEFINED_OR_NULL)
        };
        if self.is_type_any(onfulfilled_param_ty) {
            return None;
        }
        let onfulfilled_param_sigs =
            self.get_signatures_of_type(onfulfilled_param_ty, ty::SigKind::Call);
        if onfulfilled_param_sigs.is_empty() {
            // TODO: error
            return None;
        }
        let tys = onfulfilled_param_sigs
            .iter()
            .map(|sig| self.get_ty_of_first_param_of_sig(sig))
            .collect::<Vec<_>>();
        let ty = self.get_union_ty::<false>(&tys, ty::UnionReduction::Subtype, None, None, None);
        if let Some(id) = id {
            self.promise_or_awaitable_links_arena[id].set_promised_ty_of_promise(ty);
        }
        Some(ty)
    }

    fn get_ty_of_first_param_of_sig(&mut self, sig: &'cx ty::Sig<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_ty_of_first_param_of_sig_with_fallback(sig, self.never_ty)
    }

    fn get_ty_of_first_param_of_sig_with_fallback(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        fallback_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if !sig.params.is_empty() {
            self.get_ty_at_pos(sig, 0)
        } else {
            fallback_ty
        }
    }

    fn is_unwrapped_ret_ty_undefined_void_or_any(
        &mut self,
        func: ast::NodeID,
        ret_ty: &'cx ty::Ty<'cx>,
    ) -> bool {
        let flags = self.p.node(func).fn_flags();
        let Some(ty) = self.unwrap_ret_ty(ret_ty, flags) else {
            return false;
        };
        ty.maybe_type_of_kind(TypeFlags::VOID)
            || ty
                .flags
                .intersects(TypeFlags::ANY.union(TypeFlags::UNDEFINED))
    }

    fn check_ret_stmt(&mut self, node: &ast::RetStmt<'cx>) {
        let Some(container) = self.get_containing_fn_or_class_static_block(node.id) else {
            // delay bug
            return;
        };
        let sig = self.get_sig_from_decl(container);
        let ret_ty = self.get_ret_ty_of_sig(sig);

        if self.config.strict_null_checks()
            || node.expr.is_some()
            || ret_ty.flags.contains(TypeFlags::NEVER)
        {
            let expr_ty = node
                .expr
                .map(|expr| self.check_expr_cached(expr))
                .unwrap_or(self.undefined_ty);
            let c = self.p.node(container);
            if c.is_setter_decl() {
                if node.expr.is_some() {
                    let error = errors::SettersCannotReturnAValue { span: node.span };
                    self.push_error(Box::new(error));
                }
            } else if matches!(c, ast::Node::ClassCtor(_)) {
                if let Some(expr) = node.expr {
                    self.check_type_assignable_to_and_optionally_elaborate(
                        expr_ty,
                        ret_ty,
                        Some(expr.id()),
                        Some(expr.id()),
                    );
                }
            } else if self.get_ret_ty_from_anno(container).is_some() {
                let fn_flags = self.p.node(container).fn_flags();
                let unwrapped_ret_ty = self.unwrap_ret_ty(ret_ty, fn_flags).unwrap_or(ret_ty);
                self.check_ret_expr(container, unwrapped_ret_ty, node.expr, expr_ty);
            }
        } else if self.config.no_implicit_returns()
            && !self.p.node(container).is_class_ctor()
            && !self.is_unwrapped_ret_ty_undefined_void_or_any(container, ret_ty)
        {
            let error = errors::NotAllCodePathsReturnAValue { span: node.span };
            self.push_error(Box::new(error));
        }
    }

    fn check_ret_expr(
        &mut self,
        container: ast::NodeID,
        ret_ty: &'cx ty::Ty<'cx>,
        ret_expr: Option<&'cx ast::Expr<'cx>>,
        expr_ty: &'cx ty::Ty<'cx>,
    ) {
        if !(ret_ty.kind.is_indexed_access() || ret_ty.kind.is_cond_ty())
            || !self.could_contain_ty_var(ret_ty)
        {
            let error_node = ret_expr.map(|expr| expr.id());
            self.check_type_assignable_to_and_optionally_elaborate(
                expr_ty, ret_ty, error_node, error_node,
            );
        }
    }

    fn check_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.check_class_like_decl(class)
    }

    fn check_type_alias_decl(&mut self, n: &'cx ast::TypeAliasDecl<'cx>) {
        if let Some(ty_params) = n.ty_params {
            self.check_ty_params(ty_params);
        }
        self.check_ty(n.ty);
    }

    pub(super) fn check_getter_decl(&mut self, n: &'cx ast::GetterDecl<'cx>) {
        let flags = self.node_query(n.id.module()).node_flags(n.id);
        if !flags.intersects(NodeFlags::AMBIENT)
            && n.body.is_some()
            && flags.intersects(NodeFlags::HAS_IMPLICIT_RETURN)
            && !flags.intersects(NodeFlags::HAS_EXPLICIT_RETURN)
        {
            let error = errors::AGetAccessorMustReturnAValue {
                span: n.name.span(),
            };
            self.push_error(Box::new(error));
        }

        self.check_accessor_decl(n);
    }

    pub(super) fn check_accessor_decl(
        &mut self,
        decl: &impl bolt_ts_ast::r#trait::AccessorLike<'cx>,
    ) {
        if let Some(body) = decl.body() {
            self.check_block(body);
        }
    }
}

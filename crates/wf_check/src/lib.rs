use bolt_ts_ast::keyword::is_reserved_type_name;
use bolt_ts_ast::{self as ast, keyword, pprint_ident, print_prop_name};
use bolt_ts_atom::AtomIntern;
use bolt_ts_checker_errors::DeclKind;
use bolt_ts_config::{NormalizedCompilerOptions, Target};
use bolt_ts_parser::ParsedMap;
use bolt_ts_span::ModuleID;
use bolt_ts_wf_errors as errors;
use rustc_hash::FxHashSet;

mod r#trait;

pub use self::r#trait::VarLike;

pub fn well_formed_check_parallel<'cx>(
    p: &ParsedMap<'cx>,
    atoms: &AtomIntern,
    modules: &[bolt_ts_span::Module],
    compiler_options: &NormalizedCompilerOptions,
    resolve_results: &[bolt_ts_binder::ResolveResult],
) -> Vec<WellFormedCheckResult> {
    use rayon::prelude::*;

    modules
        .into_par_iter()
        .map(|m| {
            let result = well_formed_check(
                p,
                atoms,
                m.id(),
                compiler_options,
                &resolve_results[m.id().as_usize()],
            );
            debug_assert!(!m.is_default_lib() || result.diags.is_empty());
            result
        })
        .collect::<Vec<_>>()
}

fn well_formed_check<'cx, 'a>(
    p: &ParsedMap<'cx>,
    atoms: &AtomIntern,
    module_id: ModuleID,
    compiler_options: &'a NormalizedCompilerOptions,
    resolve_results: &'a bolt_ts_binder::ResolveResult,
) -> WellFormedCheckResult {
    let mut s = CheckState {
        p,
        atoms,
        compiler_options,
        resolve_results,
        module_id,

        diags: vec![],
        potential_unused_renamed_binding_elements_in_types: FxHashSet::default(),
        issue_external_export_declarations: IssueExternalExportDeclarations::default(),
    };
    let program = p.root(module_id);
    bolt_ts_ast_visitor::visit_program(&mut s, program);
    WellFormedCheckResult {
        diags: s.diags,
        potential_unused_renamed_binding_elements_in_types: s
            .potential_unused_renamed_binding_elements_in_types,
        issue_external_export_declarations: s.issue_external_export_declarations,
    }
}

pub struct WellFormedCheckResult {
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub potential_unused_renamed_binding_elements_in_types: FxHashSet<ast::NodeID>,
    pub issue_external_export_declarations: IssueExternalExportDeclarations,
}

struct CheckState<'cx, 'a> {
    p: &'a ParsedMap<'cx>,
    atoms: &'a AtomIntern,
    compiler_options: &'a NormalizedCompilerOptions,
    module_id: ModuleID,
    resolve_results: &'a bolt_ts_binder::ResolveResult,

    diags: Vec<bolt_ts_errors::Diag>,
    potential_unused_renamed_binding_elements_in_types: FxHashSet<ast::NodeID>,
    issue_external_export_declarations: IssueExternalExportDeclarations,
}

#[derive(Default)]
pub struct IssueExternalExportDeclarations(FxHashSet<ast::NodeID>);

impl<'cx> IssueExternalExportDeclarations {
    pub fn insert(&mut self, node: &'cx ast::ExportDecl<'cx>) {
        let prev = self.0.insert(node.id);
        debug_assert!(prev);
    }
    pub fn contains(&self, key: &'cx ast::ExportDecl<'cx>) -> bool {
        self.0.contains(&key.id)
    }
    pub fn join(iter: impl Iterator<Item = IssueExternalExportDeclarations>) -> Self {
        let mut set = FxHashSet::default();
        for item in iter {
            set.extend(item.0);
        }
        Self(set)
    }
}

impl<'cx, 'a> CheckState<'cx, 'a> {
    fn parent(&self, node: ast::NodeID) -> Option<ast::NodeID> {
        debug_assert!(node.module() == self.module_id);
        self.resolve_results.parent_map.parent(node)
    }
    fn node_query(&self) -> bolt_ts_binder::NodeQuery<'cx, '_> {
        bolt_ts_binder::NodeQuery::new(&self.resolve_results.parent_map, self.p.get(self.module_id))
    }

    fn push_error(&mut self, error: bolt_ts_errors::BoxedDiag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error })
    }

    fn check_collisions_for_declaration_name(&mut self, node: ast::NodeID, name: &'cx ast::Ident) {
        let n = self.p.node(node);
        let kind = if n.is_class_like() {
            Some(DeclKind::Class)
        } else if n.is_interface_decl() {
            Some(DeclKind::Interface)
        } else {
            None
        };
        if is_reserved_type_name(name.name)
            && let Some(kind) = kind
        {
            let error = errors::DeclNameCannotBe {
                span: name.span,
                name: pprint_ident(name, self.atoms),
                kind,
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_class_like(&mut self, class: &impl ast::r#trait::ClassLike<'cx>) {
        if let Some(name) = class.name() {
            self.check_collisions_for_declaration_name(class.id(), name);
        };
    }

    fn check_grammar_object_lit_expr(&mut self, _node: &'cx ast::ObjectLit<'cx>) {
        // TODO:
    }

    fn check_grammar_try_stmt(&mut self, node: &'cx ast::TryStmt<'cx>) {
        if let Some(c) = node.catch_clause
            && let Some(v) = c.var
            && let Some(init) = v.init
        {
            let error = errors::CatchClauseVariableTypeAnnotationMustBeAnyOrUnknownIfSpecified {
                span: init.span(),
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_sig_decl(&mut self, node: &impl ast::r#trait::SigDeclLike<'cx>) {
        if !(*self.compiler_options.target() >= Target::ES2015
            || !node.has_rest_param()
            || self
                .p
                .node_flags(node.id())
                .intersects(ast::NodeFlags::AMBIENT)
            || node.body().is_none())
        {
            // check_collision_with_arguments_in_generated_code
            for param in node.params() {
                if let ast::BindingKind::Ident(name) = param.name.kind
                    && name.name == keyword::IDENT_ARGUMENTS
                {
                    // TODO: skip on
                    let error = errors::DuplicateIdentifierArgumentsCompilerUsesArgumentsToInitializeRestParameters {
                            span: name.span
                        };
                    self.push_error(Box::new(error));
                }
            }
        }
    }

    fn check_type_name_is_reserved(
        &mut self,
        name: &'cx ast::Ident,
        push_error: impl FnOnce(&mut Self),
    ) {
        if keyword::is_reserved_type_name(name.name) {
            push_error(self);
        }
    }

    fn check_stmt_in_ambient(&mut self, node: ast::NodeID) -> bool {
        let flags = self.p.node_flags(node);
        if flags.contains(ast::NodeFlags::AMBIENT) {
            let parent = self.parent(node).unwrap();
            let parent_node = self.p.node(parent);
            if matches!(
                parent_node,
                ast::Node::ModuleBlock(_) | ast::Node::Program(_) | ast::Node::BlockStmt(_)
            ) {
                let error = errors::XAreNotAllowedInAmbientContexts {
                    span: self.p.node(node).span(),
                    kind: errors::AmbientContextKind::Statements,
                };
                self.push_error(Box::new(error));
                return true;
            }
        }
        false
    }

    fn check_implement_in_ambient(&mut self, node: ast::NodeID) {
        let body = match self.p.node(node) {
            ast::Node::GetterDecl(n) => n.body,
            ast::Node::SetterDecl(n) => n.body,
            ast::Node::ClassMethodElem(n) => n.body,
            ast::Node::ClassCtor(n) => n.body,
            _ => unreachable!(),
        };
        let node_flags = self.p.node_flags(node);
        if node_flags.contains(ast::NodeFlags::AMBIENT)
            && let Some(body) = body
        {
            let span = match self.p.node(node).name() {
                Some(name) => name.span(),
                None => {
                    bolt_ts_span::Span::new(body.span.lo(), body.span.lo() + 1, body.span.module())
                }
            };
            let error = errors::AnImplementationCannotBeDeclaredInAmbientContexts { span };
            self.push_error(Box::new(error));
        }
    }

    fn check_ambient_initializer(&mut self, node: &impl self::VarLike<'cx>) {
        let Some(init) = node.init() else {
            return;
        };
        let node_flags = self.p.node_flags(node.id());
        if !node_flags.contains(ast::NodeFlags::AMBIENT) {
            return;
        }
        if node.is_declaration_readonly(&self.node_query())
            || node.decl_ty().is_none() && node.is_var_const(&self.node_query())
        {
            let is_invalid_init = !(
                init.is_string_or_number_lit_like()
            // TODO: simple literal enum reference
                || matches!(init.kind, ast::ExprKind::BoolLit(_))
                // TODO: is bigint literal
            );
            if is_invalid_init {
                let error = errors::XAreNotAllowedInAmbientContexts {
                    kind: errors::AmbientContextKind::Initializers,
                    span: init.span(),
                };
                self.push_error(Box::new(error));
            }
        } else {
            let error = errors::XAreNotAllowedInAmbientContexts {
                kind: errors::AmbientContextKind::Initializers,
                span: init.span(),
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_external_import_equals_declaration(&mut self, node: &'cx ast::ImportEqualsDecl<'cx>) {
        let Some(module_name) = node.get_external_module_name() else {
            return;
        };
        let parent = self.parent(node.id).unwrap();
        let parent_node = self.p.node(parent);
        let is_ambient_external_module = parent_node.is_module_block() && {
            let parent_parent = self.parent(parent).unwrap();
            self.p.node(parent_parent).is_ambient_module()
        };
        if !parent_node.is_program() && !is_ambient_external_module {
            let error = Box::new(
                errors::ImportDeclarationsInANamespaceCannotReferenceAModule {
                    span: module_name.span,
                },
            );
            self.push_error(error);
        }
    }

    fn check_external_export_declaration(&mut self, node: &'cx ast::ExportDecl<'cx>) -> bool {
        let Some(module_name) = node.module_spec() else {
            self.issue_external_export_declarations.insert(node);
            return false;
        };
        let parent = self.parent(node.id).unwrap();
        let parent_node = self.p.node(parent);
        let is_ambient_external_module = parent_node.is_module_block() && {
            let parent_parent = self.parent(parent).unwrap();
            self.p.node(parent_parent).is_ambient_module()
        };
        if !parent_node.is_program() && !is_ambient_external_module {
            let error = Box::new(errors::ExportDeclarationsAreNotPermittedInANamespace {
                span: module_name.span,
            });
            self.push_error(error);
            self.issue_external_export_declarations.insert(node);
            return false;
        }
        // TODO:
        true
    }

    fn check_export_assignment(&mut self, node: &'cx ast::ExportAssign<'cx>) {
        if node.is_export_equals {
            let m = *self.compiler_options.module();
            if m != bolt_ts_config::Module::Preserve
                && m >= bolt_ts_config::Module::ES2015
                && let flags = self.node_query().node_flags(node.id)
                && let in_ambient = flags.contains(ast::NodeFlags::AMBIENT)
                // TODO: implied_node_format_of_root
                && !in_ambient
            {
                let error = errors::ExportAssignmentCannotBeUsedWhenTargetingEcmascriptModulesConsiderUsingExportDefaultOrAnotherModuleFormatInstead {
                    span: node.span,
                };
                self.push_error(Box::new(error));
            }
        }
    }
}

impl<'cx, 'a> bolt_ts_ast_visitor::Visitor<'cx> for CheckState<'cx, 'a> {
    type Result = ();
    fn visit_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.check_class_like(class);
        bolt_ts_ast_visitor::visit_class_decl(self, class)
    }
    fn visit_class_method_elem(&mut self, node: &'cx ast::ClassMethodElem<'cx>) {
        self.check_implement_in_ambient(node.id);
        bolt_ts_ast_visitor::visit_class_method_elem(self, node)
    }
    fn visit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) {
        self.check_collisions_for_declaration_name(node.id, node.name);
        bolt_ts_ast_visitor::visit_interface_decl(self, node)
    }
    fn visit_object_lit(&mut self, node: &'cx ast::ObjectLit<'cx>) {
        self.check_grammar_object_lit_expr(node);
        bolt_ts_ast_visitor::visit_object_lit(self, node)
    }
    fn visit_try_stmt(&mut self, node: &'cx ast::TryStmt<'cx>) {
        self.check_grammar_try_stmt(node);
        bolt_ts_ast_visitor::visit_try_stmt(self, node)
    }
    fn visit_arrow_fn_expr(&mut self, node: &'cx ast::ArrowFnExpr<'cx>) {
        self.check_sig_decl(node);
        bolt_ts_ast_visitor::visit_arrow_fn_expr(self, node)
    }
    fn visit_type_alias_decl(&mut self, node: &'cx ast::TypeAliasDecl<'cx>) {
        self.check_type_name_is_reserved(node.name, |this| {
            let error = errors::TypeAliasNameCannotBeX {
                span: node.name.span,
                name: pprint_ident(node.name, this.atoms),
            };
            this.push_error(Box::new(error));
        });
        bolt_ts_ast_visitor::visit_type_alias_decl(self, node)
    }
    fn visit_empty_stmt(&mut self, node: &'cx ast::EmptyStmt) {
        self.check_stmt_in_ambient(node.id);
    }
    fn visit_while_stmt(&mut self, node: &'cx ast::WhileStmt<'cx>) {
        self.check_stmt_in_ambient(node.id);
        bolt_ts_ast_visitor::visit_while_stmt(self, node)
    }
    fn visit_var_decl(&mut self, node: &'cx ast::VarDecl<'cx>) {
        self.check_ambient_initializer(node);
        bolt_ts_ast_visitor::visit_var_decl(self, node)
    }
    fn visit_class_prop_elem(&mut self, node: &'cx ast::ClassPropElem<'cx>) {
        self.check_ambient_initializer(node);
        bolt_ts_ast_visitor::visit_class_prop_elem(self, node)
    }
    fn visit_if_stmt(&mut self, node: &'cx ast::IfStmt<'cx>) {
        if let ast::StmtKind::Empty(s) = node.then.kind {
            let error = errors::TheBodyOfAnIfStatementCannotBeTheEmptyStatement { span: s.span };
            self.push_error(Box::new(error));
        }
        bolt_ts_ast_visitor::visit_if_stmt(self, node)
    }
    fn visit_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) {
        self.check_type_name_is_reserved(node.name, |this| {
            let error = errors::EnumNameCannotBeX {
                span: node.name.span,
                name: pprint_ident(node.name, this.atoms),
            };
            this.push_error(Box::new(error));
        });
        bolt_ts_ast_visitor::visit_enum_decl(self, node)
    }
    fn visit_getter_decl(&mut self, node: &'cx ast::GetterDecl<'cx>) {
        self.check_implement_in_ambient(node.id);
        bolt_ts_ast_visitor::visit_getter_decl(self, node)
    }
    fn visit_setter_decl(&mut self, node: &'cx ast::SetterDecl<'cx>) {
        self.check_implement_in_ambient(node.id);
        bolt_ts_ast_visitor::visit_setter_decl(self, node)
    }
    fn visit_class_ctor(&mut self, node: &'cx ast::ClassCtor<'cx>) {
        self.check_implement_in_ambient(node.id);
        bolt_ts_ast_visitor::visit_class_ctor(self, node)
    }
    fn visit_param_decl(&mut self, node: &'cx ast::ParamDecl<'cx>) {
        if node.init.is_some() {
            if node.question.is_some() {
                let error = errors::ParameterCannotHaveQuestionMarkAndInitializer {
                    span: node.name.span,
                };
                self.push_error(Box::new(error));
            }

            if let Some(f) = self.node_query().get_containing_fn(node.id)
                && self.p.node(f).fn_body().is_none()
            {
                let error =
                errors::AParameterInitializerIsOnlyAllowedInAFunctionOrConstructorImplementation {
                    span: node.span,
                };
                self.push_error(Box::new(error));
            }
        }
        bolt_ts_ast_visitor::visit_param_decl(self, node)
    }
    fn visit_object_binding_elem(
        &mut self,
        node: &'cx ast::ObjectBindingElem<'cx>,
    ) -> Self::Result {
        let is_under_parameter_in_missing_body_fn = || {
            if self.node_query().is_part_of_param_decl(node.id)
                && let Some(containing_fn) = self.node_query().get_containing_fn(node.id)
                && self.p.node(containing_fn).fn_body().is_none()
            {
                true
            } else {
                false
            }
        };

        match node.name {
            ast::ObjectBindingName::Prop { prop_name, name }
                if is_under_parameter_in_missing_body_fn() =>
            {
                if let ast::BindingKind::Ident(name) = name.kind {
                    let prev = self
                        .potential_unused_renamed_binding_elements_in_types
                        .insert(node.id);
                    debug_assert!(prev);
                    let error = Box::new(
                        errors::XIsAnUnusedRenamingOfYDidYouIntendToUseItAsATypeAnnotation {
                            span: name.span,
                            x: pprint_ident(name, self.atoms),
                            y: print_prop_name(&prop_name.kind, self.atoms),
                        },
                    );
                    self.push_error(error);
                }

                if node.init.is_some() {
                    let error = errors::AParameterInitializerIsOnlyAllowedInAFunctionOrConstructorImplementation {
                    span: prop_name.span(),
                };
                    self.push_error(Box::new(error));
                }
            }
            ast::ObjectBindingName::Shorthand(name)
                if is_under_parameter_in_missing_body_fn() && node.init.is_some() =>
            {
                let error = errors::AParameterInitializerIsOnlyAllowedInAFunctionOrConstructorImplementation {
                    span: name.span,
                };
                self.push_error(Box::new(error));
            }
            _ => {}
        }

        bolt_ts_ast_visitor::visit_object_binding_elem(self, node)
    }
    fn visit_import_equals_decl(&mut self, node: &'cx ast::ImportEqualsDecl<'cx>) -> Self::Result {
        self.check_external_import_equals_declaration(node);
        bolt_ts_ast_visitor::visit_import_equals_decl(self, node)
    }
    fn visit_ty_param(&mut self, node: &'cx ast::TyParam<'cx>) -> Self::Result {
        self.check_type_name_is_reserved(node.name, |this| {
            let error = errors::TypeParameterNameCannotBeX {
                span: node.name.span,
                name: pprint_ident(node.name, this.atoms),
            };
            this.push_error(Box::new(error));
        });
        bolt_ts_ast_visitor::visit_ty_param(self, node)
    }
    fn visit_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) -> Self::Result {
        self.check_external_export_declaration(node);
        bolt_ts_ast_visitor::visit_export_decl(self, node);
    }
    fn visit_export_assign(&mut self, node: &'cx bolt_ts_ast::ExportAssign<'cx>) -> Self::Result {
        self.check_export_assignment(node);
        bolt_ts_ast_visitor::visit_export_assign(self, node)
    }
}

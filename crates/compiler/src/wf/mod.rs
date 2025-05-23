mod errors;
use crate::check::errors::DeclKind;

use bolt_ts_atom::AtomMap;
use bolt_ts_config::{NormalizedCompilerOptions, Target};
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;

use crate::ir;
use crate::keyword::is_reserved_type_name;
use crate::parser::Parser;
use bolt_ts_ast::{self as ast, keyword, pprint_ident, visitor};

pub fn well_formed_check_parallel(
    p: &Parser,
    atoms: &AtomMap,
    modules: &[bolt_ts_span::Module],
    compiler_options: &NormalizedCompilerOptions,
) -> Vec<bolt_ts_errors::Diag> {
    use rayon::prelude::*;

    modules
        .into_par_iter()
        .flat_map(|m| {
            let diags = well_formed_check(p, atoms, m.id, compiler_options);
            assert!(!m.is_default_lib || diags.is_empty());
            diags
        })
        .collect::<Vec<_>>()
}

fn well_formed_check(
    p: &Parser,
    atoms: &AtomMap,
    module_id: ModuleID,
    compiler_options: &NormalizedCompilerOptions,
) -> Vec<bolt_ts_errors::Diag> {
    let mut s = CheckState {
        p,
        atoms,
        compiler_options,
        diags: vec![],
    };
    let program = p.root(module_id);
    visitor::visit_program(&mut s, program);
    s.diags
}

struct CheckState<'cx> {
    p: &'cx Parser<'cx>,
    atoms: &'cx AtomMap<'cx>,
    diags: Vec<bolt_ts_errors::Diag>,
    compiler_options: &'cx NormalizedCompilerOptions,
}

impl<'cx> CheckState<'cx> {
    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error })
    }
    fn check_collisions_for_decl_name(&mut self, node: ast::NodeID, name: &'cx ast::Ident) {
        let n = self.p.node(node);
        let kind = if n.is_class_like() {
            Some(DeclKind::Class)
        } else if n.is_interface_decl() {
            Some(DeclKind::Interface)
        } else {
            None
        };
        if is_reserved_type_name(name.name) {
            if let Some(kind) = kind {
                let error = errors::DeclNameCannotBe {
                    span: name.span,
                    name: pprint_ident(name, self.atoms),
                    kind,
                };
                self.push_error(Box::new(error));
            }
        }
    }
    fn check_class_like(&mut self, class: &impl ir::ClassLike<'cx>) {
        if let Some(name) = class.name() {
            self.check_collisions_for_decl_name(class.id(), name);
        };
    }
    fn check_grammar_modifiers(&mut self, node: ast::NodeID) {
        let n = self.p.node(node);
        let Some(modifiers) = n.modifiers() else {
            return;
        };
        for modifier in modifiers.list {
            use bolt_ts_ast::ModifierKind::*;
            if modifier.kind == Abstract {
                let parent = self.p.parent(node).unwrap();
                let parent_node = self.p.node(parent);
                if !(parent_node.is_class_decl()
                    && parent_node.has_syntactic_modifier(ast::ModifierKind::Abstract.into()))
                {
                    let error = if n.is_class_prop_ele() {
                        todo!()
                    } else {
                        errors::AbstractMethodsCanOnlyAppearWithinAnAbstractClass {
                            span: modifier.span,
                        }
                    };
                    self.push_error(Box::new(error));
                    return;
                }
            }
        }
    }
    fn check_grammar_object_lit_expr(&mut self, node: &'cx ast::ObjectLit<'cx>) {
        let mut seen = fx_hashmap_with_capacity(node.members.len());
        for member in node.members {
            if let ast::ObjectMemberKind::Prop(n) = member.kind {
                let name = crate::bind::prop_name(n.name);
                if let Some(prev) = seen.insert(name, n.span) {
                    let error =
                        errors::AnObjectLiteralCannotHaveMultiplePropertiesWithTheSameName {
                            span: n.name.span(),
                            old: prev,
                        };
                    self.push_error(Box::new(error));
                }
            }
        }
    }
    fn check_grammar_try_stmt(&mut self, node: &'cx ast::TryStmt<'cx>) {
        if let Some(c) = node.catch_clause {
            if let Some(v) = c.var {
                if let Some(init) = v.init {
                    let error =
                        errors::CatchClauseVariableTypeAnnotationMustBeAnyOrUnknownIfSpecified {
                            span: init.span(),
                        };
                    self.push_error(Box::new(error));
                }
            }
        }
    }

    fn check_sig_decl(&mut self, node: &impl ir::SigDeclLike) {
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
                if let ast::BindingKind::Ident(name) = param.name.kind {
                    if name.name == keyword::IDENT_ARGUMENTS {
                        // TODO: skip on
                        let error = errors::DuplicateIdentifierArgumentsCompilerUsesArgumentsToInitializeRestParameters {
                            span: name.span
                        };
                        self.push_error(Box::new(error));
                    }
                }
            }
        }
    }

    fn check_type_name_is_reserved(
        &mut self,
        name: &'cx ast::Ident,
        push_error: impl FnOnce(&mut Self),
    ) {
        if matches!(
            name.name,
            keyword::IDENT_ANY
                | keyword::IDENT_UNKNOWN
                | keyword::IDENT_NEVER
                | keyword::IDENT_NUMBER
                | keyword::IDENT_BIGINT
                | keyword::IDENT_STRING
                | keyword::IDENT_SYMBOL
                | keyword::KW_VOID
                | keyword::IDENT_OBJECT
                | keyword::KW_UNDEFINED
        ) {
            push_error(self);
        }
    }
    fn check_stmt_in_ambient(&mut self, node: ast::NodeID) -> bool {
        let flags = self.p.node_flags(node);
        if flags.intersects(ast::NodeFlags::AMBIENT) {
            let parent = self.p.parent(node).unwrap();
            let parent_node = self.p.node(parent);
            if parent_node.is_block_stmt()
                || parent_node.is_module_block()
                || parent_node.is_program()
            {
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
    fn check_ambient_initializer(&mut self, node: &impl ir::VarLike<'cx>) {
        let Some(init) = node.init() else {
            return;
        };
        if node.decl_ty().is_none() && node.is_var_const(self.p) {
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
}

impl<'cx> ast::Visitor<'cx> for CheckState<'cx> {
    fn visit_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.check_class_like(class);
        visitor::visit_class_decl(self, class);
    }
    fn visit_class_method_elem(&mut self, node: &'cx ast::ClassMethodElem<'cx>) {
        self.check_grammar_modifiers(node.id);
        visitor::visit_class_method_elem(self, node);
    }
    fn visit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) {
        self.check_collisions_for_decl_name(node.id, node.name);
        visitor::visit_interface_decl(self, node);
    }
    fn visit_object_lit(&mut self, node: &'cx bolt_ts_ast::ObjectLit<'cx>) {
        self.check_grammar_object_lit_expr(node);
        visitor::visit_object_lit(self, node);
    }
    fn visit_try_stmt(&mut self, node: &'cx bolt_ts_ast::TryStmt<'cx>) {
        self.check_grammar_try_stmt(node);
        visitor::visit_try_stmt(self, node);
    }
    fn visit_arrow_fn_expr(&mut self, node: &'cx bolt_ts_ast::ArrowFnExpr<'cx>) {
        self.check_sig_decl(node);
    }
    fn visit_type_alias_decl(&mut self, node: &'cx bolt_ts_ast::TypeAliasDecl<'cx>) {
        self.check_type_name_is_reserved(node.name, |this| {
            let error = errors::TypeAliasNameCannotBeX {
                span: node.name.span,
                name: pprint_ident(node.name, this.atoms),
            };
            this.push_error(Box::new(error));
        });
        visitor::visit_type_alias_decl(self, node);
    }
    fn visit_while_stmt(&mut self, node: &'cx bolt_ts_ast::WhileStmt<'cx>) {
        self.check_stmt_in_ambient(node.id);
        visitor::visit_while_stmt(self, node);
    }
    fn visit_var_decl(&mut self, node: &'cx bolt_ts_ast::VarDecl<'cx>) {
        let node_flags = self.p.node_flags(node.id);
        if node_flags.intersects(ast::NodeFlags::AMBIENT) {
            self.check_ambient_initializer(node);
        }

        visitor::visit_var_decl(self, node);
    }
}

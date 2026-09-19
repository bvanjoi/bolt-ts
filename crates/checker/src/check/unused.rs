use bolt_ts_ast::{self as ast, pprint_binding, pprint_prop_name};
use bolt_ts_binder::{SymbolFlags, SymbolID, SymbolTable};
use bolt_ts_checker_errors as errors;
use bolt_ts_early_resolve::resolve_symbol_by_identifier::Resolver;
use bolt_ts_errors::BoxedDiag;
use bolt_ts_parser::ParsedMap;
use bolt_ts_ty::CheckFlags;
use bolt_ts_utils::{FxIndexMap, FxIndexSet, fx_indexmap_with_capacity};

use super::TyChecker;

#[derive(Debug, Clone)]
pub enum PotentiallyUnusedIdentifier<'cx> {
    InferTy(&'cx ast::InferTy<'cx>),
    FnDecl(&'cx ast::FnDecl<'cx>),
    FnExpr(&'cx ast::FnExpr<'cx>),
    ForInStmt(&'cx ast::ForInStmt<'cx>),
    ForOfStmt(&'cx ast::ForOfStmt<'cx>),
    ClassCtor(&'cx ast::ClassCtor<'cx>),
    ClassDecl(&'cx ast::ClassDecl<'cx>),
    ClassExpr(&'cx ast::ClassExpr<'cx>),
    TypeAliasDecl(&'cx ast::TypeAliasDecl<'cx>),
    ClassMethodElem(&'cx ast::ClassMethodElem<'cx>),
    BlockModuleDecl(&'cx ast::BlockModuleDecl<'cx>),
}

impl std::hash::Hash for PotentiallyUnusedIdentifier<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

impl std::cmp::PartialEq for PotentiallyUnusedIdentifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl std::cmp::Eq for PotentiallyUnusedIdentifier<'_> {}

#[derive(Debug, Clone)]
pub struct PotentiallyUnusedIdentifiers<'cx>(FxIndexSet<PotentiallyUnusedIdentifier<'cx>>);

impl<'cx> PotentiallyUnusedIdentifiers<'cx> {
    pub(super) fn new() -> Self {
        Self(Default::default())
    }

    fn insert(&mut self, p: &ParsedMap<'cx>, n: PotentiallyUnusedIdentifier<'cx>) {
        let id = n.id();
        if p.get(id.module()).is_declaration {
            return;
        }
        let prev = self.0.insert(n);
        debug_assert!(prev);
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn mark_property_as_referenced(&mut self, prop: SymbolID) {
        let s = self.symbol(prop);
        if !s.flags.intersects(SymbolFlags::CLASS_MEMBER) {
            return;
        }
        let Some(value_declaration) = s.value_decl else {
            return;
        };
        let n = self.p.node(value_declaration);
        let ms = n.modifiers();
        let has_private_modifier =
            ms.is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::PRIVATE));
        if !has_private_modifier
            && !n
                .name()
                .is_some_and(|name| matches!(name, ast::DeclarationName::PrivateIdent(_)))
        {
            return;
        }
        // TODO: more case
        let check_flags = self.get_check_flags(prop);
        let prop = if check_flags.contains(CheckFlags::INSTANTIATED) {
            self.get_symbol_links(prop).expect_target()
        } else {
            prop
        };
        self.symbol_mut(prop).is_referenced = Some(SymbolFlags::all());
    }

    pub fn check_unused_identifiers(&mut self) {
        if !self.config.compiler_options().no_unused_locals()
            && !self.config.compiler_options().no_unused_parameters()
        {
            return;
        }
        let n = std::mem::take(&mut self.potentially_unused_identifiers.0);
        debug_assert!(n.is_sorted_by(|a, b| a.id().module().as_u32() <= b.id().module().as_u32()));
        let c = PotentiallyUnusedIdentifierChecker { c: self };
        let diags = n
            .into_iter() // TODO: `into_par_iter`
            .flat_map(|item| c.check_unused_identifier(item))
            .collect::<Vec<_>>();
        for diag in diags {
            self.push_error(diag);
        }
    }
}

struct PotentiallyUnusedIdentifierChecker<'a, 'cx> {
    c: &'a TyChecker<'cx>,
}

impl<'a, 'cx> PotentiallyUnusedIdentifierChecker<'a, 'cx> {
    fn push_unused_parameter_error(&self, diag: BoxedDiag, diags: &mut Vec<BoxedDiag>) {
        if !self.c.config.compiler_options().no_unused_parameters() {
            return;
        }
        diags.push(diag);
    }

    fn push_unused_local_error(&self, diag: BoxedDiag, diags: &mut Vec<BoxedDiag>) {
        if !self.c.config.compiler_options().no_unused_locals() {
            return;
        }
        diags.push(diag);
    }

    fn error_unused_local(
        &self,
        declaration: ast::NodeID,
        name: String,
        diags: &mut Vec<BoxedDiag>,
    ) {
        if !self.c.config.compiler_options().no_unused_locals() {
            return;
        }
        let n = self.c.p.node(declaration);
        let span = n.name().map(|n| n.span()).unwrap_or_else(|| n.span());
        if self
            .c
            .node_query(declaration.module())
            .is_type_decl(declaration)
        {
            let error = errors::XIsDeclaredButNeverUsed { span, name };
            diags.push(Box::new(error));
        } else {
            let error = errors::XIsDeclaredButItsValueIsNeverRead { span, name };
            diags.push(Box::new(error));
        }
    }

    fn check_unused_identifier(&self, n: PotentiallyUnusedIdentifier<'cx>) -> Vec<BoxedDiag> {
        let mut diags = vec![];
        match n {
            PotentiallyUnusedIdentifier::InferTy(_) => {}
            PotentiallyUnusedIdentifier::FnDecl(ast::FnDecl { body, id, .. })
            | PotentiallyUnusedIdentifier::ClassCtor(ast::ClassCtor { body, id, .. })
            | PotentiallyUnusedIdentifier::ClassMethodElem(ast::ClassMethodElem {
                body, id, ..
            }) => {
                if body.is_some() {
                    let Some(locals) = self.c.binder.locals(*id) else {
                        unreachable!()
                    };
                    self.check_unused_locals_and_parameters(locals, &mut diags);
                }
                self.check_unused_type_parameters(*id, &mut diags);
            }
            PotentiallyUnusedIdentifier::ForInStmt(ast::ForInStmt { id, .. })
            | PotentiallyUnusedIdentifier::ForOfStmt(ast::ForOfStmt { id, .. })
            | PotentiallyUnusedIdentifier::FnExpr(ast::FnExpr { id, .. })
            | PotentiallyUnusedIdentifier::BlockModuleDecl(ast::BlockModuleDecl { id, .. }) => {
                let Some(locals) = self.c.binder.locals(*id) else {
                    unreachable!()
                };
                self.check_unused_locals_and_parameters(locals, &mut diags);
            }
            PotentiallyUnusedIdentifier::ClassDecl(ast::ClassDecl { elems, id, .. })
            | PotentiallyUnusedIdentifier::ClassExpr(ast::ClassExpr { elems, id, .. }) => {
                self.check_unused_class_members(elems, &mut diags);
                self.check_unused_type_parameters(*id, &mut diags);
            }
            PotentiallyUnusedIdentifier::TypeAliasDecl(ast::TypeAliasDecl { id, .. }) => {
                self.check_unused_type_parameters(*id, &mut diags);
            }
        }
        diags
    }

    fn is_type_parameter_unused(&self, ty_param: &'cx ast::TyParam<'cx>) -> bool {
        let symbol = self.c.final_res(ty_param.id);
        let symbol = self.c.get_merged_symbol(symbol);
        let s = self.c.symbol(symbol);
        !s.is_referenced
            .is_some_and(|used| used.contains(SymbolFlags::TYPE_PARAMETER))
            && !self.is_identifier_that_starts_with_underscore(ty_param.name)
    }

    fn check_unused_type_parameters(&self, id: ast::NodeID, diags: &mut Vec<BoxedDiag>) {
        if !self.c.config.compiler_options().no_unused_parameters() {
            return;
        }
        let symbol = self.c.final_res(id);
        let Some(declarations) = self.c.symbol(symbol).decls.as_ref() else {
            return;
        };
        if declarations.last() != Some(&id) {
            return;
        }
        let ty_parameters = self.c.get_effective_ty_param_decls(id);
        for ty_parameter in ty_parameters {
            if !self.is_type_parameter_unused(ty_parameter) {
                continue;
            }
            let error = errors::XIsDeclaredButItsValueIsNeverRead {
                span: ty_parameter.name.span,
                name: self.c.atoms.get(ty_parameter.name.name).to_string(),
            };
            self.push_unused_parameter_error(Box::new(error), diags);
        }
    }

    fn check_unused_class_members(
        &self,
        elements: &'cx ast::ClassElems<'cx>,
        diags: &mut Vec<BoxedDiag>,
    ) {
        for &element in elements.list {
            match element.kind {
                ast::ClassElemKind::Setter(n)
                    if self
                        .c
                        .symbol(self.c.final_res(n.id))
                        .flags
                        .contains(SymbolFlags::GET_ACCESSOR) =>
                {
                    continue;
                }
                ast::ClassElemKind::Prop(ast::ClassPropElem {
                    id,
                    modifiers,
                    name,
                    ..
                })
                | ast::ClassElemKind::Method(ast::ClassMethodElem {
                    id,
                    modifiers,
                    name,
                    ..
                })
                | ast::ClassElemKind::Getter(ast::GetterDecl {
                    id,
                    modifiers,
                    name,
                    ..
                })
                | ast::ClassElemKind::Setter(ast::SetterDecl {
                    id,
                    modifiers,
                    name,
                    ..
                }) => {
                    let symbol = self.c.final_res(*id);
                    let s = self.c.symbol(symbol);
                    if s.is_referenced.is_none()
                        && (modifiers
                            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::PRIVATE))
                            || matches!(name.kind, ast::PropNameKind::PrivateIdent(_)))
                        && !self.c.node_flags(*id).contains(ast::NodeFlags::AMBIENT)
                    {
                        let error = errors::XIsDeclaredButItsValueIsNeverRead {
                            span: name.span(),
                            name: pprint_prop_name(&name.kind, &self.c.atoms),
                        };
                        diags.push(Box::new(error));
                    }
                }
                _ => {}
            }
        }
    }

    fn is_identifier_that_starts_with_underscore(&self, name: &'cx ast::Ident) -> bool {
        let name = self.c.atoms.get(name.name);
        name.starts_with('_')
    }

    fn check_unused_locals_and_parameters(&self, locals: &SymbolTable, diags: &mut Vec<BoxedDiag>) {
        let mut unused_variable_group = fx_indexmap_with_capacity(0);

        let add_unused_variable =
            |group: &mut FxIndexMap<ast::NodeID, Vec<&'cx ast::VarDecl<'cx>>>,
             parent: ast::NodeID,
             current: &'cx ast::VarDecl<'cx>| {
                group.entry(parent).or_default().push(current);
            };

        let try_get_root_parameter_declaration =
            |id: ast::NodeID| -> Option<&'cx ast::ParamDecl<'cx>> {
                let n = self.c.node_query(id.module()).get_root_decl(id);
                self.c.p.node(n).as_param_decl()
            };
        for &local in locals.0.values() {
            let s = self.c.symbol(local);
            if s.export_symbol.is_some()
                || (s.flags.contains(SymbolFlags::TYPE_PARAMETER)
                    && !(s.flags.intersects(SymbolFlags::VARIABLE)
                        && !(s
                            .is_referenced
                            .is_some_and(|r| r.intersects(SymbolFlags::VARIABLE)))))
                || s.is_referenced.is_some()
            {
                continue;
            }

            if let Some(declarations) = s.decls.as_ref() {
                for &declaration in declarations {
                    let n = self.c.p.node(declaration);
                    // TODO: is_valid_unused_local_declaration
                    match n {
                        // TODO: is_imported
                        // TODO: ast::Node::ArrayBinding(_) | ast::Node::ObjectBindingElem(_) if
                        ast::Node::VarDecl(n) => {
                            let nq = self.c.node_query(declaration.module());
                            let kind = nq
                                .get_combined_node_flags(declaration)
                                .intersection(ast::NodeFlags::BLOCK_SCOPED);
                            if kind != ast::NodeFlags::USING
                                && kind != ast::NodeFlags::AWAIT_USING
                                && nq.get_name_of_declaration(declaration).is_none_or(|name| {
                                    match name {
                                        ast::DeclarationName::Ident(name) => {
                                            !self.is_identifier_that_starts_with_underscore(name)
                                        }
                                        _ => true,
                                    }
                                })
                            {
                                add_unused_variable(
                                    &mut unused_variable_group,
                                    self.c.parent(declaration).unwrap(),
                                    n,
                                );
                            }
                        }
                        _ => {
                            if let Some(id) = s.value_decl
                                && let Some(p) = try_get_root_parameter_declaration(id)
                                && let Some(name) =
                                    self.c.node_query(id.module()).get_name_of_declaration(id)
                            {
                                if !p.is_parameter_property_declaration() {
                                    // TODO: bbinding
                                    let error = errors::XIsDeclaredButItsValueIsNeverRead {
                                        span: name.span(),
                                        name: s.name.to_string(&self.c.atoms),
                                    };
                                    diags.push(Box::new(error));
                                }
                            } else {
                                let name = s.name.to_string(&self.c.atoms);
                                self.error_unused_local(declaration, name, diags);
                            }
                        }
                    }
                }
            }
        }

        for (parent, declarations) in unused_variable_group {
            let has_same_length = match self.c.node(parent) {
                ast::Node::ForInStmt(ast::ForInStmt { init, .. })
                | ast::Node::ForOfStmt(ast::ForOfStmt { init, .. }) => match init {
                    ast::ForInitKind::Var(list) => list.len() == declarations.len(),
                    ast::ForInitKind::Expr(_) => unreachable!(),
                },
                ast::Node::ForStmt(n) => n.init.is_some_and(|init| match init {
                    ast::ForInitKind::Var(list) => list.len() == declarations.len(),
                    ast::ForInitKind::Expr(_) => unreachable!(),
                }),
                ast::Node::VarStmt(n) => n.list.len() == declarations.len(),
                _ => todo!(),
            };
            if has_same_length {
                if declarations.len() == 1 {
                    let declaration = declarations[0];
                    let name = declaration.name;
                    let error = errors::XIsDeclaredButItsValueIsNeverRead {
                        span: name.span,
                        name: pprint_binding(name, &self.c.atoms),
                    };
                    diags.push(Box::new(error));
                } else {
                    let error = errors::AllVariablesAreUnused {
                        span: self.c.node(parent).span(),
                    };
                    diags.push(Box::new(error));
                }
            } else {
                for _declaration in declarations {
                    todo!()
                }
            }
        }
    }
}

macro_rules! register_potentially_unused {
    (
        $(
            [$name: ident, $ident_name: ident]
        ),*
        $(,)?
    ) => {
        impl<'cx> TyChecker<'cx> {
            $(
                paste::paste! {
                    pub(super) fn [<register_potentially_unused_ $name>](
                        &mut self,
                        n: &'cx ast::$ident_name<'cx>,
                    ) {
                        let n = PotentiallyUnusedIdentifier::$ident_name(n);
                        let p = &self.p;
                        self.potentially_unused_identifiers.insert(p, n);
                    }
                }
            )*
        }

        impl<'cx> PotentiallyUnusedIdentifier<'cx> {
            fn id(&self) -> ast::NodeID {
                match self {
                    $(
                        PotentiallyUnusedIdentifier::$ident_name(n) => n.id,
                    )*
                }
            }
        }

    };
}

register_potentially_unused!(
    [infer_type, InferTy],
    [function_declaration, FnDecl],
    [function_expression, FnExpr],
    [class_constructor_declaration, ClassCtor],
    [for_in_statement, ForInStmt],
    [for_of_statement, ForOfStmt],
    [class_declaration, ClassDecl],
    [class_expression, ClassExpr],
    [type_alias_declaration, TypeAliasDecl],
    [class_method_element, ClassMethodElem],
    [block_module_declaration, BlockModuleDecl]
);

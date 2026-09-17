use bolt_ts_ast as ast;
use bolt_ts_binder::{SymbolFlags, SymbolID, SymbolTable};
use bolt_ts_checker_errors as errors;
use bolt_ts_errors::BoxedDiag;
use bolt_ts_parser::ParsedMap;
use bolt_ts_ty::CheckFlags;
use bolt_ts_utils::FxIndexSet;

use super::TyChecker;

#[derive(Debug, Clone)]
pub enum PotentiallyUnusedIdentifier<'cx> {
    InferTy(&'cx ast::InferTy<'cx>),
    FnDecl(&'cx ast::FnDecl<'cx>),
    ClassCtor(&'cx ast::ClassCtor<'cx>),
}

impl PotentiallyUnusedIdentifier<'_> {
    fn id(&self) -> ast::NodeID {
        match self {
            PotentiallyUnusedIdentifier::InferTy(n) => n.id,
            PotentiallyUnusedIdentifier::FnDecl(n) => n.id,
            PotentiallyUnusedIdentifier::ClassCtor(n) => n.id,
        }
    }
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
    pub(super) fn register_potentially_unused_infer_type_node(
        &mut self,
        n: &'cx ast::InferTy<'cx>,
    ) {
        let n = PotentiallyUnusedIdentifier::InferTy(n);
        let p = &self.p;
        self.potentially_unused_identifiers.insert(p, n);
    }

    pub(super) fn register_potentially_unused_function_declaration(
        &mut self,
        n: &'cx ast::FnDecl<'cx>,
    ) {
        let n = PotentiallyUnusedIdentifier::FnDecl(n);
        let p = &self.p;
        self.potentially_unused_identifiers.insert(p, n);
    }

    pub(super) fn register_potentially_unused_class_constructor_declaration(
        &mut self,
        n: &'cx ast::ClassCtor<'cx>,
    ) {
        let n = PotentiallyUnusedIdentifier::ClassCtor(n);
        let p = &self.p;
        self.potentially_unused_identifiers.insert(p, n);
    }

    pub(super) fn mark_property_as_referenced(&mut self, prop: SymbolID) {
        // TODO: more case
        let check_flags = self.get_check_flags(prop);
        let prop = if check_flags.contains(CheckFlags::INSTANTIATED) {
            self.get_symbol_links(prop).expect_target()
        } else {
            prop
        };
        self.binder.symbol_mut(prop).is_referenced = Some(SymbolFlags::all());
    }

    pub fn check_unused_identifiers(&mut self) {
        if !self.config.compiler_options().no_unused_locals()
            && !self.config.compiler_options().no_unused_parameters()
        {
            return;
        }
        let n = std::mem::take(&mut self.potentially_unused_identifiers.0);
        debug_assert!(n.is_sorted_by(|a, b| a.id().module().as_u32() < b.id().module().as_u32()));
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
    fn check_unused_identifier(&self, n: PotentiallyUnusedIdentifier<'cx>) -> Vec<BoxedDiag> {
        let mut diags = vec![];
        match n {
            PotentiallyUnusedIdentifier::InferTy(_) => {}
            PotentiallyUnusedIdentifier::FnDecl(n) => {
                if n.body.is_some() {
                    let Some(locals) = self.c.binder.locals(n.id) else {
                        unreachable!()
                    };
                    self.check_unused_locals_and_parameters(locals, &mut diags);
                }
            }
            PotentiallyUnusedIdentifier::ClassCtor(n) => {
                if n.body.is_some() {
                    let Some(locals) = self.c.binder.locals(n.id) else {
                        unreachable!()
                    };
                    self.check_unused_locals_and_parameters(locals, &mut diags);
                }
            }
        }
        diags
    }

    fn check_unused_locals_and_parameters(&self, locals: &SymbolTable, diags: &mut Vec<BoxedDiag>) {
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

            if let Some(id) = s.value_decl
                && let Some(p) = try_get_root_parameter_declaration(id)
                && let Some(name) = self.c.node_query(id.module()).get_name_of_declaration(id)
                && !p.is_parameter_property_declaration()
            {
                let error = errors::XIsDeclaredButItsValueIsNeverRead {
                    span: name.span(),
                    name: s.name.to_string(&self.c.atoms),
                };
                diags.push(Box::new(error));
            }
        }
    }
}

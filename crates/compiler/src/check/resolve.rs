use super::TyChecker;
use crate::atoms::AtomId;
use crate::bind::{Symbol, SymbolID, SymbolKind, SymbolName};
use crate::{ast, errors, keyword};

#[derive(Debug, Clone, Copy)]
pub enum ExpectedArgsCount {
    Count(usize),
    Range { lo: usize, hi: usize },
}

impl std::fmt::Display for ExpectedArgsCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedArgsCount::Count(c) => write!(f, "{c}"),
            ExpectedArgsCount::Range { lo, hi } => write!(f, "{lo}-{hi}"),
        }
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn resolve_symbol_by_ident(
        &mut self,
        ident: &'cx ast::Ident,
        ns: impl Fn(&SymbolKind) -> bool,
    ) -> SymbolID {
        if let Some(id) = self.binder.get(ident.id.module()).final_res.get(&ident.id) {
            return *id;
        }
        let res = resolve_symbol_by_ident(self, ident, ns);
        self.binder
            .get_mut(ident.id.module())
            .final_res
            .insert(ident.id, res);
        res
    }

    pub(super) fn on_failed_to_resolve_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        mut error: errors::CannotFindName,
    ) -> errors::CannotFindName {
        if let Some(e) = self.check_missing_prefix(ident) {
            error.errors.push(e);
        }
        if let Some(e) = self.check_using_type_as_value(ident) {
            error.errors.push(e);
        }
        error
    }

    fn check_missing_prefix(&mut self, ident: &'cx ast::Ident) -> Option<crate::Diag> {
        let mut location = ident.id;
        loop {
            if let Some(parent) = self.p.get(ident.id.module()).parent_map().parent(location) {
                location = parent;
            } else {
                break;
            }
            let node = self.p.get(ident.id.module()).nodes().get(location);
            let ast::Node::ClassDecl(class) = node else {
                continue;
            };
            // TODO: use class symbol;
            if let Some(prop) = class.elems.elems.iter().find_map(|ele| {
                let ast::ClassEleKind::Prop(prop) = ele.kind else {
                    return None;
                };
                let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                    return None;
                };

                (prop_name.name == ident.name).then(|| prop)
            }) {
                if prop
                    .modifiers
                    .map(|mods| {
                        mods.list
                            .iter()
                            .any(|m| m.kind == ast::ModifierKind::Static)
                    })
                    .unwrap_or_default()
                {
                    let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                        unreachable!()
                    };
                    let error = errors::DidYourMeanTheStaticMember {
                        span: prop.name.span(),
                        name: format!(
                            "{}.{}",
                            self.atoms.get(class.name.name),
                            self.atoms.get(prop_name.name)
                        ),
                    };
                    return Some(Box::new(error));
                }
            }
        }

        None
    }

    fn is_prim_ty_name(&self, name: AtomId) -> bool {
        name == keyword::IDENT_ANY
            || name == keyword::IDENT_NUMBER
            || name == keyword::IDENT_STRING
            || name == keyword::IDENT_BOOLEAN
            || name == keyword::IDENT_NEVER
            || name == keyword::IDENT_UNKNOWN
    }

    pub fn check_using_type_as_value(&mut self, ident: &'cx ast::Ident) -> Option<crate::Diag> {
        if self.is_prim_ty_name(ident.name) {
            let Some(grand) = self
                .p
                .get(ident.id.module())
                .parent_map()
                .parent(ident.id)
                .and_then(|id| self.p.get(ident.id.module()).parent_map().parent(id))
            else {
                return None;
            };
            if self
                .p
                .get(ident.id.module())
                .nodes()
                .get(grand)
                .is_class_like()
            {
                return Some(Box::new(errors::AClassCannotImplementAPrimTy {
                    span: ident.span,
                    ty: self.atoms.get(ident.name).to_string(),
                }));
            }
        }

        None
    }
}

fn resolve_symbol_by_ident(
    checker: &TyChecker,
    ident: &ast::Ident,
    ns: impl Fn(&SymbolKind) -> bool,
) -> SymbolID {
    let module = ident.id.module();
    let module = checker.binder.get(module);
    assert!(!module.final_res.contains_key(&ident.id));
    let name = ident.name;
    let Some(mut scope_id) = module.node_id_to_scope_id.get(&ident.id).copied() else {
        return Symbol::ERR;
    };
    let res = loop {
        if let Some(id) = module.res.get(&(scope_id, SymbolName::Normal(name))) {
            if ns(&module.symbols.get(*id).kind) {
                break *id;
            }
        }
        if let Some(parent) = module.scope_id_parent_map[&scope_id] {
            scope_id = parent;
        } else {
            break Symbol::ERR;
        }
    };
    res
}

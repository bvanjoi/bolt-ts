use super::TyChecker;
use crate::ast::{ClassEle, ClassEleKind};
use crate::bind::{Symbol, SymbolID, SymbolName};
use crate::{ast, errors};

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
    pub(super) fn resolve_symbol_by_ident(&mut self, ident: &'cx ast::Ident) -> SymbolID {
        if let Some(id) = self.final_res.get(&ident.id) {
            return *id;
        }
        let res = resolve_symbol_by_ident(self, ident);
        self.final_res.insert(ident.id, res);
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

        error
    }

    fn check_missing_prefix(&mut self, ident: &'cx ast::Ident) -> Option<crate::Diag> {
        let mut location = ident.id;
        loop {
            if let Some(parent) = self.node_parent_map.parent(location) {
                location = parent;
            } else {
                break;
            }
            let node = self.nodes.get(location);
            let ast::Node::ClassDecl(class) = node else {
                continue;
            };
            // TODO: use class symbol;
            if let Some(prop) = class.eles.iter().find_map(|ele| {
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
}

fn resolve_symbol_by_ident(checker: &TyChecker, ident: &ast::Ident) -> SymbolID {
    assert!(!checker.final_res.contains_key(&ident.id));
    let name = ident.name;
    let Some(mut scope_id) = checker.node_id_to_scope_id.get(&ident.id).copied() else {
        return Symbol::ERR;
    };
    let res = loop {
        if let Some(id) = checker.res.get(&(scope_id, SymbolName::Normal(name))) {
            break *id;
        }
        if let Some(parent) = checker.scope_id_parent_map[&scope_id] {
            scope_id = parent;
        } else {
            break Symbol::ERR;
        }
    };
    res
}

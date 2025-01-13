mod errors;

use bolt_ts_atom::AtomMap;
use bolt_ts_span::ModuleID;

use crate::ast::{self, pprint_ident, visitor};
use crate::ir;
use crate::keyword::is_reserved_type_name;
use crate::parser::Parser;

pub fn well_formed_check_parallel(
    p: &Parser,
    atoms: &AtomMap,
    modules: &[bolt_ts_span::Module],
) -> Vec<bolt_ts_errors::Diag> {
    use rayon::prelude::*;

    modules
        .into_par_iter()
        .flat_map(|m| {
            let diags = well_formed_check(p, atoms, m.id);
            assert!(!m.global || diags.is_empty());
            diags
        })
        .collect::<Vec<_>>()
}

fn well_formed_check(
    p: &Parser,
    atoms: &AtomMap,
    module_id: ModuleID,
) -> Vec<bolt_ts_errors::Diag> {
    let mut s = CheckState {
        p,
        atoms,
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
}

impl<'cx> CheckState<'cx> {
    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error })
    }
    fn check_collisions_for_decl_name(&mut self, node: ast::NodeID, name: &'cx ast::Ident) {
        if self.p.node(node).is_class_like() && is_reserved_type_name(name.name) {
            let error = errors::ClassNameCannotBe {
                span: name.span,
                name: pprint_ident(name, self.atoms),
            };
            self.push_error(Box::new(error));
        }
    }
    fn check_class_like(&mut self, class: &impl ir::ClassLike<'cx>) {
        if let Some(name) = class.name() {
            self.check_collisions_for_decl_name(class.id(), name);
        };
    }
}

impl<'cx> ast::Visitor<'cx> for CheckState<'cx> {
    fn visit_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.check_class_like(class);
        visitor::visit_class_decl(self, class);
    }
}

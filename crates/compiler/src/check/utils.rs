use crate::ast;

use super::TyChecker;

fn get_assignment_target(checker: &TyChecker, id: ast::NodeID) -> Option<ast::NodeID> {
    let mut parent = checker.node_parent_map.parent(id);
    while let Some(p) = parent {
        match checker.nodes.get(p) {
            ast::Node::AssignExpr(assign) => {
                return if let ast::ExprKind::Ident(binding) = assign.left.kind {
                    (binding.id == id).then(|| assign.id)
                } else {
                    None
                }
            },
            ast::Node::BinExpr(_) => return None,
            _ => return None,
        }
        // parent = checker.node_parent_map.parent(p);
    }
    None
}

#[derive(PartialEq)]
pub enum AssignmentKind {
    None,
    Definite,
    // Compound,
}

pub fn get_assignment_kind(checker: &TyChecker, id: ast::NodeID) -> AssignmentKind {
    let Some(target) = get_assignment_target(checker, id) else {
        return AssignmentKind::None;
    };
    match checker.nodes.get(target) {
        ast::Node::AssignExpr(_) => AssignmentKind::Definite,
        ast::Node::BinExpr(_) => AssignmentKind::Definite,
        _ => AssignmentKind::Definite,
    }
}

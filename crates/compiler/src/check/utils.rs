use crate::{ast, parser};

use super::TyChecker;

fn get_assignment_target(checker: &TyChecker, id: ast::NodeID) -> Option<ast::NodeID> {
    let mut parent = checker.p.parent(id);
    while let Some(p) = parent {
        match checker.p.node(p) {
            ast::Node::AssignExpr(assign) => {
                return if let ast::ExprKind::Ident(binding) = assign.left.kind {
                    (binding.id == id).then(|| assign.id)
                } else {
                    None
                }
            }
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
    match checker.p.node(target) {
        ast::Node::AssignExpr(_) => AssignmentKind::Definite,
        ast::Node::BinExpr(_) => AssignmentKind::Definite,
        _ => AssignmentKind::Definite,
    }
}

pub fn find_ancestor<'cx>(
    p: &'cx parser::Parser<'cx>,
    id: ast::NodeID,
    cb: impl Fn(ast::Node<'cx>) -> Option<bool>,
) -> Option<ast::NodeID> {
    let mut id = id;
    loop {
        let node = p.node(id);
        if let Some(res) = cb(node) {
            if res {
                return Some(id);
            } else {
                return None;
            }
        }
        if let Some(next) = p.parent(id) {
            id = next
        } else {
            return None;
        }
    }
}

pub fn append_if_unique<'a, T>(array: &mut Vec<&'a T>, value: &'a T) {
    if array.iter().all(|item| !std::ptr::eq(item, &value)) {
        array.push(value);
    }
}

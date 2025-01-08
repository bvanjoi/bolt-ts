use bolt_ts_span::ModuleID;

use super::ast;
use super::Parser;

#[derive(PartialEq)]
pub enum AssignmentKind {
    None,
    Definite,
    Compound,
}

impl<'cx> Parser<'cx> {
    #[inline(always)]
    pub fn root(&self, id: ModuleID) -> &ast::Program<'cx> {
        self.get(id).root()
    }

    #[inline(always)]
    pub fn node(&self, id: ast::NodeID) -> ast::Node<'cx> {
        self.get(id.module()).nodes.get(id)
    }

    #[inline(always)]
    pub fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        self.get(id.module()).parent_map.parent(id)
    }

    pub fn is_descendant_of(&self, node: ast::NodeID, ancestor: ast::NodeID) -> bool {
        self.find_ancestor(node, |node| (node.id() == ancestor).then_some(true))
            .is_some()
    }

    pub fn find_ancestor(
        &self,
        mut id: ast::NodeID,
        cb: impl Fn(ast::Node<'cx>) -> Option<bool>,
    ) -> Option<ast::NodeID> {
        loop {
            let node = self.node(id);
            if let Some(res) = cb(node) {
                if res {
                    return Some(id);
                } else {
                    return None;
                }
            }
            if let Some(parent) = self.parent(id) {
                id = parent
            } else {
                return None;
            }
        }
    }

    pub fn get_iife(&self, node: ast::NodeID) -> Option<&'cx ast::CallExpr<'cx>> {
        let n = self.node(node);
        if n.is_fn_expr() || n.is_arrow_fn_expr() {
            let mut prev = node;
            let parent_id = self.parent(node).unwrap();
            let mut parent = self.node(parent_id);
            while parent.is_paren_expr() {
                prev = parent_id;
                parent = self.node(self.parent(parent_id).unwrap());
            }
            if let Some(call) = parent.as_call_expr() {
                if call.expr.id() == prev {
                    return Some(call);
                }
            }
        }
        None
    }

    fn get_assignment_target(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        let parent = self.parent(id);
        while let Some(p) = parent {
            match self.node(p) {
                ast::Node::AssignExpr(assign) => {
                    return if let ast::ExprKind::Ident(binding) = assign.left.kind {
                        (binding.id == id).then_some(assign.id)
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

    pub fn get_assignment_kind(&self, id: ast::NodeID) -> AssignmentKind {
        let Some(target) = self.get_assignment_target(id) else {
            return AssignmentKind::None;
        };
        match self.node(target) {
            ast::Node::AssignExpr(_) => AssignmentKind::Definite,
            ast::Node::BinExpr(_) => AssignmentKind::Definite,
            _ => AssignmentKind::Definite,
        }
    }

    pub fn is_method_access_for_call(&self, id: ast::NodeID) -> bool {
        let mut id = id;
        while let Some(parent) = self.parent(id) {
            if self.node(parent).is_paren_expr() {
                id = parent;
            } else {
                break;
            }
        }
        let Some(parent) = self.parent(id) else {
            return false;
        };
        let p = self.node(parent);
        if let Some(call) = p.as_call_expr() {
            call.expr.id() == id
        } else if let Some(new) = p.as_new_expr() {
            new.expr.id() == id
        } else {
            false
        }
    }

    fn is_outer_expr(&self, id: ast::NodeID) -> bool {
        self.node(id).is_paren_expr()
    }

    fn skip_outer_expr(&self, mut id: ast::NodeID) -> ast::NodeID {
        while self.is_outer_expr(id) {
            let node = self.node(id);
            if let Some(child) = node.as_paren_expr() {
                id = child.id;
            }
        }
        id
    }

    pub fn skip_parens(&self, id: ast::NodeID) -> ast::NodeID {
        self.skip_outer_expr(id)
    }

    pub fn is_in_type_query(&self, id: ast::NodeID) -> bool {
        self.find_ancestor(id, |node| {
            if node.is_typeof_expr() || node.is_typeof_ty() {
                Some(true)
            } else if node.is_ident() {
                // TODO: qualified name
                None
            } else {
                Some(false)
            }
        })
        .is_some()
    }

    pub fn get_this_container(
        &self,
        mut id: ast::NodeID,
        include_arrow_fn: bool,
        include_class_computed_prop_name: bool,
    ) -> ast::NodeID {
        assert!(self.parent(id).is_some());
        while let Some(parent) = self.parent(id) {
            id = parent;
            let node = self.node(id);
            if node.is_arrow_fn_expr() {
                if !include_arrow_fn {
                    continue;
                } else {
                    return id;
                }
            } else if node.is_fn_decl()
                || node.is_fn_expr()
                || node.is_class_static_block_decl()
                || node.is_class_prop_ele()
                || node.is_class_method_ele()
                || node.is_class_ctor()
                || node.is_ctor_sig_decl()
            {
                return id;
            }
        }

        unreachable!();
    }

    pub fn is_object_lit_method(&self, id: ast::NodeID) -> bool {
        // TODO: handle this after parse method in object
        false
    }
}

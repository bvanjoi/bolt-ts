use bolt_ts_span::ModuleID;

use crate::ast::CallExpr;

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

    pub fn get_enclosing_blockscope_container(&self, id: ast::NodeID) -> ast::NodeID {
        let Some(parent_id) = self.parent(id) else {
            unreachable!()
        };
        self.find_ancestor(parent_id, |current| {
            let parent = self.parent(current.id()).map(|p| self.node(p));
            if current.is_block_scope(parent.as_ref()) {
                Some(true)
            } else {
                None
            }
        })
        .unwrap()
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

    pub fn skip_outer_expr(&self, mut id: ast::NodeID) -> ast::NodeID {
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

    pub fn get_containing_class(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        let parent = self.parent(id)?;
        self.find_ancestor(parent, |node| node.is_class_like().then_some(true))
    }

    pub fn get_containing_fn(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        let parent = self.parent(id)?;
        self.find_ancestor(parent, |node| node.is_fn_decl_like().then_some(true))
    }

    pub fn is_object_lit_method(&self, id: ast::NodeID) -> bool {
        // TODO: handle this after parse method in object
        self.node(id).is_object_method_member()
    }

    pub fn access_kind(&self, id: ast::NodeID) -> AccessKind {
        let Some(p) = self.parent(id) else {
            return AccessKind::Read;
        };
        use ast::Node::*;
        match self.node(p) {
            AssignExpr(n) => {
                if n.left.id() == id {
                    if n.op == ast::AssignOp::Eq {
                        AccessKind::Write
                    } else {
                        AccessKind::ReadWrite
                    }
                } else {
                    AccessKind::Read
                }
            }
            _ => AccessKind::Read,
        }
    }

    pub fn is_this_in_type_query(&self, mut id: ast::NodeID) -> bool {
        let mut n = self.node(id);
        if !n.is_this_expr() {
            return false;
        }

        loop {
            let Some(p_id) = self.parent(id) else {
                break;
            };
            let p = self.node(p_id);
            if let Some(qualified) = p.as_qualified_name() {
                if qualified.left.id() == id {
                    n = p;
                    id = p_id;
                    continue;
                }
            }
            break;
        }

        n.is_type_decl()
    }

    pub fn is_decl_name(&self, id: ast::NodeID) -> bool {
        self.parent(id).is_some_and(|p| self.node(p).is_decl())
    }

    pub fn is_decl_name_or_import_prop_name(&self, id: ast::NodeID) -> bool {
        use ast::Node::*;
        self.parent(id).is_some_and(|p| match self.node(p) {
            ImportNamedSpec(_) | ExportNamedSpec(_) => {
                matches!(self.node(id), Ident(_) | StringLit(_))
            }
            _ => self.is_decl_name(id),
        })
    }

    pub fn is_import_or_export_spec(&self, id: ast::NodeID) -> bool {
        let n = self.node(id);
        n.is_import_named_spec() || n.is_export_named_spec()
    }

    pub fn get_immediately_invoked_fn_expr(&self, id: ast::NodeID) -> Option<&'cx CallExpr<'cx>> {
        let n = self.node(id);
        if n.is_fn_expr() || n.is_arrow_fn_expr() {
            let mut prev = id;
            let mut parent_id = self.parent(id)?;
            let mut parent = self.node(parent_id);
            while parent.is_paren_expr() {
                prev = parent_id;
                parent_id = self.parent(parent_id)?;
                parent = self.node(parent_id);
            }
            if let Some(call) = parent.as_call_expr() {
                if call.expr.id() == prev {
                    return Some(call);
                }
            }
        }
        None
    }

    pub fn get_root_decl(&self, mut id: ast::NodeID) -> ast::NodeID {
        let n = self.node(id);
        while n.is_object_binding_elem() {
            let p = self.parent(id).unwrap();
            id = self.parent(p).unwrap();
        }
        id
    }

    pub fn get_control_flow_container(&self, node: ast::NodeID) -> ast::NodeID {
        let parent = self.parent(node).unwrap();
        self.find_ancestor(parent, |n| {
            if (n.is_fn_like() && self.get_immediately_invoked_fn_expr(node).is_none())
                || n.is_program()
                || n.is_class_prop_ele()
                || n.is_block_stmt()
            {
                Some(true)
            } else {
                None
            }
        })
        .unwrap()
    }

    pub fn is_resolved_by_ty_alias(&self, node: ast::NodeID) -> bool {
        let Some(p) = self.parent(node) else {
            return false;
        };
        self.find_ancestor(p, |n| {
            use ast::Node::*;
            match n {
                TypeDecl(_) => Some(true),
                // TODO: ParenTy
                ReferTy(_) | UnionTy(_) | IntersectionTy(_) | IndexedAccessTy(_) | CondTy(_)
                | TyOp(_) | ArrayTy(_) | TupleTy(_) => None,
                _ => Some(false),
            }
        })
        .is_some()
    }

    pub fn is_const_context(&self, node: ast::NodeID) -> bool {
        let Some(parent) = self.parent(node) else {
            return false;
        };
        let p = self.node(parent);
        if p.is_assertion_expr() {
            let ty = match p {
                ast::Node::AsExpr(n) => n.ty,
                _ => unreachable!(),
            };
            ty.is_const_ty_refer()
        } else if p.is_array_lit() {
            self.is_const_context(parent)
        } else {
            false
        }
    }

    pub fn index_of_node(&self, elements: &[&'cx ast::Expr<'cx>], id: ast::NodeID) -> usize {
        debug_assert!(elements.is_sorted_by_key(|probe| probe.span().lo));
        elements
            .binary_search_by_key(&self.node(id).span().lo, |probe| probe.span().lo)
            .unwrap()
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

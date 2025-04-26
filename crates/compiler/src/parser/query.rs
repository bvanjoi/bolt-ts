use bolt_ts_ast::keyword;
use bolt_ts_ast::keyword::is_prim_ty_name;
use bolt_ts_span::ModuleID;

use bolt_ts_ast::CallExpr;

use super::ParseResult;
use super::Parser;
use super::ast;
use super::bind::NodeQuery;

#[derive(PartialEq)]
pub enum AssignmentKind {
    None,
    Definite,
    Compound,
}

impl<'cx> NodeQuery<'cx> for ParseResult<'cx> {
    fn node(&self, id: bolt_ts_ast::NodeID) -> bolt_ts_ast::Node<'cx> {
        self.node(id)
    }

    fn parent(&self, id: bolt_ts_ast::NodeID) -> Option<bolt_ts_ast::NodeID> {
        self.parent(id)
    }

    fn node_flags(&self, id: bolt_ts_ast::NodeID) -> bolt_ts_ast::NodeFlags {
        self.node_flags(id)
    }

    fn is_external_or_commonjs_module(&self) -> bool {
        self.external_module_indicator.is_some() || self.commonjs_module_indicator.is_some()
    }

    fn is_external_module(&self) -> bool {
        self.external_module_indicator.is_some()
    }
}

impl<'cx> Parser<'cx> {
    #[inline(always)]
    pub fn root(&self, id: ModuleID) -> &ast::Program<'cx> {
        self.get(id).root()
    }

    #[inline(always)]
    pub fn node(&self, id: ast::NodeID) -> ast::Node<'cx> {
        self.get(id.module()).node(id)
    }

    #[inline(always)]
    pub fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        self.get(id.module()).parent(id)
    }

    pub fn is_descendant_of(&self, node: ast::NodeID, ancestor: ast::NodeID) -> bool {
        self.find_ancestor(node, |node| (node.id() == ancestor).then_some(true))
            .is_some()
    }

    pub fn find_ancestor(
        &self,
        id: ast::NodeID,
        cb: impl Fn(ast::Node<'cx>) -> Option<bool>,
    ) -> Option<ast::NodeID> {
        self.get(id.module()).find_ancestor(id, cb)
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

    fn get_assignment_target(&self, mut id: ast::NodeID) -> Option<ast::NodeID> {
        let mut parent = self.parent(id);
        use ast::Node::*;
        use ast::PostfixUnaryOp;
        use ast::PrefixUnaryOp;
        while let Some(p) = parent {
            match self.node(p) {
                AssignExpr(n) => {
                    return (n.left.id() == id).then_some(n.id);
                }
                PrefixUnaryExpr(n)
                    if matches!(n.op, PrefixUnaryOp::PlusPlus | PrefixUnaryOp::MinusMinus) =>
                {
                    return Some(n.id);
                }
                PostfixUnaryExpr(n)
                    if matches!(n.op, PostfixUnaryOp::PlusPlus | PostfixUnaryOp::MinusMinus) =>
                {
                    return Some(n.id);
                }
                // TODO: for_in and for_of
                ParenExpr(_) | ArrayLit(_) | NonNullExpr(_) => id = self.parent(p).unwrap(),
                SpreadAssignment(_) => {
                    id = self.parent(self.parent(p).unwrap()).unwrap();
                }
                _ => return None,
            }
            parent = self.parent(id);
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
        while let Some(parent) = self.parent(id) {
            id = parent;
            let node = self.node(id);
            if node.is_arrow_fn_expr() {
                if !include_arrow_fn {
                    continue;
                } else {
                    return id;
                }
            } else {
                use ast::Node::*;
                match node {
                    FnDecl(_) | FnExpr(_) | NamespaceDecl(_) | ClassPropElem(_)
                    | ClassMethodElem(_) | ClassCtor(_) | CtorSigDecl(_) | GetterDecl(_)
                    | SetterDecl(_) | IndexSigDecl(_) | EnumDecl(_) | Program(_)
                    | PropSignature(_) => return id,
                    _ => {}
                }
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
        use bolt_ts_ast::Node::*;
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

        n.is_type_alias_decl()
    }

    pub fn is_decl_name(&self, id: ast::NodeID) -> bool {
        self.parent(id).is_some_and(|p| self.node(p).is_decl())
    }

    pub fn is_decl_name_or_import_prop_name(&self, id: ast::NodeID) -> bool {
        use bolt_ts_ast::Node::*;
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

    pub fn is_part_of_ty_query(&self, n: ast::NodeID) -> bool {
        self.get(n.module()).is_part_of_ty_query(n)
    }

    pub fn is_object_lit_or_class_expr_method_or_accessor(&self, node: ast::NodeID) -> bool {
        self.get(node.module())
            .is_object_lit_or_class_expr_method_or_accessor(node)
    }

    pub fn get_immediately_invoked_fn_expr(&self, id: ast::NodeID) -> Option<&'cx CallExpr<'cx>> {
        self.get(id.module()).get_immediately_invoked_fn_expr(id)
    }

    pub fn get_root_decl(&self, id: ast::NodeID) -> ast::NodeID {
        self.get(id.module()).get_root_decl(id)
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
            use bolt_ts_ast::Node::*;
            match n {
                TypeAliasDecl(_) => Some(true),
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

    pub fn get_combined_flags<T: std::ops::BitOrAssign>(
        &self,
        id: ast::NodeID,
        get_flag: impl Fn(&Self, ast::NodeID) -> T,
    ) -> T {
        self.get(id.module())
            .get_combined_flags(id, |_, id| get_flag(self, id))
    }

    pub fn get_combined_node_flags(&self, id: ast::NodeID) -> ast::NodeFlags {
        self.get(id.module()).get_combined_node_flags(id)
    }

    pub fn get_combined_modifier_flags(
        &self,
        id: ast::NodeID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        self.get_combined_flags(id, |p, id| p.get_effective_modifier_flags(id))
    }

    pub fn get_effective_modifier_flags(
        &self,
        id: ast::NodeID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        self.get_modifier_flags(id, true, false)
    }

    fn get_modifier_flags(
        &self,
        id: ast::NodeID,
        include_js_doc: bool,
        always_include_js_doc: bool,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        let m = self.get_syntactic_modifier_flags_no_cache(id);
        ast::ModifierKind::get_syntactic_modifier_flags(m)
    }

    fn get_syntactic_modifier_flags_no_cache(
        &self,
        id: ast::NodeID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        self.get(id.module())
            .get_syntactic_modifier_flags_no_cache(id)
    }

    pub fn walk_up_paren_tys_and_get_parent_and_child(
        &self,
        n: ast::NodeID,
    ) -> (Option<&'cx ast::ParenTy<'cx>>, ast::NodeID) {
        let mut node = n;
        let mut child = None;
        loop {
            let Some(n) = self.node(node).as_paren_ty() else {
                break;
            };
            child = Some(n);
            let Some(n) = self.parent(node) else {
                break;
            };
            node = n;
        }
        (child, node)
    }

    pub fn is_part_of_ty_node(&self, n: ast::NodeID) -> bool {
        let mut node = self.node(n);
        if node.is_ty() {
            return true;
        }
        use ast::Node::*;
        match node {
            Ident(ident) if ident.name == keyword::KW_VOID => {
                let p = self.parent(n).unwrap();
                let p = self.node(p);
                return !matches!(p, VoidExpr(_));
            }
            Ident(ident) if is_prim_ty_name(ident.name) => return true,
            _ => (),
        }
        if let Ident(ident) = node {
            let p = self.parent(n).unwrap();
            let p = self.node(p);
            if p.as_qualified_name()
                .is_some_and(|p| std::ptr::eq(p.right, ident))
                || p.as_prop_access_expr()
                    .is_some_and(|p| std::ptr::eq(p.name, ident))
            {
                node = p;
            }
            assert!(node.is_ident() || node.is_qualified_name() || node.is_prop_access_expr());
        }
        match node {
            Ident(_) | QualifiedName(_) | PropAccessExpr(_) => {
                //TODO:
                false
            }
            _ => false,
        }
    }

    pub fn maybe_ty_param_reference(&self, node: ast::NodeID) -> bool {
        let Some(parent) = self.parent(node) else {
            return false;
        };
        let p = self.node(parent);
        use ast::Node::*;
        !match p {
            ReferTy(r) => r.ty_args.is_some() && node == r.name.id(),
            // TODO: import ty
            _ => false,
        }
    }

    pub fn get_annotated_accessor_ty_node(&self, node: ast::NodeID) -> Option<&'cx ast::Ty<'cx>> {
        let node = self.node(node);
        match node {
            ast::Node::GetterDecl(n) => n.ty,
            ast::Node::SetterDecl(n) => n.get_effective_ty_annotation_node(),
            ast::Node::PropSignature(_) => todo!(),
            _ => None,
        }
    }

    pub fn is_immediately_used_in_init_or_block_scoped_var(
        &self,
        decl: &'cx ast::VarDecl<'cx>,
        usage: ast::NodeID,
        decl_container: ast::NodeID,
    ) -> bool {
        let parent = self.parent(decl.id).unwrap();
        match self.node(parent) {
            ast::Node::VarStmt(_)
                if self.is_same_scope_descendent_of(usage, Some(decl.id), decl_container) =>
            {
                return true;
            }
            // TODO: handle other case
            _ => (),
        }

        let grand = self.parent(parent).unwrap();
        let g = self.node(grand);
        if let Some(g) = g.as_for_in_stmt() {
            self.is_same_scope_descendent_of(usage, Some(g.expr.id()), decl_container)
        } else if let Some(g) = g.as_for_of_stmt() {
            self.is_same_scope_descendent_of(usage, Some(g.expr.id()), decl_container)
        } else {
            false
        }
    }

    fn is_same_scope_descendent_of(
        &self,
        init: ast::NodeID,
        parent: Option<ast::NodeID>,
        stop_at: ast::NodeID,
    ) -> bool {
        let Some(parent) = parent else {
            return false;
        };
        self.find_ancestor(init, |n| {
            let id = n.id();
            if id == parent {
                Some(true)
            } else if id == stop_at
                || n.is_fn_like()
                    && (self.get_immediately_invoked_fn_expr(id).is_none()
                        || n.fn_flags().intersects(ast::FnFlags::ASYNC_GENERATOR))
            {
                Some(false)
            } else {
                None
            }
        })
        .is_some()
    }

    pub fn is_external_or_commonjs_module(&self, id: ast::NodeID) -> bool {
        self.get(id.module()).is_external_or_commonjs_module()
    }

    pub fn is_global_source_file(&self, id: ast::NodeID) -> bool {
        self.get(id.module()).is_global_source_file(id)
    }

    pub fn is_alias_symbol_decl(&self, id: ast::NodeID) -> bool {
        self.get(id.module()).is_alias_symbol_decl(id)
    }

    pub fn get_module_spec_for_import_or_export(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::StringLit> {
        self.get(id.module())
            .get_module_spec_for_import_or_export(id)
    }

    pub fn get_name_of_decl(&self, id: ast::NodeID) -> Option<ast::DeclarationName<'cx>> {
        self.get(id.module()).get_name_of_decl(id)
    }

    pub fn has_dynamic_name(&self, id: ast::NodeID) -> bool {
        self.get(id.module()).has_dynamic_name(id)
    }

    pub fn is_type_decl(&self, id: ast::NodeID) -> bool {
        self.get(id.module()).is_type_decl(id)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

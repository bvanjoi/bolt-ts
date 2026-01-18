use bolt_ts_ast as ast;
use bolt_ts_ast::keyword::{self, is_prim_ty_name};

use super::{
    AccessKind, ParentMap,
    container_flags::{ContainerFlags, container_flags_for_node},
};

pub struct NodeQuery<'cx, 'a> {
    parent_map: &'a ParentMap,
    parse_result: &'a bolt_ts_parser::ParseResultForGraph<'cx>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum ModuleInstanceState {
    NonInstantiated = 0,
    Instantiated = 1,
    ConstEnumOnly = 2,
}

#[derive(PartialEq)]
pub enum AssignmentKind {
    None,
    Definite,
    Compound,
}

impl<'cx, 'a> NodeQuery<'cx, 'a> {
    pub fn new(
        parent_map: &'a ParentMap,
        parse_result: &'a bolt_ts_parser::ParseResultForGraph<'cx>,
    ) -> Self {
        Self {
            parent_map,
            parse_result,
        }
    }

    pub fn node(&self, id: ast::NodeID) -> ast::Node<'cx> {
        self.parse_result.nodes.get(id)
    }

    pub fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        self.parent_map.parent(id)
    }

    pub fn node_flags(&self, id: ast::NodeID) -> ast::NodeFlags {
        self.parse_result.node_flags_map.get(id)
    }

    pub fn get_assigned_name(&self, id: ast::NodeID) -> Option<ast::DeclarationName<'cx>> {
        let parent = self.parent(id)?;
        let p = self.node(parent);
        use ast::Node::*;
        match p {
            ObjectBindingElem(n) => ast::DeclarationName::from_object_binding_name(n.name),
            VarDecl(n) => ast::DeclarationName::from_binding(n.name),
            Ident(n) => Some(ast::DeclarationName::Ident(n)),
            _ => None,
        }
    }

    pub fn get_name_of_decl(&self, id: ast::NodeID) -> Option<ast::DeclarationName<'cx>> {
        self.parse_result
            .nodes
            .get_non_assigned_name_of_decl(id)
            .or_else(|| {
                use ast::Node::*;
                let n = self.node(id);
                matches!(n, FnExpr(_) | ArrowFnExpr(_) | ClassExpr(_))
                    .then(|| self.get_assigned_name(id))
                    .flatten()
            })
    }

    pub fn has_dynamic_name(&self, id: ast::NodeID) -> bool {
        let Some(name) = self.get_name_of_decl(id) else {
            return false;
        };
        name.is_dynamic_name()
    }

    pub fn walkup_binding_elements_and_patterns(&self, binding: ast::NodeID) -> ast::NodeID {
        use ast::Node::*;
        let mut n = self.parent(binding).unwrap();
        debug_assert!(matches!(self.node(n), ObjectPat(_) | ArrayPat(_)));
        loop {
            let p = self.parent(n).unwrap();
            if self.node(p).is_binding() {
                n = p;
            } else {
                break p;
            }
        }
    }

    pub fn get_combined_flags<T: std::ops::BitOrAssign>(
        &self,
        mut id: ast::NodeID,
        get_flag: impl Fn(&Self, ast::NodeID) -> T,
    ) -> T {
        use ast::Node::*;
        let mut n = self.node(id);
        if matches!(n, ObjectBindingElem(_) | ArrayBinding(_)) {
            id = self.walkup_binding_elements_and_patterns(id);
            n = self.node(id);
        }
        let mut flags = get_flag(self, id);

        if n.is_var_decl() {
            id = self.parent(id).unwrap();
            n = self.node(id);
        }
        match n {
            VarStmt(stmt) => flags |= get_flag(self, stmt.id),
            ForOfStmt(stmt) => flags |= get_flag(self, stmt.id),
            // TODO: ForIn, For
            _ => {}
        }
        flags
    }

    pub fn get_combined_node_flags(&self, id: ast::NodeID) -> ast::NodeFlags {
        self.get_combined_flags(id, |p, id| p.node_flags(id))
    }

    pub fn get_combined_modifier_flags(
        &self,
        id: ast::NodeID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        self.get_combined_flags(id, |p, id| p.get_effective_modifier_flags(id))
    }

    pub fn is_enum_const(&self, e: &ast::EnumDecl) -> bool {
        self.get_combined_modifier_flags(e.id)
            .intersects(ast::ModifierKind::Const)
    }

    pub fn get_module_instance_state(
        &self,
        m: &'cx ast::ModuleDecl<'cx>,
        visited: Option<&mut nohash_hasher::IntMap<u32, Option<ModuleInstanceState>>>,
    ) -> ModuleInstanceState {
        fn cache<'cx>(
            this: &NodeQuery<'cx, '_>,
            node: ast::NodeID,
            visited: &mut nohash_hasher::IntMap<u32, Option<ModuleInstanceState>>,
        ) -> ModuleInstanceState {
            let key = node.index_as_u32();
            if let Some(v) = visited.get(&key).copied() {
                v.unwrap_or(ModuleInstanceState::Instantiated)
            } else {
                visited.insert(key, None);
                let state = _get_module_instance_state(this, node, visited);
                visited.insert(key, Some(state));
                state
            }
        }

        fn _get_module_instance_state<'cx>(
            this: &NodeQuery<'cx, '_>,
            node: ast::NodeID,
            visited: &mut nohash_hasher::IntMap<u32, Option<ModuleInstanceState>>,
        ) -> ModuleInstanceState {
            use ast::Node::*;
            let n = this.node(node);
            match n {
                InterfaceDecl(_) | TypeAliasDecl(_) => ModuleInstanceState::NonInstantiated,
                EnumDecl(e) if this.is_enum_const(e) => ModuleInstanceState::ConstEnumOnly,
                // TODO: import eq
                ImportDecl(_) if !n.has_syntactic_modifier(ast::ModifierKind::Export.into()) => {
                    ModuleInstanceState::NonInstantiated
                }
                ExportDecl(_) => {
                    todo!()
                }
                ModuleBlock(m) => {
                    let mut state = ModuleInstanceState::NonInstantiated;
                    for item in m.stmts {
                        let child_state = cache(this, item.id(), visited);
                        match child_state {
                            ModuleInstanceState::NonInstantiated => (),
                            ModuleInstanceState::Instantiated => {
                                state = ModuleInstanceState::Instantiated
                            }
                            ModuleInstanceState::ConstEnumOnly => {
                                state = ModuleInstanceState::ConstEnumOnly
                            }
                        }
                    }
                    state
                }
                ModuleDecl(ns) => this.get_module_instance_state(ns, Some(visited)),
                Ident(_)
                    if this
                        .node_flags(node)
                        .intersects(ast::NodeFlags::IDENTIFIER_IS_IN_JS_DOC_NAMESPACE) =>
                {
                    ModuleInstanceState::NonInstantiated
                }
                _ => ModuleInstanceState::Instantiated,
            }
        }

        if let Some(block) = m.block {
            if let Some(visited) = visited {
                cache(self, block.id, visited)
            } else {
                let mut default_map = nohash_hasher::IntMap::default();
                cache(self, block.id, &mut default_map)
            }
        } else {
            ModuleInstanceState::Instantiated
        }
    }

    pub fn is_block_or_catch_scoped(&self, id: ast::NodeID) -> bool {
        self.get_combined_node_flags(id)
            .intersects(ast::NodeFlags::BLOCK_SCOPED)
            || self.is_catch_clause_var_decl_or_binding_ele(id)
    }

    pub fn get_immediately_invoked_fn_expr(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::CallExpr<'cx>> {
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
            if let Some(call) = parent.as_call_expr()
                && call.expr.id() == prev
            {
                return Some(call);
            }
        }
        None
    }

    fn get_effective_modifier_flags(
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

    pub fn get_syntactic_modifier_flags_no_cache(
        &self,
        id: ast::NodeID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        let n = self.node(id);
        let flags = n.modifiers().map_or(Default::default(), |m| m.flags);
        let node_flags = self.node_flags(id);
        if node_flags.intersects(ast::NodeFlags::NESTED_NAMESPACE)
            || n.is_ident()
                && node_flags.intersects(ast::NodeFlags::IDENTIFIER_IS_IN_JS_DOC_NAMESPACE)
        {
            flags | ast::ModifierKind::Export
        } else {
            flags
        }
    }

    pub fn is_catch_clause_var_decl_or_binding_ele(&self, id: ast::NodeID) -> bool {
        let n = self.get_root_decl(id);
        self.node(n).is_var_decl() && self.node(self.parent(n).unwrap()).is_catch_clause()
    }

    pub fn get_root_decl(&self, mut id: ast::NodeID) -> ast::NodeID {
        let mut n = self.node(id);
        loop {
            if n.is_object_binding_elem() {
                let p = self.parent(id).unwrap();
                debug_assert!(self.node(p).is_object_pat());
                let p = self.parent(p).unwrap();
                debug_assert!(self.node(p).is_binding());
                id = self.parent(p).unwrap();
                n = self.node(id);
            } else if n.is_array_binding() {
                let p = self.parent(id).unwrap();
                debug_assert!(self.node(p).is_array_pat(), "span: {:#?}", self.node(p));
                let p = self.parent(p).unwrap();
                debug_assert!(self.node(p).is_binding());
                id = self.parent(p).unwrap();
                n = self.node(id);
            } else if n.is_binding() {
                id = self.parent(id).unwrap();
                n = self.node(id);
            } else {
                break;
            }
        }
        id
    }

    pub fn is_part_of_param_decl(&self, id: ast::NodeID) -> bool {
        let root = self.get_root_decl(id);
        self.node(root).is_param_decl()
    }

    pub fn is_module_augmentation_external(&self, ns: &ast::ModuleDecl<'_>) -> bool {
        let p = self.parent(ns.id).unwrap();
        match self.node(p) {
            ast::Node::Program(_) => self.parse_result.external_module_indicator.is_some(),
            ast::Node::ModuleBlock(n) => {
                let p_id = self.parent(n.id).unwrap();
                let p = self.node(p_id).expect_module_decl();
                p.is_ambient()
                    && self.node(self.parent(p_id).unwrap()).is_program()
                    && self.parse_result.external_module_indicator.is_none()
            }
            _ => false,
        }
    }

    pub fn is_external_module_augmentation(&self, id: ast::NodeID) -> bool {
        let n = self.node(id);
        n.as_module_decl()
            .is_some_and(|ns| ns.is_ambient() && self.is_module_augmentation_external(ns))
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

    pub fn get_containing_class(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        let p = self.parent(id)?;
        self.find_ancestor(p, |n| n.is_class_like().then_some(true))
    }

    pub fn is_part_of_ty_query(&self, mut n: ast::NodeID) -> bool {
        let mut node = self.node(n);
        while matches!(node, ast::Node::QualifiedName(_) | ast::Node::Ident(_)) {
            let p = self.parent(n).unwrap();
            n = p;
            node = self.node(p);
        }
        matches!(node, ast::Node::TypeofTy(_))
    }

    pub fn is_object_lit_or_class_expr_method_or_accessor(&self, node: ast::NodeID) -> bool {
        use ast::Node::*;
        let n = self.node(node);
        if n.is_object_method_member() {
            true
        } else if matches!(n, ClassMethodElem(_) | GetterDecl(_) | SetterDecl(_)) {
            let p = self.parent(node).unwrap();
            let p = self.node(p);
            matches!(p, ObjectLit(_) | ClassExpr(_))
        } else {
            false
        }
    }

    pub fn is_type_decl(&self, id: ast::NodeID) -> bool {
        use ast::Node::*;
        let n = self.node(id);
        matches!(
            n,
            ParamDecl(_) | ClassDecl(_) | InterfaceDecl(_) | TypeAliasDecl(_) | EnumDecl(_)
        ) || n.as_import_clause().is_some_and(|i| i.is_type_only)
            || (n.is_import_named_spec() && {
                let p = self.parent(id).unwrap();
                self.node(p).expect_import_clause().is_type_only
            })
            || (n.is_export_named_spec()) && {
                let p = self.parent(id).unwrap();
                let p = self.parent(id).unwrap();
                self.node(p).expect_export_decl().clause.is_type_only
            }
    }

    pub fn get_this_container(
        &self,
        mut id: ast::NodeID,
        include_arrow_fn: bool,
        include_class_computed_prop_name: bool,
    ) -> ast::NodeID {
        use ast::Node::*;
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
                match node {
                    FnDecl(_) | FnExpr(_) | ModuleDecl(_) | ClassPropElem(_)
                    | ClassMethodElem(_) | ClassCtor(_) | CtorSigDecl(_) | GetterDecl(_)
                    | SetterDecl(_) | IndexSigDecl(_) | EnumDecl(_) | Program(_)
                    | PropSignature(_) => return id,
                    _ => {}
                }
            }
        }
        unreachable!()
    }

    pub fn get_super_container(
        &self,
        mut id: ast::NodeID,
        stop_on_functions: bool,
    ) -> Option<ast::NodeID> {
        use ast::Node::*;
        loop {
            let parent = self.parent(id)?;
            id = parent;
            use ast::Node::*;
            match self.node(id) {
                ComputedPropName(_) => {
                    id = self.parent(id).unwrap();
                }
                FnDecl(_) | FnExpr(_) | ArrowFnExpr(_) if stop_on_functions => return Some(id),
                PropSignature(_)
                | ClassPropElem(_)
                | ObjectMethodMember(_)
                | ClassMethodElem(_)
                | MethodSignature(_)
                | ClassCtor(_)
                | GetterDecl(_)
                | SetterDecl(_)
                | ClassStaticBlockDecl(_) => return Some(id),
                // TODO: decorator
                _ => {}
            }
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

    pub fn get_assignment_target(&self, mut id: ast::NodeID) -> Option<ast::NodeID> {
        use ast::Node::*;
        use ast::PostfixUnaryOp;
        use ast::PrefixUnaryOp;

        let mut parent = self.parent(id);
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

    pub fn get_assignment_target_kind(&self, id: ast::NodeID) -> AssignmentKind {
        let Some(target) = self.get_assignment_target(id) else {
            return AssignmentKind::None;
        };
        match self.node(target) {
            ast::Node::AssignExpr(_) => AssignmentKind::Definite,
            ast::Node::BinExpr(_) => AssignmentKind::Definite,
            _ => AssignmentKind::Definite,
        }
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

    pub fn is_decl_name(&self, id: ast::NodeID) -> bool {
        self.parent(id).is_some_and(|p| self.node(p).is_decl())
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
            if let Some(qualified) = p.as_qualified_name()
                && qualified.left.id() == id
            {
                n = p;
                id = p_id;
                continue;
            }
            break;
        }

        n.is_type_alias_decl()
    }

    pub fn access_kind(&self, id: ast::NodeID) -> AccessKind {
        let Some(p) = self.parent(id) else {
            return AccessKind::Read;
        };
        match self.node(p) {
            ast::Node::AssignExpr(n) => {
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

    pub fn is_descendant_of(&self, node: ast::NodeID, ancestor: ast::NodeID) -> bool {
        self.find_ancestor(node, |node| (node.id() == ancestor).then_some(true))
            .is_some()
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
            if let Some(call) = parent.as_call_expr()
                && call.expr.id() == prev
            {
                return Some(call);
            }
        }
        None
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

    pub fn get_containing_fn(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        let parent = self.parent(id)?;
        self.find_ancestor(parent, |node| node.is_fn_decl_like().then_some(true))
    }

    pub fn get_declaration_container(&self, id: ast::NodeID) -> ast::NodeID {
        let root = self.get_root_decl(id);
        let Some(n) = self.find_ancestor(root, |n| {
            // TODO: VariableDeclarationList | ImportSpecifier | NamedImports
            if matches!(
                n,
                ast::Node::VarDecl(_) | ast::Node::ImportClause(_) | ast::Node::NsImport(_)
            ) {
                return None;
            } else {
                Some(true)
            }
        }) else {
            unreachable!()
        };
        self.parent(n).unwrap()
    }

    pub fn get_enclosing_container(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        let Some(parent_id) = self.parent(id) else {
            unreachable!()
        };
        self.find_ancestor(parent_id, |current| {
            let flags = container_flags_for_node(self.parse_result, self.parent_map, current.id());
            flags.contains(ContainerFlags::IS_CONTAINER).then_some(true)
        })
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

    pub fn get_control_flow_container(&self, node: ast::NodeID) -> ast::NodeID {
        let parent = self.parent(node).unwrap();
        self.find_ancestor(parent, |n| {
            if (n.is_fn_like() && self.get_immediately_invoked_fn_expr(node).is_none())
                || n.is_program()
                || n.is_class_prop_elem()
                || n.is_module_block()
            {
                Some(true)
            } else {
                None
            }
        })
        .unwrap()
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

    pub fn get_module_spec_for_import_or_export(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::StringLit> {
        let node = self.node(id);
        use ast::Node::*;
        match node {
            ImportClause(_) => {
                let p = self.parent(id).unwrap();
                let p = self.node(p).expect_import_decl();
                Some(p.module)
            }
            NsImport(_) | ImportNamedSpec(_) => {
                let p = self.parent(id).unwrap();
                let p = self.parent(p).unwrap();
                let p = self.node(p).expect_import_decl();
                Some(p.module)
            }
            ImportExportShorthandSpec(_) => {
                // export {a}
                // export {a} from 'xxx'
                let p = self.parent(id).unwrap();
                if let Some(n) = self.node(p).as_specs_export() {
                    return n.module;
                }
                // import {a} from 'xxx'
                let p = self.parent(p).unwrap();
                let p = self.node(p).expect_import_decl();
                Some(p.module)
            }
            ExportNamedSpec(_) => {
                let p = self.parent(id).unwrap();
                let n = self.node(p).expect_specs_export();
                n.module
            }
            _ => unreachable!(),
        }
    }

    pub fn is_node_within_class(&self, node: ast::NodeID, class_decl: ast::NodeID) -> bool {
        debug_assert!(self.node(class_decl).is_class_like());
        self.for_each_enclosing_class(node, |c| (c == class_decl).then_some(true))
            .is_some()
    }

    pub fn for_each_enclosing_class<T>(
        &self,
        mut node: ast::NodeID,
        f: impl Fn(ast::NodeID) -> Option<T>,
    ) -> Option<T> {
        while let Some(class) = self.get_containing_class(node) {
            debug_assert!(self.node(class).is_class_like());
            if let Some(res) = f(class) {
                return Some(res);
            }
            node = class;
        }
        None
    }

    pub fn is_in_prop_initializer_or_class_static_block(
        &self,
        node: ast::NodeID,
        ignore_arrow_fn: bool,
    ) -> bool {
        use ast::Node::*;
        self.find_ancestor(node, |n| match n {
            ClassPropElem(_) | ClassStaticBlockDecl(_) => Some(true),
            TypeofExpr(_) | JsxClosingElem(_) => Some(false),
            ArrowFnExpr(_) => {
                if ignore_arrow_fn {
                    None
                } else {
                    Some(false)
                }
            }
            BlockStmt(b) => {
                let p = self.parent(b.id).unwrap();
                let p = self.node(p);
                if p.is_arrow_fn_expr() && p.is_fn_decl_like() {
                    Some(false)
                } else {
                    None
                }
            }
            _ => None,
        })
        .is_some()
    }

    pub fn get_source_file_of_node(&self) -> &'cx ast::Program<'cx> {
        self.parse_result.root()
    }
}

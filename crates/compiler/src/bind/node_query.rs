use super::ast;

use crate::bind::ModuleInstanceState;

pub trait NodeQuery<'cx>: Sized {
    fn node(&self, id: ast::NodeID) -> ast::Node<'cx>;
    fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID>;
    fn node_flags(&self, id: ast::NodeID) -> ast::NodeFlags;
    fn is_external_or_commonjs_module(&self) -> bool;

    fn find_ancestor(
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

    fn walkup_binding_elements_and_patterns(&self, binding: ast::NodeID) -> ast::NodeID {
        let mut n = self.parent(binding).unwrap();
        loop {
            let p = self.parent(n).unwrap();
            if self.node(p).is_binding() {
                n = self.parent(p).unwrap()
            } else {
                break self.parent(n).unwrap();
            }
        }
    }

    fn get_combined_flags<T: std::ops::BitOrAssign>(
        &self,
        mut id: ast::NodeID,
        get_flag: impl Fn(&Self, ast::NodeID) -> T,
    ) -> T {
        let mut n = self.node(id);
        if n.is_binding() {
            id = self.walkup_binding_elements_and_patterns(id);
            n = self.node(id);
        }
        let mut flags = get_flag(self, id);

        if n.is_var_decl() {
            id = self.parent(id).unwrap();
            n = self.node(id);
        }
        // TODO: variable list
        if let Some(s) = n.as_var_stmt() {
            flags |= get_flag(self, s.id);
        }
        flags
    }

    fn get_combined_node_flags(&self, id: ast::NodeID) -> ast::NodeFlags {
        self.get_combined_flags(id, |p, id| p.node_flags(id))
    }

    fn get_combined_modifier_flags(
        &self,
        id: ast::NodeID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        self.get_combined_flags(id, |p, id| p.get_effective_modifier_flags(id))
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

    fn get_syntactic_modifier_flags_no_cache(
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

    fn is_part_of_ty_query(&self, mut n: ast::NodeID) -> bool {
        let mut node = self.node(n);
        while matches!(node, ast::Node::QualifiedName(_) | ast::Node::Ident(_)) {
            let p = self.parent(n).unwrap();
            n = p;
            node = self.node(p);
        }
        matches!(node, ast::Node::TypeofTy(_))
    }

    fn is_part_of_param_decl(&self, id: ast::NodeID) -> bool {
        let root = self.get_root_decl(id);
        self.node(root).is_param_decl()
    }

    fn is_object_lit_or_class_expr_method_or_accessor(&self, node: ast::NodeID) -> bool {
        let n = self.node(node);
        use ast::Node::*;
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

    fn get_immediately_invoked_fn_expr(&self, id: ast::NodeID) -> Option<&'cx ast::CallExpr<'cx>> {
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

    fn is_enum_const(&self, e: &ast::EnumDecl) -> bool {
        self.get_combined_modifier_flags(e.id)
            .intersects(ast::ModifierKind::Const)
    }

    fn get_module_instance_state(
        &self,
        m: &'cx ast::NsDecl<'cx>,
        visited: Option<&mut nohash_hasher::IntMap<u32, Option<ModuleInstanceState>>>,
    ) -> ModuleInstanceState {
        fn cache<'cx>(
            this: &impl NodeQuery<'cx>,
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
            this: &impl NodeQuery<'cx>,
            node: ast::NodeID,
            visited: &mut nohash_hasher::IntMap<u32, Option<ModuleInstanceState>>,
        ) -> ModuleInstanceState {
            let n = this.node(node);
            use ast::Node::*;
            match n {
                InterfaceDecl(_) | TypeDecl(_) => ModuleInstanceState::NonInstantiated,
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
                NamespaceDecl(ns) => this.get_module_instance_state(ns, Some(visited)),
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

    fn is_alias_symbol_decl(&self, id: ast::NodeID) -> bool {
        let node = self.node(id);
        use ast::Node::*;
        matches!(node, ImportNamedSpec(_) | ShorthandSpec(_) | NsImport(_))
    }

    fn is_global_source_file(&self, id: ast::NodeID) -> bool {
        !self.is_external_or_commonjs_module() && self.node(id).is_program()
    }

    fn is_block_or_catch_scoped(&self, id: ast::NodeID) -> bool {
        self.get_combined_node_flags(id)
            .intersects(ast::NodeFlags::BLOCK_SCOPED)
            || self.is_catch_clause_var_decl_or_binding_ele(id)
    }

    fn is_catch_clause_var_decl_or_binding_ele(&self, id: ast::NodeID) -> bool {
        let n = self.get_root_decl(id);
        self.node(n).is_var_decl() && self.node(self.parent(n).unwrap()).is_catch_clause()
    }

    fn get_root_decl(&self, mut id: ast::NodeID) -> ast::NodeID {
        let mut n = self.node(id);
        while n.is_object_binding_elem() {
            let p = self.parent(id).unwrap();
            // id = self.parent(p).unwrap();
            id = p;
            n = self.node(id);
        }
        id
    }

    fn is_param_prop_decl(&self, id: ast::NodeID, parent: ast::NodeID) -> bool {
        let n = self.node(id);
        n.is_param_decl()
            && n.has_syntactic_modifier(ast::ModifierKind::PARAMETER_PROPERTY)
            && self.node(parent).is_class_ctor()
    }
}

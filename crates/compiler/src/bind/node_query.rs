use super::ast;

use crate::bind::ModuleInstanceState;

pub trait NodeQuery<'cx>: Sized {
    fn node(&self, id: ast::NodeID) -> ast::Node<'cx>;
    fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID>;
    fn node_flags(&self, id: ast::NodeID) -> ast::NodeFlags;
    fn is_external_module(&self) -> bool;

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

    fn get_containing_class(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        let p = self.parent(id)?;
        self.find_ancestor(p, |n| n.is_class_like().then_some(true))
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
        m: &'cx ast::ModuleDecl<'cx>,
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

    fn is_alias_symbol_decl(&self, id: ast::NodeID) -> bool {
        let node = self.node(id);
        use ast::Node::*;
        match node {
            ImportNamedSpec(_) |  // `import { a as b } from 'xxx'`
            ShorthandSpec(_) |    // `export { spec }` or `import { spec } from 'xxx'`
            ExportNamedSpec(_) |  // `export { a as b }`
            NsImport(_)           // `import * as ns from 'xxx'`
            => true,
            ImportClause(n) => n.name.is_some(), // `import a from 'xxx'`
            ExportAssign(n) => n.is_aliasable(),
            _ => false
        }
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
        loop {
            if n.is_object_binding_elem() {
                let p = self.parent(id).unwrap();
                debug_assert!(self.node(p).is_object_pat());
                let p = self.parent(p).unwrap();
                debug_assert!(self.node(p).is_binding());
                id = self.parent(p).unwrap();
                n = self.node(id);
            } else if n.is_array_binding_elem() {
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

    fn is_param_prop_decl(&self, id: ast::NodeID, parent: ast::NodeID) -> bool {
        let n = self.node(id);
        n.as_param_decl()
            .is_some_and(|param| self.param_is_prop_decl(param, parent))
    }

    fn param_is_prop_decl(&self, param: &'cx ast::ParamDecl<'cx>, parent: ast::NodeID) -> bool {
        // TODO: has_syntactic_modifier
        param
            .modifiers
            .is_some_and(|mods| mods.flags.intersects(ast::ModifierKind::PARAMETER_PROPERTY))
            && self.node(parent).is_class_ctor()
    }

    fn is_external_module_augmentation(&self, id: ast::NodeID) -> bool {
        let n = self.node(id);
        n.as_module_decl()
            .is_some_and(|ns| ns.is_ambient() && self.is_module_augmentation_external(ns))
    }

    fn is_module_augmentation_external(&self, ns: &ast::ModuleDecl<'_>) -> bool {
        let p = self.parent(ns.id).unwrap();
        match self.node(p) {
            ast::Node::Program(_) => self.is_external_module(),
            ast::Node::ModuleBlock(n) => {
                let p_id = self.parent(n.id).unwrap();
                let p = self.node(p_id).expect_module_decl();
                p.is_ambient()
                    && self.node(self.parent(p_id).unwrap()).is_program()
                    && !self.is_external_module()
            }
            _ => false,
        }
    }

    fn get_module_spec_for_import_or_export(&self, id: ast::NodeID) -> Option<&'cx ast::StringLit> {
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
            ShorthandSpec(_) => {
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

    fn get_non_assigned_name_of_decl(&self, id: ast::NodeID) -> Option<ast::DeclarationName<'cx>> {
        use ast::Node::*;
        let n = self.node(id);
        match n {
            Ident(n) => Some(ast::DeclarationName::Ident(n)),
            CallExpr(_) | BinExpr(_) => {
                // TODO:
                None
            }
            ExportAssign(_) => {
                // TODO:
                None
            }
            _ => n.name(),
        }
    }

    fn get_assigned_name(&self, id: ast::NodeID) -> Option<ast::DeclarationName<'cx>> {
        let parent = self.parent(id)?;
        let p = self.node(parent);
        use ast::Node::*;
        match p {
            ObjectBindingElem(n) => ast::DeclarationName::from_object_binding_name(n.name),
            VarDecl(n) => ast::DeclarationName::from_binding(n.binding),
            Ident(n) => Some(ast::DeclarationName::Ident(n)),
            _ => None,
        }
    }

    fn get_name_of_decl(&self, id: ast::NodeID) -> Option<ast::DeclarationName<'cx>> {
        self.get_non_assigned_name_of_decl(id).or_else(|| {
            let n = self.node(id);
            use ast::Node::*;
            matches!(n, FnExpr(_) | ArrowFnExpr(_) | ClassExpr(_))
                .then(|| self.get_assigned_name(id))
                .flatten()
        })
    }

    fn has_dynamic_name(&self, id: ast::NodeID) -> bool {
        let Some(name) = self.get_name_of_decl(id) else {
            return false;
        };
        name.is_dynamic_name()
    }

    fn is_type_decl(&self, id: ast::NodeID) -> bool {
        let n = self.node(id);
        use ast::Node::*;
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
}

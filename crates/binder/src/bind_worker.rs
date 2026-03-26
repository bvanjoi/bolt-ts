use bolt_ts_ast as ast;
use bolt_ts_ast::r#trait;
use bolt_ts_ast::update_strict_mode_statement_list;

use super::AssignmentDeclarationKind;
use super::BinderState;
use super::Symbol;
use super::argument_name_from_element_access_node;
use super::create::DeclareSymbolProperty;
use super::node_query::ModuleInstanceState;
use super::param_index_in_parameter_list;
use super::symbol::SymbolFlags;
use super::symbol::SymbolTableLocation;
use super::symbol::{SymbolID, SymbolName};

impl<'cx, 'atoms, 'parser> BinderState<'cx, 'atoms, 'parser> {
    fn bind_ns_decl(&mut self, ns: &'cx ast::ModuleDecl<'cx>) {
        let name = match ns.name {
            ast::ModuleName::Ident(ident) => SymbolName::Atom(ident.name),
            ast::ModuleName::StringLit(lit) => SymbolName::Atom(lit.val),
        };

        let s = if ns.is_ambient() {
            if self.node_query().is_module_augmentation_external(ns) {
                self.declare_module_symbol(name, ns)
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    name,
                    ns.id,
                    SymbolFlags::VALUE_MODULE,
                    SymbolFlags::VALUE_MODULE_EXCLUDES,
                )
            }
        } else {
            self.declare_module_symbol(name, ns)
        };
        self.create_final_res(ns.id, s);
    }

    fn declare_module_symbol(
        &mut self,
        name: SymbolName,
        ns: &'cx ast::ModuleDecl<'cx>,
    ) -> SymbolID {
        let nq = self.node_query();
        let state = nq.get_module_instance_state(ns, None, |_, index| {
            if self.block_parent_stack.len() == index {
                None
            } else {
                let index = self.block_parent_stack.len() - 1 - index;
                Some(self.block_parent_stack[index])
            }
        });
        let instantiated = state != ModuleInstanceState::NonInstantiated;
        let (includes, excludes) = if instantiated {
            (
                SymbolFlags::VALUE_MODULE,
                SymbolFlags::VALUE_MODULE_EXCLUDES,
            )
        } else {
            (
                SymbolFlags::NAMESPACE_MODULE,
                SymbolFlags::NAMESPACE_MODULE_EXCLUDES,
            )
        };
        self.declare_symbol_and_add_to_symbol_table(name, ns.id, includes, excludes)
    }

    fn bind_type_alias_decl(&mut self, t: &'cx ast::TypeAliasDecl<'cx>) {
        let name = SymbolName::Atom(t.name.name);
        let symbol = self.bind_block_scoped_decl(
            t.id,
            name,
            SymbolFlags::TYPE_ALIAS,
            SymbolFlags::TYPE_ALIAS_EXCLUDES,
        );
        self.create_final_res(t.id, symbol);
    }

    fn bind_ty_param(&mut self, ty_param: &'cx ast::TyParam<'cx>) {
        let name = SymbolName::Atom(ty_param.name.name);
        let parent = self.parent_map.parent(ty_param.id).unwrap();
        // TODO: is_js_doc_template_tag
        let s = if let Some(infer_ty) = self.p.node(parent).as_infer_ty() {
            assert!(ty_param.default.is_none());
            let extends_ty = self.node_query().find_ancestor(infer_ty.id, |n| {
                let n_id = n.id();
                let p = self.parent_map.parent(n_id)?;
                if let Some(cond) = self.p.node(p).as_cond_ty()
                    && cond.extends_ty.id() == n_id
                {
                    Some(true)
                } else {
                    None
                }
            });
            let cond_container = extends_ty.map(|extends_ty| {
                let p = self.parent_map.parent(extends_ty).unwrap();
                let n = self.p.node(p);
                assert!(n.is_cond_ty());
                p
            });
            if let Some(cond_container) = cond_container {
                let loc = SymbolTableLocation::locals(cond_container);
                self.declare_symbol(
                    Some(name),
                    loc,
                    None,
                    ty_param.id,
                    SymbolFlags::TYPE_PARAMETER,
                    SymbolFlags::TYPE_ALIAS_EXCLUDES,
                    DeclareSymbolProperty::empty(),
                )
            } else {
                self.bind_anonymous_decl(ty_param.id, SymbolFlags::TYPE_PARAMETER, name)
            }
        } else {
            assert!(!self.p.node(self.container.unwrap()).is_infer_ty());
            self.declare_symbol_and_add_to_symbol_table(
                name,
                ty_param.id,
                SymbolFlags::TYPE_PARAMETER,
                SymbolFlags::TYPE_PARAMETER_EXCLUDES,
            )
        };
        self.create_final_res(ty_param.id, s);
    }

    fn bind_interface_decl(&mut self, i: &'cx ast::InterfaceDecl<'cx>) {
        let name = SymbolName::Atom(i.name.name);
        let symbol = self.bind_block_scoped_decl(
            i.id,
            name,
            SymbolFlags::INTERFACE,
            SymbolFlags::INTERFACE_EXCLUDES,
        );
        self.create_final_res(i.id, symbol);
    }

    fn bind_fn_decl(&mut self, f: &'cx ast::FnDecl<'cx>) {
        let ele_name = f
            .name
            .map(|name| SymbolName::Atom(name.name))
            .unwrap_or_else(|| SymbolName::ExportDefault);
        if self.in_strict_mode {
            let symbol = self.bind_block_scoped_decl(
                f.id,
                ele_name,
                SymbolFlags::FUNCTION,
                SymbolFlags::FUNCTION_EXCLUDES,
            );
            self.create_final_res(f.id, symbol);
        } else {
            let symbol = self.declare_symbol_and_add_to_symbol_table(
                ele_name,
                f.id,
                SymbolFlags::FUNCTION,
                SymbolFlags::FUNCTION_EXCLUDES,
            );
            self.create_final_res(f.id, symbol);
        }
    }

    fn bind_fn_expr(&mut self, f: &impl r#trait::FnExprLike<'cx>) {
        let id = f.id();
        if let Some(current_flow) = self.current_flow {
            self.flow_nodes.insert_flow_of_node(id, current_flow);
        }
        let name = f.name().map(SymbolName::Atom).unwrap_or(SymbolName::Fn);
        let symbol = self.bind_anonymous_decl(id, SymbolFlags::FUNCTION, name);
        self.create_final_res(id, symbol);
    }

    fn bind_fn_or_ctor_ty(&mut self, id: ast::NodeID, symbol_name: SymbolName) {
        let symbol = self.create_symbol(symbol_name, SymbolFlags::SIGNATURE);
        self.add_declaration_to_symbol(symbol, id, SymbolFlags::SIGNATURE);

        let ty_lit_symbol = self.create_symbol(symbol_name, SymbolFlags::TYPE_LITERAL);
        self.add_declaration_to_symbol(ty_lit_symbol, id, SymbolFlags::TYPE_LITERAL);
        let Some(members) = &mut self.symbols.get_mut(ty_lit_symbol).members else {
            unreachable!()
        };
        assert!(members.0.is_empty());
        members.0.insert(symbol_name, symbol);
        self.create_final_res(id, ty_lit_symbol);
    }

    fn bind_var_decl(&mut self, n: &ast::VarDecl<'cx>) {
        if self.in_strict_mode {
            // TODO:
        }

        match n.name.kind {
            ast::BindingKind::Ident(name) => {
                let symbol = self.bind_var(n.id, name.name);
                self.create_final_res(n.id, symbol);
            }
            ast::BindingKind::ArrayPat(_) | ast::BindingKind::ObjectPat(_) => return,
        };
    }

    fn bind_object_binding_ele(&mut self, n: &ast::ObjectBindingElem<'cx>) {
        if self.in_strict_mode {
            // TODO:
        }

        use ast::ObjectBindingName::*;
        match n.name {
            Shorthand(name) => {
                let symbol = self.bind_var(n.id, name.name);
                self.create_final_res(n.id, symbol);
            }
            Prop { name, .. } => {
                if let ast::BindingKind::Ident(name) = name.kind {
                    let symbol = self.bind_var(n.id, name.name);
                    self.create_final_res(n.id, symbol);
                } else {
                    return;
                }
            }
        };
    }

    fn bind_array_binding(&mut self, n: &ast::ArrayBinding<'cx>) {
        if self.in_strict_mode {
            // TODO:
        }

        use bolt_ts_ast::BindingKind::*;
        match n.name.kind {
            Ident(ident) => {
                let symbol = self.bind_var(n.id, ident.name);
                self.create_final_res(n.id, symbol);
            }
            _ => {}
        };
    }

    fn bind_var(&mut self, id: ast::NodeID, name: bolt_ts_atom::Atom) -> SymbolID {
        let name = SymbolName::Atom(name);

        if self.node_query().is_block_or_catch_scoped(id) {
            self.bind_block_scoped_decl(
                id,
                name,
                SymbolFlags::BLOCK_SCOPED_VARIABLE,
                SymbolFlags::BLOCK_SCOPED_VARIABLE_EXCLUDES,
            )
        } else if self.node_query().is_part_of_param_decl(id) {
            self.declare_symbol_and_add_to_symbol_table(
                name,
                id,
                SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                SymbolFlags::PARAMETER_EXCLUDES,
            )
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                name,
                id,
                SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                SymbolFlags::FUNCTION_SCOPED_VARIABLE_EXCLUDES,
            )
        }
    }

    fn bind_param_decl(&mut self, n: &ast::ParamDecl<'cx>) {
        use bolt_ts_ast::BindingKind::*;
        match n.name.kind {
            Ident(ident) => {
                let name = SymbolName::Atom(ident.name);
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    n.id,
                    SymbolFlags::FUNCTION_SCOPED_VARIABLE,
                    SymbolFlags::PARAMETER_EXCLUDES,
                );
                self.create_final_res(n.id, symbol);
            }
            ArrayPat(_) | ObjectPat(_) => {
                let p = self.node_query().parent(n.id).unwrap();
                let params = self.p.node(p).params().unwrap();
                let name = param_index_in_parameter_list(n.id, params);
                let symbol =
                    self.bind_anonymous_decl(n.id, SymbolFlags::FUNCTION_SCOPED_VARIABLE, name);
                self.create_final_res(n.id, symbol);
            }
        }

        let p = self.parent_map.parent(n.id).unwrap();

        if self.p.nodes.param_is_prop_decl(n, p) {
            let class_decl = self.parent_map.parent(p).unwrap();
            let loc = SymbolTableLocation::members(class_decl);
            let includes = SymbolFlags::PROPERTY
                | if n.question.is_some() {
                    SymbolFlags::OPTIONAL
                } else {
                    SymbolFlags::empty()
                };
            let name = match n.name.kind {
                bolt_ts_ast::BindingKind::Ident(ident) => SymbolName::Atom(ident.name),
                bolt_ts_ast::BindingKind::ObjectPat(_) => {
                    todo!()
                }
                bolt_ts_ast::BindingKind::ArrayPat(_) => todo!(),
            };
            self.declare_symbol(
                Some(name),
                loc,
                None,
                n.id,
                includes,
                SymbolFlags::PROPERTY_EXCLUDES,
                DeclareSymbolProperty::empty(),
            );
        }
    }

    pub(super) fn bind_worker(&mut self, node: ast::NodeID) {
        let n = self.p.node(node);
        use ast::Node::*;
        match n {
            Ident(_) => {
                // TODO: identifier with NodeFlags.IdentifierIsInJSDocNamespace
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_flow_of_node(node, flow);
                }
            }
            ThisExpr(_) => {
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_flow_of_node(node, flow);
                }
            }
            QualifiedName(_) => {
                if let Some(flow) = self.current_flow
                    && self.node_query().is_part_of_ty_query(node)
                {
                    self.flow_nodes.insert_flow_of_node(node, flow);
                }
            }
            // TODO: meta
            SuperExpr(_) => {
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_flow_of_node(node, flow);
                } else {
                    self.flow_nodes.reset_flow_of_node(node);
                }
            }
            // TODO: private
            PropAccessExpr(n) => {
                if let Some(flow) = self.current_flow
                    && self.is_narrowable_reference(n.expr)
                {
                    self.flow_nodes.insert_flow_of_node(node, flow);
                }
                // TODO: is_special_prop_decl
                // TODO: js
            }
            EleAccessExpr(n) => {
                if let Some(flow) = self.current_flow
                    && self.ele_access_is_narrowable_reference(n)
                {
                    self.flow_nodes.insert_flow_of_node(node, flow);
                }
                // TODO: is_special_prop_decl
                // TODO: js
            }
            BinExpr(_) => {
                // TODO: special_kind
            }
            CatchClause(_) => {
                // TODO: self.check_strict_mode_catch_clause()
            }
            // TODO: delete expr
            PostfixUnaryExpr(_) => {
                // TODO: check
            }
            PrefixUnaryExpr(_) => {
                // TODO: check
            }
            // TODO: with stmt
            // TODO: label
            // TODO: this type
            TyParam(n) => {
                self.bind_ty_param(n);
            }
            ParamDecl(n) => self.bind_param_decl(n),
            VarDecl(n) => self.bind_var_decl(n),
            ObjectBindingElem(n) => {
                // self.flow_nodes
                //     .insert_container_map(n.id, self.current_flow.unwrap());
                self.bind_object_binding_ele(n);
            }
            ArrayBinding(n) => self.bind_array_binding(n),
            PropSignature(ast::PropSignature { name, question, .. })
            | ClassPropElem(ast::ClassPropElem { name, question, .. }) => {
                // TODO: is_auto_accessor
                let includes = SymbolFlags::PROPERTY
                    | if question.is_some() {
                        SymbolFlags::OPTIONAL
                    } else {
                        SymbolFlags::empty()
                    };
                let symbol = self.bind_prop_or_method_or_access::<false>(
                    node,
                    name,
                    includes,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            ObjectPropAssignment(n) => {
                let symbol = self.bind_prop_or_method_or_access::<false>(
                    node,
                    n.name,
                    SymbolFlags::PROPERTY,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            ObjectShorthandMember(n) => {
                let name = SymbolName::Atom(n.name.name);
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    node,
                    SymbolFlags::PROPERTY,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            EnumMember(m) => {
                let symbol = self.bind_prop_or_method_or_access::<false>(
                    node,
                    m.name,
                    SymbolFlags::ENUM_MEMBER,
                    SymbolFlags::ENUM_MEMBER_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            CallSigDecl(_) | CtorSigDecl(_) | IndexSigDecl(_) => {
                let name = if n.is_call_sig_decl() {
                    SymbolName::Call
                } else if n.is_ctor_sig_decl() {
                    SymbolName::New
                } else if n.is_index_sig_decl() {
                    SymbolName::Index
                } else {
                    unreachable!()
                };
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    node,
                    SymbolFlags::SIGNATURE,
                    SymbolFlags::empty(),
                );
                self.create_final_res(node, symbol);
            }
            MethodSignature(node) => {
                let includes = SymbolFlags::METHOD
                    | if node.question.is_some() {
                        SymbolFlags::OPTIONAL
                    } else {
                        SymbolFlags::empty()
                    };
                let symbol = self.bind_prop_or_method_or_access::<false>(
                    node.id,
                    node.name,
                    includes,
                    SymbolFlags::METHOD_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            ClassMethodElem(node) => {
                // TODO: is_optional
                let includes = SymbolFlags::METHOD;
                let bind_flow = self
                    .parent
                    .is_some_and(|p| matches!(self.p.node(p), ast::Node::ClassExpr(_)));
                let symbol = if bind_flow {
                    self.bind_prop_or_method_or_access::<true>(
                        node.id,
                        node.name,
                        includes,
                        SymbolFlags::METHOD_EXCLUDES,
                    )
                } else {
                    self.bind_prop_or_method_or_access::<false>(
                        node.id,
                        node.name,
                        includes,
                        SymbolFlags::METHOD_EXCLUDES,
                    )
                };
                self.create_final_res(node.id, symbol);
            }
            ObjectMethodMember(node) => {
                // TODO: is_optional
                let includes = SymbolFlags::METHOD;
                let symbol = self.bind_prop_or_method_or_access::<true>(
                    node.id,
                    node.name,
                    includes,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            FnDecl(node) => {
                self.bind_fn_decl(node);
            }
            ClassCtor(node) => {
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    SymbolName::Constructor,
                    node.id,
                    SymbolFlags::CONSTRUCTOR,
                    SymbolFlags::empty(),
                );
                self.create_final_res(node.id, symbol);
            }
            GetterDecl(node) => {
                let bind_flow = self.parent.is_some_and(|p| {
                    matches!(
                        self.p.node(p),
                        ast::Node::ClassExpr(_) | ast::Node::ObjectLit(_)
                    )
                });
                let symbol = if bind_flow {
                    self.bind_prop_or_method_or_access::<true>(
                        node.id,
                        node.name,
                        SymbolFlags::GET_ACCESSOR,
                        SymbolFlags::GET_ACCESSOR_EXCLUDES,
                    )
                } else {
                    self.bind_prop_or_method_or_access::<false>(
                        node.id,
                        node.name,
                        SymbolFlags::GET_ACCESSOR,
                        SymbolFlags::GET_ACCESSOR_EXCLUDES,
                    )
                };
                self.create_final_res(node.id, symbol);
            }
            SetterDecl(node) => {
                let bind_flow = self.parent.is_some_and(|p| {
                    matches!(
                        self.p.node(p),
                        ast::Node::ClassExpr(_) | ast::Node::ObjectLit(_)
                    )
                });
                let symbol = if bind_flow {
                    self.bind_prop_or_method_or_access::<true>(
                        node.id,
                        node.name,
                        SymbolFlags::SET_ACCESSOR,
                        SymbolFlags::SET_ACCESSOR_EXCLUDES,
                    )
                } else {
                    self.bind_prop_or_method_or_access::<false>(
                        node.id,
                        node.name,
                        SymbolFlags::SET_ACCESSOR,
                        SymbolFlags::SET_ACCESSOR_EXCLUDES,
                    )
                };
                self.create_final_res(node.id, symbol);
            }
            FnTy(_) => {
                self.bind_fn_or_ctor_ty(node, SymbolName::Call);
            }
            CtorTy(_) => {
                self.bind_fn_or_ctor_ty(node, SymbolName::New);
            }
            ObjectLitTy(_) | MappedTy(_) => {
                let s = self.bind_anonymous_decl(node, SymbolFlags::TYPE_LITERAL, SymbolName::Type);
                self.create_final_res(node, s);
            }
            ObjectLit(n) => {
                let s =
                    self.bind_anonymous_decl(n.id, SymbolFlags::OBJECT_LITERAL, SymbolName::Object);
                self.create_final_res(n.id, s);
            }
            FnExpr(n) => {
                self.bind_fn_expr(n);
            }
            ArrowFnExpr(n) => {
                self.bind_fn_expr(n);
            }
            CallExpr(_) => {}
            ClassExpr(node) => {
                self.in_strict_mode = true;
                self.bind_class_like_decl::<true>(node);
            }
            ClassDecl(node) => {
                self.in_strict_mode = true;
                self.bind_class_like_decl::<false>(node);
            }
            InterfaceDecl(node) => self.bind_interface_decl(node),
            TypeAliasDecl(node) => self.bind_type_alias_decl(node),
            EnumDecl(node) => self.bind_enum_decl(node),
            ModuleDecl(node) => self.bind_ns_decl(node),
            ImportEqualsDecl(ast::ImportEqualsDecl { id, name, .. })
            | ImportNamedSpec(ast::ImportNamedSpec { id, name, .. })
            | ImportShorthandSpec(ast::ImportShorthandSpec { id, name, .. })
            | ExportShorthandSpec(ast::ExportShorthandSpec { id, name, .. })
            | NsImport(ast::NsImport { id, name, .. }) => {
                // importEqualsDeclaration:
                //  - import name = xxx
                //  - import name = xxx.yyy
                // ImportNamedSpec:
                //  - import { prop_name as name } from 'xxx'
                // ImportShorthandSpec:
                //  - import { name } from 'xxx'
                // ExportShorthandSpec:
                //  - export { name } from 'xxx'
                // NsImport:
                //  - import * as name from 'xxx'
                let name = SymbolName::Atom(name.name);
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    *id,
                    SymbolFlags::ALIAS,
                    SymbolFlags::ALIAS_EXCLUDES,
                );
                self.create_final_res(*id, symbol);
            }
            ExportNamedSpec(node) => {
                let n = |name: &ast::ModuleExportName| {
                    use bolt_ts_ast::ModuleExportNameKind::*;
                    match name.kind {
                        Ident(ident) => SymbolName::Atom(ident.name),
                        StringLit(lit) => SymbolName::Atom(lit.val),
                    }
                };
                let name = n(node.name);
                let symbol = self.declare_symbol_and_add_to_symbol_table(
                    name,
                    node.id,
                    SymbolFlags::ALIAS,
                    SymbolFlags::ALIAS_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            ImportClause(node) => self.bind_import_clause(node),
            ExportDecl(node) => self.bind_export_decl(node),
            ExportAssign(node) => self.bind_export_assign(node),
            Program(node) => {
                self.update_strict_mode_statement_list(node.stmts);
                self.bind_source_file_if_external_module(node);
            }
            BlockStmt(n)
                if self
                    .p
                    .node(self.parent_map.parent(node).unwrap())
                    .is_fn_like_or_class_static_block_decl() =>
            {
                self.update_strict_mode_statement_list(n.stmts)
            }
            ModuleBlock(n) => self.update_strict_mode_statement_list(n.stmts),
            ThisTy(_) => {
                self.seen_this_keyword = true;
            }
            AssignExpr(n) => {
                let special_kind = self
                    .node_query()
                    .get_assignment_declaration_kind_for_assign_expr(n);
                match special_kind {
                    AssignmentDeclarationKind::Property => self.bind_special_prop_assignment(n),
                    _ => {
                        // TODO
                    }
                }
            }
            _ => {}
        }
    }

    fn bind_special_prop_assignment(&mut self, node: &'cx ast::AssignExpr<'cx>) {
        if node.left.is_bindable_static_name_expr::<false>() {
            self.bind_static_prop_assignment(node.left);
        }
    }

    fn bind_static_prop_assignment(&mut self, node: &'cx ast::Expr<'cx>) {
        match node.kind {
            ast::ExprKind::EleAccess(n) => {
                let node_id = node.id();
                // self.parent_map.insert(n.expr.id(), node_id);
                let Some(key_name) = argument_name_from_element_access_node(n) else {
                    return;
                };
                self.bind_prop_assignment::<false, false>(n.expr, key_name, node_id);
            }
            ast::ExprKind::Ident(_) => unreachable!(),
            _ => {}
        }
    }

    fn bind_prop_assignment<const IS_PROTOTYPE_PROPERTY: bool, const CONTAINER_IS_CLASS: bool>(
        &mut self,
        prop_name: &'cx ast::Expr<'cx>,
        key_name: SymbolName,
        prop_access: ast::NodeID,
    ) -> Option<SymbolID> {
        let namespace_symbol = self.lookup_symbol_for_prop_access(
            prop_name.id(),
            self.block_scope_container
                .unwrap_or(self.container.unwrap()),
        );
        self.bind_potentially_new_expando_member_to_namespace::<IS_PROTOTYPE_PROPERTY>(
            prop_access,
            namespace_symbol,
            key_name,
        )
    }

    fn lookup_symbol_for_prop_access(
        &self,
        node: ast::NodeID,
        lookup_container: ast::NodeID,
    ) -> Option<SymbolID> {
        let n = self.p.node(node);
        let symbol = match n {
            ast::Node::Ident(ident) => {
                return self.lookup_symbol_for_name(lookup_container, SymbolName::Atom(ident.name));
            }
            ast::Node::EleAccessExpr(p) => {
                self.lookup_symbol_for_prop_access(p.expr.id(), self.container.unwrap())
            }
            ast::Node::PropAccessExpr(p) => {
                // TODO:
                return None;
            }
            _ => unreachable!(),
        }?;
        let exports = self.symbols.get(symbol).exports.as_ref()?;
        let name = match n {
            ast::Node::EleAccessExpr(p) => argument_name_from_element_access_node(p)?,
            _ => unreachable!(),
        };
        exports.0.get(&name).copied()
    }

    fn lookup_symbol_for_name(&self, container: ast::NodeID, name: SymbolName) -> Option<SymbolID> {
        let c = self.p.node(container);
        if c.has_locals()
            && let Some(local) = self
                .locals
                .get(&container)
                .and_then(|locals| locals.0.get(&name).copied())
        {
            return Some(self.symbols.get(local).export_symbol.unwrap_or(local));
        }

        //TODO: source file level

        if Symbol::can_have_symbol(self.p.node(container)) {
            let symbol = self.final_res.get(&container)?;
            let exports = self.symbols.get(*symbol).exports.as_ref()?;
            return exports.0.get(&name).copied();
        }

        None
    }

    fn bind_potentially_new_expando_member_to_namespace<const IS_PROTOTYPE_PROPERTY: bool>(
        &mut self,
        decl: ast::NodeID,
        namespace_symbol: Option<SymbolID>,
        key_name: SymbolName,
    ) -> Option<SymbolID> {
        let namespace_symbol = namespace_symbol?;
        if !self.symbols.get(namespace_symbol).is_expando_symbol() {
            return None;
        }

        let location = if IS_PROTOTYPE_PROPERTY {
            SymbolTableLocation::symbol_members(namespace_symbol)
        } else {
            SymbolTableLocation::symbol_exports(namespace_symbol)
        };

        let includes = SymbolFlags::METHOD;
        let excludes = SymbolFlags::METHOD_EXCLUDES;

        Some(self.declare_symbol(
            Some(key_name),
            location,
            Some(namespace_symbol),
            decl,
            includes | SymbolFlags::ASSIGNMENT,
            excludes & !SymbolFlags::ASSIGNMENT,
            DeclareSymbolProperty::empty(),
        ))
    }

    fn update_strict_mode_statement_list(&mut self, stmts: ast::Stmts<'cx>) {
        update_strict_mode_statement_list(stmts, &mut self.in_strict_mode);
    }

    fn bind_export_assign(&mut self, node: &'cx ast::ExportAssign<'cx>) {
        let container = self.container.unwrap();
        let flags = if node.is_aliasable() {
            SymbolFlags::ALIAS
        } else {
            SymbolFlags::PROPERTY
        };
        let loc = SymbolTableLocation::exports(container);
        let name = if node.is_export_equals {
            SymbolName::ExportEquals
        } else {
            SymbolName::ExportDefault
        };
        let symbol = self.declare_symbol(
            Some(name),
            loc,
            // TODO: use `Some(self.final_res[&container])`
            self.final_res.get(&container).copied(),
            node.id,
            flags,
            SymbolFlags::all(),
            DeclareSymbolProperty::empty(),
        );
        self.create_final_res(node.id, symbol);
        if node.is_export_equals {
            self.set_value_declaration(symbol, node.id);
        }
    }

    fn bind_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) {
        let (includes, excludes) = if node.is_const() {
            (SymbolFlags::CONST_ENUM, SymbolFlags::CONST_ENUM_EXCLUDES)
        } else {
            (
                SymbolFlags::REGULAR_ENUM,
                SymbolFlags::REGULAR_ENUM_EXCLUDES,
            )
        };
        let name = SymbolName::Atom(node.name.name);
        let symbol = self.bind_block_scoped_decl(node.id, name, includes, excludes);
        let prev = self.final_res.insert(node.id, symbol);
        debug_assert!(prev.is_none());
    }

    fn bind_import_clause(&mut self, node: &'cx ast::ImportClause<'cx>) {
        if let Some(name) = node.name {
            // import name from 'xxxx'
            let symbol = self.declare_symbol_and_add_to_symbol_table(
                SymbolName::Atom(name.name),
                node.id,
                SymbolFlags::ALIAS,
                SymbolFlags::ALIAS_EXCLUDES,
            );
            self.create_final_res(node.id, symbol);
        }
    }

    fn bind_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) {
        let container = self.container.unwrap();
        match node.clause.kind {
            ast::ExportClauseKind::Glob(_) => {
                let name = SymbolName::ExportStar;
                let loc = SymbolTableLocation::exports(container);
                let symbol = self.declare_symbol(
                    Some(name),
                    loc,
                    None,
                    node.id,
                    SymbolFlags::EXPORT_STAR,
                    SymbolFlags::empty(),
                    DeclareSymbolProperty::empty(),
                );
                self.create_final_res(node.id, symbol);
            }
            ast::ExportClauseKind::Ns(_) => {}
            ast::ExportClauseKind::Specs(_) => {}
        }
    }

    fn bind_source_file_if_external_module(&mut self, node: &'cx ast::Program<'cx>) {
        if self.p.is_external_or_commonjs_module() {
            self.bind_source_file_as_external_module(node);
        }
        // TODO: json source file
    }

    fn bind_source_file_as_external_module(&mut self, node: &'cx ast::Program<'cx>) {
        let s = self.bind_anonymous_decl(
            node.id,
            SymbolFlags::VALUE_MODULE,
            SymbolName::Atom(self.p.filepath),
        );
        assert_eq!(s, SymbolID::container(node.id.module()));
        self.final_res.insert(node.id, s);
    }

    fn ele_access_is_narrowable_reference(&self, n: &ast::EleAccessExpr) -> bool {
        (n.arg.is_string_or_number_lit_like() || n.arg.is_entity_name_expr())
            && self.is_narrowable_reference(n.expr)
    }

    pub(super) fn is_narrowable_reference(&self, expr: &ast::Expr<'_>) -> bool {
        use ast::ExprKind::*;
        match expr.kind {
            // TODO: metaProperty
            Ident(_) | This(_) | Super(_) => true,
            PropAccess(n) => self.is_narrowable_reference(n.expr),
            Paren(n) => self.is_narrowable_reference(n.expr),
            EleAccess(n) => self.ele_access_is_narrowable_reference(n),
            NonNull(n) => self.is_narrowable_reference(n.expr),
            Bin(_) => {
                // TODO: n.op.kind == Comma
                false
            }
            Assign(n) => n.left.is_left_hand_side_expr_kind(),
            _ => false,
        }
    }

    pub(super) fn is_narrowable_expression(&self, expr: &'cx ast::Expr<'cx>) -> bool {
        use ast::ExprKind::*;
        match expr.kind {
            Ident(_) | This(_) | Super(_) => true,
            PropAccess(_) | EleAccess(_) => self.contains_narrowable_reference(expr),
            Paren(n) => {
                // TODO: return false if expr is js
                self.is_narrowable_expression(n.expr)
            }
            Call(n) => self.has_narrowable_argument(n),
            // TODO: other case
            _ => false,
        }
    }

    fn has_narrowable_argument(&self, expr: &'cx ast::CallExpr<'cx>) -> bool {
        for argument in expr.args {
            if self.contains_narrowable_reference(argument) {
                return true;
            }
        }

        match expr.expr.kind {
            ast::ExprKind::PropAccess(n) if self.contains_narrowable_reference(n.expr) => true,
            _ => false,
        }
    }

    fn contains_narrowable_reference(&self, expr: &'cx ast::Expr<'cx>) -> bool {
        self.is_narrowable_reference(expr) || {
            let nq = self.node_query();
            if !nq
                .node_flags(expr.id())
                .contains(ast::NodeFlags::OPTIONAL_CHAIN)
            {
                return false;
            }
            match expr.kind {
                ast::ExprKind::PropAccess(n) => self.contains_narrowable_reference(n.expr),
                ast::ExprKind::EleAccess(n) => self.contains_narrowable_reference(n.expr),
                ast::ExprKind::Call(n) => self.contains_narrowable_reference(n.expr),
                ast::ExprKind::NonNull(n) => self.contains_narrowable_reference(n.expr),
                _ => false,
            }
        }
    }
}

use super::BinderState;
use super::ModuleInstanceState;
use super::NodeQuery;
use super::SymbolTable;
use super::symbol::SymbolFlags;
use super::symbol::SymbolTableLocation;
use super::symbol::{SymbolID, SymbolName};

use bolt_ts_ast as ast;
use bolt_ts_ast::NodeFlags;

use crate::ir;
use crate::parser::is_left_hand_side_expr_kind;

impl<'cx, 'atoms, 'parser> BinderState<'cx, 'atoms, 'parser> {
    fn bind_ns_decl(&mut self, ns: &'cx ast::NsDecl<'cx>) {
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

    fn declare_module_symbol(&mut self, name: SymbolName, ns: &'cx ast::NsDecl<'cx>) -> SymbolID {
        let state = self.node_query().get_module_instance_state(ns, None);
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

    fn bind_type_decl(&mut self, t: &'cx ast::TypeDecl<'cx>) {
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
        let parent = self.parent_map.parent_unfinished(ty_param.id).unwrap();
        // TODO: is_js_doc_template_tag
        let s = if let Some(infer_ty) = self.p.node(parent).as_infer_ty() {
            assert!(ty_param.default.is_none());
            let extends_ty = self.node_query().find_ancestor(infer_ty.id, |n| {
                let n_id = n.id();
                let p = self.parent_map.parent_unfinished(n_id)?;
                if let Some(cond) = self.p.node(p).as_cond_ty() {
                    if cond.extends_ty.id() == n_id {
                        return Some(true);
                    }
                }
                None
            });
            let cond_container = extends_ty.map(|extends_ty| {
                let p = self.parent_map.parent_unfinished(extends_ty).unwrap();
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
                    false,
                    false,
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
        // if self.in_strict_mode {
        // } else {
        let ele_name = SymbolName::Atom(f.name.name);
        let symbol = self.declare_symbol_and_add_to_symbol_table(
            ele_name,
            f.id,
            SymbolFlags::FUNCTION,
            SymbolFlags::FUNCTION_EXCLUDES,
        );
        self.create_final_res(f.id, symbol);
        // }
    }

    fn check_contextual_ident(&mut self, ident: ast::NodeID) {
        // TODO:
    }

    fn bind_fn_expr(&mut self, f: &impl ir::FnExprLike<'cx>) {
        let name = f.name().map(SymbolName::Atom).unwrap_or(SymbolName::Fn);
        let id = f.id();
        let symbol = self.bind_anonymous_decl(id, SymbolFlags::FUNCTION, name);
        self.create_final_res(id, symbol);
    }

    fn bind_object_lit(&mut self, n: &ast::ObjectLit<'cx>) -> SymbolID {
        let s = self.bind_anonymous_decl(n.id, SymbolFlags::OBJECT_LITERAL, SymbolName::Object);
        self.create_final_res(n.id, s);
        s
    }

    fn bind_anonymous_ty(&mut self, id: ast::NodeID) -> SymbolID {
        let s = self.bind_anonymous_decl(id, SymbolFlags::TYPE_LITERAL, SymbolName::Type);
        self.create_final_res(id, s);
        s
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

        let name = match n.binding.kind {
            bolt_ts_ast::BindingKind::Ident(name) => name,
            _ => return, // TODO: handle more case
        };
        let symbol = self.bind_var(n.id, name.name);
        self.create_final_res(n.id, symbol);
    }

    fn bind_object_binding_ele(&mut self, n: &ast::ObjectBindingElem<'cx>) {
        if self.in_strict_mode {
            // TODO:
        }

        use ast::ObjectBindingName::*;
        let name = match n.name {
            Shorthand(name) => name,
            Prop { .. } => return,
        };
        let symbol = self.bind_var(n.id, name.name);
        // TODO: use binding.id
        self.create_final_res(name.id, symbol);
    }

    fn bind_var(&mut self, id: ast::NodeID, name: bolt_ts_atom::AtomId) -> SymbolID {
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
        if self.in_strict_mode && !self.p.node_flags(n.id).intersects(NodeFlags::AMBIENT) {
            // TODO: check
        }
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
                let idx = {
                    let p = self.node_query().parent(n.id).unwrap();
                    let params = self.p.node(p).params().unwrap();
                    params.iter().position(|p| p.id == n.id).unwrap()
                };
                let name = SymbolName::ParamIdx(idx as u32);
                let symbol =
                    self.bind_anonymous_decl(n.id, SymbolFlags::FUNCTION_SCOPED_VARIABLE, name);
                self.create_final_res(n.id, symbol);
            }
        }

        let p = self.parent_map.parent_unfinished(n.id).unwrap();

        if n.modifiers
            .is_some_and(|mods| mods.flags.intersects(ast::ModifierKind::PARAMETER_PROPERTY))
            && self.p.node(p).is_class_ctor()
        {
            let class_decl = self.parent_map.parent_unfinished(p).unwrap();
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
                false,
                false,
            );
        }
    }

    pub(super) fn _bind(&mut self, node: ast::NodeID) {
        let n = self.p.node(node);
        use ast::Node::*;
        match n {
            Ident(_) => {
                // TODO: identifier with NodeFlags.IdentifierIsInJSDocNamespace
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_container_map(node, flow);
                }
                self.check_contextual_ident(node);
            }
            ThisExpr(_) => {
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_container_map(node, flow);
                }
                self.check_contextual_ident(node);
            }
            QualifiedName(_) => {
                if let Some(flow) = self.current_flow {
                    if self.node_query().is_part_of_ty_query(node) {
                        self.flow_nodes.insert_container_map(node, flow);
                    }
                }
            }
            // TODO: meta
            SuperExpr(_) => {
                if let Some(flow) = self.current_flow {
                    self.flow_nodes.insert_container_map(node, flow);
                } else {
                    self.flow_nodes.reset_container_map(node);
                }
            }
            // TODO: private
            PropAccessExpr(p) => {
                if let Some(flow) = self.current_flow {
                    if self.is_narrowable_reference(p.expr) {
                        self.flow_nodes.insert_container_map(node, flow);
                    }
                }
                // TODO: is_special_prop_decl
                // TODO: js
            }
            EleAccessExpr(e) => {
                if let Some(flow) = self.current_flow {
                    if self.ele_access_is_narrowable_reference(e) {
                        self.flow_nodes.insert_container_map(node, flow);
                    }
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
            VarDecl(n) => {
                self.bind_var_decl(n);
            }
            ObjectBindingElem(n) => {
                // self.flow_nodes
                //     .insert_container_map(node, self.current_flow.unwrap());
                self.bind_object_binding_ele(n);
            }
            PropSignature(ast::PropSignature { name, question, .. })
            | ClassPropElem(ast::ClassPropElem { name, question, .. }) => {
                // TODO: is_auto_accessor
                let includes = SymbolFlags::PROPERTY
                    | if question.is_some() {
                        SymbolFlags::OPTIONAL
                    } else {
                        SymbolFlags::empty()
                    };
                let symbol = self.bind_prop_or_method_or_access(
                    node,
                    name,
                    includes,
                    SymbolFlags::PROPERTY_EXCLUDES,
                );
                self.create_final_res(node, symbol);
            }
            ObjectPropMember(n) => {
                let symbol = self.bind_prop_or_method_or_access(
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
                let symbol = self.bind_prop_or_method_or_access(
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
                let symbol = self.bind_prop_or_method_or_access(
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
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    node.name,
                    includes,
                    SymbolFlags::METHOD_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            ObjectMethodMember(node) => {
                // TODO: is_optional
                let includes = SymbolFlags::METHOD;
                let symbol = self.bind_prop_or_method_or_access(
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
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    node.name,
                    SymbolFlags::GET_ACCESSOR,
                    SymbolFlags::GET_ACCESSOR_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            SetterDecl(node) => {
                let symbol = self.bind_prop_or_method_or_access(
                    node.id,
                    node.name,
                    SymbolFlags::SET_ACCESSOR,
                    SymbolFlags::SET_ACCESSOR_EXCLUDES,
                );
                self.create_final_res(node.id, symbol);
            }
            FnTy(_) => {
                self.bind_fn_or_ctor_ty(node, SymbolName::Call);
            }
            CtorTy(_) => {
                self.bind_fn_or_ctor_ty(node, SymbolName::New);
            }
            ObjectLitTy(_) | MappedTy(_) => {
                self.bind_anonymous_ty(node);
            }
            ObjectLit(n) => {
                self.bind_object_lit(n);
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
                self.bind_class_like_decl(node, true);
            }
            ClassDecl(node) => {
                self.in_strict_mode = true;
                self.bind_class_like_decl(node, false);
            }
            InterfaceDecl(node) => self.bind_interface_decl(node),
            TypeDecl(node) => self.bind_type_decl(node),
            EnumDecl(node) => self.bind_enum_decl(node),
            NamespaceDecl(node) => self.bind_ns_decl(node),
            ShorthandSpec(ast::ShorthandSpec { id, name, .. })
            | NsImport(ast::NsImport { id, name, .. }) => {
                // import { name } from 'xxx'
                // import * as name from 'xxx'
                // export { name } from 'xxx'
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
                // TODO: `update_strict_module_statement_list`
                self.bind_source_file_if_external_module(node);
            }
            BlockStmt(_)
                if self
                    .p
                    .node(self.parent_map.parent_unfinished(node).unwrap())
                    .is_fn_like_or_class_static_block_decl() => {}
            BlockStmt(_) | ModuleBlock(_) => {
                // TODO: `update_strict_module_statement_list`
            }
            _ => {}
        }
    }

    fn bind_export_assign(&mut self, node: &'cx ast::ExportAssign<'cx>) {
        let container = self.container.unwrap();
        let flags = if node.is_aliasable() {
            SymbolFlags::ALIAS
        } else {
            SymbolFlags::PROPERTY
        };
        let loc = SymbolTableLocation::exports(container);
        let symbol = self.declare_symbol(
            Some(if node.is_export_equals {
                SymbolName::ExportEquals
            } else {
                SymbolName::ExportDefault
            }),
            loc,
            None,
            node.id,
            flags,
            SymbolFlags::all(),
            false,
            false,
        );
        self.create_final_res(node.id, symbol);
        if node.is_export_equals {
            self.set_value_declaration(symbol, node.id);
        }
    }

    fn bind_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) {
        // TODO: is const
        let name = SymbolName::Atom(node.name.name);
        let symbol = self.bind_block_scoped_decl(
            node.id,
            name,
            SymbolFlags::REGULAR_ENUM,
            SymbolFlags::REGULAR_ENUM_EXCLUDES,
        );
        self.final_res.insert(node.id, symbol);
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
                    false,
                    false,
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
        (n.arg.is_string_or_number_lit_like() || n.arg.is_prop_access_entity_name_expr())
            && self.is_narrowable_reference(n.expr)
    }

    fn is_narrowable_reference(&self, expr: &ast::Expr<'_>) -> bool {
        use ast::ExprKind::*;
        match expr.kind {
            // TODO: metaProperty
            Ident(_) | This(_) | Super(_) => true,
            PropAccess(n) => self.is_narrowable_reference(n.expr),
            Paren(n) => self.is_narrowable_reference(n.expr),
            NonNull(n) => self.is_narrowable_reference(n.expr),
            EleAccess(n) => self.ele_access_is_narrowable_reference(n),
            Bin(_) => {
                // TODO: n.op.kind == Comma
                false
            }
            Assign(n) => is_left_hand_side_expr_kind(n.left),
            _ => false,
        }
    }
}

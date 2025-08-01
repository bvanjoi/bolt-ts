use crate::r#trait;
use crate::ty::TypeFlags;

use super::TyChecker;
use super::ast;
use super::errors;
use super::symbol_info::SymbolInfo;
use super::ty;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_stmt(&mut self, stmt: &'cx ast::Stmt) {
        use bolt_ts_ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.check_var_stmt(var),
            Expr(expr) => {
                self.check_expr(expr.expr);
            }
            Fn(f) => self.check_fn_decl(f),
            If(i) => self.check_if_stmt(i),
            Block(block) => self.check_block(block),
            Ret(ret) => self.check_ret_stmt(ret),
            Class(class) => self.check_class_decl(class),
            Interface(interface) => self.check_interface_decl(interface),
            Module(m) => self.check_module_decl(m),
            TypeAlias(ty) => self.check_type_alias_decl(ty),
            For(node) => self.check_for_stmt(node),
            ForIn(node) => self.check_for_in_stmt(node),
            Import(node) => self.check_import_decl(node),
            Export(node) => self.check_export_decl(node),
            ExportAssign(_) => {}
            Empty(_) => {}
            Throw(_) => {}
            Enum(_) => {}
            ForOf(_) => {}
            Break(_) => {}
            Continue(_) => {}
            Try(_) => {}
            While(_) => {}
            Do(_) => {}
            Debugger(_) => {}
            Labeled(n) => {
                self.check_stmt(n.stmt);
            }
        };
    }

    fn check_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) {
        if node.module_spec().is_none() || self.check_external_module_name(node.id) {
            if let ast::ExportClauseKind::Specs(specs) = node.clause.kind {
                // export { a, b as c } from 'xxxx'
                // export { a, b as c }
                for spec in specs.list {
                    self.check_export_spec(spec);
                }
            }
        }
    }

    fn check_export_spec(&mut self, spec: &'cx ast::ExportSpec<'cx>) {
        let id = spec.id();
        self.check_alias_symbol(id);
    }

    fn check_external_module_name(&mut self, node: ast::NodeID) -> bool {
        let Some(module_name) = self.p.node(node).get_external_module_name() else {
            return false;
        };
        // TODO: more checks
        true
    }

    fn check_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) {
        if let Some(clause) = node.clause.kind {
            use bolt_ts_ast::ImportClauseKind::*;
            match clause {
                Ns(n) => {
                    // import * as ns from 'xxxx'
                    self.check_import_binding(n.id);
                }
                Specs(specs) => {
                    // import { a, b as c } from 'xxxx'
                    for spec in specs {
                        use bolt_ts_ast::ImportSpecKind::*;
                        match spec.kind {
                            Shorthand(n) => {
                                // import { a } from 'xxxx'
                                self.check_import_binding(n.id);
                            }
                            Named(_) => {
                                // import { a as b } from 'xxxx'
                            }
                        }
                    }
                }
            }
        }
    }

    fn check_for_in_stmt(&mut self, node: &'cx ast::ForInStmt<'cx>) {
        let right_ty = {
            let ty = self.check_expr(node.expr);
            self.get_non_nullable_ty(ty)
        };
        let left_ty = match node.init {
            ast::ForInitKind::Expr(expr) => self.check_expr(expr),
            ast::ForInitKind::Var(_) => {
                // TODO:
                self.any_ty
            }
        };

        if right_ty == self.never_ty
            || !self.is_type_assignable_to_kind(
                right_ty,
                TypeFlags::NON_PRIMITIVE | TypeFlags::INSTANTIABLE,
                false,
            )
        {
            let error = errors::TheRightHandSideOfAForInStatementMustBeOfTypeAnyAnObjectTypeOrATypeParameterButHereHasType0 {
                span: node.expr.span(),
                ty: self.print_ty(right_ty).to_string(),
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_for_stmt(&mut self, node: &'cx ast::ForStmt<'cx>) {
        if let Some(init) = node.init {
            match init {
                ast::ForInitKind::Var(list) => self.check_var_decl_list(list),
                ast::ForInitKind::Expr(expr) => {
                    self.check_expr(expr);
                }
            }
        }
        if let Some(cond) = node.cond {
            self.check_truthiness_expr(cond);
        }

        if let Some(incr) = node.incr {
            self.check_expr(incr);
        }

        self.check_stmt(node.body);
    }

    fn check_var_stmt(&mut self, var: &'cx ast::VarStmt<'cx>) {
        self.check_var_decl_list(var.list);
    }

    fn check_var_decl_list(&mut self, list: &[&'cx ast::VarDecl<'cx>]) {
        for decl in list {
            self.check_var_decl(decl);
        }
    }

    fn check_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>) {
        self.check_var_like_decl(decl);
    }

    fn check_fn_decl(&mut self, f: &'cx ast::FnDecl<'cx>) {
        self.check_fn_like_decl(f);
    }

    fn check_if_stmt(&mut self, i: &'cx ast::IfStmt) {
        self.check_expr(i.expr);
        self.check_stmt(i.then);
        if let Some(else_then) = i.else_then {
            self.check_stmt(else_then);
        }
    }

    pub(super) fn check_block(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        for item in block.stmts {
            self.check_stmt(item);
        }
    }

    fn check_module_decl(&mut self, ns: &'cx ast::ModuleDecl<'cx>) {
        if let Some(block) = ns.block {
            for item in block.stmts {
                self.check_stmt(item);
            }
        }

        let is_global_augmentation = ns.is_global_scope_argument();
        let is_ambient_external_module = ns.is_ambient();
        if is_ambient_external_module {
            let p = self.parent(ns.id).unwrap();
            if self.p.get(p.module()).is_global_source_file(p) {
                if is_global_augmentation {
                    let error = errors::AugmentationsForTheGlobalScopeCanOnlyBeDirectlyNestedInExternalModulesOrAmbientModuleDeclarations {
                        span: ns.name.span()
                    };
                    self.push_error(Box::new(error));
                } else if let ast::ModuleName::StringLit(lit) = ns.name {
                    let module_name = self.atoms.get(lit.val);
                    if bolt_ts_path::is_external_module_relative(module_name) {
                        let error =
                            errors::AmbientModuleDeclarationCannotSpecifyRelativeModuleName {
                                span: lit.span,
                            };
                        self.push_error(Box::new(error));
                    }
                }
            }
        }
    }

    fn check_ret_stmt(&mut self, node: &ast::RetStmt<'cx>) {
        let Some(container) = self.get_containing_fn_or_class_static_block(node.id) else {
            // delay bug
            return;
        };
        let sig = self.get_sig_from_decl(container);
        let ret_ty = self.get_ret_ty_of_sig(sig);

        if self.config.strict_null_checks()
            || node.expr.is_some()
            || ret_ty.flags.intersects(TypeFlags::NEVER)
        {
            let expr_ty = node
                .expr
                .map(|expr| self.check_expr_with_cache(expr))
                .unwrap_or(self.undefined_ty);
            let c = self.p.node(container);
            if c.is_setter_decl() {
                if node.expr.is_some() {
                    let error = errors::SettersCannotReturnAValue { span: node.span };
                    self.push_error(Box::new(error));
                }
            } else if matches!(c, ast::Node::ClassCtor(_)) {
                if let Some(expr) = node.expr {
                    self.check_type_assignable_to_and_optionally_elaborate(
                        expr_ty,
                        ret_ty,
                        Some(expr.id()),
                        Some(expr.id()),
                    );
                }
            } else if self.get_ret_ty_from_anno(container).is_some() {
                self.check_ret_expr(container, ret_ty, node.expr, expr_ty);
            }
        }
    }

    fn check_ret_expr(
        &mut self,
        container: ast::NodeID,
        ret_ty: &'cx ty::Ty<'cx>,
        ret_expr: Option<&'cx ast::Expr<'cx>>,
        expr_ty: &'cx ty::Ty<'cx>,
    ) {
        if !(ret_ty.kind.is_indexed_access() || ret_ty.kind.is_cond_ty())
            || !self.could_contain_ty_var(ret_ty)
        {
            let error_node = ret_expr.map(|expr| expr.id());
            self.check_type_assignable_to_and_optionally_elaborate(
                expr_ty, ret_ty, error_node, error_node,
            );
        }
    }

    fn check_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.check_class_decl_like(class)
    }

    fn check_type_alias_decl(&mut self, ty: &'cx ast::TypeAliasDecl<'cx>) {
        if let Some(ty_params) = ty.ty_params {
            self.check_ty_params(ty_params);
        }
        self.check_ty(ty.ty);
    }

    pub(super) fn check_accessor_decl(&mut self, decl: &impl r#trait::AccessorLike<'cx>) {
        if let Some(body) = decl.body() {
            self.check_block(body);
        }
    }
}

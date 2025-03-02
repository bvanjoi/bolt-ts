use crate::ty::TypeFlags;

use super::TyChecker;
use super::ast;
use super::errors;
use super::ty;

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_stmt(&mut self, stmt: &'cx ast::Stmt) {
        use bolt_ts_ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.check_var_stmt(var),
            Expr(expr) => {
                self.check_expr(expr);
            }
            Fn(f) => self.check_fn_decl(f),
            If(i) => self.check_if_stmt(i),
            Block(block) => self.check_block(block),
            Return(ret) => self.check_ret_stmt(ret),
            Class(class) => self.check_class_decl(class),
            Interface(interface) => self.check_interface_decl(interface),
            Namespace(ns) => self.check_ns_decl(ns),
            Type(ty) => self.check_type_decl(ty),
            For(node) => self.check_for_stmt(node),
            ForIn(node) => self.check_for_in_stmt(node),
            Empty(_) => {}
            Throw(_) => {}
            Enum(_) => {}
            Import(_) => {}
            Export(_) => {}
            ForOf(_) => {}
            Break(_) => {}
            Continue(_) => {}
            Try(_) => {}
            While(_) => {}
            Do(_) => {}
            Debugger(_) => {}
        };
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
                ast::ForInitKind::Var((kind, list)) => self.check_var_decl_list(list),
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

    fn check_ns_decl(&mut self, ns: &'cx ast::NsDecl<'cx>) {
        if let Some(block) = ns.block {
            self.check_block(block);
        }
    }

    fn check_ret_stmt(&mut self, ret_stmt: &ast::RetStmt<'cx>) {
        let Some(container) = self.get_containing_fn_or_class_static_block(ret_stmt.id) else {
            // delay bug
            return;
        };
        let sig = self.get_sig_from_decl(container);
        let ret_ty = self.get_ret_ty_of_sig(sig);

        let expr_ty = ret_stmt
            .expr
            .map(|expr| self.check_expr_with_cache(expr))
            .unwrap_or(self.undefined_ty);
        if matches!(self.p.node(container), ast::Node::ClassCtor(_)) {
            if let Some(expr) = ret_stmt.expr {
                self.check_type_assignable_to_and_optionally_elaborate(
                    expr_ty,
                    ret_ty,
                    Some(expr.id()),
                    Some(expr.id()),
                );
            }
        } else if self.get_ret_ty_from_anno(container).is_some() {
            self.check_ret_expr(container, ret_ty, ret_stmt.expr, expr_ty);
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

    fn check_type_decl(&mut self, ty: &'cx ast::TypeDecl<'cx>) {
        if let Some(ty_params) = ty.ty_params {
            self.check_ty_params(ty_params);
        }
        self.check_ty(ty.ty);
    }
}

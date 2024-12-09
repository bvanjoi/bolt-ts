mod errors;
mod resolve_class_like;

use bolt_ts_span::ModuleID;

use super::{BinderState, SymbolID, SymbolKind};

use crate::ast;
use crate::bind::{Symbol, SymbolName};
use crate::keyword::{is_prim_ty_name, is_prim_value_name};
use crate::parser::Parser;

pub(super) fn resolve<'cx>(
    state: &mut BinderState<'cx>,
    root: &'cx ast::Program<'cx>,
    p: &'cx Parser<'cx>,
) -> Vec<bolt_ts_errors::Diag> {
    let mut resolver = Resolver {
        state,
        p,
        diags: vec![],
    };
    resolver.resolve_program(root);
    resolver.diags
}

pub(super) struct Resolver<'cx, 'r> {
    state: &'r mut BinderState<'cx>,
    p: &'cx Parser<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

impl<'cx, 'r> Resolver<'cx, 'r> {
    fn push_error(&mut self, module_id: ModuleID, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag {
            module_id,
            inner: error,
        });
    }

    fn resolve_program(&mut self, root: &'cx ast::Program<'cx>) {
        for stmt in root.stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &'cx ast::Stmt<'cx>) {
        use ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.resolve_var_stmt(var),
            Expr(expr) => self.resolve_expr(expr),
            Fn(f) => self.resolve_fn_decl(f),
            If(i) => self.resolve_if_stmt(i),
            Block(block) => self.resolve_block_stmt(block),
            Return(ret) => self.resolve_return_stmt(ret),
            Empty(_) => {}
            Class(class) => self.resolve_class_decl(class),
            Interface(interface) => self.resolve_interface_decl(interface),
            Type(ty) => self.resolve_type_decl(ty),
            Namespace(_) => {}
        };
    }

    fn resolve_type_decl(&mut self, ty: &'cx ast::TypeDecl<'cx>) {
        if let Some(ty_params) = ty.ty_params {
            self.resolve_ty_params(ty_params);
        }
        self.resolve_ty(ty.ty);
    }

    fn resolve_ty_params(&mut self, ty_params: ast::TyParams<'cx>) {
        for ty_param in ty_params {
            self.resolve_ty_param(ty_param);
        }
    }

    fn resolve_ty_param(&mut self, ty_param: &'cx ast::TyParam<'cx>) {
        if let Some(constraint) = ty_param.constraint {
            self.resolve_ty(constraint);
        }
        if let Some(default) = ty_param.default {
            self.resolve_ty(default);
        }
    }

    fn resolve_var_stmt(&mut self, var: &'cx ast::VarStmt<'cx>) {
        for item in var.list {
            self.resolve_var_decl(item);
        }
    }

    fn resolve_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>) {
        if let Some(ty) = decl.ty {
            self.resolve_ty(ty);
        }
        if let Some(init) = decl.init {
            self.resolve_expr(init);
        }
    }

    fn resolve_ty(&mut self, ty: &'cx ast::Ty<'cx>) {
        use ast::TyKind::*;
        match ty.kind {
            Refer(refer) => {
                self.resolve_ty_by_ident(refer.name);
                if let Some(args) = refer.args {
                    for arg in args {
                        self.resolve_ty(arg);
                    }
                }
            }
            Array(array) => {
                self.resolve_array(array);
            }
            IndexedAccess(indexed) => {
                self.resolve_ty(indexed.ty);
                self.resolve_ty(indexed.index_ty);
            }
            Fn(f) => {
                self.resolve_params(f.params);
                self.resolve_ty(f.ret_ty);
            }
            ObjectLit(lit) => {
                for member in lit.members {
                    self.resolve_object_ty_member(member);
                }
            }
            ExprWithArg(node) => {
                if let ast::ExprKind::Ident(ident) = node.kind {
                    self.resolve_ty_by_ident(ident);
                }
            }
            Tuple(tuple) => {
                for ty in tuple.tys {
                    self.resolve_ty(ty);
                }
            }
            Rest(rest) => {
                self.resolve_ty(rest.ty);
            }
            Cond(cond) => {
                self.resolve_ty(cond.check_ty);
                self.resolve_ty(cond.extends_ty);
                self.resolve_ty(cond.true_ty);
                self.resolve_ty(cond.false_ty);
            }
            NumLit(_) | StringLit(_) => {}
            Union(u) => {
                for ty in u.tys {
                    self.resolve_ty(ty);
                }
            }
            Intersection(i) => {
                for ty in i.tys {
                    self.resolve_ty(ty);
                }
            }
        }
    }

    fn resolve_object_ty_member(&mut self, m: &'cx ast::ObjectTyMember<'cx>) {
        use ast::ObjectTyMemberKind::*;
        match m.kind {
            Prop(m) => {
                if let Some(ty) = m.ty {
                    self.resolve_ty(ty);
                }
            }
            Method(_) => {}
            CallSig(_) => {
                // let name = SymbolName::;
                // (name, self.create_object_member_symbol(name, m.id))
            }
            IndexSig(_) => {}
        }
    }

    fn resolve_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.resolve_param(param);
        }
    }

    fn resolve_param(&mut self, param: &'cx ast::ParamDecl<'cx>) {
        if let Some(ty) = param.ty {
            self.resolve_ty(ty);
        }
        if let Some(init) = param.init {
            self.resolve_expr(init);
        }
    }

    fn resolve_array(&mut self, ty: &'cx ast::ArrayTy<'cx>) {
        self.resolve_ty(ty.ele);
    }

    fn resolve_expr(&mut self, expr: &'cx ast::Expr<'cx>) {
        use ast::ExprKind::*;
        match expr.kind {
            Ident(ident) => {
                self.resolve_value_by_ident(ident);
            }
            Call(call) => {
                self.resolve_expr(call.expr);
                for arg in call.args {
                    self.resolve_expr(arg);
                }
            }
            Bin(bin) => {
                self.resolve_expr(bin.left);
                self.resolve_expr(bin.right);
            }
            Assign(assign) => {
                self.resolve_expr(assign.left);
                self.resolve_expr(assign.right);
            }
            ObjectLit(lit) => self.resolve_object_lit(lit),
            ArrayLit(lit) => {
                for ele in lit.elems {
                    self.resolve_expr(ele);
                }
            }
            Cond(cond) => {
                self.resolve_expr(cond.cond);
                self.resolve_expr(cond.when_true);
                self.resolve_expr(cond.when_false);
            }
            Paren(paren) => self.resolve_expr(paren.expr),
            ArrowFn(f) => {
                self.resolve_params(f.params);
                use ast::ArrowFnExprBody::*;
                match f.body {
                    Block(block) => self.resolve_block_stmt(block),
                    Expr(expr) => self.resolve_expr(expr),
                }
            }
            Fn(f) => {
                self.resolve_params(f.params);
                self.resolve_block_stmt(f.body);
            }
            New(new) => self.resolve_expr(new.expr),
            Class(class) => self.resolve_class_like(class),
            _ => (),
        }
    }

    fn resolve_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) {
        for member in lit.members {
            self.resolve_object_member_field(member);
        }
    }

    fn resolve_object_member_field(&mut self, member: &'cx ast::ObjectMemberField<'cx>) {
        self.resolve_expr(member.value);
    }

    fn resolve_fn_decl(&mut self, f: &'cx ast::FnDecl<'cx>) {
        self.resolve_params(f.params);
        if let Some(body) = f.body {
            self.resolve_block_stmt(body);
        }
    }
    fn resolve_if_stmt(&mut self, stmt: &'cx ast::IfStmt<'cx>) {
        self.resolve_expr(stmt.expr);
        self.resolve_stmt(stmt.then);
        if let Some(else_then) = stmt.else_then {
            self.resolve_stmt(else_then);
        }
    }
    fn resolve_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        for stmt in block.stmts {
            self.resolve_stmt(stmt);
        }
    }
    fn resolve_return_stmt(&mut self, ret: &'cx ast::RetStmt<'cx>) {
        if let Some(expr) = ret.expr {
            self.resolve_expr(expr);
        }
    }
    fn resolve_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.resolve_class_like(class);
    }
    fn resolve_interface_decl(&mut self, interface: &'cx ast::InterfaceDecl<'cx>) {
        if let Some(extends) = interface.extends {
            for ty in extends.tys {
                self.resolve_ty(ty);
            }
        }
        for member in interface.members {
            self.resolve_object_ty_member(member);
        }
    }

    fn resolve_value_by_ident(&mut self, ident: &'cx ast::Ident) {
        if is_prim_value_name(ident.name) {
            return;
        }
        let res = self.resolve_symbol_by_ident(ident, SymbolKind::is_value);
        if res == Symbol::ERR {
            let error = errors::CannotFindName {
                span: ident.span,
                name: self.state.atoms.get(ident.name).to_string(),
                errors: vec![],
            };
            let error = self.on_failed_to_resolve_value_symbol(ident, error);
            self.push_error(ident.span.module, Box::new(error));
        }
    }

    fn resolve_ty_by_ident(&mut self, ident: &'cx ast::Ident) {
        if is_prim_ty_name(ident.name) {
            if let Some(error) = self.check_using_type_as_value(ident) {
                self.push_error(ident.span.module, error);
            }
            return;
        }
        self.resolve_symbol_by_ident(ident, SymbolKind::is_type);
    }

    fn resolve_symbol_by_ident(
        &mut self,
        ident: &'cx ast::Ident,
        ns: impl Fn(&SymbolKind) -> bool,
    ) -> SymbolID {
        let res = resolve_symbol_by_ident(self, ident, ns);
        let prev = self.state.final_res.insert(ident.id, res);
        assert!(prev.is_none());
        res
    }

    fn on_failed_to_resolve_value_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        mut error: errors::CannotFindName,
    ) -> errors::CannotFindName {
        if let Some(e) = self.check_missing_prefix(ident) {
            error.errors.push(e);
        }
        if let Some(e) = self.check_using_type_as_value(ident) {
            error.errors.push(e);
        }
        error
    }

    fn check_missing_prefix(&mut self, ident: &'cx ast::Ident) -> Option<crate::Diag> {
        let mut location = ident.id;
        loop {
            if let Some(parent) = self.p.parent(location) {
                location = parent;
            } else {
                break;
            }
            let node = self.p.node(location);
            let ast::Node::ClassDecl(class) = node else {
                continue;
            };
            // TODO: use class symbol;
            if let Some(prop) = class.elems.elems.iter().find_map(|ele| {
                let ast::ClassEleKind::Prop(prop) = ele.kind else {
                    return None;
                };
                let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                    return None;
                };

                (prop_name.name == ident.name).then(|| prop)
            }) {
                if prop
                    .modifiers
                    .map(|mods| {
                        mods.list
                            .iter()
                            .any(|m| m.kind == ast::ModifierKind::Static)
                    })
                    .unwrap_or_default()
                {
                    let ast::PropNameKind::Ident(prop_name) = prop.name.kind else {
                        unreachable!()
                    };
                    let error = errors::DidYourMeanTheStaticMember {
                        span: prop.name.span(),
                        name: format!(
                            "{}.{}",
                            self.state.atoms.get(class.name.name),
                            self.state.atoms.get(prop_name.name)
                        ),
                    };
                    return Some(Box::new(error));
                }
            }
        }

        None
    }

    fn check_using_type_as_value(&mut self, ident: &'cx ast::Ident) -> Option<crate::Diag> {
        if is_prim_ty_name(ident.name) {
            let Some(grand) = self.p.parent(ident.id).and_then(|id| self.p.parent(id)) else {
                return None;
            };
            if self.p.node(grand).is_class_like() {
                return Some(Box::new(errors::AClassCannotImplementAPrimTy {
                    span: ident.span,
                    ty: self.state.atoms.get(ident.name).to_string(),
                }));
            }
        }

        None
    }
}

fn resolve_symbol_by_ident(
    resolver: &Resolver,
    ident: &ast::Ident,
    ns: impl Fn(&SymbolKind) -> bool,
) -> SymbolID {
    let binder = &resolver.state;
    assert!(!binder.final_res.contains_key(&ident.id));
    let name = ident.name;
    let Some(mut scope_id) = binder.node_id_to_scope_id.get(&ident.id).copied() else {
        let name = resolver.state.atoms.get(ident.name);
        unreachable!("the scope of {name:?} is not stored");
    };
    let res = loop {
        if let Some(id) = binder
            .res
            .get(&(scope_id, SymbolName::Normal(name)))
            .copied()
        {
            if ns(&binder.symbols.get(id).kind) {
                break id;
            }
        }

        if let Some(parent) = binder.scope_id_parent_map[&scope_id] {
            scope_id = parent;
        } else {
            break Symbol::ERR;
        }
    };
    res
}

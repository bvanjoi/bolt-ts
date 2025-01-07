mod errors;
mod resolve_call_like;
mod resolve_class_like;

use bolt_ts_span::Module;
use bolt_ts_utils::fx_hashmap_with_capacity;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

use crate::bind::{BinderState, GlobalSymbols, Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::keyword::{is_prim_ty_name, is_prim_value_name};
use crate::parser::Parser;
use crate::{ast, keyword};

pub struct EarlyResolveResult {
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

pub fn early_resolve_parallel<'cx>(
    modules: &[Module],
    states: &[BinderState<'cx>],
    p: &'cx Parser<'cx>,
    global: &'cx GlobalSymbols,
) -> Vec<EarlyResolveResult> {
    modules
        .into_par_iter()
        .enumerate()
        .map(|(idx, m)| {
            let module_id = m.id;
            let root = p.root(module_id);
            let state = &states[idx];
            early_resolve(state, root, p, global)
        })
        .collect()
}

fn early_resolve<'cx>(
    state: &'cx BinderState<'cx>,
    root: &'cx ast::Program<'cx>,
    p: &'cx Parser<'cx>,
    global: &'cx GlobalSymbols,
) -> EarlyResolveResult {
    let final_res = fx_hashmap_with_capacity(state.res.len());
    let mut resolver = Resolver {
        diags: vec![],
        state: &state,
        final_res,
        p,
        global,
    };
    resolver.resolve_program(root);
    let diags = std::mem::take(&mut resolver.diags);
    EarlyResolveResult {
        final_res: resolver.final_res,
        diags,
    }
}

pub(super) struct Resolver<'cx, 'r> {
    state: &'r BinderState<'cx>,
    p: &'cx Parser<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    final_res: FxHashMap<ast::NodeID, SymbolID>,
    global: &'cx GlobalSymbols,
}

impl<'cx> Resolver<'cx, '_> {
    fn push_error(&mut self, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error });
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
            Namespace(ns) => self.resolve_ns_decl(ns),
            Throw(t) => {
                self.resolve_expr(t.expr);
            }
            Enum(enum_decl) => {}
            Import(import_decl) => {}
            Export(export_decl) => {}
            For(for_stmt) => {}
            ForOf(n) => {
                self.resolve_expr(n.expr);
                self.resolve_stmt(n.body);
            }
            ForIn(n) => {
                self.resolve_expr(n.expr);
                self.resolve_stmt(n.body);
            }
            Break(n) => {
                if let Some(ident) = n.label {
                    self.resolve_symbol_by_ident(ident, Symbol::is_value);
                }
            }
            Continue(n) => {
                if let Some(ident) = n.label {
                    self.resolve_symbol_by_ident(ident, Symbol::is_value);
                }
            }
        };
    }

    fn resolve_ns_decl(&mut self, ns: &'cx ast::NsDecl<'cx>) {
        self.resolve_block_stmt(ns.block);
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

    fn resolve_entity_name(&mut self, name: &'cx ast::EntityName<'cx>) {
        use ast::EntityNameKind::*;
        match name.kind {
            Ident(ident) => self.resolve_ty_by_ident(ident),
            Qualified(qualified) => {
                // self.resolve_entity_name(qualified.left);
                // self.resolve_ty_by_ident(qualified.right);
            }
        }
    }

    fn resolve_refer_ty(&mut self, refer: &'cx ast::ReferTy<'cx>) {
        self.resolve_entity_name(refer.name);
        if let Some(ty_args) = refer.ty_args {
            for ty_arg in ty_args.list {
                self.resolve_ty(ty_arg);
            }
        }
    }

    fn resolve_ty(&mut self, ty: &'cx ast::Ty<'cx>) {
        use ast::TyKind::*;
        match ty.kind {
            Refer(refer) => self.resolve_refer_ty(refer),
            Array(array) => {
                self.resolve_array(array);
            }
            IndexedAccess(indexed) => {
                self.resolve_ty(indexed.ty);
                self.resolve_ty(indexed.index_ty);
            }
            Fn(f) => {
                self.resolve_params(f.params);
                self.resolve_ty(f.ty);
            }
            ObjectLit(lit) => {
                for member in lit.members {
                    self.resolve_object_ty_member(member);
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
            Typeof(n) => {
                self.resolve_entity_name(n.name);
            }
            BooleanLit(_) | NullLit(_) => unreachable!(),
        }
    }

    fn resolve_index_sig(&mut self, sig: &'cx ast::IndexSigDecl<'cx>) {
        self.resolve_params(sig.params);
        self.resolve_ty(sig.ty);
    }

    fn resolve_object_ty_member(&mut self, m: &'cx ast::ObjectTyMember<'cx>) {
        use ast::ObjectTyMemberKind::*;
        match m.kind {
            Prop(m) => {
                if let Some(ty) = m.ty {
                    self.resolve_ty(ty);
                }
            }
            Method(m) => {
                if let Some(ty_params) = m.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_params(m.params);
                if let Some(ty) = m.ty {
                    self.resolve_ty(ty);
                }
            }
            CallSig(call) => {
                self.resolve_params(call.params);
                if let Some(ty) = call.ty {
                    self.resolve_ty(ty);
                }
            }
            IndexSig(index) => self.resolve_index_sig(index),
            CtorSig(decl) => {
                self.resolve_params(decl.params);
                if let Some(ty) = decl.ty {
                    self.resolve_ty(ty);
                }
            }
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
                self.resolve_call_like_expr(call);
            }
            New(new) => {
                self.resolve_call_like_expr(new);
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
            Class(class) => self.resolve_class_like(class),
            PrefixUnary(unary) => self.resolve_expr(unary.expr),
            PropAccess(node) => {
                self.resolve_expr(node.expr);
            }
            Typeof(node) => {
                self.resolve_expr(node.expr);
            }
            _ => {}
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
        if let Some(ty_params) = f.ty_params {
            self.resolve_ty_params(ty_params);
        }
        self.resolve_params(f.params);
        if let Some(body) = f.body {
            self.resolve_block_stmt(body);
        }
        if let Some(ty) = f.ty {
            self.resolve_ty(ty);
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
            for ty in extends.list {
                self.resolve_refer_ty(ty);
            }
        }
        for member in interface.members {
            self.resolve_object_ty_member(member);
        }
    }

    fn resolve_value_by_ident(&mut self, ident: &'cx ast::Ident) {
        if ident.name == keyword::IDENT_EMPTY {
            // delay bug
            let prev = self.final_res.insert(ident.id, Symbol::ERR);
            assert!(prev.is_none());
            return;
        } else if is_prim_value_name(ident.name) {
            return;
        }
        let res = self.resolve_symbol_by_ident(ident, Symbol::is_value);
        if res == Symbol::ERR {
            let error = errors::CannotFindName {
                span: ident.span,
                name: self.state.atoms.get(ident.name).to_string(),
                errors: vec![],
            };
            let error = self.on_failed_to_resolve_value_symbol(ident, error);
            self.push_error(Box::new(error));
        }
    }

    fn resolve_ty_by_ident(&mut self, ident: &'cx ast::Ident) {
        if ident.name == keyword::IDENT_EMPTY {
            // delay bug
            let prev = self.final_res.insert(ident.id, Symbol::ERR);
            assert!(prev.is_none());
            return;
        } else if is_prim_ty_name(ident.name) {
            if let Some(error) = self.check_using_type_as_value(ident) {
                self.push_error(error.into_diag());
            }
            return;
        }
        let res = self.resolve_symbol_by_ident(ident, Symbol::is_type);

        if res == Symbol::ERR {
            let error = errors::CannotFindName {
                span: ident.span,
                name: self.state.atoms.get(ident.name).to_string(),
                errors: vec![],
            };
            self.push_error(Box::new(error));
        }
    }

    fn resolve_symbol_by_ident(
        &mut self,
        ident: &'cx ast::Ident,
        ns: impl Fn(&Symbol<'cx>) -> bool,
    ) -> SymbolID {
        let res = resolve_symbol_by_ident(self, ident, ns);
        let prev = self.final_res.insert(ident.id, res);
        assert!(prev.is_none());
        res
    }

    fn check_using_namespace_as_value(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::CannotFindNameHelperKind> {
        let symbol =
            resolve_symbol_by_ident(self, ident, |ns| ns.flags == SymbolFlags::NAMESPACE_MODULE);
        if symbol == Symbol::ERR {
            None
        } else {
            let ns = self.state.symbols.get(symbol).expect_ns().decls[0];
            let span = self.p.node(ns).expect_namespace_decl().name.span;
            let error = errors::CannotFindNameHelperKind::CannotUseNamespaceAsTyOrValue(
                errors::CannotUseNamespaceAsTyOrValue { span, is_ty: false },
            );
            Some(error)
        }
    }

    fn on_failed_to_resolve_value_symbol(
        &mut self,
        ident: &'cx ast::Ident,
        mut error: errors::CannotFindName,
    ) -> errors::CannotFindName {
        if let Some(e) = self.check_missing_prefix(ident) {
            error
                .errors
                .push(errors::CannotFindNameHelperKind::DidYouMeanTheStaticMember(
                    e,
                ));
        }
        if let Some(e) = self.check_using_type_as_value(ident) {
            error.errors.push(e);
        }
        if let Some(e) = self.check_using_namespace_as_value(ident) {
            error.errors.push(e);
        }
        error
    }

    fn check_missing_prefix(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::DidYourMeanTheStaticMember> {
        let mut location = ident.id;
        while let Some(parent) = self.p.parent(location) {
            location = parent;
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

                (prop_name.name == ident.name).then_some(prop)
            }) {
                if prop
                    .modifiers
                    .map(|mods| mods.flags.contains(ast::ModifierKind::Static))
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
                    return Some(error);
                }
            }
        }

        None
    }

    fn check_using_type_as_value(
        &mut self,
        ident: &'cx ast::Ident,
    ) -> Option<errors::CannotFindNameHelperKind> {
        if is_prim_ty_name(ident.name) {
            let grand = self
                .p
                .parent(ident.id)
                .and_then(|parent| self.p.parent(parent))?;
            let container = self.p.parent(grand)?;
            let grand_node = self.p.node(grand);
            let container_node = self.p.node(container);
            if grand_node.as_class_implements_clause().is_some() && container_node.is_class_like() {
                return Some(
                    errors::CannotFindNameHelperKind::AClassCannotImplementAPrimTy(
                        errors::AClassCannotImplementAPrimTy {
                            span: ident.span,
                            ty: self.state.atoms.get(ident.name).to_string(),
                        },
                    ),
                );
            } else if grand_node.as_interface_extends_clause().is_some()
                && container_node.is_interface_decl()
            {
                return Some(
                    errors::CannotFindNameHelperKind::AnInterfaceCannotExtendAPrimTy(
                        errors::AnInterfaceCannotExtendAPrimTy {
                            span: ident.span,
                            ty: self.state.atoms.get(ident.name).to_string(),
                        },
                    ),
                );
            }
        }

        None
    }
}

fn resolve_symbol_by_ident<'a, 'cx>(
    resolver: &'a Resolver<'cx, 'a>,
    ident: &'cx ast::Ident,
    ns: impl Fn(&'a Symbol<'cx>) -> bool,
) -> SymbolID {
    let binder = &resolver.state;
    let key = SymbolName::Normal(ident.name);
    let Some(mut scope_id) = binder.node_id_to_scope_id.get(&ident.id).copied() else {
        let name = resolver.state.atoms.get(ident.name);
        unreachable!("the scope of {name:?} is not stored");
    };
    loop {
        if !scope_id.is_root() {
            if let Some(id) = binder.res.get(&(scope_id, SymbolName::Container)).copied() {
                let symbol = binder.symbols.get(id);
                if ns(symbol) {
                    let container = symbol.expect_block_container();
                    if let Some(id) = container.locals.get(&key) {
                        break *id;
                    }
                }
            }
        }

        if let Some(id) = binder.res.get(&(scope_id, key)).copied() {
            if ns(binder.symbols.get(id)) {
                break id;
            }
        }

        if let Some(parent) = binder.scope_id_parent_map[scope_id.index_as_usize()] {
            scope_id = parent;
        } else if let Some(symbol) = resolver.global.get(key) {
            break symbol;
        } else {
            break Symbol::ERR;
        }
    }
}

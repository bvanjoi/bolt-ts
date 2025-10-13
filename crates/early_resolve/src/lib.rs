mod errors;
mod on_failed_value_resolve;
mod on_success_resolve;
mod resolve_call_like;
mod resolve_class_like;

use bolt_ts_parser::ParsedMap;
use rayon::prelude::*;
use rustc_hash::FxHashMap;

use bolt_ts_ast::keyword;
use bolt_ts_ast::keyword::{is_prim_ty_name, is_prim_value_name};

use bolt_ts_ast::{self as ast, NodeFlags};
use bolt_ts_binder::{
    BinderResult, GlobalSymbols, MergedSymbols, Symbol, SymbolFlags, SymbolID, SymbolName,
    SymbolTable,
};
use bolt_ts_span::Module;
use bolt_ts_utils::fx_hashmap_with_capacity;

pub struct EarlyResolveResult {
    // TODO: use `NodeId::index` is enough
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

pub fn early_resolve_parallel<'cx>(
    modules: &[Module],
    states: &[BinderResult<'cx>],
    p: &'cx ParsedMap<'cx>,
    globals: &'cx GlobalSymbols,
    merged: &'cx MergedSymbols,
    atoms: &'cx bolt_ts_atom::AtomIntern,
) -> Vec<EarlyResolveResult> {
    modules
        .into_par_iter()
        .map(|m| {
            let module_id = m.id();
            let is_default_lib = m.is_default_lib();
            let root = p.root(module_id);
            let result = early_resolve(states, module_id, root, p, globals, merged, atoms);
            assert!(!is_default_lib || result.diags.is_empty());
            result
        })
        .collect()
}

fn early_resolve<'cx>(
    states: &[BinderResult<'cx>],
    module_id: bolt_ts_span::ModuleID,
    root: &'cx ast::Program<'cx>,
    p: &'cx ParsedMap<'cx>,
    globals: &'cx GlobalSymbols,
    merged: &'cx MergedSymbols,
    atoms: &'cx bolt_ts_atom::AtomIntern,
) -> EarlyResolveResult {
    let final_res = fx_hashmap_with_capacity(states[module_id.as_usize()].final_res.len());
    let mut resolver = Resolver {
        diags: vec![],
        states,
        module_id,
        final_res,
        p,
        globals,
        merged,
        atoms,
    };
    resolver.resolve_program(root);
    let diags = std::mem::take(&mut resolver.diags);
    EarlyResolveResult {
        final_res: resolver.final_res,
        diags,
    }
}

pub struct Resolver<'cx, 'r, 'atoms> {
    states: &'r [BinderResult<'cx>],
    module_id: bolt_ts_span::ModuleID,
    p: &'cx ParsedMap<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    final_res: FxHashMap<ast::NodeID, SymbolID>,
    globals: &'cx GlobalSymbols,
    merged: &'cx MergedSymbols,
    atoms: &'atoms bolt_ts_atom::AtomIntern,
}

impl<'cx> Resolver<'cx, '_, '_> {
    fn locals(&self, id: ast::NodeID) -> Option<&SymbolTable> {
        let idx = id.module().as_usize();
        debug_assert!(idx < self.states.len());
        unsafe { self.states.get_unchecked(idx).locals.get(&id) }
    }

    fn symbol(&self, symbol_id: SymbolID) -> &bolt_ts_binder::Symbol {
        let idx = symbol_id.module().as_usize();
        debug_assert!(idx < self.states.len());
        unsafe { self.states.get_unchecked(idx).symbols.get(symbol_id) }
    }

    fn symbol_of_decl(&self, decl: ast::NodeID) -> SymbolID {
        let idx = decl.module().as_usize();
        debug_assert!(idx < self.states.len());
        unsafe { self.states.get_unchecked(idx).final_res[&decl] }
    }

    fn push_error(&mut self, error: bolt_ts_middle::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error });
    }

    fn resolve_program(&mut self, root: &'cx ast::Program<'cx>) {
        for stmt in root.stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn parent(&self, node: ast::NodeID) -> Option<ast::NodeID> {
        debug_assert!(node.module() == self.module_id);
        let idx = self.module_id.as_usize();
        debug_assert!(idx < self.states.len());
        unsafe { self.states.get_unchecked(idx).parent_map.parent(node) }
    }

    fn node_query(&self) -> bolt_ts_binder::NodeQuery<'cx, '_> {
        bolt_ts_binder::NodeQuery::new(
            &self.states[self.module_id.as_usize()].parent_map,
            self.p.get(self.module_id),
        )
    }

    fn resolve_stmt(&mut self, stmt: &'cx ast::Stmt<'cx>) {
        use bolt_ts_ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.resolve_var_stmt(var),
            Expr(expr) => self.resolve_expr(expr.expr),
            Fn(f) => self.resolve_fn_decl(f),
            If(i) => self.resolve_if_stmt(i),
            Block(block) => self.resolve_block_stmt(block),
            Ret(ret) => self.resolve_return_stmt(ret),
            Empty(_) => {}
            Class(class) => self.resolve_class_decl(class),
            Interface(interface) => self.resolve_interface_decl(interface),
            TypeAlias(node) => self.resolve_type_alias_decl(node),
            Module(ns) => self.resolve_module_decl(ns),
            Throw(t) => {
                self.resolve_expr(t.expr);
            }
            Enum(n) => {
                for member in n.members {
                    self.resolve_enum_member(member);
                }
            }
            Import(_) => {}
            Export(n) => self.resolve_export(n),
            For(n) => {
                if let Some(init) = n.init {
                    match init {
                        ast::ForInitKind::Var(decls) => {
                            for decl in decls {
                                self.resolve_var_decl(decl)
                            }
                        }
                        ast::ForInitKind::Expr(expr) => {
                            self.resolve_expr(expr);
                        }
                    }
                }
                if let Some(cond) = n.cond {
                    self.resolve_expr(cond);
                }
                if let Some(update) = n.incr {
                    self.resolve_expr(update);
                }
                self.resolve_stmt(n.body);
            }
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
                    self.resolve_symbol_by_ident(ident, SymbolFlags::VALUE);
                }
            }
            Continue(n) => {
                if let Some(ident) = n.label {
                    self.resolve_symbol_by_ident(ident, SymbolFlags::VALUE);
                }
            }
            Try(_) => {}
            While(_) => {}
            Do(_) => {}
            Debugger(_) => {}
            ExportAssign(n) => match n.expr.kind {
                bolt_ts_ast::ExprKind::Ident(ident) => {
                    let res = self.resolve_symbol_by_ident(ident, SymbolFlags::all());
                    if res.symbol == Symbol::ERR {
                        let name = self.atoms.get(ident.name).to_string();
                        let error = errors::CannotFindName {
                            span: ident.span,
                            name,
                            errors: vec![],
                        };
                        self.push_error(Box::new(error));
                    }
                }
                _ => self.resolve_expr(n.expr),
            },
            Labeled(n) => {
                self.resolve_stmt(n.stmt);
            }
            Switch(_) => {}
        };
    }

    fn resolve_enum_member(&mut self, n: &'cx ast::EnumMember<'cx>) {
        if let Some(init) = n.init {
            self.resolve_expr(init);
        }
    }

    fn resolve_export(&mut self, export: &'cx ast::ExportDecl<'cx>) {
        match export.clause.kind {
            ast::ExportClauseKind::Glob(_) => {}
            ast::ExportClauseKind::Ns(_) => {}
            ast::ExportClauseKind::Specs(specs) => {
                for spec in specs.list {
                    use ast::ExportSpecKind::*;
                    match spec.kind {
                        Shorthand(n) => {
                            const MEANING: SymbolFlags = SymbolFlags::VALUE
                                .union(SymbolFlags::TYPE)
                                .union(SymbolFlags::NAMESPACE);
                            self.resolve_symbol_by_ident(n.name, MEANING);
                        }
                        Named(_) => {
                            // TODO
                        }
                    }
                }
            }
        }
    }

    fn resolve_module_decl(&mut self, ns: &'cx ast::ModuleDecl<'cx>) {
        if let Some(block) = ns.block {
            self.resolve_module_block(block);
        }
    }

    fn resolve_module_block(&mut self, block: &'cx ast::ModuleBlock<'cx>) {
        for stmt in block.stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_type_alias_decl(&mut self, ty: &'cx ast::TypeAliasDecl<'cx>) {
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

    fn resolve_entity_name(&mut self, name: &'cx ast::EntityName<'cx>, meaning: SymbolFlags) {
        use bolt_ts_ast::EntityNameKind::*;
        match name.kind {
            Ident(ident) => {
                if meaning.contains(SymbolFlags::TYPE) {
                    let report = meaning == SymbolFlags::TYPE;
                    let res = self.resolve_ty_by_ident(ident, report);
                    if res != Symbol::ERR || report {
                        let prev = self.final_res.insert(ident.id, res);
                        assert!(prev.is_none());
                        return;
                    }
                }

                if meaning == SymbolFlags::VALUE {
                    self.resolve_value_by_ident(ident);
                } else if meaning.contains(SymbolFlags::NAMESPACE) {
                    self.resolve_symbol_by_ident(ident, meaning);
                } else {
                    unreachable!()
                }
            }
            Qualified(qualified) => {
                self.resolve_entity_name(
                    qualified.left,
                    SymbolFlags::NAMESPACE.union(SymbolFlags::TYPE),
                );
                // resolve the value of right in checker.
            }
        }
    }

    fn resolve_refer_ty(&mut self, refer: &'cx ast::ReferTy<'cx>) {
        self.resolve_entity_name(refer.name, SymbolFlags::TYPE);
        if let Some(ty_args) = refer.ty_args {
            self.resolve_tys(ty_args.list);
        }
    }

    fn resolve_tys(&mut self, tys: &'cx [&'cx ast::Ty<'cx>]) {
        for ty in tys {
            self.resolve_ty(ty);
        }
    }

    fn resolve_ty(&mut self, ty: &'cx ast::Ty<'cx>) {
        use bolt_ts_ast::TyKind::*;
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
                if let Some(ty_params) = f.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_params(f.params);
                self.resolve_ty(f.ty);
            }
            Ctor(node) => {
                if let Some(ty_params) = node.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_params(node.params);
                self.resolve_ty(node.ty);
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
            Lit(_) => {}
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
                self.resolve_entity_name(n.name, SymbolFlags::VALUE);
            }
            Mapped(n) => {
                self.resolve_ty_param(n.ty_param);
                if let Some(name_ty) = n.name_ty {
                    self.resolve_ty(name_ty);
                }
                if let Some(ty) = n.ty {
                    self.resolve_ty(ty);
                }
            }
            TyOp(n) => {
                self.resolve_ty(n.ty);
            }
            Pred(n) => {
                // self.resolve_value_by_ident(n.name);
                if let Some(ty) = n.ty {
                    self.resolve_ty(ty);
                }
            }
            Paren(n) => {
                self.resolve_ty(n.ty);
            }
            Infer(n) => {
                self.resolve_ty_param(n.ty_param);
            }
            Nullable(n) => {
                self.resolve_ty(n.ty);
            }
            Intrinsic(_) | This(_) => {}
            NamedTuple(n) => {
                self.resolve_ty(n.ty);
            }
            TemplateLit(n) => {
                for item in n.spans {
                    self.resolve_ty(item.ty);
                }
            }
        }
    }

    fn resolve_index_sig(&mut self, sig: &'cx ast::IndexSigDecl<'cx>) {
        self.resolve_ty(sig.key_ty);
        self.resolve_ty(sig.ty);
    }

    fn resolve_prop_name(&mut self, name: &'cx ast::PropName<'cx>) {
        use bolt_ts_ast::PropNameKind::*;
        if let Computed(n) = name.kind {
            self.resolve_expr(n.expr);
        }
    }

    fn resolve_object_ty_member(&mut self, m: &'cx ast::ObjectTyMember<'cx>) {
        use bolt_ts_ast::ObjectTyMemberKind::*;
        match m.kind {
            Prop(m) => {
                self.resolve_prop_name(m.name);
                if let Some(ty) = m.ty {
                    self.resolve_ty(ty);
                }
            }
            Method(m) => {
                if let Some(ty_params) = m.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_prop_name(m.name);
                self.resolve_params(m.params);
                if let Some(ty) = m.ty {
                    self.resolve_ty(ty);
                }
            }
            CallSig(call) => {
                if let Some(ty_params) = call.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_params(call.params);
                if let Some(ty) = call.ty {
                    self.resolve_ty(ty);
                }
            }
            IndexSig(index) => self.resolve_index_sig(index),
            CtorSig(decl) => {
                if let Some(ty_params) = decl.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_params(decl.params);
                if let Some(ty) = decl.ty {
                    self.resolve_ty(ty);
                }
            }
            Setter(n) => {
                self.resolve_params(n.params);
            }
            Getter(n) => {
                if let Some(ty) = n.ty {
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
        use bolt_ts_ast::ExprKind::*;
        match expr.kind {
            ArrowFn(f) => {
                if let Some(ty_params) = f.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_params(f.params);
                if let Some(ty) = f.ty {
                    self.resolve_ty(ty);
                }
                use bolt_ts_ast::ArrowFnExprBody::*;
                match f.body {
                    Block(block) => self.resolve_block_stmt(block),
                    Expr(expr) => self.resolve_expr(expr),
                }
            }
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
            Fn(f) => {
                self.resolve_params(f.params);
                if let Some(ty) = f.ty {
                    self.resolve_ty(ty);
                }
                self.resolve_block_stmt(f.body);
            }
            Class(class) => self.resolve_class_like(class),
            PrefixUnary(unary) => self.resolve_expr(unary.expr),
            PostfixUnary(unary) => self.resolve_expr(unary.expr),
            PropAccess(node) => {
                self.resolve_prop_access_expr(node);
            }
            EleAccess(node) => {
                self.resolve_expr(node.expr);
                self.resolve_expr(node.arg);
            }
            Typeof(node) => {
                self.resolve_expr(node.expr);
            }
            Void(n) => {
                self.resolve_expr(n.expr);
            }
            As(n) => {
                self.resolve_expr(n.expr);
                if !n.ty.is_const_ty_refer() {
                    self.resolve_ty(n.ty);
                }
            }
            TyAssertion(n) => {
                self.resolve_ty(n.ty);
                self.resolve_expr(n.expr);
            }
            Template(n) => {
                self.resolve_template_expr(n);
            }
            NonNull(n) => {
                self.resolve_expr(n.expr);
            }
            ExprWithTyArgs(n) => {
                self.resolve_expr(n.expr);
                if let Some(ty_args) = n.ty_args {
                    self.resolve_tys(ty_args.list);
                }
            }
            SpreadElement(n) => {
                self.resolve_expr(n.expr);
            }
            Satisfies(n) => {
                self.resolve_expr(n.expr);
                self.resolve_ty(n.ty);
            }
            TaggedTemplate(n) => {
                self.resolve_expr(n.tag);
                if let Some(ty_args) = n.ty_args {
                    self.resolve_tys(ty_args.list);
                }
                self.resolve_expr(n.tpl);
            }
            This(_)
            | BoolLit(_)
            | NumLit(_)
            | BigIntLit(_)
            | StringLit(_)
            | NullLit(_)
            | Omit(_)
            | Super(_)
            | RegExpLit(_)
            | NoSubstitutionTemplateLit(_)
            | JsxFrag(_) => {}
            JsxElem(n) => {
                self.resolve_jsx_ele(n);
            }
            JsxSelfClosingElem(n) => {
                self.resolve_jsx_self_closing_ele(n);
            }
            Delete(n) => {
                self.resolve_expr(n.expr);
            }
        }
    }

    fn resolve_prop_access_expr(&mut self, n: &'cx ast::PropAccessExpr<'cx>) {
        self.resolve_expr(n.expr);
        // don't try resolve `n.ident` because `n.expr` maybe a
        // late symbol, for example, `const obj = { [${xxxx}abc]: 'xxxx' }`
    }

    fn resolve_jsx_tag_name(&mut self, n: ast::JsxTagName<'cx>) {
        use bolt_ts_ast::JsxTagName::*;
        match n {
            Ident(ident) => {
                // TODO:
                // self.resolve_value_by_ident(ident)
            }
            Ns(ns) => {
                // TODO:
                // self.resolve_value_by_ident(ns.name)
            }
            PropAccess(n) => {
                // TODO:
                // self.resolve_prop_access_expr(n)
            }
            This(_) => {}
        };
    }

    fn resolve_jsx_attr(&mut self, n: &'cx ast::JsxAttr<'cx>) {
        use ast::JsxAttr::*;
        match n {
            Spread(n) => {
                self.resolve_expr(n.expr);
            }
            Named(n) => {
                if let Some(v) = n.init {
                    use bolt_ts_ast::JsxAttrValue::*;
                    match v {
                        Expr(n) => {
                            if let Some(expr) = n.expr {
                                self.resolve_expr(expr)
                            }
                        }
                        Ele(n) => self.resolve_jsx_ele(n),
                        SelfClosingEle(n) => {
                            self.resolve_jsx_self_closing_ele(n);
                        }
                        StringLit(_) | Frag(_) => {}
                    }
                }
            }
        }
    }

    fn resolve_jsx_self_closing_ele(&mut self, ele: &'cx ast::JsxSelfClosingElem<'cx>) {
        self.resolve_jsx_tag_name(ele.tag_name);
        if let Some(ty_args) = ele.ty_args {
            self.resolve_tys(ty_args.list);
        }
        for attr in ele.attrs {
            self.resolve_jsx_attr(attr);
        }
    }

    fn resolve_jsx_ele(&mut self, ele: &'cx ast::JsxElem<'cx>) {
        self.resolve_jsx_tag_name(ele.opening_elem.tag_name);
        if let Some(ty_args) = ele.opening_elem.ty_args {
            self.resolve_tys(ty_args.list);
        }
        for attr in ele.opening_elem.attrs {
            self.resolve_jsx_attr(attr);
        }

        for child in ele.children {
            self.resolve_jsx_child(child);
        }

        self.resolve_jsx_tag_name(ele.closing_elem.tag_name);
    }

    fn resolve_jsx_child(&mut self, child: &'cx ast::JsxChild<'cx>) {
        use bolt_ts_ast::JsxChild::*;
        match child {
            Expr(n) => {
                if let Some(expr) = n.expr {
                    self.resolve_expr(expr);
                }
            }
            Elem(n) => {
                self.resolve_jsx_ele(n);
            }
            SelfClosingEle(n) => {
                self.resolve_jsx_self_closing_ele(n);
            }
            Frag(_) | Text(_) => {}
        }
    }

    fn resolve_template_expr(&mut self, n: &'cx ast::TemplateExpr<'cx>) {
        for item in n.spans {
            self.resolve_expr(item.expr);
        }
    }

    fn resolve_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) {
        for member in lit.members {
            self.resolve_object_member(member);
        }
    }

    fn resolve_object_member(&mut self, member: &'cx ast::ObjectMember<'cx>) {
        use bolt_ts_ast::ObjectMemberKind::*;
        match member.kind {
            Shorthand(n) => {
                self.resolve_value_by_ident(n.name);
            }
            PropAssignment(n) => {
                self.resolve_expr(n.init);
            }
            Method(n) => {
                if let Some(ty_params) = n.ty_params {
                    self.resolve_ty_params(ty_params);
                }
                self.resolve_params(n.params);
                if let Some(ty) = n.ty {
                    self.resolve_ty(ty);
                }
                self.resolve_block_stmt(n.body);
            }
            SpreadAssignment(n) => {
                self.resolve_expr(n.expr);
            }
            Getter(n) => {
                self.resolve_prop_name(n.name);
                if let Some(ty) = n.ty {
                    self.resolve_ty(ty);
                }
                if let Some(body) = n.body {
                    self.resolve_block_stmt(body);
                }
            }
            Setter(n) => {
                self.resolve_prop_name(n.name);
                self.resolve_params(n.params);
                if let Some(body) = n.body {
                    self.resolve_block_stmt(body);
                }
            }
        }
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
        if let Some(ty_params) = interface.ty_params {
            self.resolve_ty_params(ty_params);
        }
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
            // TODO: delay bug
            let prev = self.final_res.insert(ident.id, Symbol::ERR);
            assert!(prev.is_none());
            return;
        } else if is_prim_value_name(ident.name) {
            return;
        }
        let res = self.resolve_symbol_by_ident(ident, SymbolFlags::VALUE);
        if res.symbol == Symbol::ERR {
            let name = self.atoms.get(ident.name).to_string();
            let error = errors::CannotFindName {
                span: ident.span,
                name,
                errors: vec![],
            };
            let error = self.on_failed_to_resolve_value_symbol(ident, error);
            self.push_error(Box::new(error));
        } else {
            self.on_success_resolved_value_symbol(
                ident,
                res.symbol,
                res.associated_declaration_for_containing_initializer_or_binding_name,
            );
        }
    }

    fn resolve_ty_by_ident(&mut self, ident: &'cx ast::Ident, report: bool) -> SymbolID {
        if ident.name == keyword::IDENT_EMPTY {
            // delay bug
            return Symbol::ERR;
        } else if is_prim_ty_name(ident.name) {
            if let Some(error) = self.check_using_type_as_value(ident) {
                self.push_error(error.into_diag());
            }
            return Symbol::ERR;
        }

        let mut res = resolve_symbol_by_ident(self, ident, SymbolFlags::TYPE).symbol;

        if res == Symbol::ERR && report {
            let name = self.atoms.get(ident.name).to_string();
            let error = errors::CannotFindName {
                span: ident.span,
                name,
                errors: vec![],
            };
            self.push_error(Box::new(error));
        } else if res != Symbol::ERR {
            self.on_success_resolved_type_symbol(ident, &mut res);
        };

        res
    }

    fn resolve_symbol_by_ident(
        &mut self,
        ident: &'cx ast::Ident,
        meaning: SymbolFlags,
    ) -> ResolvedResult {
        let res = resolve_symbol_by_ident(self, ident, meaning);
        let prev = self.final_res.insert(ident.id, res.symbol);
        assert!(
            prev.is_none(),
            "the symbol of {:?} is already resolved",
            self.atoms.get(ident.name)
        );
        res
    }

    fn is_type_param_symbol_declared_in_container(
        &self,
        symbol: SymbolID,
        container: ast::NodeID,
    ) -> bool {
        let Some(decls) = &self.symbol(symbol).decls else {
            return false;
        };
        for decl in decls {
            let decl = *decl;
            if self.p.node(decl).is_ty_param() {
                // TODO: js doc template tag
                let parent = self.parent(decl);
                if let Some(parent) = parent
                    && parent == container
                {
                    // TODO: js doc template tag
                    return true;
                }
            }
        }
        false
    }
}

pub struct ResolvedResult {
    symbol: SymbolID,
    associated_declaration_for_containing_initializer_or_binding_name: Option<ast::NodeID>,
}

pub fn resolve_symbol_by_ident<'a, 'cx>(
    resolver: &'a Resolver<'cx, 'a, '_>,
    ident: &'cx ast::Ident,
    meaning: SymbolFlags,
) -> ResolvedResult {
    use ast::Node::*;
    let key = SymbolName::Atom(ident.name);
    let mut associated_declaration_for_containing_initializer_or_binding_name = None;
    let mut last_location = Some(ident.id);
    let mut location = resolver.parent(ident.id);

    fn get_symbol(
        resolver: &Resolver,
        symbols: &SymbolTable,
        name: SymbolName,
        meaning: SymbolFlags,
    ) -> Option<SymbolID> {
        if !meaning.is_empty()
            && let Some(symbol) = symbols.0.get(&name)
        {
            let symbols = &resolver.states[symbol.module().as_usize()].symbols;
            let symbol = resolver.merged.get_merged_symbol(*symbol, symbols);
            let flags = resolver.symbol(symbol).flags;
            if flags.intersects(meaning) {
                return Some(symbol);
            } else if flags.intersects(SymbolFlags::ALIAS) {
                // bound of parallel, handle this case in late_resolve
                return Some(symbol);
            }
        }
        None
    }

    while let Some(id) = location {
        if let Some(locals) = resolver.locals(id)
            && !resolver.p.get(id.module()).is_global_source_file(id)
            && let Some(symbol) = get_symbol(resolver, locals, key, meaning)
        {
            let res_flags = resolver.symbol(symbol).flags;
            if res_flags.intersects(SymbolFlags::ALIAS) {
                // handle this case in late_resolve
                return ResolvedResult {
                    symbol,
                    associated_declaration_for_containing_initializer_or_binding_name,
                };
            }

            let mut use_result = true;
            let n = resolver.p.node(id);
            if n.is_fn_like()
                && last_location.is_some_and(|last_location| match n {
                    FnDecl(f) => f.body.is_none_or(|body| last_location != body.id),
                    _ => false, //TODO: other function decl,
                })
            {
                let flags = meaning.intersection(res_flags);
                if flags.intersects(SymbolFlags::TYPE) {
                    // TODO:
                }
                if flags.intersects(SymbolFlags::VARIABLE)
                    && res_flags.intersects(SymbolFlags::FUNCTION_SCOPED_VARIABLE)
                {
                    let last = resolver.p.node(last_location.unwrap());
                    use_result = last.is_param_decl();
                };
            } else if let Some(cond) = n.as_cond_ty() {
                use_result = last_location.is_some_and(|last| last == cond.true_ty.id());
            }
            if use_result {
                return ResolvedResult {
                    symbol,
                    associated_declaration_for_containing_initializer_or_binding_name,
                };
            }
        }

        let n = resolver.p.node(id);
        match n {
            Program(_) if !resolver.p.get(id.module()).is_external_or_commonjs_module() => (),
            Program(_) | ModuleDecl(_) => {
                let symbol_id = resolver.merged.get_merged_symbol(
                    resolver.symbol_of_decl(id),
                    &resolver.states[id.module().as_usize()].symbols,
                );
                let module_exports = &resolver.symbol(symbol_id).exports();
                if n.is_program()
                    || (n
                        .as_module_decl()
                        .is_some_and(|n| !n.is_global_scope_argument())
                        && resolver.p.node_flags(id).intersects(NodeFlags::AMBIENT))
                {
                    // TODO: default
                    if let Some(module_export) = module_exports.and_then(|e| e.0.get(&key).copied())
                        && resolver
                            .symbol(module_export)
                            .flags
                            .intersects(SymbolFlags::ALIAS)
                    {
                        // TODO:
                    }
                }

                if let Some(module_export) = module_exports.and_then(|e| e.0.get(&key).copied())
                    && resolver
                        .symbol(module_export)
                        .flags
                        .intersects(meaning & SymbolFlags::MODULE_MEMBER)
                {
                    return ResolvedResult {
                        symbol: module_export,
                        associated_declaration_for_containing_initializer_or_binding_name,
                    };
                }
            }
            EnumDecl(_) => {
                if let Some(exports) = resolver.symbol(resolver.symbol_of_decl(id)).exports()
                    && let Some(res) =
                        get_symbol(resolver, exports, key, meaning & SymbolFlags::ENUM_MEMBER)
                {
                    return ResolvedResult {
                        symbol: res,
                        associated_declaration_for_containing_initializer_or_binding_name,
                    };
                }
            }
            ClassDecl(_) | ClassExpr(_) | InterfaceDecl(_) => {
                if let Some(res) = resolver
                    .symbol(resolver.symbol_of_decl(id))
                    .members()
                    .and_then(|m| m.0.get(&key))
                    .copied()
                    && resolver
                        .symbol(res)
                        .flags
                        .intersects(meaning & SymbolFlags::TYPE)
                {
                    if !resolver.is_type_param_symbol_declared_in_container(res, id) {
                        break;
                    }
                    // TODO: last location
                    return ResolvedResult {
                        symbol: res,
                        associated_declaration_for_containing_initializer_or_binding_name,
                    };
                }
                if let Some(c) = n.as_class_expr()
                    && meaning.intersects(SymbolFlags::CLASS)
                    && c.name.is_some_and(|n| n.name == ident.name)
                {
                    return ResolvedResult {
                        symbol: resolver.symbol_of_decl(id),
                        associated_declaration_for_containing_initializer_or_binding_name,
                    };
                }
            }
            ExprWithTyArgs(expr) => {
                if last_location.is_some_and(|l| l == expr.expr.id())
                    && resolver
                        .p
                        .node(resolver.parent(id).unwrap())
                        .is_class_extends_clause()
                {
                    let container = resolver.parent(resolver.parent(id).unwrap()).unwrap();
                    let c = resolver.p.node(container);
                    if c.is_class_like()
                        && let Some(res) = resolver
                            .symbol(resolver.symbol_of_decl(container))
                            .members()
                            .and_then(|m| get_symbol(resolver, m, key, meaning & SymbolFlags::TYPE))
                    {
                        assert!(!resolver.symbol(res).flags.intersects(SymbolFlags::ALIAS));
                        // TODO: throw ERROR
                        return ResolvedResult {
                            symbol: Symbol::ERR,
                            associated_declaration_for_containing_initializer_or_binding_name,
                        };
                    }
                }
            }
            ArrowFnExpr(_) | ClassMethodElem(_) | ClassCtor(_) | GetterDecl(_) | SetterDecl(_)
            | FnDecl(_) => {
                if meaning.intersects(SymbolFlags::VARIABLE)
                    && ident.name == keyword::IDENT_ARGUMENTS
                {
                    return ResolvedResult {
                        symbol: Symbol::ARGUMENTS,
                        associated_declaration_for_containing_initializer_or_binding_name,
                    };
                }
            }
            FnExpr(f) => {
                if meaning.intersects(SymbolFlags::VARIABLE)
                    && ident.name == keyword::IDENT_ARGUMENTS
                {
                    return ResolvedResult {
                        symbol: Symbol::ARGUMENTS,
                        associated_declaration_for_containing_initializer_or_binding_name,
                    };
                }
                if meaning.intersects(SymbolFlags::FUNCTION)
                    && f.name.is_some_and(|n| n.name == ident.name)
                {
                    return ResolvedResult {
                        symbol: resolver.symbol_of_decl(id),
                        associated_declaration_for_containing_initializer_or_binding_name,
                    };
                }
            }
            ParamDecl(p) => {
                if let Some(last_location) = last_location
                    && p.init.is_some_and(|init| init.id() == last_location)
                    && associated_declaration_for_containing_initializer_or_binding_name.is_none()
                {
                    associated_declaration_for_containing_initializer_or_binding_name = Some(id);
                }
            }
            _ => {}
        }
        last_location = location;
        location = resolver.parent(id);
    }

    if let Some(symbol) = get_symbol(resolver, resolver.globals, key, meaning) {
        assert!(!resolver.symbol(symbol).flags.intersects(SymbolFlags::ALIAS));
        return ResolvedResult {
            symbol,
            associated_declaration_for_containing_initializer_or_binding_name,
        };
    } else if ident.name == keyword::IDENT_GLOBAL_THIS && meaning.intersects(SymbolFlags::MODULE) {
        return ResolvedResult {
            symbol: Symbol::GLOBAL_THIS,
            associated_declaration_for_containing_initializer_or_binding_name,
        };
    }

    ResolvedResult {
        symbol: Symbol::ERR,
        associated_declaration_for_containing_initializer_or_binding_name,
    }
}

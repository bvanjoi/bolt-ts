mod on_failed_value_resolve;
mod on_success_resolve;
mod resolve_call_like;
mod resolve_class_like;
pub mod resolve_symbol_by_identifier;

use bolt_ts_early_resolve_errors as errors;
use rayon::prelude::*;
use rustc_hash::FxHashMap;

use bolt_ts_ast::keyword;
use bolt_ts_ast::keyword::{is_prim_ty_name, is_prim_value_name};
use bolt_ts_ast::{self as ast};
use bolt_ts_binder::SymbolTable;
use bolt_ts_binder::{BinderResult, GlobalSymbols, MergedSymbols};
use bolt_ts_binder::{NodeQuery, ParentMap, Symbol, SymbolFlags, SymbolID};
use bolt_ts_parser::ParsedMap;
use bolt_ts_span::Module;
use bolt_ts_utils::fx_hashmap_with_capacity;

use self::resolve_symbol_by_identifier::{ResolvedResult, get_symbol, resolve_symbol_by_ident};

pub struct EarlyResolveResult {
    // TODO: use `NodeId::index` is enough
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

#[allow(clippy::too_many_arguments)]
pub fn early_resolve_parallel<'cx>(
    modules: &[Module],
    states: &[BinderResult<'cx>],
    p: &'cx ParsedMap<'cx>,
    globals: &'cx GlobalSymbols,
    merged: &'cx MergedSymbols,
    atoms: &'cx bolt_ts_atom::AtomIntern,
    emit_standard_class_fields: bool,
    options: &'cx bolt_ts_config::NormalizedCompilerOptions,
) -> Vec<EarlyResolveResult> {
    modules
        .into_par_iter()
        .map(|m| {
            let module_id = m.id();
            let root = p.root(module_id);
            let result = early_resolve(
                states,
                module_id,
                root,
                p,
                globals,
                merged,
                atoms,
                emit_standard_class_fields,
                options,
            );
            assert!(!m.is_default_lib() || result.diags.is_empty());
            result
        })
        .collect()
}

#[allow(clippy::too_many_arguments)]
fn early_resolve<'cx>(
    states: &[BinderResult<'cx>],
    module_id: bolt_ts_span::ModuleID,
    root: &'cx ast::Program<'cx>,
    p: &'cx ParsedMap<'cx>,
    globals: &'cx GlobalSymbols,
    merged: &'cx MergedSymbols,
    atoms: &'cx bolt_ts_atom::AtomIntern,
    emit_standard_class_fields: bool,
    options: &'cx bolt_ts_config::NormalizedCompilerOptions,
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
        emit_standard_class_fields,
        options,
    };
    resolver.resolve_program(root);
    let diags = std::mem::take(&mut resolver.diags);
    EarlyResolveResult {
        final_res: resolver.final_res,
        diags,
    }
}

const MEANING_FOR_VALUE: SymbolFlags = SymbolFlags::VALUE.union(SymbolFlags::EXPORT_VALUE);
const MEANING_FOR_IMPORT_EQUAL: SymbolFlags = SymbolFlags::VALUE
    .union(SymbolFlags::NAMESPACE)
    .union(SymbolFlags::TYPE);

pub struct Resolver<'cx, 'r, 'atoms> {
    states: &'r [BinderResult<'cx>],
    module_id: bolt_ts_span::ModuleID,
    p: &'cx ParsedMap<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    final_res: FxHashMap<ast::NodeID, SymbolID>,
    globals: &'cx GlobalSymbols,
    merged: &'cx MergedSymbols,
    atoms: &'atoms bolt_ts_atom::AtomIntern,
    emit_standard_class_fields: bool,
    options: &'cx bolt_ts_config::NormalizedCompilerOptions,
}

impl<'cx, 'a> Resolver<'cx, 'a, '_> {
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

    fn local_symbol(&self, id: ast::NodeID) -> Option<SymbolID> {
        let idx = id.module().as_usize();
        debug_assert!(idx < self.states.len());
        unsafe {
            self.states
                .get_unchecked(idx)
                .local_symbols
                .get(&id.index_as_u32())
                .copied()
        }
    }

    #[inline]
    fn symbol_of_decl(&self, decl: ast::NodeID) -> SymbolID {
        let idx = decl.module().as_usize();
        debug_assert!(idx < self.states.len());
        unsafe { self.states.get_unchecked(idx).final_res[&decl] }
    }

    fn push_error(&mut self, error: bolt_ts_errors::BoxedDiag) {
        self.diags.push(bolt_ts_errors::Diag { inner: error });
    }

    fn resolve_program(&mut self, root: &'cx ast::Program<'cx>) {
        for stmt in root.stmts() {
            self.resolve_stmt(stmt);
        }
    }

    fn parent(&self, node: ast::NodeID) -> Option<ast::NodeID> {
        debug_assert!(node.module() == self.module_id);
        let idx = self.module_id.as_usize();
        debug_assert!(idx < self.states.len());
        unsafe { self.states.get_unchecked(idx).parent_map.parent(node) }
    }

    fn node_query(&self) -> bolt_ts_binder::NodeQuery<'cx, 'a> {
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
            NestedModule(n) => {
                self.resolve_module_block(n.block.module_block());
            }
            BlockModule(n) => {
                if let Some(block) = n.block {
                    self.resolve_module_block(block);
                }
            }
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
                if let Some(init) = &n.init {
                    self.resolve_for_init_kind(init);
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
                self.resolve_for_init_kind(&n.init);
                self.resolve_expr(n.expr);
                self.resolve_stmt(n.body);
            }
            ForIn(n) => {
                self.resolve_for_init_kind(&n.init);
                self.resolve_expr(n.expr);
                self.resolve_stmt(n.body);
            }
            Break(n) => {
                if let Some(ident) = n.label {
                    self.resolve_symbol_by_ident(ident, MEANING_FOR_VALUE);
                }
            }
            Continue(n) => {
                if let Some(ident) = n.label {
                    self.resolve_symbol_by_ident(ident, MEANING_FOR_VALUE);
                }
            }
            Try(n) => {
                self.resolve_block_stmt(n.try_block);
                if let Some(n) = n.catch_clause {
                    if let Some(n) = n.var {
                        self.resolve_var_decl(n);
                    }
                    self.resolve_block_stmt(n.block);
                }
                if let Some(n) = n.finally_block {
                    self.resolve_block_stmt(n);
                }
            }
            While(n) => {
                self.resolve_expr(n.expr);
                self.resolve_stmt(n.stmt);
            }
            Do(n) => {
                self.resolve_stmt(n.stmt);
                self.resolve_expr(n.expr);
            }
            Debugger(_) => {}
            ExportAssign(n) => match n.expr.kind {
                bolt_ts_ast::ExprKind::Ident(ident) => {
                    let res = self.resolve_symbol_by_ident(ident, SymbolFlags::all());
                    if res.symbol() == Symbol::ERR {
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
            Switch(n) => self.resolve_switch_stmt(n),
            ImportEquals(n) => {
                // import a = |b|; // Namespace
                // import a = |b.c|; // Value, type, namespace
                // import a = |b.c|.d; // Namespace
                if let ast::ModuleReferenceKind::EntityName(n) = n.module_reference {
                    match n.kind {
                        ast::EntityNameKind::Ident(_) => {
                            self.resolve_entity_name::<false>(n, SymbolFlags::NAMESPACE);
                        }
                        ast::EntityNameKind::Qualified(_) => {
                            self.resolve_entity_name::<false>(n, MEANING_FOR_IMPORT_EQUAL);
                        }
                    }
                }
            }
        };
    }

    fn resolve_switch_stmt(&mut self, n: &'cx ast::SwitchStmt<'cx>) {
        self.resolve_expr(n.expr);
        for clause in n.case_block.clauses {
            use ast::CaseOrDefaultClause::*;
            match clause {
                Case(n) => {
                    self.resolve_expr(n.expr);
                    for stmt in n.stmts {
                        self.resolve_stmt(stmt);
                    }
                }
                Default(n) => {
                    for stmt in n.stmts {
                        self.resolve_stmt(stmt);
                    }
                }
            }
        }
    }

    fn resolve_for_init_kind(&mut self, init: &'cx ast::ForInitKind<'cx>) {
        match init {
            ast::ForInitKind::Var(decls) => {
                for decl in decls.iter() {
                    self.resolve_var_decl(decl)
                }
            }
            ast::ForInitKind::Expr(expr) => {
                self.resolve_expr(expr);
            }
        }
    }

    fn resolve_enum_member(&mut self, n: &'cx ast::EnumMember<'cx>) {
        if let Some(init) = n.init {
            self.resolve_expr(init);
        }
    }

    fn resolve_export(&mut self, export: &'cx ast::ExportDecl<'cx>) {
        let is_type = export.clause.is_type_only;
        const TYPE_MEANING: SymbolFlags = SymbolFlags::TYPE
            .union(SymbolFlags::NAMESPACE)
            .union(SymbolFlags::EXPORT_VALUE);
        const DEFAULT_MEANING: SymbolFlags = MEANING_FOR_VALUE
            .union(SymbolFlags::TYPE)
            .union(SymbolFlags::NAMESPACE);
        let meaning = if is_type {
            TYPE_MEANING
        } else {
            DEFAULT_MEANING
        };
        match export.clause.kind {
            ast::ExportClauseKind::Glob(_) => {}
            ast::ExportClauseKind::Ns(_) => {}
            ast::ExportClauseKind::Specs(specs) => {
                if specs.module.is_some() {
                    return;
                }
                for spec in specs.list {
                    use ast::ExportSpecKind::*;
                    match spec.kind {
                        Shorthand(n) => {
                            let res = self.resolve_symbol_by_ident(n.name, meaning);
                            if res.symbol() == Symbol::ERR {
                                let name = self.atoms.get(n.name.name).to_string();
                                let error = errors::CannotFindName {
                                    span: n.name.span,
                                    name,
                                    errors: vec![],
                                };
                                self.push_error(Box::new(error));
                            }
                        }
                        Named(n) => {
                            match n.prop_name.kind {
                                ast::ModuleExportNameKind::Ident(ident) => {
                                    self.resolve_symbol_by_ident(ident, meaning)
                                }
                                ast::ModuleExportNameKind::StringLit(_) => {
                                    todo!()
                                }
                            };
                        }
                    }
                }
            }
        }
    }

    fn resolve_module_block(&mut self, block: &'cx ast::ModuleBlock<'cx>) {
        for stmt in block.stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_type_alias_decl(&mut self, ty: &'cx ast::TypeAliasDecl<'cx>) {
        self.resolve_ty_params(ty.ty_params);
        self.resolve_ty(ty.ty);
    }

    fn resolve_ty_params(&mut self, ty_params: Option<ast::TyParams<'cx>>) {
        if let Some(ty_params) = ty_params {
            self.resolve_ty_params_worker(ty_params);
        }
    }

    fn resolve_ty_params_worker(&mut self, ty_params: ast::TyParams<'cx>) {
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

    fn resolve_binding(&mut self, binding: &'cx ast::Binding<'cx>) {
        match binding.kind {
            ast::BindingKind::ObjectPat(pat) => {
                for elem in pat.elems {
                    match elem.name {
                        ast::ObjectBindingName::Shorthand(_) => {
                            debug_assert!(self.symbol_of_decl(elem.id) != Symbol::ERR);
                        }
                        ast::ObjectBindingName::Prop {
                            prop_name, name, ..
                        } => {
                            match prop_name.kind {
                                ast::PropNameKind::Ident(_) => {
                                    // TODO: debug_assert!(self.symbol_of_decl(elem.id) != Symbol::ERR);
                                }
                                ast::PropNameKind::Computed(n) => {
                                    self.resolve_expr(n.expr);
                                }
                                _ => {}
                            }
                            self.resolve_binding(name);
                        }
                    }
                    if let Some(init) = elem.init {
                        self.resolve_expr(init);
                    }
                }
            }
            ast::BindingKind::ArrayPat(pat) => {
                for elem in pat.elems {
                    match elem.kind {
                        ast::ArrayBindingElemKind::Omit(_) => {}
                        ast::ArrayBindingElemKind::Binding(binding) => {
                            self.resolve_binding(binding.name);
                            if let Some(init) = binding.init {
                                self.resolve_expr(init);
                            }
                        }
                    }
                }
            }
            ast::BindingKind::Ident(_) => {}
        }
    }

    fn resolve_var_decl(&mut self, n: &'cx ast::VarDecl<'cx>) {
        self.resolve_binding(n.name);
        if let Some(ty) = n.ty {
            self.resolve_ty(ty);
        }
        if let Some(init) = n.init {
            self.resolve_expr(init);
        }
        check_var_declared_named_not_shadowed_for_variable_declaration(self, n);
    }

    fn resolve_entity_name<const UNDER_TYPE_QUERY: bool>(
        &mut self,
        name: &'cx ast::EntityName<'cx>,
        meaning: SymbolFlags,
    ) {
        use bolt_ts_ast::EntityNameKind::*;
        match name.kind {
            Ident(ident) => {
                if meaning == MEANING_FOR_VALUE {
                    self.resolve_value_by_ident(ident);
                } else if meaning == SymbolFlags::NAMESPACE || meaning == MEANING_FOR_IMPORT_EQUAL {
                    let res = self.resolve_symbol_by_ident(ident, meaning);
                    if res.symbol() == Symbol::ERR {
                        let name = self.atoms.get(ident.name).to_string();
                        let error = errors::CannotFindName {
                            span: ident.span,
                            name,
                            errors: vec![],
                        };
                        let error = self.on_failed_to_resolve_namespace_symbol(ident, &res, error);
                        self.push_error(Box::new(error));
                    }
                } else {
                    debug_assert!(meaning == SymbolFlags::TYPE);
                    let res = self.resolve_type_by_ident(ident);
                    let prev = self.final_res.insert(ident.id, res);
                    debug_assert!(prev.is_none());
                }
            }
            Qualified(qualified) => {
                let meaning = if UNDER_TYPE_QUERY {
                    MEANING_FOR_VALUE
                } else {
                    SymbolFlags::NAMESPACE
                };
                self.resolve_entity_name::<UNDER_TYPE_QUERY>(qualified.left, meaning);
                // resolve the value of right in checker.
            }
        }
    }

    fn resolve_refer_ty(&mut self, refer: &'cx ast::ReferTy<'cx>) {
        self.resolve_entity_name::<false>(refer.name, SymbolFlags::TYPE);
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
                self.resolve_ty_params(f.ty_params);
                self.resolve_params(f.params);
                self.resolve_ty(f.ty);
            }
            Ctor(node) => {
                self.resolve_ty_params(node.ty_params);
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
                match n.name.kind {
                    ast::EntityNameKind::Ident(n) if n.name == keyword::KW_THIS => {}
                    ast::EntityNameKind::Qualified(n)
                        if n.left.get_first_identifier().name == keyword::KW_THIS => {}
                    _ => {
                        self.resolve_entity_name::<true>(n.name, MEANING_FOR_VALUE);
                    }
                }
                if let Some(ty_args) = n.ty_args {
                    self.resolve_tys(ty_args.list);
                }
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
            TypeOp(n) => {
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
            NamedTuple(n) => {
                self.resolve_ty(n.ty);
            }
            TemplateLit(n) => {
                for item in n.spans {
                    self.resolve_ty(item.ty);
                }
            }
            Intrinsic(_) | This(_) => {}
            Import(_) => {}
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
                self.resolve_ty_params(m.ty_params);
                self.resolve_prop_name(m.name);
                self.resolve_params(m.params);
                if let Some(ty) = m.ty {
                    self.resolve_ty(ty);
                }
            }
            CallSig(call) => {
                self.resolve_ty_params(call.ty_params);
                self.resolve_params(call.params);
                if let Some(ty) = call.ty {
                    self.resolve_ty(ty);
                }
            }
            IndexSig(index) => self.resolve_index_sig(index),
            CtorSig(decl) => {
                self.resolve_ty_params(decl.ty_params);
                self.resolve_params(decl.params);
                if let Some(ty) = decl.ty {
                    self.resolve_ty(ty);
                }
            }
            Setter(n) => {
                self.resolve_prop_name(n.name);
                self.resolve_params(n.params);
            }
            Getter(n) => {
                self.resolve_prop_name(n.name);
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
        self.resolve_binding(param.name);
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
                self.resolve_ty_params(f.ty_params);
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
                self.resolve_ty_params(f.ty_params);
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
                match n.tpl {
                    ast::TemplateExpressionKind::NoSubstitutionTemplateLit(_) => {}
                    ast::TemplateExpressionKind::TemplateExpr(n) => {
                        self.resolve_template_expr(n);
                    }
                }
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
            Await(n) => {
                self.resolve_expr(n.expr);
            }
            Yield(n) => {
                if let Some(expr) = n.expr {
                    self.resolve_expr(expr);
                }
            }
            Import(_) => {}
            NewMetaProperty(_) => {}
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
            Ident(_) => {
                // TODO:
                // self.resolve_value_by_ident(ident)
            }
            Ns(_) => {
                // TODO:
                // self.resolve_value_by_ident(ns.name)
            }
            PropAccess(_) => {
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
                self.resolve_prop_name(n.name);
                self.resolve_expr(n.init);
            }
            Method(n) => {
                self.resolve_prop_name(n.name);
                self.resolve_ty_params(n.ty_params);
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
        self.resolve_ty_params(f.ty_params);
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

    fn resolve_interface_decl(&mut self, n: &'cx ast::InterfaceDecl<'cx>) {
        self.resolve_ty_params(n.ty_params);
        if let Some(extends) = n.extends {
            for ty in extends.list {
                self.resolve_refer_ty(ty);
            }
        }
        for member in n.members {
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
        let res = self.resolve_symbol_by_ident(ident, MEANING_FOR_VALUE);
        if res.symbol() == Symbol::ERR {
            let name = self.atoms.get(ident.name).to_string();
            let mut error = errors::CannotFindName {
                span: ident.span,
                name,
                errors: vec![],
            };
            if let Some(property_with_invalid_initializer) = res.property_with_invalid_initializer()
            {
                if let Some(sub_error) = self
                    .on_property_with_invalid_initializer(ident, property_with_invalid_initializer)
                {
                    error.errors.push(errors::CannotFindNameHelperKind::InitializerOfInstanceMemberVariable0CannotReferenceIdentifier1DeclaredInTheConstructor(sub_error));
                }
            } else {
                error = self.on_failed_to_resolve_value_symbol(ident, error);
            }
            self.push_error(Box::new(error));
        } else {
            if let Some(property_with_invalid_initializer) = res.property_with_invalid_initializer()
                && let Some(error) = self
                    .on_property_with_invalid_initializer(ident, property_with_invalid_initializer)
            {
                self.push_error(Box::new(error));
            }
            self.on_success_resolved_value_symbol(
                ident,
                res.symbol(),
                res.associated_declaration_for_containing_initializer_or_binding_name(),
                res.within_deferred_context(),
            );
        }
    }

    fn resolve_type_by_ident(&mut self, ident: &'cx ast::Ident) -> SymbolID {
        if ident.name == keyword::IDENT_EMPTY {
            // delay bug
            return Symbol::ERR;
        } else if is_prim_ty_name(ident.name) {
            if let Some(error) = self.check_using_type_as_value(ident) {
                self.push_error(error.into_diag());
            }
            return Symbol::ERR;
        }

        let res = resolve_symbol_by_ident(self, ident, SymbolFlags::TYPE);
        let mut symbol = res.symbol();

        if symbol == Symbol::ERR {
            let name = self.atoms.get(ident.name).to_string();
            let error = errors::CannotFindName {
                span: ident.span,
                name,
                errors: vec![],
            };
            let error = self.on_failed_to_resolve_type_symbol(ident, &res, error);
            self.push_error(Box::new(error));
        } else {
            self.on_success_resolved_type_symbol(ident, &mut symbol);
        };
        symbol
    }

    fn resolve_symbol_by_ident(
        &mut self,
        ident: &'cx ast::Ident,
        meaning: SymbolFlags,
    ) -> ResolvedResult {
        let res = resolve_symbol_by_ident(self, ident, meaning);
        let prev = self.final_res.insert(ident.id, res.symbol());
        assert!(
            prev.is_none(),
            "the symbol of {:?} is already resolved",
            self.atoms.get(ident.name)
        );
        res
    }
}

fn check_var_declared_named_not_shadowed_for_variable_declaration<'a, 'cx>(
    r: &mut Resolver<'cx, 'a, '_>,
    n: &'cx ast::VarDecl<'cx>,
) {
    fn check_for_binding<'a, 'cx>(
        r: &mut Resolver<'cx, 'a, '_>,
        binding: &'cx ast::Binding<'cx>,
        parent: ast::NodeID,
    ) {
        match binding.kind {
            bolt_ts_ast::BindingKind::Ident(_) => {
                if let Some(error) = check_var_declared_names_not_shadowed(r, parent) {
                    r.push_error(Box::new(error));
                }
            }
            bolt_ts_ast::BindingKind::ObjectPat(pat) => {
                for element in pat.elems {
                    match element.name {
                        bolt_ts_ast::ObjectBindingName::Shorthand(_) => {
                            if let Some(error) =
                                check_var_declared_names_not_shadowed(r, element.id)
                            {
                                r.push_error(Box::new(error));
                            }
                        }
                        bolt_ts_ast::ObjectBindingName::Prop { name, .. } => {
                            check_for_binding(r, name, element.id);
                        }
                    }
                }
            }
            bolt_ts_ast::BindingKind::ArrayPat(pat) => {
                for element in pat.elems {
                    match element.kind {
                        ast::ArrayBindingElemKind::Omit(_) => {}
                        ast::ArrayBindingElemKind::Binding(n) => {
                            check_for_binding(r, n.name, n.id);
                        }
                    }
                }
            }
        }
    }
    check_for_binding(r, n.name, n.id);
}

fn check_var_declared_names_not_shadowed<'a, 'cx>(
    r: &'a Resolver<'cx, 'a, '_>,
    node: ast::NodeID,
) -> Option<errors::CannotInitializeOuterScopedVariableXInTheSameScopeAsBlockScopedDeclarationY> {
    let nq = r.node_query();
    if nq
        .get_combined_node_flags(node)
        .intersects(ast::NodeFlags::BLOCK_SCOPED)
        || nq.is_part_of_param_decl(node)
    {
        return None;
    }

    let symbol = r.symbol_of_decl(node);
    let s = r.symbol(symbol);
    if !s.flags.contains(SymbolFlags::FUNCTION_SCOPED_VARIABLE) {
        return None;
    }
    let name = match r.p.node(node) {
        ast::Node::VarDecl(n) => match n.name.kind {
            ast::BindingKind::Ident(ident) => ident,
            _ => unreachable!(),
        },
        ast::Node::ArrayBinding(n) => match n.name.kind {
            ast::BindingKind::Ident(ident) => ident,
            _ => unreachable!(),
        },
        ast::Node::ObjectBindingElem(n) => match n.name {
            ast::ObjectBindingName::Shorthand(ident) => ident,
            ast::ObjectBindingName::Prop { name, .. } => match name.kind {
                ast::BindingKind::Ident(ident) => ident,
                _ => unreachable!(),
            },
        },
        _ => unreachable!(),
    };
    let local_declaration_symbol_id =
        resolve_symbol_by_ident(r, name, SymbolFlags::VARIABLE).symbol();

    if local_declaration_symbol_id != Symbol::ERR
        && local_declaration_symbol_id != symbol
        && let local_declaration_symbol = r.symbol(local_declaration_symbol_id)
        && local_declaration_symbol
            .flags
            .contains(SymbolFlags::BLOCK_SCOPED_VARIABLE)
    {
        debug_assert!(local_declaration_symbol_id.module() == r.module_id);
        let value_declaration = local_declaration_symbol.value_decl?;
        debug_assert!(value_declaration.module() == r.module_id);
        let declaration_node_flags = r.node_query().get_combined_node_flags(value_declaration);
        if declaration_node_flags.intersects(ast::NodeFlags::BLOCK_SCOPED) {
            let container = if let Some(parent) = r.parent(node)
                && r.p.node(parent).is_var_stmt()
            {
                r.parent(parent)
            } else {
                None
            };
            let name_share_scope = if let Some(container) = container {
                match r.p.node(container) {
                    ast::Node::BlockStmt(_) => r
                        .parent(container)
                        .is_some_and(|p| r.p.node(p).is_fn_like()),
                    ast::Node::ModuleBlock(_)
                    | ast::Node::NestedModuleDecl(_)
                    | ast::Node::BlockModuleDecl(_)
                    | ast::Node::Program(_) => true,
                    _ => false,
                }
            } else {
                false
            };
            if !name_share_scope {
                let span = name.span;
                let name = local_declaration_symbol.name.to_string(r.atoms);
                return Some(errors::CannotInitializeOuterScopedVariableXInTheSameScopeAsBlockScopedDeclarationY {
                    span,
                    x: name.clone(),
                    y: name
                });
            }
        }
    }

    None
}

pub fn get_declaration_node_flags_from_symbol<'cx>(
    s: &Symbol,
    parent_map: &ParentMap,
    parse_result: &bolt_ts_parser::ParseResultForGraph<'cx>,
) -> ast::NodeFlags {
    let Some(decl) = s.value_decl else {
        return ast::NodeFlags::empty();
    };
    let nq = NodeQuery::new(parent_map, parse_result);
    nq.get_combined_node_flags(decl)
}

mod bind_call_like;
mod bind_class_like;
mod bind_fn_like;
mod create;
mod errors;
mod merged_symbol;
mod symbol;

use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;
use symbol::FunctionScopedVarSymbol;
use symbol::TransientSymbol;
use thin_vec::thin_vec;

use self::symbol::ClassSymbol;
use self::symbol::IndexSymbol;
use self::symbol::SymbolKind;
use self::symbol::TyLitSymbol;
pub use self::symbol::{GlobalSymbols, Symbol, SymbolID, SymbolName, Symbols};
pub use self::symbol::{SymbolFlags, SymbolFnKind};

use crate::ast::{self, NodeID};
use crate::atoms::AtomMap;
use crate::parser::Parser;
use crate::resolve::ResolveResult;
use crate::utils::fx_hashmap_with_capacity;

bolt_ts_span::new_index_with_module!(ScopeID);

impl ScopeID {
    pub const fn is_root(&self) -> bool {
        self.index == 0
    }
}

pub struct Binder<'cx> {
    p: &'cx Parser<'cx>,
    atoms: &'cx AtomMap<'cx>,
    binder_result: Vec<ResolveResult<'cx>>,
    transient_binder: ResolveResult<'cx>,
}

impl<'cx> Binder<'cx> {
    pub fn new(p: &'cx Parser<'cx>, atoms: &'cx AtomMap) -> Self {
        Self {
            p,
            atoms,
            binder_result: Vec::with_capacity(p.module_count() + 1),
            transient_binder: ResolveResult {
                symbols: Symbols::new(ModuleID::MOCK),
                final_res: Default::default(),
                diags: Default::default(),
            },
        }
    }

    pub fn insert(&mut self, id: ModuleID, result: ResolveResult<'cx>) {
        assert_eq!(self.binder_result.len(), id.as_usize());
        self.binder_result.push(result);
    }

    #[inline(always)]
    fn get(&self, id: ModuleID) -> &ResolveResult<'cx> {
        if id == ModuleID::MOCK {
            &self.transient_binder
        } else {
            let index = id.as_usize();
            &self.binder_result[index]
        }
    }

    #[inline(always)]
    fn get_mut(&mut self, id: ModuleID) -> &mut ResolveResult<'cx> {
        if id == ModuleID::MOCK {
            &mut self.transient_binder
        } else {
            unreachable!("{id:#?} are immutable")
        }
    }

    #[inline(always)]
    pub fn final_res(&self, id: NodeID) -> SymbolID {
        self.get(id.module())
            .final_res
            .get(&id)
            .copied()
            .unwrap_or_else(|| {
                let node = self.p.node(id).expect_ident();
                let name = self.atoms.get(node.name);
                panic!("The resolution of `{name}` is not found.");
            })
    }

    #[inline(always)]
    pub fn symbols(&self, id: ModuleID) -> &Symbols<'cx> {
        &self.get(id).symbols
    }

    #[inline(always)]
    pub fn symbol(&self, id: SymbolID) -> &Symbol<'cx> {
        self.get(id.module()).symbols.get(id)
    }

    pub(crate) fn get_transient(&self, symbol: SymbolID) -> Option<&TransientSymbol<'cx>> {
        match &self.symbol(symbol).kind.0 {
            SymbolKind::Transient(ty) => Some(ty),
            _ => None,
        }
    }

    pub(crate) fn get_mut_transient(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&mut TransientSymbol<'cx>> {
        if symbol.module() != ModuleID::MOCK {
            return None;
        }
        match &mut self.get_mut(ModuleID::MOCK).symbols.get_mut(symbol).kind.0 {
            SymbolKind::Transient(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn create_transient_symbol(
        &mut self,
        name: SymbolName,
        symbol_flags: SymbolFlags,
        origin: Option<SymbolID>,
        links: crate::check::SymbolLinks<'cx>,
    ) -> SymbolID {
        let symbol_flags = symbol_flags | SymbolFlags::TRANSIENT;
        let symbol = Symbol::new(
            name,
            symbol_flags,
            SymbolKind::Transient(TransientSymbol { links, origin }),
        );
        self.transient_binder.symbols.insert(symbol)
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        self.binder_result
            .iter_mut()
            .flat_map(|result| std::mem::take(&mut result.diags))
            .collect()
    }
}

pub struct BinderState<'cx> {
    scope_id: ScopeID,
    p: &'cx Parser<'cx>,
    pub(crate) diags: Vec<bolt_ts_errors::Diag>,
    pub(crate) atoms: &'cx AtomMap<'cx>,
    pub(crate) scope_id_parent_map: Vec<Option<ScopeID>>,
    pub(crate) node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub(crate) symbols: Symbols<'cx>,
    pub(super) res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    pub(crate) final_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub fn bind<'cx>(
    atoms: &'cx AtomMap<'cx>,
    parser: &'cx Parser<'cx>,
    root: &'cx ast::Program,
    module_id: ModuleID,
) -> BinderState<'cx> {
    let mut state = BinderState::new(atoms, parser, module_id);
    state.bind_program(root);
    state
}

impl<'cx> BinderState<'cx> {
    fn new(atoms: &'cx AtomMap, parser: &'cx Parser<'cx>, module_id: ModuleID) -> Self {
        let symbols = Symbols::new(module_id);
        BinderState {
            atoms,
            p: parser,
            scope_id: ScopeID::root(module_id),
            scope_id_parent_map: Vec::with_capacity(512),
            res: fx_hashmap_with_capacity(128),
            final_res: fx_hashmap_with_capacity(256),
            node_id_to_scope_id: fx_hashmap_with_capacity(32),
            symbols,
            diags: Vec::new(),
        }
    }

    fn push_error(&mut self, module_id: ModuleID, error: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag {
            module_id,
            inner: error,
        });
    }

    fn connect(&mut self, node_id: NodeID) {
        let prev = self.node_id_to_scope_id.insert(node_id, self.scope_id);
        assert!(prev.is_none());
    }

    fn new_scope(&mut self) -> ScopeID {
        let next = ScopeID {
            module: self.scope_id.module,
            index: self.scope_id_parent_map.len() as u32,
        };
        self.scope_id_parent_map.push(Some(self.scope_id));
        next
    }

    fn bind_program(&mut self, root: &'cx ast::Program) {
        assert_eq!(self.scope_id.index_as_u32(), 0);
        assert!(self.scope_id_parent_map.is_empty());
        self.scope_id_parent_map.push(None);
        self.connect(root.id);
        self.create_block_container_symbol(root.id);
        for stmt in root.stmts {
            self.bind_stmt(root.id, stmt)
        }
    }

    fn bind_stmt(&mut self, container: ast::NodeID, stmt: &'cx ast::Stmt) {
        use ast::StmtKind::*;
        match stmt.kind {
            Empty(_) => (),
            Var(var) => self.bind_var_stmt(var),
            Expr(expr) => self.bind_expr(expr),
            Fn(f) => self.bind_fn_decl(container, f),
            If(stmt) => {
                self.bind_expr(stmt.expr);
                self.bind_stmt(container, stmt.then);
                if let Some(alt) = stmt.else_then {
                    self.bind_stmt(container, alt)
                }
            }
            Block(block) => self.bind_block_stmt(block),
            Return(ret) => {
                if let Some(expr) = ret.expr {
                    self.bind_expr(expr)
                }
            }
            Class(class) => self.bind_class_like(class, false),
            Interface(interface) => self.bind_interface_decl(interface),
            Type(t) => self.bind_type_decl(t),
            Namespace(ns) => self.bind_ns_decl(container, ns),
            Throw(t) => {
                self.bind_expr(t.expr);
            }
            Enum(enum_decl) => {}
        }
    }

    fn bind_ns_decl(&mut self, container: ast::NodeID, ns: &'cx ast::NsDecl<'cx>) {
        let flags = if ns.block.stmts.is_empty() {
            SymbolFlags::NAMESPACE_MODULE
        } else {
            SymbolFlags::VALUE_MODULE
        };

        let symbol = self.create_symbol_with_ns(
            SymbolName::Normal(ns.name.name),
            flags,
            symbol::NsSymbol {
                decls: thin_vec::thin_vec![ns.id],
            },
        );

        let container = self.final_res[&container];
        if let SymbolKind::BlockContainer(c) = &mut self.symbols.get_mut(container).kind.0 {
            let name = SymbolName::Normal(ns.name.name);
            c.locals.insert(name, symbol);
        }

        self.bind_block_stmt(ns.block);
    }

    fn bind_type_decl(&mut self, t: &'cx ast::TypeDecl<'cx>) {
        let symbol = self.create_var_symbol(
            t.name.name,
            SymbolFlags::TYPE_ALIAS,
            SymbolKind::TyAlias(symbol::TyAliasSymbol { decl: t.id }),
        );
        self.create_final_res(t.id, symbol);
        if let Some(ty_params) = t.ty_params {
            self.bind_ty_params(ty_params);
        }
        self.bind_ty(t.ty);
    }

    fn bind_ty_params(&mut self, ty_params: ast::TyParams<'cx>) {
        for ty_param in ty_params {
            self.bind_ty_param(ty_param)
        }
    }

    fn bind_ty_param(&mut self, param: &'cx ast::TyParam<'cx>) {
        let symbol = self.create_var_symbol(
            param.name.name,
            SymbolFlags::TYPE_PARAMETER,
            SymbolKind::TyParam(symbol::TyParamSymbol { decl: param.id }),
        );
        self.create_final_res(param.id, symbol);
    }

    fn bind_index_sig(
        &mut self,
        container: NodeID,
        index: &'cx ast::IndexSigDecl<'cx>,
    ) -> SymbolID {
        let name = SymbolName::Index;
        let symbol = self.create_symbol(
            SymbolName::Index,
            SymbolFlags::SIGNATURE,
            SymbolKind::Index(IndexSymbol { decl: index.id }),
        );
        self.create_final_res(index.id, symbol);
        let container = self.final_res[&container];
        let s = self.symbols.get_mut(container);
        if let Some(i) = &mut s.kind.1 {
            let prev = i.members.insert(name, symbol);
            // FIXME: multiple index sig
            assert!(prev.is_none());
        } else if let SymbolKind::Class(c) = &mut s.kind.0 {
            let prev = c.members.insert(name, symbol);
            // FIXME: multiple index sig
            assert!(prev.is_none());
        } else {
            todo!()
        }

        self.bind_params(index.params);
        self.bind_ty(index.ty);

        symbol
    }

    fn bind_object_ty_member(&mut self, container: ast::NodeID, m: &'cx ast::ObjectTyMember<'cx>) {
        use ast::ObjectTyMemberKind::*;
        match m.kind {
            Prop(m) => {
                if let Some(ty) = m.ty {
                    self.bind_ty(ty);
                }
                let name = Self::prop_name(m.name);
                let symbol = self.create_object_member_symbol(name, m.id);
                let s = self.final_res[&container];
                let s = self.symbols.get_mut(s);
                if let Some(i) = &mut s.kind.1 {
                    if let std::collections::hash_map::Entry::Vacant(e) = i.members.entry(name) {
                        e.insert(symbol);
                    } else {
                        // TODO: prev
                    };
                } else if let SymbolKind::Object(o) = &mut s.kind.0 {
                    let prev = o.members.insert(name, symbol);
                    assert!(prev.is_none());
                } else {
                    todo!()
                }
            }
            Method(m) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();

                if let Some(ty_params) = m.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(m.params);
                if let Some(ty) = m.ty {
                    self.bind_ty(ty);
                }

                let name = Self::prop_name(m.name);
                self.create_fn_decl_like_symbol(container, m, name, SymbolFnKind::Method);
                self.scope_id = old;
            }
            CallSig(call) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();

                if let Some(ty_params) = call.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(call.params);
                if let Some(ty) = call.ty {
                    self.bind_ty(ty);
                }

                let name = SymbolName::Call;
                self.create_fn_decl_like_symbol(container, call, name, SymbolFnKind::Call);
                self.scope_id = old;
            }
            IndexSig(index) => {
                self.bind_index_sig(container, index);
            }
            CtorSig(decl) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                if let Some(ty_params) = decl.ty_params {
                    self.bind_ty_params(ty_params);
                }
                self.bind_params(decl.params);
                if let Some(ty) = decl.ty {
                    self.bind_ty(ty);
                }
                self.create_fn_decl_like_symbol(
                    container,
                    decl,
                    SymbolName::Constructor,
                    SymbolFnKind::Ctor,
                );
                self.scope_id = old;
            }
        }
    }

    fn bind_interface_decl(&mut self, i: &'cx ast::InterfaceDecl<'cx>) {
        self.create_interface_symbol(i.id, i.name.name, Default::default());
        let old = self.scope_id;
        self.scope_id = self.new_scope();

        if let Some(ty_params) = i.ty_params {
            self.bind_ty_params(ty_params);
        }
        if let Some(extends) = i.extends {
            for ty in extends.list {
                self.bind_refer_ty(ty);
            }
        }

        for m in i.members {
            self.bind_object_ty_member(i.id, m)
        }
        self.scope_id = old;
    }

    pub(super) fn prop_name(name: &ast::PropName) -> SymbolName {
        match name.kind {
            ast::PropNameKind::Ident(ident) => SymbolName::Ele(ident.name),
            ast::PropNameKind::NumLit(num) => SymbolName::EleNum(num.val.into()),
            ast::PropNameKind::StringLit(lit) => SymbolName::Ele(lit.val),
        }
    }

    fn bind_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();

        self.create_block_container_symbol(block.id);

        for stmt in block.stmts {
            self.bind_stmt(block.id, stmt)
        }

        self.scope_id = old;
    }

    fn bind_ident(&mut self, ident: &'cx ast::Ident) {
        self.connect(ident.id)
    }

    fn bind_expr(&mut self, expr: &'cx ast::Expr) {
        use ast::ExprKind::*;
        match expr.kind {
            Ident(ident) => self.bind_ident(ident),
            Call(call) => self.bind_call_like(call),
            New(new) => self.bind_call_like(new),
            Bin(bin) => {
                self.bind_expr(bin.left);
                self.bind_expr(bin.right);
            }
            Assign(assign) => {
                self.bind_expr(assign.left);
                self.bind_expr(assign.right);
            }
            ObjectLit(lit) => self.bind_object_lit(lit),
            ArrayLit(lit) => self.bind_array_lit(lit),
            Cond(cond) => self.bind_cond_expr(cond),
            Paren(paren) => self.bind_expr(paren.expr),
            ArrowFn(f) => self.bind_arrow_fn_expr(f),
            Fn(f) => self.bind_fn_expr(f),
            Class(class) => self.bind_class_like(class, true),
            PrefixUnary(unary) => self.bind_expr(unary.expr),
            PropAccess(node) => {
                self.bind_expr(node.expr);
            }
            _ => (),
        }
    }

    fn bind_fn_expr(&mut self, f: &'cx ast::FnExpr<'cx>) {
        self.create_fn_expr_symbol(f.id);
        for param in f.params {
            self.bind_param(param);
        }
        self.bind_block_stmt(f.body);
    }

    fn bind_arrow_fn_expr(&mut self, f: &'cx ast::ArrowFnExpr<'cx>) {
        self.create_fn_expr_symbol(f.id);
        self.bind_params(f.params);
        use ast::ArrowFnExprBody::*;
        match f.body {
            Block(block) => self.bind_block_stmt(block),
            Expr(expr) => self.bind_expr(expr),
        }
    }

    fn bind_cond_expr(&mut self, cond: &'cx ast::CondExpr<'cx>) {
        self.bind_expr(cond.cond);
        self.bind_expr(cond.when_true);
        self.bind_expr(cond.when_false);
    }

    fn bind_array_lit(&mut self, lit: &'cx ast::ArrayLit<'cx>) {
        for expr in lit.elems {
            self.bind_expr(expr);
        }
    }

    fn bind_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();
        let members = lit
            .members
            .iter()
            .map(|member| {
                let name = Self::prop_name(member.name);
                let symbol = self.create_object_member_symbol(name, member.id);
                self.bind_expr(member.value);
                (name, symbol)
            })
            .collect();
        self.scope_id = old;
        self.create_object_lit_symbol(lit.id, members);
    }

    fn bind_var_stmt(&mut self, var: &'cx ast::VarStmt) {
        self.connect(var.id);
        let kind = var.kind;
        for item in var.list {
            self.bind_var_decl(item, kind);
        }
    }

    fn bind_entity_name(&mut self, name: &'cx ast::EntityName) {
        match name.kind {
            ast::EntityNameKind::Ident(ident) => self.bind_ident(ident),
            ast::EntityNameKind::Qualified(q) => {
                self.bind_entity_name(q.left);
                self.bind_ident(q.right);
            }
        }
    }

    fn bind_refer_ty(&mut self, refer: &'cx ast::ReferTy) {
        self.bind_entity_name(refer.name);
        if let Some(ty_args) = refer.ty_args {
            for ty_arg in ty_args.list {
                self.bind_ty(ty_arg);
            }
        }
    }

    fn bind_ty(&mut self, ty: &'cx ast::Ty) {
        use ast::TyKind::*;
        match ty.kind {
            Array(array) => self.bind_array_ty(array),
            Tuple(tup) => {
                for ty in tup.tys {
                    self.bind_ty(ty);
                }
            }
            ObjectLit(lit) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                let symbol = self.create_object_lit_symbol(lit.id, Default::default());
                for m in lit.members {
                    self.bind_object_ty_member(lit.id, m);
                }
                self.scope_id = old;
            }
            Refer(refer) => self.bind_refer_ty(refer),
            Cond(cond) => {
                self.bind_ty(cond.check_ty);
                self.bind_ty(cond.extends_ty);
                self.bind_ty(cond.true_ty);
                self.bind_ty(cond.false_ty);
            }
            IndexedAccess(indexed) => {
                self.bind_ty(indexed.ty);
                self.bind_ty(indexed.index_ty);
            }
            Rest(rest) => {
                self.bind_ty(rest.ty);
            }
            Fn(f) => {
                self.create_fn_ty_symbol(f);
                self.bind_params(f.params);
                self.bind_ty(f.ty);
            }
            NumLit(_) | StringLit(_) | NullLit(_) | BooleanLit(_) => {}
            Union(u) => {
                for ty in u.tys {
                    self.bind_ty(ty);
                }
            }
            Intersection(i) => {
                for ty in i.tys {
                    self.bind_ty(ty);
                }
            }
            Typeof(n) => {
                self.bind_entity_name(n.name);
            }
        }
    }

    fn bind_array_ty(&mut self, array: &'cx ast::ArrayTy) {
        self.bind_ty(array.ele)
    }

    fn bind_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>, kind: ast::VarKind) {
        self.connect(decl.id);
        let kind = if kind == ast::VarKind::Let || kind == ast::VarKind::Const {
            SymbolKind::BlockScopedVar { decl: decl.id }
        } else {
            SymbolKind::FunctionScopedVar(FunctionScopedVarSymbol { decl: decl.id })
        };
        self.create_var_decl(decl, kind);
        if let Some(ty) = decl.ty {
            self.bind_ty(ty);
        }
        if let Some(init) = decl.init {
            self.bind_expr(init);
        }
    }

    fn bind_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.bind_param(param);
        }
    }

    fn bind_param(&mut self, param: &'cx ast::ParamDecl) {
        let symbol = self.create_var_symbol(
            param.name.name,
            SymbolFlags::FUNCTION_SCOPED_VARIABLE,
            SymbolKind::FunctionScopedVar(FunctionScopedVarSymbol { decl: param.id }),
        );
        self.create_final_res(param.id, symbol);
        if let Some(ty) = param.ty {
            self.bind_ty(ty);
        }
        if let Some(init) = param.init {
            self.bind_expr(init);
        }
    }

    fn bind_fn_decl(&mut self, container: ast::NodeID, f: &'cx ast::FnDecl<'cx>) {
        self.connect(f.id);
        self.create_fn_symbol(container, f);

        if let Some(ty_params) = f.ty_params {
            self.bind_ty_params(ty_params);
        }

        let old = self.scope_id;
        self.scope_id = self.new_scope();
        self.bind_params(f.params);
        if let Some(body) = f.body {
            self.bind_block_stmt(body);
        }
        if let Some(ty) = f.ty {
            self.bind_ty(ty);
        }
        self.scope_id = old;
    }
}

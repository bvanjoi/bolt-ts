mod bind_call_like;
mod bind_class_like;
mod bind_fn_like;
mod create;
mod symbol;

use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;
pub use symbol::ClassSymbol;
use symbol::IndexSymbol;
pub use symbol::SymbolFlags;
pub use symbol::SymbolFnKind;
use symbol::SymbolKind;
pub use symbol::{GlobalSymbols, Symbol, SymbolID, SymbolName, Symbols};

use crate::ast::{self, NodeID};
use crate::atoms::AtomMap;
use crate::parser::Parser;
use crate::resolve::ResolveResult;

bolt_ts_span::new_index_with_module!(ScopeID);

impl ScopeID {
    pub const fn is_root(&self) -> bool {
        self.index == 0
    }
}

pub struct Binder<'cx> {
    p: &'cx Parser<'cx>,
    atoms: &'cx AtomMap<'cx>,
    binder_result: Vec<ResolveResult>,
    anonymous_binder: ResolveResult,
}

impl<'cx> Binder<'cx> {
    pub fn new(p: &'cx Parser<'cx>, atoms: &'cx AtomMap) -> Self {
        Self {
            p,
            atoms,
            binder_result: Vec::with_capacity(p.module_count() + 1),
            anonymous_binder: ResolveResult {
                symbols: Symbols::new(ModuleID::MOCK),
                final_res: Default::default(),
                diags: Default::default(),
            },
        }
    }

    pub fn insert(&mut self, id: ModuleID, result: ResolveResult) {
        assert_eq!(self.binder_result.len(), id.as_usize());
        self.binder_result.push(result);
    }

    #[inline(always)]
    fn get(&self, id: ModuleID) -> &ResolveResult {
        let idx = id.as_usize();
        &self.binder_result[idx]
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
    pub fn symbols(&self, id: ModuleID) -> &Symbols {
        &self.get(id).symbols
    }

    #[inline(always)]
    pub fn symbol(&self, id: SymbolID) -> &Symbol {
        self.get(id.module()).symbols.get(id)
    }

    #[inline(always)]
    pub fn create_anonymous_symbol(&mut self, name: SymbolName, flags: SymbolFlags) -> SymbolID {
        let len = self.anonymous_binder.symbols.len();
        let id = SymbolID::mock(len as u32);
        let symbol = Symbol::new(name, flags, SymbolKind::ElementProperty);
        self.anonymous_binder.symbols.insert(id, symbol);
        id
    }

    pub fn steal_errors(&mut self, id: ModuleID) -> Vec<bolt_ts_errors::Diag> {
        std::mem::take(&mut self.binder_result[id.as_usize()].diags)
    }
}

pub struct BinderState<'cx> {
    scope_id: ScopeID,
    max_scope_id: ScopeID,
    symbol_id: SymbolID,
    pub(crate) atoms: &'cx AtomMap<'cx>,
    pub(crate) scope_id_parent_map: Vec<Option<ScopeID>>,
    pub(crate) node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub(crate) symbols: Symbols,
    pub(super) res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    pub(crate) final_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub fn bind<'cx>(
    atoms: &'cx AtomMap<'cx>,
    root: &'cx ast::Program,
    module_id: ModuleID,
) -> BinderState<'cx> {
    let mut state = BinderState::new(atoms, module_id);
    state.bind_program(root);
    state
}

impl<'cx> BinderState<'cx> {
    fn new(atoms: &'cx AtomMap, module_id: ModuleID) -> Self {
        let symbols = Symbols::new(module_id);
        let mut symbol_id = SymbolID::root(module_id);
        symbol_id = symbol_id.next();
        BinderState {
            atoms,
            scope_id: ScopeID::root(module_id),
            max_scope_id: ScopeID::root(module_id),
            scope_id_parent_map: Vec::with_capacity(512),
            res: Default::default(),
            final_res: Default::default(),
            node_id_to_scope_id: FxHashMap::default(),
            symbol_id,
            symbols,
        }
    }

    fn connect(&mut self, node_id: NodeID) {
        let prev = self.node_id_to_scope_id.insert(node_id, self.scope_id);
        assert!(prev.is_none());
    }

    fn new_scope(&mut self) -> ScopeID {
        let old = self.scope_id;
        let next = self.max_scope_id.next();
        self.max_scope_id = next;
        assert_eq!(next.index_as_usize(), self.scope_id_parent_map.len());
        self.scope_id_parent_map.push(Some(old));
        next
    }

    fn bind_program(&mut self, root: &'cx ast::Program) {
        assert_eq!(self.scope_id.index_as_u32(), 0);
        assert_eq!(self.symbol_id.index_as_u32(), 1);
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
            Class(class) => self.bind_class_like(class),
            Interface(interface) => self.bind_interface_decl(interface),
            Type(t) => self.bind_type_decl(t),
            Namespace(_) => {}
        }
    }

    fn bind_type_decl(&mut self, t: &'cx ast::TypeDecl<'cx>) {
        let symbol = self.create_var_symbol(
            t.name.name,
            SymbolFlags::TYPE_ALIAS,
            SymbolKind::TyAlias(symbol::TyAliasSymbol { decl: t.id }),
        );
        self.final_res.insert(t.id, symbol);
        if let Some(ty_params) = t.ty_params {
            self.bind_ty_params(ty_params);
        }
        self.bind_ty(&t.ty);
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
        self.final_res.insert(param.id, symbol);
    }

    fn bind_index_sig(&mut self, index: &'cx ast::IndexSigDecl<'cx>) -> SymbolID {
        self.create_symbol(
            SymbolName::Index,
            SymbolFlags::SIGNATURE,
            SymbolKind::Index(IndexSymbol { decl: index.id }),
        )
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
                let Some(s) = self.final_res.get(&container).copied() else {
                    unreachable!()
                };
                let s = self.symbols.get_mut(s);
                if let Some(i) = &mut s.kind.1 {
                    let prev = i.members.insert(name, symbol);
                    assert!(prev.is_none());
                } else if let SymbolKind::Object(o) = &mut s.kind.0 {
                    let prev = o.members.insert(name, symbol);
                    assert!(prev.is_none());
                } else {
                    todo!()
                }
            }
            Method(_) => {}
            CallSig(_) => {
                // let name = SymbolName::;
                // (name, self.create_object_member_symbol(name, m.id))
            }
            IndexSig(_) => {}
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
            for ty in extends.tys {
                self.bind_ty(ty);
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
        }
    }

    fn bind_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();
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
            Class(class) => self.bind_class_like(class),
            PrefixUnary(unary) => self.bind_expr(unary.expr),
            _ => (),
        }
    }

    fn bind_fn_expr(&mut self, f: &'cx ast::FnExpr<'cx>) {
        self.create_fn_expr_symbol(f.id);
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
            ExprWithArg(expr) => self.bind_expr(expr),
            Refer(refer) => {
                self.bind_ident(refer.name);
                if let Some(args) = refer.args {
                    for arg in args {
                        self.bind_ty(arg);
                    }
                }
            }
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
                self.bind_params(f.params);
                self.bind_ty(f.ret_ty);
            }
            NumLit(_) | StringLit(_) => {}
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
            SymbolKind::FunctionScopedVar { decl: decl.id }
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
            SymbolKind::FunctionScopedVar { decl: param.id },
        );
        self.final_res.insert(param.id, symbol);
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
            self.create_block_container_symbol(body.id);
            for stmt in body.stmts {
                self.bind_stmt(body.id, stmt)
            }
        }
        self.scope_id = old;
    }
}

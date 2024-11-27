mod bind_class_like;
mod create;
mod symbol;

use bolt_ts_span::ModuleID;
use rustc_hash::FxHashMap;
pub use symbol::{GlobalSymbols, Symbol, SymbolFnKind, SymbolID, SymbolKind, SymbolName, Symbols};

use crate::ast::{self, NodeID};
use crate::atoms::AtomMap;

bolt_ts_span::new_index!(ScopeID);

pub struct Binder {
    map: FxHashMap<ModuleID, BinderResult>,
}

impl Binder {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
        }
    }

    pub fn insert(&mut self, id: ModuleID, result: BinderResult) {
        let prev = self.map.insert(id, result);
        assert!(prev.is_none());
    }

    pub fn get(&self, id: ModuleID) -> &BinderResult {
        if self.map.get(&id).is_none() {
            dbg!(id);
            panic!("BinderResult not found for module {:?}", id);
        }
        self.map.get(&id).unwrap()
    }

    pub fn get_mut(&mut self, id: ModuleID) -> &mut BinderResult {
        self.map.get_mut(&id).unwrap()
    }

    pub fn take(&mut self, id: ModuleID) -> BinderResult {
        self.map.remove(&id).unwrap()
    }
}

struct BinderState<'cx> {
    scope_id: ScopeID,
    max_scope_id: ScopeID,
    symbol_id: SymbolID,
    atoms: &'cx AtomMap<'cx>,
    scope_id_parent_map: FxHashMap<ScopeID, Option<ScopeID>>,
    node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    symbols: Symbols,
    res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    final_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub struct BinderResult {
    pub scope_id_parent_map: FxHashMap<ScopeID, Option<ScopeID>>,
    pub node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub symbols: Symbols,
    pub res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub fn bind<'cx>(atoms: &'cx AtomMap<'cx>, p: &'cx ast::Program) -> BinderResult {
    let mut state = BinderState::new(atoms);
    state.bind_program(p);
    BinderResult {
        scope_id_parent_map: state.scope_id_parent_map,
        node_id_to_scope_id: state.node_id_to_scope_id,
        symbols: state.symbols,
        res: state.res,
        final_res: state.final_res,
    }
}

impl<'cx> BinderState<'cx> {
    fn new(atoms: &'cx AtomMap<'cx>) -> Self {
        let symbols = Symbols::new();
        let mut symbol_id = SymbolID::root();
        symbol_id = symbol_id.next();
        BinderState {
            atoms,
            scope_id: ScopeID::root(),
            max_scope_id: ScopeID::root(),
            scope_id_parent_map: FxHashMap::default(),
            res: FxHashMap::default(),
            final_res: FxHashMap::default(),
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
        self.scope_id_parent_map.insert(next, Some(old));
        next
    }

    fn bind_program(&mut self, p: &'cx ast::Program) {
        assert_eq!(self.scope_id.as_u32(), 0);
        assert_eq!(self.symbol_id.as_u32(), 1);
        self.scope_id_parent_map.insert(self.scope_id, None);
        self.connect(p.id);
        self.create_block_container_symbol(p.id);
        for stmt in p.stmts {
            self.bind_stmt(p.id, stmt)
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
            Interface(interface) => self.bind_interface(interface),
        }
    }

    fn bind_index_sig(&mut self, index: &'cx ast::IndexSigDecl<'cx>) -> SymbolID {
        self.create_symbol(SymbolName::Index, SymbolKind::Index { decl: index.id })
    }

    fn bind_interface(&mut self, i: &'cx ast::InterfaceDecl<'cx>) {
        if let Some(extends) = i.extends {
            for ty in extends.tys {
                self.bind_ty(ty);
            }
        }
        let members = i
            .members
            .iter()
            .flat_map(|m| {
                use ast::ObjectTyMemberKind::*;
                match m.kind {
                    Prop(m) => {
                        let name = Self::prop_name(m.name);
                        Some((name, self.create_object_member_symbol(name, m.id)))
                    }
                    Method(_) => None,
                    CallSig(_) => {
                        // let name = SymbolName::;
                        // (name, self.create_object_member_symbol(name, m.id))
                        None
                    }
                }
            })
            .collect();
        self.create_interface_symbol(i.id, members);
    }

    fn prop_name(name: &ast::PropName) -> SymbolName {
        match name.kind {
            ast::PropNameKind::Ident(ident) => SymbolName::Ele(ident.name),
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
            Call(call) => {
                self.bind_expr(call.expr);
                for arg in call.args {
                    self.bind_expr(arg);
                }
            }
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
            New(new) => self.bind_expr(new.expr),
            Class(class) => self.bind_class_like(class),
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
            Ident(ident) => self.bind_ident(ident),
            Array(array) => self.bind_array_ty(array),
            Lit(lit) => {
                let old = self.scope_id;
                self.scope_id = self.new_scope();
                self.create_object_lit_symbol(lit.id, Default::default());
                self.scope_id = old;
            }
            ExprWithArg(expr) => self.bind_expr(expr),
            _ => (),
        }
    }

    fn bind_array_ty(&mut self, array: &'cx ast::ArrayTy) {
        self.bind_ty(array.ele)
    }

    fn bind_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>, kind: ast::VarKind) {
        self.connect(decl.id);
        let kind = if kind == ast::VarKind::Let || kind == ast::VarKind::Const {
            SymbolKind::FunctionScopedVar
        } else {
            SymbolKind::BlockScopedVar
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
        self.connect(param.id);
        self.create_var_symbol(param.name.name, SymbolKind::FunctionScopedVar);
    }

    fn bind_fn_decl(&mut self, container: ast::NodeID, f: &'cx ast::FnDecl<'cx>) {
        self.connect(f.id);
        self.create_fn_symbol(container, f);

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

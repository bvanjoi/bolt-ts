mod symbol;

use rustc_hash::FxHashMap;
pub use symbol::{Symbol, SymbolID, SymbolKind, Symbols};

use crate::ast::{self, NodeID};
use crate::atoms::{AtomId, AtomMap};
use thin_vec::thin_vec;

rts_span::new_index!(ScopeID);

pub struct Binder<'cx> {
    scope_id: ScopeID,
    symbol_id: SymbolID,
    atoms: &'cx AtomMap<'cx>,
    pub scope_id_parent_map: FxHashMap<ScopeID, Option<ScopeID>>,
    pub node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub symbols: Symbols,
    pub res: FxHashMap<(ScopeID, AtomId), SymbolID>,
}

impl<'cx> Binder<'cx> {
    pub fn new(atoms: &'cx AtomMap<'cx>) -> Self {
        Binder {
            atoms,
            scope_id: ScopeID::root(),
            scope_id_parent_map: FxHashMap::default(),
            res: FxHashMap::default(),
            symbol_id: SymbolID::root(),
            symbols: Symbols::new(),
            node_id_to_scope_id: FxHashMap::default(),
        }
    }

    fn create_symbol(&mut self, name: AtomId, kind: SymbolKind) {
        let id = self.symbol_id;
        self.symbol_id = self.symbol_id.next();
        let is_blocked_scope_var = matches!(kind, SymbolKind::BlockedScopeVar);
        self.symbols.insert(id, Symbol::new(name, kind));
        let prev = self.res.insert((self.scope_id, name), id);
        if !is_blocked_scope_var {
            assert!(
                prev.is_none(),
                "`{}` is a duplicate symbol ",
                self.atoms.get(name)
            );
        }
    }

    fn connect(&mut self, node_id: NodeID) {
        let prev = self.node_id_to_scope_id.insert(node_id, self.scope_id);
        assert!(prev.is_none());
    }

    fn new_scope(&mut self) -> ScopeID {
        let old = self.scope_id;
        let next = self.scope_id.next();
        self.scope_id_parent_map.insert(next, Some(old));
        next
    }

    pub fn bind_program(&mut self, p: &'cx ast::Program) {
        self.scope_id_parent_map.insert(self.scope_id, None);
        self.connect(p.id);
        for stmt in p.stmts {
            self.bind_stmt(stmt)
        }
    }

    fn bind_stmt(&mut self, stmt: &'cx ast::Stmt) {
        self.connect(stmt.id);
        use ast::StmtKind::*;
        match stmt.kind {
            Empty(_) => (),
            Var(var) => self.bind_var_stmt(var),
            Expr(expr) => self.bind_expr(expr),
            Fn(f) => self.bind_fn_decl(f),
            If(stmt) => {
                self.bind_expr(stmt.expr);
                self.bind_stmt(stmt.then);
                if let Some(alt) = stmt.else_then {
                    self.bind_stmt(alt)
                }
            }
            Block(block) => self.bind_block(block),
            Return(ret) => {
                if let Some(expr) = ret.expr {
                    self.bind_expr(expr)
                }
            }
        }
    }

    fn bind_block(&mut self, block: ast::Stmts<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();
        for stmt in block {
            self.bind_stmt(stmt)
        }
        self.scope_id = old;
    }

    fn bind_expr(&mut self, expr: &'cx ast::Expr) {
        use ast::ExprKind::*;
        match expr.kind {
            Ident(ident) => self.connect(ident.id),
            Call(call) => {
                self.bind_expr(call.expr);
                for arg in call.args {
                    self.bind_expr(arg);
                }
            }
            _ => (),
        }
    }

    fn bind_var_stmt(&mut self, var: &'cx ast::VarStmt) {
        self.connect(var.id);
        for item in var.list {
            self.bind_var_decl(item);
        }
    }

    fn bind_var_decl(&mut self, decl: &'cx ast::VarDecl) {
        self.connect(decl.id);
        self.create_symbol(decl.name.name, SymbolKind::BlockedScopeVar);
        if let Some(init) = decl.init {
            self.bind_expr(init);
        }
    }

    fn create_fn_symbol(&mut self, name: AtomId, id: NodeID) {
        if let Some(s) = self.res.get(&(self.scope_id, name)).copied() {
            let symbol = self.symbols.get_mut(s);
            match &mut symbol.kind {
                SymbolKind::BlockedScopeVar => todo!(),
                SymbolKind::Function(vec) => {
                    assert!(!vec.is_empty());
                    vec.push(id)
                }
            }
        } else {
            self.create_symbol(name, SymbolKind::Function(thin_vec::thin_vec![id]));
        }
    }

    fn bind_fn_decl(&mut self, f: &'cx ast::FnDecl) {
        self.connect(f.id);
        self.create_fn_symbol(f.name.name, f.id);
        let old = self.scope_id;
        self.scope_id = self.new_scope();
        self.scope_id = old;
    }
}

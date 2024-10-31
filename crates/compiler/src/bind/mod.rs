mod symbol;

use rustc_hash::FxHashMap;
pub use symbol::{Symbol, SymbolID, SymbolKind, SymbolName, Symbols};

use crate::ast::{self, NodeID};
use crate::atoms::{AtomId, AtomMap};
use thin_vec::thin_vec;

rts_span::new_index!(ScopeID);

pub struct Binder<'cx> {
    scope_id: ScopeID,
    max_scope_id: ScopeID,
    symbol_id: SymbolID,
    atoms: &'cx AtomMap<'cx>,
    pub scope_id_parent_map: FxHashMap<ScopeID, Option<ScopeID>>,
    pub node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub symbols: Symbols,
    pub res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
}

impl<'cx> Binder<'cx> {
    pub fn new(atoms: &'cx AtomMap<'cx>) -> Self {
        let mut symbol_id = SymbolID::root();
        let symbols = Symbols::new(symbol_id);
        symbol_id = symbol_id.next();
        Binder {
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

    fn create_symbol(&mut self, name: SymbolName, kind: SymbolKind) -> SymbolID {
        let id = self.symbol_id;
        self.symbol_id = self.symbol_id.next();
        let is_blocked_scope_var = matches!(kind, SymbolKind::BlockScopedVar);
        self.symbols.insert(id, Symbol::new(name, kind));
        let prev = self.res.insert((self.scope_id, name), id);
        if !is_blocked_scope_var {
            let name = match name {
                SymbolName::Normal(atom_id) => self.atoms.get(atom_id),
            };
            assert!(prev.is_none(), "`{name:#?}` is a duplicate symbol ");
        }
        id
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
            Block(block) => self.bind_block_stmt(block),
            Return(ret) => {
                if let Some(expr) = ret.expr {
                    self.bind_expr(expr)
                }
            }
            Class(class) => self.bind_class(class),
            Interface(_) => {}
        }
    }

    fn bind_class(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.connect(class.id);
        self.create_symbol(SymbolName::Normal(class.name.name), SymbolKind::Class);
    }

    fn bind_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();
        for stmt in block.stmts {
            self.bind_stmt(stmt)
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
                self.bind_ident(assign.binding);
                self.bind_expr(assign.right);
            }
            ObjectLit(lit) => self.bind_object_lit(lit),
            _ => (),
        }
    }

    fn bind_object_lit(&mut self, lit: &'cx ast::ObjectLit) {
        let old = self.scope_id;
        self.scope_id = self.new_scope();
        for member in lit.members {
            let name = match member.name.kind {
                ast::PropNameKind::Ident(ident) => SymbolName::Normal(ident.name),
            };
            let symbol = self.create_symbol(name, SymbolKind::Property);
            self.final_res.insert(member.id, symbol);
        }
        self.scope_id = old;
    }

    fn bind_var_stmt(&mut self, var: &'cx ast::VarStmt) {
        self.connect(var.id);
        let kind = var.kind;
        for item in var.list {
            self.bind_var_decl(item, kind);
        }
    }

    fn bind_var_decl(&mut self, decl: &'cx ast::VarDecl, kind: ast::VarKind) {
        self.connect(decl.id);
        let kind = if kind == ast::VarKind::Let || kind == ast::VarKind::Const {
            SymbolKind::FunctionScopedVar
        } else {
            SymbolKind::BlockScopedVar
        };
        let symbol = self.create_symbol(SymbolName::Normal(decl.binding.name), kind);
        self.final_res.insert(decl.id, symbol);
        if let Some(init) = decl.init {
            self.bind_expr(init);
        }
    }

    fn create_fn_symbol(&mut self, name: AtomId, id: NodeID) {
        let name = SymbolName::Normal(name);
        if let Some(s) = self.res.get(&(self.scope_id, name)).copied() {
            let symbol = self.symbols.get_mut(s);
            match &mut symbol.kind {
                SymbolKind::Function(vec) => {
                    assert!(!vec.is_empty());
                    vec.push(id)
                }
                _ => unreachable!(),
            }
        } else {
            self.create_symbol(name, SymbolKind::Function(thin_vec::thin_vec![id]));
        }
    }

    fn bind_params(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.bind_param(param);
        }
    }

    fn bind_param(&mut self, param: &'cx ast::ParamDecl) {
        self.connect(param.id);
        self.create_symbol(
            SymbolName::Normal(param.name.name),
            SymbolKind::FunctionScopedVar,
        );
    }

    fn bind_fn_decl(&mut self, f: &'cx ast::FnDecl) {
        self.connect(f.id);
        self.create_fn_symbol(f.name.name, f.id);

        let old = self.scope_id;
        self.scope_id = self.new_scope();
        self.bind_params(f.params);
        self.bind_fn_block(f.body.stmts);
        self.scope_id = old;
    }

    fn bind_fn_block(&mut self, block: ast::Stmts<'cx>) {
        for stmt in block {
            self.bind_stmt(stmt)
        }
    }
}

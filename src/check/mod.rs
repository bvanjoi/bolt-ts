use rustc_hash::FxHashMap;

use crate::atoms::AtomMap;
use crate::ty::{Ty, TyFlags, TyID, TyKind};
use crate::{ast, ty};

pub struct TyChecker<'cx> {
    atoms: &'cx AtomMap,
    arena: &'cx bumpalo::Bump,
    next_ty_id: TyID,
    pub tys: FxHashMap<TyID, Ty<'cx>>,
    num_lit_tys: FxHashMap<u64, TyID>,
}

impl<'cx> TyChecker<'cx> {
    pub fn new(ty_arena: &'cx bumpalo::Bump, atoms: &'cx AtomMap) -> Self {
        assert!(ty_arena.allocation_limit().is_none());
        Self {
            atoms,
            tys: FxHashMap::default(),
            num_lit_tys: FxHashMap::default(),
            next_ty_id: TyID::root(),
            arena: ty_arena,
        }
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.arena.alloc(t)
    }

    fn new_ty(&mut self, flags: TyFlags, kind: TyKind<'cx>) -> Ty<'cx> {
        let id = self.next_node_id();
        let ty = Ty::new(id, flags, kind);
        let prev = self.tys.insert(id, ty);
        assert!(prev.is_none());
        ty
    }

    fn next_node_id(&mut self) -> TyID {
        let old = self.next_ty_id;
        self.next_ty_id = self.next_ty_id.next();
        old
    }

    pub fn check_program(&mut self, program: &ast::Program) {
        for stmt in program.stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt.kind {
            ast::StmtKind::ExprStmt(expr) => self.check_expr(expr),
        };
    }

    fn check_expr(&mut self, expr: &ast::Expr) -> Ty {
        use ast::ExprKind::*;
        match expr.kind {
            // BinOp(bin) => self.check_bin_expr(bin),
            NumLit(lit) => self.get_number_literal_type(lit.val),
            _ => todo!(),
        }
    }

    fn get_number_literal_type(&mut self, val: f64) -> Ty {
        let key = unsafe { std::mem::transmute::<f64, u64>(val) };
        let id = self.num_lit_tys.get(&key).copied();
        if let Some(id) = id {
            return self.tys[&id];
        }
        let flags = TyFlags::NumberLiteral;
        let kind = TyKind::NumLit(self.alloc(ty::NumLitTy { val }));
        let ty = self.new_ty(flags, kind);
        self.num_lit_tys.insert(key, ty.id);
        ty
    }

    // fn check_bin_expr(&mut self, node: &ast::BinExpr) -> Ty {
    //     enum State {
    //         Enter,
    //         Left,
    //         Op,
    //         Right,
    //         Exit,
    //         Done,
    //     }

    //     impl State {
    //         fn next(self) -> State {
    //             let len = State::Done as u8;
    //             let next = u8::min((self as u8) + 1, len);
    //             unsafe { std::mem::transmute(next) }
    //         }
    //     }

    //     let stack = vec![node];
    //     let mut index = 0;
    // }
}

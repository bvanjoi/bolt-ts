use rts_span::ModuleID;
use rustc_hash::FxHashMap;

use crate::ast::{BinOp, ExprKind};
use crate::atoms::{AtomId, AtomMap};
use crate::ty::{Ty, TyFlags, TyID, TyKind};
use crate::{ast, errors, keyword, ty};

pub struct TyChecker<'cx> {
    atoms: AtomMap,
    arena: &'cx bumpalo::Bump,
    next_ty_id: TyID,
    pub tys: FxHashMap<TyID, Ty<'cx>>,
    num_lit_tys: FxHashMap<u64, TyID>,
    intrinsic_tys: FxHashMap<AtomId, Ty<'cx>>,
    pub diags: Vec<rts_errors::Diag>,
}

macro_rules! intrinsic_type {
    ($(($name: ident, $atom_id: expr, $ty_flags: expr)),* $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(fn $name(&self) -> Ty<'cx> {
                self.intrinsic_tys[&$atom_id]
            })*
        }
        static INTRINSIC_TYPES: &[(TyFlags, AtomId)] = &[
            $(($ty_flags, $atom_id),)*
        ];
    };
}

intrinsic_type!(
    (any_type, keyword::IDENT_ANY, TyFlags::Any),
    (undefined_type, keyword::IDENT_UNDEFINED, TyFlags::Undefined),
    (null_type, keyword::KW_NULL, TyFlags::Number),
    (true_type, keyword::KW_TRUE, TyFlags::BooleanLiteral),
    (false_type, keyword::KW_FALSE, TyFlags::BooleanLiteral),
    (number_type, keyword::IDENT_NUMBER, TyFlags::Number),
);

impl<'cx> TyChecker<'cx> {
    pub fn new(ty_arena: &'cx bumpalo::Bump, atoms: AtomMap) -> Self {
        assert!(ty_arena.allocation_limit().is_none());
        let mut this = Self {
            intrinsic_tys: FxHashMap::default(),
            atoms,
            tys: FxHashMap::default(),
            num_lit_tys: FxHashMap::default(),
            next_ty_id: TyID::root(),
            arena: ty_arena,
            diags: Vec::with_capacity(32),
        };
        for (flags, ty_name) in INTRINSIC_TYPES {
            let kind = ty::TyKind::Intrinsic(ty_arena.alloc(ty::IntrinsicTy { name: *ty_name }));
            let ty = this.new_ty(*flags, kind);
            let prev = this.intrinsic_tys.insert(*ty_name, ty);
            assert!(prev.is_none());
        }
        this
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
        use ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.check_var_stmt(var),
            Expr(expr) => {
                self.check_expr(expr);
            }
        };
    }

    fn check_var_stmt(&mut self, var: &ast::VarStmt) {
        self.check_var_decl_list(var.list);
    }

    fn check_var_decl_list(&mut self, list: &[&ast::VarDecl]) {
        for decl in list {
            self.check_var_decl(decl);
        }
    }

    fn check_var_decl(&mut self, decl: &ast::VarDecl) {
        self.check_var_like_decl(decl);
    }

    fn check_var_like_decl(&mut self, decl: &ast::VarDecl) {
        if let Some(init) = decl.init {
            let _init_ty = self.check_expr(init);
        }
    }

    fn check_expr(&mut self, expr: &ast::Expr) -> Ty<'cx> {
        use ast::ExprKind::*;
        match expr.kind {
            BinOp(bin) => self.check_bin_expr(bin),
            NumLit(lit) => self.get_number_literal_type(lit.val),
            BoolLit(lit) => {
                if lit.val {
                    self.true_type()
                } else {
                    self.false_type()
                }
            }
            NullLit(_) => self.null_type(),
            Ident(ident) => self.check_ident(ident),
        }
    }

    fn check_ident(&mut self, ident: &ast::Ident) -> Ty<'cx> {
        if ident.name == keyword::IDENT_UNDEFINED {
            self.undefined_type()
        } else {
            todo!()
        }
    }

    fn get_number_literal_type(&mut self, val: f64) -> Ty<'cx> {
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

    fn check_bin_expr(&mut self, node: &ast::BinExpr) -> Ty<'cx> {
        let l = self.check_expr(node.left);
        let r = self.check_expr(node.right);
        self.check_bin_like_expr(node, node.op, node.left, l, node.right, r)

        // #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        // enum State {
        //     Enter,
        //     Left,
        //     Op,
        //     Right,
        //     Exit,
        //     Done,
        // }

        // impl State {
        //     fn next(self) -> State {
        //         let len = State::Done as u8;
        //         let next = u8::min((self as u8) + 1, len);
        //         unsafe { std::mem::transmute(next) }
        //     }
        // }

        // #[derive(Debug, Clone, Copy)]
        // struct WorkArea {
        //     types: [Option<TyID>; 2],
        // }

        // let mut states = vec![State::Enter];
        // // let stack = vec![node];
        // let mut ty_states = vec![];
        // let index = 0;
        // while states[index] != State::Done {
        //     match states[index] {
        //         State::Enter => {
        //             states[index] = State::Left;
        //             ty_states.push(WorkArea {
        //                 types: [None, None],
        //             });
        //         }
        //         State::Left => {
        //             states[index] = State::Op;
        //             let l = self.check_expr(node.left);
        //             ty_states[index].types[0] = Some(l.id);
        //         }
        //         State::Op => {
        //             states[index] = State::Right;
        //         }
        //         State::Right => {
        //             states[index] = State::Exit;
        //             let r = self.check_expr(node.left);
        //             ty_states[index].types[1] = Some(r.id);
        //         }
        //         State::Exit => {
        //             states[index] = State::Done;
        //             let l = self.tys[&ty_states[index].types[0].unwrap()];
        //             let r = self.tys[&ty_states[index].types[0].unwrap()];
        //             return self.check_bin_like_expr(node, node.op, node.left, l, node.right, r);
        //         }
        //         State::Done => {
        //             unreachable!()
        //         }
        //     }
        // };
        // unreachable!();
    }

    fn check_non_null_type(&mut self, expr: &ast::Expr) -> Ty<'cx> {
        if matches!(expr.kind, ExprKind::NullLit(_)) {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "null".to_string(),
            };
            self.push_error(expr.span().module, Box::new(error));
            self.null_type()
        } else if matches!(expr.kind, ExprKind::Ident(ast::Ident { name, .. }) if *name == keyword::IDENT_UNDEFINED)
        {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "undefined".to_string(),
            };
            self.push_error(expr.span().module, Box::new(error));
            self.null_type()
        } else {
            self.null_type()
        }
    }

    fn check_bin_like_expr(
        &mut self,
        node: &ast::BinExpr,
        op: BinOp,
        left: &ast::Expr,
        left_ty: Ty<'cx>,
        right: &ast::Expr,
        right_ty: Ty<'cx>,
    ) -> Ty<'cx> {
        use ast::BinOpKind::*;

        match op.kind {
            Pipe => {
                let left = self.check_non_null_type(left);
                let right = self.check_non_null_type(right);
            }
            _ => (),
        };

        match op.kind {
            Add => {
                if self.is_type_assignable_to_kind(left_ty, TyFlags::NumberLike, true)
                    && self.is_type_assignable_to_kind(right_ty, TyFlags::NumberLike, true)
                {
                    self.number_type()
                } else {
                    let error = errors::OperatorCannotBeAppliedToTy1AndTy2 {
                        op: op.kind.as_str().to_string(),
                        ty1: left_ty.kind.as_str(&self.atoms).to_string(),
                        ty2: right_ty.kind.as_str(&self.atoms).to_string(),
                        span: node.span,
                    };
                    self.push_error(node.span.module, Box::new(error));
                    self.any_type()
                }
            }
            Sub => todo!(),
            Mul => todo!(),
            Div => todo!(),
            Pipe => self.number_type(),
        }
    }

    fn push_error(
        &mut self,
        module_id: ModuleID,
        error: Box<dyn rts_errors::miette::Diagnostic + Send + Sync + 'static>,
    ) {
        self.diags.push(rts_errors::Diag {
            module_id,
            inner: error,
        })
    }

    fn is_type_assignable_to_kind(&self, source: Ty, kind: TyFlags, strict: bool) -> bool {
        if !(source.flags & kind).is_empty() {
            return true;
        }
        // if strict && source.flags.contains(TyFlags::AnyOrUnknown | TyFlags::Void | TyFlags::Undefined | TyFlags::Null) {
        //     return false;
        // }
        !!(!(kind & TyFlags::NumberLike).is_empty())
            && is_type_assignable_to(source, self.number_type())
    }
}

struct Relation {}

fn is_type_related_to(source: Ty, target: Ty, relation: &Relation) -> bool {
    false
}

fn is_type_assignable_to(source: Ty, target: Ty) -> bool {
    false
}

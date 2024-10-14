mod get;
mod relation;
mod resolve;
mod sig;

use rts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;
use sig::Sig;

use self::relation::RelationKind;
pub use self::resolve::ExpectedArgsCount;

use crate::ast::{BinOp, ExprKind};
use crate::atoms::{AtomId, AtomMap};
use crate::bind::{ScopeID, Symbol, SymbolID, Symbols};
use crate::parser::Nodes;
use crate::ty::{IntrinsicTyKind, ObjectTyKind, Ty, TyID, TyKind, Tys};
use crate::{ast, errors, keyword, ty};

pub struct TyChecker<'cx> {
    pub atoms: &'cx AtomMap<'cx>,
    pub diags: Vec<rts_errors::Diag>,
    nodes: &'cx Nodes<'cx>,
    arena: &'cx bumpalo::Bump,
    next_ty_id: TyID,
    tys: FxHashMap<TyID, &'cx Ty<'cx>>,
    num_lit_tys: FxHashMap<u64, TyID>,
    intrinsic_tys: FxHashMap<AtomId, &'cx Ty<'cx>>,
    boolean_ty: std::cell::OnceCell<&'cx Ty<'cx>>,
    type_name: FxHashMap<TyID, String>,
    type_symbol: FxHashMap<TyID, SymbolID>,
    scope_id_parent_map: FxHashMap<ScopeID, Option<ScopeID>>,
    node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    node_id_to_sig: FxHashMap<ast::NodeID, Sig<'cx>>,
    symbols: Symbols,
    res: FxHashMap<(ScopeID, AtomId), SymbolID>,
    final_res: FxHashMap<ast::NodeID, SymbolID>,
    global_tys: FxHashMap<TyID, &'cx Ty<'cx>>
}

macro_rules! intrinsic_type {
    ($(($name: ident, $atom_id: expr, $ty_flags: expr)),* $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(fn $name(&self) -> &'cx Ty<'cx> {
                self.intrinsic_tys[&$atom_id]
            })*
        }
        static INTRINSIC_TYPES: &[(IntrinsicTyKind, AtomId)] = &[
            $(($ty_flags, $atom_id),)*
        ];
    };
}

intrinsic_type!(
    (any_ty, keyword::IDENT_ANY, IntrinsicTyKind::Any),
    (error_ty, keyword::IDENT_ERROR, IntrinsicTyKind::Undefined),
    (void_ty, keyword::IDENT_VOID, IntrinsicTyKind::Void),
    (
        undefined_ty,
        keyword::IDENT_UNDEFINED,
        IntrinsicTyKind::Undefined
    ),
    (null_ty, keyword::KW_NULL, IntrinsicTyKind::Null),
    (true_ty, keyword::KW_TRUE, IntrinsicTyKind::True),
    (false_ty, keyword::KW_FALSE, IntrinsicTyKind::False),
    (number_ty, keyword::IDENT_NUMBER, IntrinsicTyKind::Number),
    (string_ty, keyword::IDENT_STRING, IntrinsicTyKind::String),
);

impl<'cx> TyChecker<'cx> {
    pub fn new(
        ty_arena: &'cx bumpalo::Bump,
        nodes: &'cx Nodes<'cx>,
        atoms: &'cx AtomMap<'cx>,
        scope_id_parent_map: FxHashMap<ScopeID, Option<ScopeID>>,
        node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
        symbols: Symbols,
        res: FxHashMap<(ScopeID, AtomId), SymbolID>,
    ) -> Self {
        assert!(ty_arena.allocation_limit().is_none());
        let mut this = Self {
            intrinsic_tys: FxHashMap::default(),
            atoms,
            tys: FxHashMap::default(),
            num_lit_tys: FxHashMap::default(),
            next_ty_id: TyID::root(),
            arena: ty_arena,
            diags: Vec::with_capacity(32),
            boolean_ty: Default::default(),
            type_name: FxHashMap::default(),
            scope_id_parent_map,
            node_id_to_scope_id,
            symbols,
            nodes,
            type_symbol: FxHashMap::default(),
            node_id_to_sig: FxHashMap::default(),
            res,
            final_res: FxHashMap::default(),
            global_tys: FxHashMap::default(),
        };
        for (kind, ty_name) in INTRINSIC_TYPES {
            let ty = ty::TyKind::Intrinsic(ty_arena.alloc(ty::IntrinsicTy {
                name: *ty_name,
                kind: *kind,
            }));
            let ty = this.new_ty(ty);
            let prev = this.intrinsic_tys.insert(*ty_name, ty);
            assert!(prev.is_none());
        }
        let boolean_ty = this.get_union_type(this.alloc([this.true_ty(), this.false_ty()]));
        this.type_name.insert(boolean_ty.id, "boolean".to_string());
        this.boolean_ty.set(boolean_ty).unwrap();

        this
    }

    pub fn print_ty(&mut self, ty: &Ty) -> &str {
        self.type_name
            .entry(ty.id)
            .or_insert_with(|| ty.kind.to_string(&self.atoms))
            .as_str()
    }

    fn boolean_ty(&self) -> &'cx Ty<'cx> {
        self.boolean_ty.get().unwrap()
    }

    fn alloc<T>(&self, t: T) -> &'cx T {
        self.arena.alloc(t)
    }

    fn new_ty(&mut self, kind: TyKind<'cx>) -> &'cx Ty<'cx> {
        let id = self.next_ty_id();
        let ty = self.alloc(Ty::new(id, kind));
        let prev = self.tys.insert(id, ty);
        assert!(prev.is_none());
        ty
    }

    fn next_ty_id(&mut self) -> TyID {
        let old = self.next_ty_id;
        self.next_ty_id = self.next_ty_id.next();
        old
    }

    pub fn check_program(&mut self, program: &'cx ast::Program) {
        for stmt in program.stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &'cx ast::Stmt) {
        use ast::StmtKind::*;
        match stmt.kind {
            Var(var) => self.check_var_stmt(var),
            Expr(expr) => {
                self.check_expr(expr);
            }
            Fn(f) => self.check_fn_decl(f),
            If(i) => self.check_if_stmt(i),
            Block(block) => self.check_block(block),
            Return(ret) => self.check_return(ret),
            Empty(_) => {}
        };
    }

    fn check_return(&mut self, ret: &ast::RetStmt<'cx>) {
        if let Some(expr) = ret.expr {
            self.check_expr(expr);
        }
    }

    fn check_block(&mut self, block: ast::Stmts<'cx>) {
        for item in block {
            self.check_stmt(item);
        }
    }

    fn check_if_stmt(&mut self, i: &'cx ast::IfStmt) {
        self.check_expr(i.expr);
        self.check_stmt(i.then);
        if let Some(else_then) = i.else_then {
            self.check_stmt(else_then);
        }
    }

    fn check_fn_decl(&mut self, f: &'cx ast::FnDecl) {
        self.check_block(f.body);
    }

    fn check_var_stmt(&mut self, var: &'cx ast::VarStmt) {
        self.check_var_decl_list(var.list);
    }

    fn check_var_decl_list(&mut self, list: &[&'cx ast::VarDecl]) {
        for decl in list {
            self.check_var_decl(decl);
        }
    }

    fn check_var_decl(&mut self, decl: &'cx ast::VarDecl) {
        self.check_var_like_decl(decl);
    }

    fn get_widened_ty_for_var_like_decl(
        &mut self,
        decl: &'cx ast::VarDecl,
    ) -> Option<&'cx Ty<'cx>> {
        self.get_ty_for_var_like_decl(decl)
    }

    fn get_ty_for_var_like_decl(&mut self, decl: &'cx ast::VarDecl) -> Option<&'cx Ty<'cx>> {
        let decl_ty = self.try_get_type_from_effective_type_node(decl);
        decl_ty
    }

    fn try_get_type_from_effective_type_node(
        &mut self,
        decl: &'cx ast::VarDecl,
    ) -> Option<&'cx Ty<'cx>> {
        self.get_effective_type_annotation_node(decl)
            .map(|ty| self.get_ty_from_type_node(ty))
    }

    fn get_effective_type_annotation_node(
        &self,
        decl: &'cx ast::VarDecl<'cx>,
    ) -> Option<&'cx ast::Ty<'cx>> {
        decl.ty
    }

    fn check_type_assignable_to_and_optionally_elaborate(
        &mut self,
        span: Span,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) {
        self.check_type_related_to_and_optionally_elaborate(
            span,
            source,
            target,
            RelationKind::Assignable,
            |this, span, source, target| {
                Box::new(errors::TypeIsNotAssignableToType {
                    span,
                    ty1: this.print_ty(source).to_string(),
                    ty2: this.print_ty(target).to_string(),
                })
            },
        )
    }

    fn check_type_related_to_and_optionally_elaborate(
        &mut self,
        span: Span,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
        error: impl FnOnce(
            &mut Self,
            Span,
            &'cx Ty<'cx>,
            &'cx Ty<'cx>,
        ) -> crate::Diag,
    ) {
        if !self.is_type_related_to(source, target, relation) {
            let err = error(self, span, source, target);
            self.push_error(span.module, err);
        }
    }

    fn check_type_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        self.is_related_to(source, target)
    }

    fn each_type_related_to_type(
        &mut self,
        source: &'cx Ty<'cx>,
        sources: Tys<'cx>,
        target: &'cx Ty<'cx>,
        targets: Tys<'cx>,
    ) -> bool {
        let mut res = true;
        for (idx, source_ty) in sources.iter().enumerate() {
            // if idx <= targets.len() {
            //     let related = self.is_related_to(source_ty, targets[idx]);
            // }
            let related = self.is_related_to(source_ty, target);
            if !related {
                return false;
            }
        }
        res
    }

    fn union_or_intersection_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
    ) -> bool {
        if let TyKind::Union(s) = source.kind {
            if let TyKind::Union(t) = target.kind {
                self.each_type_related_to_type(source, s.tys, target, t.tys)
            } else {
                false
            }
        } else {
            false
        }
    }

    fn structured_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        if source.kind.is_union_or_intersection() || target.kind.is_union_or_intersection() {
            self.union_or_intersection_related_to(source, target)
        } else {
            todo!()
        }
    }

    fn recur_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        self.structured_related_to(source, target)
    }

    fn is_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        if source.id == target.id {
            return true;
        }

        if source.kind.definitely_non_nullable() && target.kind.is_union() {
            let TyKind::Union(t) = target.kind else {
                unreachable!()
            };
            let candidate = match t.tys.len() {
                2 if t.tys[0].kind.is_nullable() => Some(t.tys[1]),
                3 if t.tys[0].kind.is_nullable() && t.tys[1].kind.is_nullable() => Some(t.tys[1]),
                _ => None,
            };
            if let Some(candidate) = candidate {
                if !candidate.kind.is_nullable() && candidate.id == source.id {
                    return true;
                }
            }
        }

        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            self.recur_related_to(source, target)
        } else {
            false
        }
    }

    fn is_simple_type_related_to(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        if source.kind.is_number_like() && target.kind.is_number() {
            true
        } else {
            false
        }
    }

    fn is_type_related_to(
        &mut self,
        source: &'cx Ty<'cx>,
        target: &'cx Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        if source.id == target.id {
            return true;
        }
        if self.is_simple_type_related_to(source, target) {
            return true;
        }
        if source.kind.is_structured_or_instantiable()
            || target.kind.is_structured_or_instantiable()
        {
            self.check_type_related_to(source, target)
        } else {
            false
        }
    }

    fn check_var_like_decl(&mut self, decl: &'cx ast::VarDecl) {
        let decl_ty = self.get_widened_ty_for_var_like_decl(decl);
        if let Some(init) = decl.init {
            let init_ty = self.check_expr(init);
            if let Some(decl_ty) = decl_ty {
                self.check_type_assignable_to_and_optionally_elaborate(
                    decl.name.span,
                    init_ty,
                    decl_ty,
                );
            }
        }
    }

    fn check_expr_with_contextual_ty(
        &mut self,
        expr: &'cx ast::Expr,
        ctx_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        self.check_expr(expr)
    }

    fn check_expr(&mut self, expr: &'cx ast::Expr) -> &'cx Ty<'cx> {
        use ast::ExprKind::*;
        match expr.kind {
            BinOp(bin) => self.check_bin_expr(bin),
            NumLit(lit) => self.get_number_literal_type(lit.val),
            BoolLit(lit) => {
                if lit.val {
                    self.true_ty()
                } else {
                    self.false_ty()
                }
            }
            NullLit(_) => self.null_ty(),
            Ident(ident) => self.check_ident(ident),
            StringLit(_) => self.string_ty(),
            ArrayLit(lit) => self.check_array_lit(lit),
            Omit(_) => self.undefined_ty(),
            Paren(paren) => self.check_expr(paren.expr),
            Cond(cond) => self.check_cond(cond),
            ObjectLit(lit) => self.check_object_lit(lit),
            Call(call) => self.check_call_expr(call),
        }
    }

    fn check_call_expr(&mut self, call: &'cx ast::CallExpr<'cx>) -> &'cx Ty<'cx> {
        let ty = self.resolve_call_expr(call);
        ty
    }

    fn check_object_lit(&mut self, lit: &'cx ast::ObjectLit) -> &'cx Ty<'cx> {
        self.undefined_ty()
    }

    fn check_cond(&mut self, cond: &'cx ast::CondExpr) -> &'cx Ty<'cx> {
        let ty = self.check_expr(cond.cond);
        let ty1 = self.check_expr(cond.when_true);
        let ty2 = self.check_expr(cond.when_false);
        let tys = self.alloc([ty1, ty2]);
        self.get_union_type(tys)
    }

    fn check_array_lit(&mut self, lit: &'cx ast::ArrayLit) -> &'cx Ty<'cx> {
        if lit.elems.is_empty() {
            self.undefined_ty()
        } else {
            let ty = self.check_expr(lit.elems[0]);
            let ty = TyKind::ArrayLit(self.alloc(ty::ArrayTy { ty }));
            self.new_ty(ty)
        }
    }

    fn check_ident(&mut self, ident: &'cx ast::Ident) -> &'cx Ty<'cx> {
        if ident.name == keyword::IDENT_UNDEFINED {
            self.undefined_ty()
        } else {
            match self.resolve_symbol_by_ident(ident) {
                Symbol::ERR => {
                    let error = errors::CannotFindName {
                        span: ident.span,
                        name: self.atoms.get(ident.name).to_string(),
                    };
                    self.push_error(ident.span.module, Box::new(error));
                    self.error_ty()
                }
                id => self.get_type_of_symbol(id),
            }
        }
    }

    fn create_object_ty(&mut self, ty: ObjectTyKind<'cx>, symbol: SymbolID) -> &'cx Ty<'cx> {
        let kind = TyKind::Object(self.alloc(ty::ObjectTy { kind: ty }));
        self.new_ty(kind)
    }

    fn check_bin_expr(&mut self, node: &'cx ast::BinExpr) -> &'cx Ty<'cx> {
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

    fn check_non_null_type(&mut self, expr: &'cx ast::Expr) -> &'cx Ty<'cx> {
        if matches!(expr.kind, ExprKind::NullLit(_)) {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "null".to_string(),
            };
            self.push_error(expr.span().module, Box::new(error));
            self.null_ty()
        } else if matches!(expr.kind, ExprKind::Ident(ast::Ident { name, .. }) if *name == keyword::IDENT_UNDEFINED)
        {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "undefined".to_string(),
            };
            self.push_error(expr.span().module, Box::new(error));
            self.null_ty()
        } else {
            self.null_ty()
        }
    }

    fn check_bin_like_expr(
        &mut self,
        node: &'cx ast::BinExpr,
        op: BinOp,
        left: &'cx ast::Expr,
        left_ty: &Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &Ty<'cx>,
    ) -> &'cx Ty<'cx> {
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
                if self.is_type_assignable_to_kind(left_ty, |ty| ty.kind.is_number_like(), true)
                    && self.is_type_assignable_to_kind(
                        right_ty,
                        |ty| ty.kind.is_number_like(),
                        true,
                    )
                {
                    self.number_ty()
                } else if self.is_type_assignable_to_kind(
                    left_ty,
                    |ty| ty.kind.is_string_like(),
                    true,
                ) && self.is_type_assignable_to_kind(
                    right_ty,
                    |ty| ty.kind.is_string_like(),
                    true,
                ) {
                    self.string_ty()
                } else {
                    let error = errors::OperatorCannotBeAppliedToTy1AndTy2 {
                        op: op.kind.as_str().to_string(),
                        ty1: left_ty.kind.to_string(&self.atoms),
                        ty2: right_ty.kind.to_string(&self.atoms),
                        span: node.span,
                    };
                    self.push_error(node.span.module, Box::new(error));
                    self.any_ty()
                }
            }
            Sub => todo!(),
            Mul => todo!(),
            Div => todo!(),
            Pipe => self.number_ty(),
        }
    }

    fn push_error(
        &mut self,
        module_id: ModuleID,
        error: crate::Diag,
    ) {
        self.diags.push(rts_errors::Diag {
            module_id,
            inner: error,
        })
    }

    fn is_type_assignable_to_kind(
        &self,
        source: &Ty,
        kind: impl FnOnce(&Ty) -> bool,
        strict: bool,
    ) -> bool {
        if kind(source) {
            return true;
        }
        // if strict && source.flags.contains(TyFlags::AnyOrUnknown | TyFlags::Void | TyFlags::Undefined | TyFlags::Null) {
        //     return false;
        // }

        false
    }
}

struct Relation {}

fn is_type_related_to(source: &Ty, target: &Ty, relation: &Relation) -> bool {
    false
}

fn is_type_assignable_to(source: &Ty, target: &Ty) -> bool {
    false
}

mod check_bin_like;
mod check_call_like;
mod check_class_like_decl;
mod check_fn_like_decl;
mod check_fn_like_expr;
mod check_var_like;
mod create_ty;
mod get_contextual_ty;
mod get_declared_ty;
mod get_symbol;
mod get_ty;
mod relation;
mod resolve;
mod sig;
mod symbol_links;
mod utils;

use rts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;
use sig::Sig;
use symbol_links::SymbolLinks;
use utils::{get_assignment_kind, AssignmentKind};

pub use self::resolve::ExpectedArgsCount;

use crate::ast::{BinOp, ExprKind};
use crate::atoms::{AtomId, AtomMap};
use crate::bind::{ScopeID, Symbol, SymbolID, SymbolName, Symbols};
use crate::parser::{Nodes, ParentMap};
use crate::ty::{has_type_facts, IntrinsicTyKind, Ty, TyID, TyVarID, TypeFacts};
use crate::{ast, errors, keyword, ty};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct F64Represent {
    inner: u64,
}

impl F64Represent {
    fn new(val: f64) -> Self {
        Self {
            inner: unsafe { std::mem::transmute::<f64, u64>(val) },
        }
    }
}

pub struct TyChecker<'cx> {
    pub atoms: &'cx AtomMap<'cx>,
    pub diags: Vec<rts_errors::Diag>,
    arena: &'cx bumpalo::Bump,
    next_ty_id: TyID,
    next_ty_var_id: TyVarID,
    tys: FxHashMap<TyID, &'cx Ty<'cx>>,
    num_lit_tys: FxHashMap<F64Represent, TyID>,
    intrinsic_tys: FxHashMap<AtomId, &'cx Ty<'cx>>,
    type_name: FxHashMap<TyID, String>,
    symbol_links: FxHashMap<SymbolID, SymbolLinks<'cx>>,
    ty_vars: FxHashMap<TyVarID, &'cx Ty<'cx>>,
    // === ast ===
    nodes: &'cx Nodes<'cx>,
    node_parent_map: &'cx ParentMap,
    // === global ===
    global_tys: FxHashMap<TyID, &'cx Ty<'cx>>,
    boolean_ty: std::cell::OnceCell<&'cx Ty<'cx>>,
    // === resolver ===
    scope_id_parent_map: &'cx FxHashMap<ScopeID, Option<ScopeID>>,
    node_id_to_scope_id: &'cx FxHashMap<ast::NodeID, ScopeID>,
    node_id_to_sig: FxHashMap<ast::NodeID, Sig<'cx>>,
    symbols: &'cx Symbols,
    res: &'cx FxHashMap<(ScopeID, SymbolName), SymbolID>,
    final_res: FxHashMap<ast::NodeID, SymbolID>,
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
    (error_ty, keyword::IDENT_ERROR, IntrinsicTyKind::Any),
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

fn get_suggestion_boolean_op(op: &str) -> Option<&str> {
    match op {
        "^" | "^=" => Some("!=="),
        "&" | "&=" => Some("&&"),
        "|" | "|=" => Some("||"),
        _ => None,
    }
}

impl<'cx> TyChecker<'cx> {
    pub fn new(
        ty_arena: &'cx bumpalo::Bump,
        nodes: &'cx Nodes<'cx>,
        node_parent_map: &'cx ParentMap,
        atoms: &'cx AtomMap<'cx>,
        scope_id_parent_map: &'cx FxHashMap<ScopeID, Option<ScopeID>>,
        node_id_to_scope_id: &'cx FxHashMap<ast::NodeID, ScopeID>,
        symbols: &'cx Symbols,
        res: &'cx FxHashMap<(ScopeID, SymbolName), SymbolID>,
        final_res: FxHashMap<ast::NodeID, SymbolID>,
    ) -> Self {
        assert!(ty_arena.allocation_limit().is_none());
        let mut this = Self {
            intrinsic_tys: FxHashMap::default(),
            atoms,
            tys: FxHashMap::default(),
            num_lit_tys: FxHashMap::default(),
            next_ty_id: TyID::root(),
            next_ty_var_id: TyVarID::root(),
            arena: ty_arena,
            diags: Vec::with_capacity(32),
            boolean_ty: Default::default(),
            type_name: FxHashMap::default(),
            scope_id_parent_map,
            node_id_to_scope_id,
            symbols,
            nodes,
            node_parent_map,
            node_id_to_sig: FxHashMap::default(),
            res,
            final_res,
            global_tys: FxHashMap::default(),
            ty_vars: FxHashMap::default(),
            symbol_links: FxHashMap::default(),
        };
        for (kind, ty_name) in INTRINSIC_TYPES {
            let ty = ty::TyKind::Intrinsic(ty_arena.alloc(ty::IntrinsicTy { kind: *kind }));
            let ty = this.new_ty(ty);
            let prev = this.intrinsic_tys.insert(*ty_name, ty);
            assert!(prev.is_none());
        }
        let boolean_ty = this.create_union_type(vec![this.true_ty(), this.false_ty()]);
        this.type_name.insert(boolean_ty.id, "boolean".to_string());
        this.boolean_ty.set(boolean_ty).unwrap();

        this
    }

    fn get_symbol_links(&mut self, symbol: SymbolID) -> &SymbolLinks<'cx> {
        self.symbol_links
            .entry(symbol)
            .or_insert_with(SymbolLinks::new)
    }

    fn get_mut_symbol_links(&mut self, symbol: SymbolID) -> &mut SymbolLinks<'cx> {
        self.symbol_links.get_mut(&symbol).unwrap()
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
            Class(class) => self.check_class_decl(class),
            Interface(_) => {}
        };
    }

    fn check_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.check_class_like_decl(class)
    }

    fn check_class_method_ele(&mut self, method: &'cx ast::ClassMethodEle<'cx>) {
        self.check_fn_like_decl(method);
    }

    fn check_class_prop_ele(&mut self, prop: &'cx ast::ClassPropEle<'cx>) {
        self.check_var_like_decl(prop);
    }

    fn check_return(&mut self, ret: &ast::RetStmt<'cx>) {
        if let Some(expr) = ret.expr {
            self.check_expr(expr);
        }
    }

    fn check_block(&mut self, block: &'cx ast::BlockStmt<'cx>) {
        for item in block.stmts {
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

    fn check_fn_decl(&mut self, f: &'cx ast::FnDecl<'cx>) {
        self.check_fn_like_decl(f);
    }

    fn check_var_stmt(&mut self, var: &'cx ast::VarStmt<'cx>) {
        self.check_var_decl_list(var.list);
    }

    fn check_var_decl_list(&mut self, list: &[&'cx ast::VarDecl<'cx>]) {
        for decl in list {
            self.check_var_decl(decl);
        }
    }

    fn check_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>) {
        self.check_var_like_decl(decl);
    }

    // fn try_get_type_from_effective_type_node(
    //     &mut self,
    //     decl: &'cx ast::VarDecl,
    // ) -> Option<&'cx Ty<'cx>> {
    //     self.get_effective_type_annotation_node(decl)
    //         .map(|ty| self.get_ty_from_type_node(ty))
    // }

    // fn get_effective_type_annotation_node(
    //     &self,
    //     decl: &'cx ast::VarDecl<'cx>,
    // ) -> Option<&'cx ast::Ty<'cx>> {
    //     decl.ty
    // }

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
            Bin(bin) => self.check_bin_expr(bin),
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
            Call(call) => self.check_call_like_expr(call),
            New(call) => self.check_call_like_expr(call),
            Fn(f) => self.check_fn_like_expr(f),
            ArrowFn(f) => self.check_fn_like_expr(f),
            Assign(assign) => self.check_assign_expr(assign),
            PrefixUnary(_) => self.undefined_ty(),
            Class(class) => {
                self.check_class_like_decl(class);
                self.undefined_ty()
            }
            PropAccess(_) => self.undefined_ty(),
        }
    }

    fn check_arithmetic_op_ty(
        &mut self,
        t: &'cx Ty<'cx>,
        push_error: impl FnOnce(&mut Self),
    ) -> &'cx Ty<'cx> {
        if !self.is_type_assignable_to(t, self.number_ty()) {
            push_error(self)
        }
        t
    }

    fn check_bin_expr_for_normal(
        &mut self,
        expr_span: Span,
        left_ty: &'cx Ty<'cx>,
        left_span: Span,
        right_ty: &'cx Ty<'cx>,
        right_span: Span,
        op: &str,
    ) -> &'cx Ty<'cx> {
        assert!(matches!(op, "^" | "^=" | "&" | "&=" | "|" | "|="));
        if left_ty.kind.is_boolean_like() && right_ty.kind.is_boolean_like() {
            if let Some(sugg) = get_suggestion_boolean_op(op) {
                let error = errors::TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
                    span: expr_span,
                    op1: op.to_string(),
                    op2: sugg.to_string(),
                };
                self.push_error(expr_span.module, Box::new(error));
                return self.number_ty();
            }
        }

        let left = self.check_arithmetic_op_ty(left_ty, |this| {
            let error =
                errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                    span: left_span,
                    left_or_right: errors::LeftOrRight::Left,
                };
            this.push_error(left_span.module, Box::new(error));
        });
        let right = self.check_arithmetic_op_ty(right_ty, |this| {
            let error =
                errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                    span: right_span,
                    left_or_right: errors::LeftOrRight::Right,
                };
            this.push_error(left_span.module, Box::new(error));
        });

        self.number_ty()
    }

    fn check_assign_expr(&mut self, assign: &'cx ast::AssignExpr<'cx>) -> &'cx Ty<'cx> {
        let l = self.check_ident(assign.binding);
        let r = self.check_expr(assign.right);
        use ast::AssignOp::*;
        let ty = match assign.op {
            Eq => self.check_binary_like_expr(assign, l, r),
            AddEq => self.check_binary_like_expr_for_add(l, r),
            SubEq => self.undefined_ty(),
            MulEq => self.undefined_ty(),
            DivEq => self.undefined_ty(),
            ModEq => self.undefined_ty(),
            ShlEq => self.undefined_ty(),
            ShrEq => self.undefined_ty(),
            UShrEq => self.undefined_ty(),
            BitOrEq | BitAndEq | BitXorEq => self.check_bin_expr_for_normal(
                assign.span,
                l,
                assign.binding.span,
                r,
                assign.right.span(),
                assign.op.as_str(),
            ),
        };
        // if ty.id == self.any_ty().id {
        //     let error = errors::CannotAssignToNameBecauseItIsATy {
        //         name: self.atoms.get(assign.binding.name).to_string(),
        //         ty: l.kind.to_string(self.atoms),
        //         span: assign.span,
        //     };
        //     self.push_error(assign.span.module, Box::new(error));
        // }
        ty
    }

    fn check_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) -> &'cx Ty<'cx> {
        // let ty = self.get_contextual_ty(lit.id);
        let entires = lit.members.iter().map(|member| {
            // let member_symbol = self.get_symbol_of_decl(member.id);
            let member_ty = self.check_expr(member.value);
            let name = match member.name.kind {
                ast::PropNameKind::Ident(ident) => ident.name,
            };
            (name, member_ty)
        });
        let map = FxHashMap::from_iter(entires);
        let members = self.alloc(map);
        if !self.final_res.contains_key(&lit.id) {
            // TODO: delete this
            unreachable!()
        }
        self.create_object_lit_ty(ty::ObjectLitTy {
            members,
            symbol: self.final_res[&lit.id],
        })
    }

    fn check_cond(&mut self, cond: &'cx ast::CondExpr) -> &'cx Ty<'cx> {
        let ty = self.check_expr(cond.cond);
        let ty1 = self.check_expr(cond.when_true);
        let ty2 = self.check_expr(cond.when_false);
        self.create_union_type(vec![ty1, ty2])
    }

    fn check_array_lit(&mut self, lit: &'cx ast::ArrayLit) -> &'cx Ty<'cx> {
        let mut elems = Vec::with_capacity(lit.elems.len());
        for elem in lit.elems.iter() {
            elems.push(self.check_expr(elem));
        }
        let ty = if elems.is_empty() {
            // FIXME: use type var
            self.create_ty_var()
        } else {
            self.create_union_type(elems)
        };
        self.create_array_ty(ty::ArrayTy { ty })
    }

    fn is_block_scoped_name_declared_before_use(&self, decl: ast::NodeID, used_span: Span) -> bool {
        used_span.lo > self.nodes.get(decl).span().hi
    }

    fn check_resolved_block_scoped_var(&mut self, ident: &'cx ast::Ident, id: SymbolID) {
        use crate::bind::SymbolKind::*;
        match self.symbols.get(id).kind {
            Class { decl } => {
                if !self.is_block_scoped_name_declared_before_use(decl, ident.span) {
                    let decl_span = match self.nodes.get(decl) {
                        ast::Node::ClassDecl(class) => class.name.span,
                        _ => unreachable!(),
                    };
                    let kind = errors::DeclKind::Class;
                    let name = self.atoms.get(ident.name).to_string();
                    let error = errors::CannotUsedBeforeItsDeclaration {
                        span: ident.span,
                        kind,
                        name: name.to_string(),
                        related: [errors::DefinedHere {
                            span: decl_span,
                            kind,
                            name,
                        }],
                    };
                    self.push_error(ident.span.module, Box::new(error));
                }
            }
            _ => unreachable!(),
        }
    }

    fn check_ident(&mut self, ident: &'cx ast::Ident) -> &'cx Ty<'cx> {
        if ident.name == keyword::IDENT_UNDEFINED {
            return self.undefined_ty();
        }

        let symbol = match self.resolve_symbol_by_ident(ident) {
            Symbol::ERR => {
                let error = errors::CannotFindName {
                    span: ident.span,
                    name: self.atoms.get(ident.name).to_string(),
                    errors: vec![],
                };
                let error = self.on_failed_to_resolve_symbol(ident, error);
                self.push_error(ident.span.module, Box::new(error));
                return self.error_ty();
            }
            id => id,
        };

        if self.symbols.get(symbol).kind.is_class() {
            self.check_resolved_block_scoped_var(ident, symbol);
        }

        let ty = self.get_type_of_symbol(symbol);
        let assignment_kind = get_assignment_kind(self, ident.id);
        if assignment_kind != AssignmentKind::None {
            let symbol_kind = &self.symbols.get(symbol).kind;
            if !symbol_kind.is_variable() {
                let error = errors::CannotAssignToNameBecauseItIsATy {
                    span: ident.span,
                    name: self.atoms.get(ident.name).to_string(),
                    ty: symbol_kind.as_str().to_string(),
                };
                self.push_error(ident.span.module, Box::new(error));
                return self.error_ty();
            }
        }

        ty
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

    fn check_binary_like_expr_for_add(
        &mut self,
        left_ty: &'cx Ty<'cx>,
        right_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        if self.is_type_assignable_to_kind(left_ty, |ty| ty.kind.is_number_like(), true)
            && self.is_type_assignable_to_kind(right_ty, |ty| ty.kind.is_number_like(), true)
        {
            self.number_ty()
        } else if self.is_type_assignable_to_kind(left_ty, |ty| ty.kind.is_string_like(), true)
            && self.is_type_assignable_to_kind(right_ty, |ty| ty.kind.is_string_like(), true)
        {
            self.string_ty()
        } else {
            self.any_ty()
        }
    }

    fn check_bin_like_expr(
        &mut self,
        node: &'cx ast::BinExpr,
        op: BinOp,
        left: &'cx ast::Expr,
        left_ty: &'cx Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx Ty<'cx>,
    ) -> &'cx Ty<'cx> {
        use ast::BinOpKind::*;
        match op.kind {
            Add => {
                let ty = self.check_binary_like_expr_for_add(left_ty, right_ty);
                if ty.id == self.any_ty().id {
                    let error = errors::OperatorCannotBeAppliedToTy1AndTy2 {
                        op: "+".to_string(),
                        ty1: left_ty.kind.to_string(&self.atoms),
                        ty2: right_ty.kind.to_string(&self.atoms),
                        span: node.span,
                    };
                    self.push_error(node.span.module, Box::new(error));
                }
                ty
            }
            Sub => todo!(),
            Mul => todo!(),
            Div => todo!(),
            Pipe => {
                let left = self.check_non_null_type(left);
                let right = self.check_non_null_type(right);
                self.number_ty()
            }
            LogicalAnd => {
                if has_type_facts(left_ty, TypeFacts::TRUTHY) {
                    left_ty
                } else {
                    right_ty
                }
            }
            PipePipe => {
                if has_type_facts(left_ty, TypeFacts::FALSE_FACTS) {
                    right_ty
                } else {
                    left_ty
                }
            }
            EqEq => self.boolean_ty(),
            EqEqEq => self.boolean_ty(),
            Less => todo!(),
            LessEq => todo!(),
            Shl => todo!(),
            Great => todo!(),
            GreatEq => todo!(),
            Shr => todo!(),
            UShr => todo!(),
            BitAnd => todo!(),
        }
    }

    fn push_error(&mut self, module_id: ModuleID, error: crate::Diag) {
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

    fn check_type_reference_or_import(&mut self, node: &'cx ast::Ty<'cx>) {
        let ty = self.get_ty_from_type_node(node);
    }

    fn check_type_reference_node(&mut self, node: &'cx ast::Ty<'cx>) {
        self.check_type_reference_or_import(node);
    }
}

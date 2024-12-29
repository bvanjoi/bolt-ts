mod check_bin_like;
mod check_call_like;
mod check_class_decl_like;
mod check_fn_like_decl;
mod check_fn_like_expr;
mod check_fn_like_symbol;
mod check_interface;
mod check_var_like;
mod create_ty;
mod errors;
mod expect;
mod get_base_ty;
mod get_contextual_ty;
mod get_declared_ty;
mod get_effective_node;
mod get_symbol;
mod get_ty;
mod get_type_from_ty_refer_like;
mod get_type_from_var_like;
mod infer;
mod instantiate;
mod node_links;
mod relation;
mod resolve;
mod resolve_structured_member;
mod sig;
mod symbol_links;
mod utils;

use self::node_links::NodeLinks;
pub use self::resolve::ExpectedArgsCount;
pub use self::symbol_links::SymbolLinks;
use self::utils::{find_ancestor, get_assignment_kind, AssignmentKind};
use bolt_ts_span::{ModuleID, Span};
use rustc_hash::FxHashMap;

use crate::ast::{BinOp, NodeID};
use crate::atoms::{AtomId, AtomMap};
use crate::bind::{self, GlobalSymbols, Symbol, SymbolFlags, SymbolID, SymbolName};
use crate::parser::Parser;
use crate::ty::{
    has_type_facts, CheckFlags, Sig, SigFlags, TupleShape, Ty, TyID, TyKind, TyVarID, TypeFacts,
};
use crate::utils::fx_hashmap_with_capacity;
use crate::{ast, ensure_sufficient_stack, keyword, ty};

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct Ternary: u8 {
        const FALSE = 0x0;
        const UNKNOWN = 0x1;
        const MAYBE = 0x3;
        const TRUE = u8::MAX;
    }
}

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

impl From<f64> for F64Represent {
    fn from(val: f64) -> Self {
        F64Represent::new(val)
    }
}

impl From<usize> for F64Represent {
    fn from(val: usize) -> Self {
        F64Represent::new(val as f64)
    }
}

pub struct TyChecker<'cx> {
    pub atoms: &'cx AtomMap<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    arena: &'cx bumpalo::Bump,
    next_ty_id: TyID,
    next_ty_var_id: TyVarID,
    tys: Vec<&'cx Ty<'cx>>,
    num_lit_tys: FxHashMap<F64Represent, TyID>,
    string_lit_tys: FxHashMap<AtomId, TyID>,
    intrinsic_tys: FxHashMap<AtomId, &'cx Ty<'cx>>,
    type_name: FxHashMap<TyID, String>,
    ty_vars: FxHashMap<TyVarID, &'cx Ty<'cx>>,
    symbol_links: FxHashMap<SymbolID, SymbolLinks<'cx>>,
    node_links: FxHashMap<NodeID, NodeLinks<'cx>>,
    pub ty_structured_members: FxHashMap<TyID, &'cx ty::StructuredMembers<'cx>>,
    // === ast ===
    pub p: &'cx Parser<'cx>,
    // === global ===
    global_array_ty: std::cell::OnceCell<&'cx Ty<'cx>>,
    global_number_ty: std::cell::OnceCell<&'cx Ty<'cx>>,
    global_string_ty: std::cell::OnceCell<&'cx Ty<'cx>>,
    boolean_ty: std::cell::OnceCell<&'cx Ty<'cx>>,
    tuple_shapes: FxHashMap<u32, &'cx TupleShape<'cx>>,
    unknown_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
    // === resolver ===
    pub binder: &'cx mut bind::Binder<'cx>,
    global_symbols: &'cx GlobalSymbols,

    node_id_to_sig: FxHashMap<ast::NodeID, &'cx Sig<'cx>>,
    resolution_tys: thin_vec::ThinVec<SymbolID>,
    resolution_res: thin_vec::ThinVec<bool>,
}

macro_rules! intrinsic_type {
    ($(($name: ident, $atom_id: expr, $ty_flags: expr)),* $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(pub fn $name(&self) -> &'cx Ty<'cx> {
                self.intrinsic_tys[&$atom_id]
            })*
        }
        static INTRINSIC_TYPES: &[(TyKind, AtomId)] = &[
            $(($ty_flags, $atom_id),)*
        ];
    };
}

intrinsic_type!(
    (any_ty, keyword::IDENT_ANY, TyKind::Any),
    (error_ty, keyword::IDENT_ERROR, TyKind::Any),
    (void_ty, keyword::IDENT_VOID, TyKind::Void),
    (undefined_ty, keyword::IDENT_UNDEFINED, TyKind::Undefined),
    (null_ty, keyword::KW_NULL, TyKind::Null),
    (true_ty, keyword::KW_TRUE, TyKind::TrueLit),
    (false_ty, keyword::KW_FALSE, TyKind::FalseLit),
    (number_ty, keyword::IDENT_NUMBER, TyKind::Number),
    (string_ty, keyword::IDENT_STRING, TyKind::String),
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
        p: &'cx Parser<'cx>,
        atoms: &'cx AtomMap,
        binder: &'cx mut bind::Binder<'cx>,
        global_symbols: &'cx GlobalSymbols,
    ) -> Self {
        assert_eq!(ty_arena.allocated_bytes(), 0);
        let mut this = Self {
            intrinsic_tys: fx_hashmap_with_capacity(1024),
            atoms,
            p,
            tys: Vec::with_capacity(p.module_count() * 1024),
            num_lit_tys: fx_hashmap_with_capacity(1024 * 8),
            string_lit_tys: fx_hashmap_with_capacity(1024 * 8),
            next_ty_id: TyID::root(),
            next_ty_var_id: TyVarID::root(),
            arena: ty_arena,
            diags: Vec::with_capacity(p.module_count() * 32),
            boolean_ty: Default::default(),
            global_number_ty: Default::default(),
            global_string_ty: Default::default(),
            global_array_ty: Default::default(),
            unknown_sig: Default::default(),
            tuple_shapes: fx_hashmap_with_capacity(1024 * 8),
            type_name: fx_hashmap_with_capacity(1024 * 8),
            node_id_to_sig: fx_hashmap_with_capacity(p.module_count() * 256),
            ty_vars: FxHashMap::default(),
            ty_structured_members: fx_hashmap_with_capacity(p.module_count() * 1024),
            symbol_links: fx_hashmap_with_capacity(p.module_count() * 1024),
            node_links: fx_hashmap_with_capacity(p.module_count() * 1024),
            resolution_tys: thin_vec::ThinVec::with_capacity(128),
            resolution_res: thin_vec::ThinVec::with_capacity(128),
            binder,
            global_symbols,
        };
        for (kind, ty_name) in INTRINSIC_TYPES {
            let ty = this.new_ty(*kind);
            let prev = this.intrinsic_tys.insert(*ty_name, ty);
            assert!(prev.is_none());
        }
        let boolean_ty = this.create_union_type(
            vec![this.true_ty(), this.false_ty()],
            ty::UnionReduction::Lit,
        );
        this.type_name.insert(boolean_ty.id, "boolean".to_string());
        this.boolean_ty.set(boolean_ty).unwrap();

        let global_number_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_NUMBER_CLASS));
        this.global_number_ty.set(global_number_ty).unwrap();

        let global_array_ty = this.get_global_type(SymbolName::Normal(keyword::IDENT_ARRAY_CLASS));
        this.global_array_ty.set(global_array_ty).unwrap();

        let global_string_ty =
            this.get_global_type(SymbolName::Normal(keyword::IDENT_STRING_CLASS));
        this.global_string_ty.set(global_string_ty).unwrap();

        let unknown_sig = this.alloc(Sig {
            flags: SigFlags::empty(),
            ty_params: None,
            params: &[],
            min_args_count: 0,
            ret: None,
            node_id: ast::NodeID::root(ModuleID::MOCK),
            target: None,
            mapper: None,
        });
        this.unknown_sig.set(unknown_sig).unwrap();

        this
    }

    fn check_flags(&self, symbol: SymbolID) -> CheckFlags {
        if let Some(t) = self.binder.get_transient(symbol) {
            t.links.get_check_flags().unwrap()
        } else {
            bitflags::Flags::empty()
        }
    }

    fn get_symbol_links(&mut self, symbol: SymbolID) -> &SymbolLinks<'cx> {
        if let Some(t) = self.binder.get_transient(symbol) {
            &t.links
        } else {
            self.symbol_links.entry(symbol).or_default()
        }
    }

    fn get_mut_symbol_links(&mut self, symbol: SymbolID) -> &mut SymbolLinks<'cx> {
        if let Some(t) = self.binder.get_mut_transient(symbol) {
            &mut t.links
        } else {
            self.symbol_links.get_mut(&symbol).unwrap()
        }
    }

    fn get_node_links(&mut self, node: NodeID) -> &NodeLinks<'cx> {
        self.node_links.entry(node).or_default()
    }

    fn get_mut_node_links(&mut self, node: NodeID) -> &mut NodeLinks<'cx> {
        self.node_links.get_mut(&node).unwrap()
    }

    pub fn print_ty<'a>(&'a mut self, ty: &'cx Ty) -> &'a str {
        let type_name = (!self.type_name.contains_key(&ty.id)).then(|| ty.to_string(self));
        self.type_name
            .entry(ty.id)
            .or_insert_with(|| type_name.unwrap())
    }

    pub(crate) fn boolean_ty(&self) -> &'cx Ty<'cx> {
        self.boolean_ty.get().unwrap()
    }

    #[inline(always)]
    fn global_number_ty(&self) -> &'cx Ty<'cx> {
        self.global_number_ty.get().unwrap()
    }

    #[inline(always)]
    fn global_string_ty(&self) -> &'cx Ty<'cx> {
        self.global_string_ty.get().unwrap()
    }

    #[inline(always)]
    pub fn global_array_ty(&self) -> &'cx Ty<'cx> {
        self.global_array_ty.get().unwrap()
    }

    pub fn unknown_sig(&self) -> &'cx Sig<'cx> {
        self.unknown_sig.get().unwrap()
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
            Return(ret) => self.check_return_stmt(ret),
            Class(class) => self.check_class_decl(class),
            Interface(interface) => self.check_interface_decl(interface),
            Namespace(ns) => self.check_ns_decl(ns),
            Empty(_) => {}
            Type(_) => {}
            Throw(_) => {}
            Enum(enum_decl) => {}
        };
    }

    fn check_ns_decl(&mut self, ns: &'cx ast::NsDecl<'cx>) {
        self.check_block(ns.block);
    }

    fn is_applicable_index_ty(&mut self, source: &'cx Ty<'cx>, target: &'cx Ty<'cx>) -> bool {
        self.is_type_assignable_to(source, target)
    }

    fn get_apparent_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_number_like() {
            self.global_number_ty()
        } else if ty.kind.is_string_like() {
            self.global_string_ty()
        } else {
            ty
        }
    }

    fn get_applicable_index_infos(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop_name_ty: &'cx Ty<'cx>,
    ) -> Vec<&'cx ty::IndexInfo<'cx>> {
        self.index_infos(ty)
            .iter()
            .filter_map(|info| {
                if self.is_applicable_index_ty(prop_name_ty, info.key_ty) {
                    Some(*info)
                } else {
                    None
                }
            })
            .collect()
    }

    fn get_applicable_index_info(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop_name_ty: &'cx Ty<'cx>,
    ) -> Option<&'cx ty::IndexInfo<'cx>> {
        self.index_infos(ty)
            .iter()
            .find(|info| self.is_applicable_index_ty(prop_name_ty, info.key_ty))
            .copied()
    }

    fn check_index_constraint_for_prop(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop: SymbolID,
        prop_name_ty: &'cx Ty<'cx>,
        prop_ty: &'cx ty::Ty<'cx>,
    ) {
        for index_info in self.get_applicable_index_infos(ty, prop_name_ty) {
            if !self.is_type_assignable_to(prop_ty, index_info.val_ty) {
                let prop_decl = prop.decl(self.binder);
                let prop_node = self.p.node(prop_decl);
                let prop_name = match prop_node {
                    ast::Node::ClassPropEle(prop) => prop.name,
                    ast::Node::PropSignature(prop) => prop.name,
                    _ => unreachable!(),
                };
                let prop_name = match prop_name.kind {
                    ast::PropNameKind::Ident(ident) => self.atoms.get(ident.name).to_string(),
                    ast::PropNameKind::NumLit(num) => num.val.to_string(),
                    ast::PropNameKind::StringLit(lit) => self.atoms.get(lit.val).to_string(),
                };
                let error = errors::PropertyAOfTypeBIsNotAssignableToCIndexTypeD {
                    span: prop_node.span(),
                    prop: prop_name,
                    ty_b: prop_ty.to_string(self),
                    ty_c: index_info.key_ty.to_string(self),
                    index_ty_d: index_info.val_ty.to_string(self),
                };
                self.push_error(prop_node.span().module, Box::new(error));
                return;
            }
        }

        if let Some(i) = ty.kind.as_object_interface() {
            let base_tys = self.ty_structured_members[&ty.id].base_tys;
            for base_ty in base_tys {
                self.check_index_constraint_for_prop(base_ty, prop, prop_name_ty, prop_ty);
            }
        } else {
            // unreachable!("{:#?}", ty)
        }
    }

    fn get_lit_ty_from_prop(&mut self, prop: SymbolID) -> &'cx ty::Ty<'cx> {
        let symbol = self.binder.symbol(prop);
        if symbol
            .flags
            .intersects(SymbolFlags::PROPERTY | SymbolFlags::FUNCTION)
        {
            self.string_ty()
        } else {
            self.undefined_ty()
        }
    }

    fn check_index_constraints(&mut self, ty: &'cx ty::Ty<'cx>, symbol: SymbolID) {
        for prop in self.properties_of_object_type(ty) {
            let prop_ty = self.get_type_of_symbol(*prop);
            let prop_name_ty = self.get_lit_ty_from_prop(*prop);
            self.check_index_constraint_for_prop(ty, *prop, prop_name_ty, prop_ty);
        }
    }

    fn check_class_decl(&mut self, class: &'cx ast::ClassDecl<'cx>) {
        self.check_class_decl_like(class)
    }

    fn get_containing_fn_or_class_static_block(&self, node: ast::NodeID) -> Option<ast::NodeID> {
        find_ancestor(self.p, node, |node| {
            if node.is_fn_like_or_class_static_block_decl() {
                Some(true)
            } else {
                None
            }
        })
    }

    fn check_return_stmt(&mut self, ret: &ast::RetStmt<'cx>) {
        let Some(container) = self.get_containing_fn_or_class_static_block(ret.id) else {
            // delay bug
            return;
        };
        let sig = self.get_sig_from_decl(container);
        let expr_ty = ret
            .expr
            .map(|expr| self.check_expr(expr))
            .unwrap_or(self.undefined_ty());
        if matches!(self.p.node(container), ast::Node::ClassCtor(_)) {
            if let Some(expr) = ret.expr {
                let ret = sig
                    .ret
                    .map(|ret| {
                        let symbol = self.binder.final_res(ret);
                        self.get_declared_ty_of_symbol(symbol)
                    })
                    .unwrap_or_else(|| self.undefined_ty());
                self.check_type_assignable_to_and_optionally_elaborate(expr.span(), expr_ty, ret);
            }
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

    fn check_expr_with_cache(&mut self, expr: &'cx ast::Expr) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_node_links(expr.id()).get_resolved_ty() {
            return ty;
        }
        let ty = self.check_expr(expr);
        self.get_mut_node_links(expr.id()).set_resolved_ty(ty);
        ty
    }

    fn get_widened_literal_ty(&self, ty: &'cx Ty<'cx>) -> &'cx Ty<'cx> {
        if ty.kind.is_number_lit() {
            self.number_ty()
        } else if ty.kind.is_string_lit() {
            self.string_ty()
        } else {
            ty
        }
    }

    fn check_expr_for_mutable_location(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx Ty<'cx> {
        let ty = self.check_expr(expr);
        self.get_widened_literal_ty(ty)
    }

    fn check_expr(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx Ty<'cx> {
        use ast::ExprKind::*;
        match expr.kind {
            Bin(bin) => self.check_bin_expr(bin),
            NumLit(lit) => self.get_number_literal_type(lit.val),
            StringLit(lit) => self.get_string_literal_type(lit.val),
            BoolLit(lit) => {
                if lit.val {
                    self.true_ty()
                } else {
                    self.false_ty()
                }
            }
            NullLit(_) => self.null_ty(),
            Ident(ident) => self.check_ident(ident),
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
            PrefixUnary(prefix) => self.check_prefix_unary_expr(prefix),
            Class(class) => {
                self.check_class_decl_like(class);
                self.undefined_ty()
            }
            PropAccess(node) => self.check_prop_access_expr(node),
            EleAccess(_) => self.undefined_ty(),
            This(_) => self.undefined_ty(),
        }
    }

    fn check_prop_access_expr(&mut self, node: &'cx ast::PropAccessExpr<'cx>) -> &'cx Ty<'cx> {
        let left = self.check_expr(node.expr);
        let Some(symbol) = self.get_prop_of_ty(left, SymbolName::Ele(node.name.name)) else {
            return self.undefined_ty();
        };
        self.get_type_of_symbol(symbol)
    }

    fn check_prefix_unary_expr(&mut self, expr: &'cx ast::PrefixUnaryExpr<'cx>) -> &'cx Ty<'cx> {
        let op_ty = self.check_expr(expr.expr);
        use ast::ExprKind::*;
        match expr.expr.kind {
            NumLit(_) => match expr.op {
                ast::PrefixUnaryOp::Plus => op_ty,
                ast::PrefixUnaryOp::Minus => {
                    let val = if let ty::TyKind::NumberLit(n) = op_ty.kind {
                        -n.val
                    } else {
                        todo!()
                    };
                    self.get_number_literal_type(val)
                }
                ast::PrefixUnaryOp::PlusPlus => {
                    let val = if let ty::TyKind::NumberLit(n) = op_ty.kind {
                        n.val + 1.
                    } else {
                        todo!()
                    };
                    self.get_number_literal_type(val)
                }
                ast::PrefixUnaryOp::MinusMinus => {
                    let val = if let ty::TyKind::NumberLit(n) = op_ty.kind {
                        n.val - 1.
                    } else {
                        todo!()
                    };
                    self.get_number_literal_type(val)
                }
            },
            _ => self.undefined_ty(),
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
        let l = self.check_expr(assign.left);
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
                assign.left.span(),
                r,
                assign.right.span(),
                assign.op.as_str(),
            ),
        };
        // if ty.id == self.any_ty().id {
        //     let error = errors::CannotAssignToNameBecauseItIsATy {
        //         name: self.atoms.get(assign.binding.name).to_string(),
        //         ty: l.kind.to_string(self.binder,self.atoms),
        //         span: assign.span,
        //     };
        //     self.push_error(assign.span.module, Box::new(error));
        // }
        ty
    }

    fn check_object_lit(&mut self, lit: &'cx ast::ObjectLit<'cx>) -> &'cx Ty<'cx> {
        // let ty = self.get_contextual_ty(lit.id);
        let entires = lit.members.iter().map(|member| {
            let member_symbol = self.get_symbol_of_decl(member.id);
            // let member_ty = self.check_expr(member.value);
            match member.name.kind {
                ast::PropNameKind::Ident(ident) => (SymbolName::Ele(ident.name), member_symbol),
                ast::PropNameKind::NumLit(num) => (
                    SymbolName::EleNum(F64Represent::new(num.val)),
                    member_symbol,
                ),
                ast::PropNameKind::StringLit(lit) => (SymbolName::Ele(lit.val), member_symbol),
            }
        });
        let map = FxHashMap::from_iter(entires);
        let members = self.alloc(map);
        let declared_props = self.get_props_from_members(members);
        self.create_object_lit_ty(ty::ObjectLitTy {
            members,
            declared_props,
            symbol: self.binder.final_res(lit.id),
        })
    }

    fn check_cond(&mut self, cond: &'cx ast::CondExpr) -> &'cx Ty<'cx> {
        let ty = self.check_expr(cond.cond);
        let ty1 = self.check_expr(cond.when_true);
        let ty2 = self.check_expr(cond.when_false);
        self.create_union_type(vec![ty1, ty2], ty::UnionReduction::Subtype)
    }

    fn check_array_lit(&mut self, lit: &'cx ast::ArrayLit) -> &'cx Ty<'cx> {
        let mut elems = Vec::with_capacity(lit.elems.len());
        for elem in lit.elems.iter() {
            elems.push(self.check_expr_for_mutable_location(elem));
        }
        let ty = if elems.is_empty() {
            // FIXME: use type var
            self.create_ty_var()
        } else {
            self.create_union_type(elems, ty::UnionReduction::Subtype)
        };
        let refer = self.global_array_ty().kind.expect_object_reference();
        let ty = self.create_reference_ty(ty::ReferenceTy {
            target: refer.target,
            resolved_ty_args: self.alloc(vec![ty]),
        });
        ty
    }

    fn is_used_in_fn_or_instance_prop(&self, used: &'cx ast::Ident, decl: ast::NodeID) -> bool {
        assert!(used.id.module() == decl.module());
        find_ancestor(self.p, used.id, |current| {
            if current.id() == decl {
                return Some(false);
            } else if current.is_fn_like() {
                return Some(true);
            } else if current.is_class_static_block_decl() {
                return Some(self.p.node(decl).span().lo < used.span.lo);
            }
            None
        })
        .is_some()
    }

    fn is_block_scoped_name_declared_before_use(
        &self,
        decl: ast::NodeID,
        used: &'cx ast::Ident,
    ) -> bool {
        let used_span = used.span;
        let decl_span = self.p.node(decl).span();
        let decl_pos = decl_span.lo;
        if decl_pos < used_span.lo {
            return true;
        }

        if self.is_used_in_fn_or_instance_prop(used, decl) {
            return true;
        }

        false
    }

    fn check_resolved_block_scoped_var(&mut self, ident: &'cx ast::Ident, id: SymbolID) {
        let symbol = self.binder.symbol(id).expect_class();
        let decl = symbol.decl;
        if !self.is_block_scoped_name_declared_before_use(decl, ident) {
            let decl_span = match self.p.node(decl) {
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

    fn check_ident(&mut self, ident: &'cx ast::Ident) -> &'cx Ty<'cx> {
        if ident.name == keyword::IDENT_UNDEFINED {
            return self.undefined_ty();
        }

        let symbol = self.resolve_symbol_by_ident(ident);

        if self.binder.symbol(symbol).flags == SymbolFlags::CLASS {
            self.check_resolved_block_scoped_var(ident, symbol);
        }

        let ty = self.get_type_of_symbol(symbol);
        let assignment_kind = get_assignment_kind(self, ident.id);
        if assignment_kind != AssignmentKind::None && symbol != Symbol::ERR {
            let symbol = self.binder.symbol(symbol);
            if !symbol.is_variable() {
                let error = errors::CannotAssignToNameBecauseItIsATy {
                    span: ident.span,
                    name: self.atoms.get(ident.name).to_string(),
                    ty: symbol.as_str().to_string(),
                };
                self.push_error(ident.span.module, Box::new(error));
                return self.error_ty();
            }
        }

        ty
    }

    fn check_bin_expr(&mut self, node: &'cx ast::BinExpr) -> &'cx Ty<'cx> {
        let l = self.check_expr(node.left);
        let r = ensure_sufficient_stack(|| self.check_expr(node.right));
        self.check_bin_like_expr(node, node.op, node.left, l, node.right, r)
    }

    fn check_non_null_type(&mut self, expr: &'cx ast::Expr) -> &'cx Ty<'cx> {
        if matches!(expr.kind, ast::ExprKind::NullLit(_)) {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "null".to_string(),
            };
            self.push_error(expr.span().module, Box::new(error));
            self.null_ty()
        } else if matches!(expr.kind, ast::ExprKind::Ident(ast::Ident { name, .. }) if *name == keyword::IDENT_UNDEFINED)
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
                        ty1: left_ty.to_string(self),
                        ty2: right_ty.to_string(self),
                        span: node.span,
                    };
                    self.push_error(node.span.module, Box::new(error));
                }
                ty
            }
            Sub => todo!(),
            Mul => self.undefined_ty(),
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
        self.diags.push(bolt_ts_errors::Diag {
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

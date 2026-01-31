use std::cell::OnceCell;

use super::FlowLoopTypesArenaId;
use super::TyChecker;
use super::create_ty::IntersectionFlags;
use super::symbol_info::SymbolInfo;
use super::ty::{self, ObjectFlags, TypeFacts, TypeFlags};
use super::type_predicate::TyPred;
use super::type_predicate::TyPredKind;

use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_atom::Atom;
use bolt_ts_binder::AssignmentKind;
use bolt_ts_binder::{FlowFlags, FlowID, FlowInNode, FlowNode, FlowNodeKind};
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID};

#[derive(Debug, Clone, Copy)]
pub enum FlowTy<'cx> {
    Ty(&'cx ty::Ty<'cx>),
    Incomplete {
        flags: TypeFlags,
        ty: &'cx ty::Ty<'cx>,
    },
}

impl FlowTy<'_> {
    pub fn is_incomplete(&self) -> bool {
        match self {
            FlowTy::Incomplete { .. } => true,
            FlowTy::Ty(t) => t.flags == TypeFlags::empty(),
        }
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_flow_node_of_node(&self, node: ast::NodeID) -> Option<FlowID> {
        let module = node.module().as_usize();
        debug_assert!(module < self.flow_in_nodes.len());
        unsafe {
            self.flow_nodes
                .get_unchecked(module)
                .get_flow_node_of_node(node)
        }
    }

    pub(super) fn flow_node(&self, id: FlowID) -> &FlowNode<'cx> {
        let module = id.module().as_usize();
        debug_assert!(module < self.flow_in_nodes.len());
        unsafe { self.flow_nodes.get_unchecked(module).get_flow_node(id) }
    }

    pub(super) fn get_flow_in_node_of_node(&self, node: ast::NodeID) -> FlowInNode {
        let module = node.module().as_usize();
        debug_assert!(module < self.flow_in_nodes.len());
        unsafe { self.flow_in_nodes.get_unchecked(module).get(node) }
    }

    pub(super) fn get_flow_ty_of_reference(
        &mut self,
        refer: ast::NodeID,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: Option<&'cx ty::Ty<'cx>>,
        flow_container: Option<ast::NodeID>,
        flow_node: Option<FlowID>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(flow_node) = flow_node.or_else(|| self.get_flow_node_of_node(refer)) else {
            return declared_ty;
        };
        let mut key = OnceCell::new();

        let init_ty = init_ty.unwrap_or(declared_ty);

        let shared_flow_start = self.shared_flow_info.len();

        let evolved_ty = {
            let flow_ty = self.get_ty_at_flow_node(
                flow_node,
                refer,
                shared_flow_start,
                declared_ty,
                init_ty,
                flow_container,
                &mut key,
            );
            self.get_ty_from_flow_ty(flow_ty)
        };

        self.shared_flow_info.truncate(shared_flow_start);

        let result_ty = if !evolved_ty
            .get_object_flags()
            .contains(ObjectFlags::EVOLVING_ARRAY)
        {
            evolved_ty
        } else {
            // TODO: if is_evolving_array_op_target
            todo!()
        };

        evolved_ty
    }

    fn get_ty_at_flow_node(
        &mut self,
        mut flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
        flow_container: Option<ast::NodeID>,
        key: &mut OnceCell<Option<FlowCacheKey>>,
    ) -> FlowTy<'cx> {
        let mut shared_flow = None;
        loop {
            let n = self.flow_node(flow);
            let flags = n.flags;
            if flags.intersects(FlowFlags::SHARED) {
                for i in shared_flow_start..self.shared_flow_info.len() {
                    let (n, ty) = self.shared_flow_info[i];
                    if flow == n {
                        return ty;
                    }
                }
                shared_flow = Some(flow);
            }
            let ty;
            if flags.contains(FlowFlags::ASSIGNMENT) {
                let Some(t) = self.get_ty_at_flow_assign(
                    flow,
                    refer,
                    shared_flow_start,
                    declared_ty,
                    init_ty,
                ) else {
                    flow = match &self.flow_node(flow).kind {
                        FlowNodeKind::Switch(n) => n.antecedent,
                        FlowNodeKind::Assign(n) => n.antecedent,
                        _ => unreachable!(),
                    };
                    continue;
                };
                ty = t;
            } else if flags.contains(FlowFlags::CALL) {
                if let Some(t) = self.get_ty_at_flow_call(
                    flow,
                    refer,
                    shared_flow_start,
                    declared_ty,
                    init_ty,
                    flow_container,
                    key,
                ) {
                    ty = t;
                } else {
                    let FlowNodeKind::Call(n) = &self.flow_node(flow).kind else {
                        unreachable!()
                    };
                    flow = n.antecedent;
                    continue;
                }
            } else if flags.intersects(FlowFlags::CONDITION) {
                ty = self.get_ty_at_flow_cond(
                    flow,
                    refer,
                    shared_flow_start,
                    declared_ty,
                    init_ty,
                    flow_container,
                    key,
                );
            } else if flags.contains(FlowFlags::START) {
                let FlowNodeKind::Start(start) = &n.kind else {
                    unreachable!()
                };
                if let Some(start) = start.node {
                    // TODO:
                }
                ty = FlowTy::Ty(init_ty);
            } else if flags.intersects(FlowFlags::LABEL) {
                let FlowNodeKind::Label(label) = &n.kind else {
                    unreachable!()
                };
                let antecedent = label.antecedent.as_ref().unwrap();
                if antecedent.len() == 1 {
                    flow = antecedent[0];
                    continue;
                }
                ty = if flags.contains(FlowFlags::BRANCH_LABEL) {
                    self.get_ty_at_flow_branch_label(
                        flow,
                        refer,
                        shared_flow_start,
                        declared_ty,
                        init_ty,
                        flow_container,
                        key,
                    )
                } else {
                    self.get_ty_at_flow_loop_label(
                        flow,
                        refer,
                        shared_flow_start,
                        declared_ty,
                        init_ty,
                        flow_container,
                        key,
                    )
                };
            } else {
                ty = FlowTy::Ty(self.convert_auto_to_any(declared_ty));
            }

            if let Some(shared_flow) = shared_flow {
                self.shared_flow_info.push((shared_flow, ty));
            }
            return ty;
        }
    }

    fn get_ty_at_flow_branch_label(
        &mut self,
        flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
        flow_container: Option<ast::NodeID>,
        key: &mut OnceCell<Option<FlowCacheKey>>,
    ) -> FlowTy<'cx> {
        let FlowNodeKind::Label(n) = &self.flow_node(flow).kind else {
            unreachable!()
        };
        let antecedents = n.antecedent.clone().unwrap();
        let antecedent_tys_id = self.flow_loop_types_arena.alloc(Vec::new());
        let mut subtype_reduction = false;
        let mut seen_incomplete = false;
        for antecedent in antecedents {
            if self
                .flow_node(antecedent)
                .flags
                .contains(FlowFlags::SWITCH_CLAUSE)
            {
                todo!()
            }

            let flow_ty = self.get_ty_at_flow_node(
                antecedent,
                refer,
                shared_flow_start,
                declared_ty,
                init_ty,
                flow_container,
                key,
            );
            let ty = self.get_ty_from_flow_ty(flow_ty);
            if ty == declared_ty && declared_ty == init_ty {
                return FlowTy::Ty(declared_ty);
            }
            let antecedent_tys = self.flow_loop_types_arena.get_mut(antecedent_tys_id);
            if !antecedent_tys.contains(&ty) {
                antecedent_tys.push(ty);
            }
            if !self.is_ty_sub_type_of(ty, init_ty) {
                subtype_reduction = true;
            }
            if flow_ty.is_incomplete() {
                seen_incomplete = true;
            }
        }
        let antecedent_tys = self.flow_loop_types_arena.get(antecedent_tys_id);
        let ty = self.get_union_or_evolving_array_ty(
            &antecedent_tys.clone(),
            if subtype_reduction {
                ty::UnionReduction::Subtype
            } else {
                ty::UnionReduction::Lit
            },
            declared_ty,
        );
        self.create_flow_ty(ty, seen_incomplete)
    }

    fn get_ty_at_flow_loop_label(
        &mut self,
        flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
        flow_container: Option<ast::NodeID>,
        key: &mut OnceCell<Option<FlowCacheKey>>,
    ) -> FlowTy<'cx> {
        let FlowNodeKind::Label(n) = &self.flow_node(flow).kind else {
            unreachable!()
        };
        let flow_cache_key = key
            .get_or_init(|| self.get_flow_cache_key(refer, declared_ty, init_ty, flow_container));
        let Some(flow_cache_key) = flow_cache_key else {
            return FlowTy::Ty(declared_ty);
        };
        if let Some(cached) = self
            .flow_loop_caches
            .get(&flow)
            .and_then(|caches| caches.get(flow_cache_key))
        {
            return FlowTy::Ty(cached);
        };

        for i in self.flow_loop_start..flow_loop_ctx_len(self) {
            let i = i as usize;
            if self.flow_loop_nodes[i] == flow
                && !self.flow_loop_types.is_empty()
                && self.flow_loop_keys[i].eq(flow_cache_key)
            {
                let id = self.flow_loop_types[i];
                let tys = self.flow_loop_types_arena.get(id);
                let ty = self.get_union_or_evolving_array_ty(
                    &tys.clone(),
                    ty::UnionReduction::Lit,
                    declared_ty,
                );
                return self.create_flow_ty(ty, true);
            }
        }

        let antecedents = n.antecedent.clone().unwrap();
        let antecedent_tys_id = self.flow_loop_types_arena.alloc(Vec::new());
        let mut subtype_reduction = false;
        let mut first_antecedent_ty = None;
        for antecedent in antecedents {
            let flow_ty;
            if first_antecedent_ty.is_none() {
                let ty = self.get_ty_at_flow_node(
                    antecedent,
                    refer,
                    shared_flow_start,
                    declared_ty,
                    init_ty,
                    flow_container,
                    key,
                );
                first_antecedent_ty = Some(ty);
                flow_ty = ty;
            } else {
                push_flow_loop_ctx(
                    self,
                    flow,
                    key.get().unwrap().clone().unwrap(),
                    antecedent_tys_id,
                );
                let save_flow_ty_cache = self.flow_ty_cache.take();
                flow_ty = self.get_ty_at_flow_node(
                    antecedent,
                    refer,
                    shared_flow_start,
                    declared_ty,
                    init_ty,
                    flow_container,
                    key,
                );
                self.flow_ty_cache = save_flow_ty_cache;
                pop_flow_loop_ctx(self);
                if let Some(cached) = self.flow_loop_caches.get(&flow).and_then(|caches| {
                    let key = key.get().unwrap().as_ref().unwrap();
                    caches.get(key)
                }) {
                    return FlowTy::Ty(cached);
                }
            }
            let ty = self.get_ty_from_flow_ty(flow_ty);
            let antecedent_tys = self.flow_loop_types_arena.get_mut(antecedent_tys_id);
            if !antecedent_tys.contains(&ty) {
                antecedent_tys.push(ty);
            }
            if !self.is_ty_sub_type_of(ty, init_ty) {
                subtype_reduction = true;
            }
            if ty == declared_ty {
                break;
            }
        }
        let antecedent_tys = self.flow_loop_types_arena.get(antecedent_tys_id);
        let result = self.get_union_or_evolving_array_ty(
            &antecedent_tys.clone(),
            if subtype_reduction {
                ty::UnionReduction::Subtype
            } else {
                ty::UnionReduction::Lit
            },
            declared_ty,
        );

        let Some(first_antecedent_ty) = first_antecedent_ty else {
            unreachable!()
        };
        if first_antecedent_ty.is_incomplete() {
            self.create_flow_ty(result, true)
        } else {
            use std::collections::hash_map::Entry;
            let flow_cache_key = key.get().unwrap().clone().unwrap();
            match self.flow_loop_caches.entry(flow) {
                Entry::Occupied(occ) => {
                    occ.into_mut().insert(flow_cache_key, result);
                }
                Entry::Vacant(vac) => {
                    let mut map = rustc_hash::FxHashMap::default();
                    map.insert(flow_cache_key, result);
                    vac.insert(map);
                }
            }
            FlowTy::Ty(result)
        }
    }

    fn is_evolving_array_op_target(&self, tys: &[&'cx ty::Ty<'cx>]) -> bool {
        let mut has_evolving_array_ty = false;
        for t in tys {
            if !t.flags.contains(TypeFlags::NEVER) {
                if !t.get_object_flags().contains(ObjectFlags::EVOLVING_ARRAY) {
                    return false;
                }
                has_evolving_array_ty = true;
            }
        }
        has_evolving_array_ty
    }

    fn get_union_or_evolving_array_ty(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        subtype_reduction: ty::UnionReduction,
        declared_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if self.is_evolving_array_op_target(tys) {
            todo!()
        }

        let result = {
            // TODO: finalize_evolving_array_ty;
            let ty = self.get_union_ty(tys, subtype_reduction, false, None, None);
            self.recombine_unknown_ty(ty)
        };
        if result != declared_ty
            && let Some(result_tys) = result.kind.as_union()
            && let Some(declared_tys) = declared_ty.kind.as_union()
            && self.array_is_equal(Some(result_tys.tys), Some(declared_tys.tys))
        {
            declared_ty
        } else {
            result
        }
    }

    fn get_init_or_assigned_ty(&mut self, flow: FlowID, refer: ast::NodeID) -> &'cx ty::Ty<'cx> {
        let FlowNodeKind::Assign(n) = &self.flow_node(flow).kind else {
            unreachable!()
        };
        let ty = match self.p.node(n.node) {
            // TODO: ObjectBindingElement, ArrayBinding
            ast::Node::VarDecl(n) => self.get_init_ty_of_var_decl(n),
            _ => self.get_assigned_ty(n.node),
        };
        self.get_narrow_ty_for_reference(ty, refer, None)
    }

    fn get_assigned_ty(&mut self, n: ast::NodeID) -> &'cx ty::Ty<'cx> {
        let parent = self.parent(n).unwrap();
        let parent_node = self.p.node(parent);
        match parent_node {
            ast::Node::ForInStmt(_) => self.string_ty,
            ast::Node::ForOfStmt(_) => {
                todo!()
            }
            ast::Node::AssignExpr(n) => self.get_assigned_ty_of_assign_expr(n),
            _ => self.error_ty,
        }
    }

    fn get_assigned_ty_of_assign_expr(&mut self, n: &'cx ast::AssignExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let parent = self.parent(n.id).unwrap();
        let parent_node = self.p.node(parent);
        let is_destructuring_default_assignment = match parent_node {
            ast::Node::ArrayLit(_) => todo!(),
            ast::Node::PropAccessExpr(_) => todo!(),
            _ => false,
        };
        if is_destructuring_default_assignment {
            todo!()
        } else {
            self.get_ty_of_expr(n.right)
        }
    }

    fn get_ty_at_flow_assign(
        &mut self,
        flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
    ) -> Option<FlowTy<'cx>> {
        let node = match &self.flow_node(flow).kind {
            FlowNodeKind::Switch(n) => n.node.id,
            FlowNodeKind::Assign(n) => n.node,
            _ => unreachable!(),
        };
        if self.is_matching_reference(refer, node) {
            if !self.is_reachable_flow_node(flow) {
                // TODO: unreachable_never_ty
            }
            let nq = self.node_query(node.module());
            if nq.get_assignment_target_kind(node) == AssignmentKind::Compound {
                // TODO:
            }
            {}
            if declared_ty == self.auto_ty || declared_ty == self.auto_array_ty() {
                // TODO:
            }
            let t = if nq.is_in_compound_like_assignment(node) {
                self.get_base_ty_of_literal_ty(declared_ty)
            } else {
                declared_ty
            };
            let t = if t.kind.is_union() {
                let init_ty = self.get_init_or_assigned_ty(flow, refer);
                self.get_assign_reduced_ty(t, init_ty)
            } else {
                t
            };
            return Some(FlowTy::Ty(t));
        }
        None
    }

    fn get_ty_at_flow_call(
        &mut self,
        flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
        flow_container: Option<ast::NodeID>,
        key: &mut OnceCell<Option<FlowCacheKey>>,
    ) -> Option<FlowTy<'cx>> {
        let FlowNodeKind::Call(n) = &self.flow_node(flow).kind else {
            unreachable!()
        };
        let n_antecedent = n.antecedent;
        let n_node = n.node;
        let sig = self.get_effects_sig(n_node.id)?;
        let pred = self.get_ty_predicate_of_sig(sig);
        if let Some(pred) = pred
            && matches!(
                pred.kind,
                TyPredKind::AssertsThis(_) | TyPredKind::AssertsIdent(_)
            )
        {
            let flow_ty = self.get_ty_at_flow_node(
                n_antecedent,
                refer,
                shared_flow_start,
                declared_ty,
                init_ty,
                flow_container,
                key,
            );
            let ty = self.finalize_evolving_array_ty(self.get_ty_from_flow_ty(flow_ty));
            let narrowed_ty = if pred.ty().is_some() {
                self.narrow_ty_by_ty_pred(ty, refer, pred, n_node, true)
            } else if let TyPredKind::AssertsIdent(i) = pred.kind {
                if (i.param_index as usize) < n_node.args.len() {
                    let expr = n_node.args[i.param_index as usize];
                    self.narrow_ty_by_assertion(ty, refer, expr)
                } else {
                    ty
                }
            } else {
                ty
            };

            return Some(if narrowed_ty == ty {
                flow_ty
            } else {
                let incomplete = flow_ty.is_incomplete();
                self.create_flow_ty(narrowed_ty, incomplete)
            });
        }

        if self.get_ret_ty_of_sig(sig).flags.contains(TypeFlags::NEVER) {
            Some(FlowTy::Ty(self.unreachable_never_ty))
        } else {
            None
        }
    }

    fn get_ty_at_flow_cond(
        &mut self,
        flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
        flow_container: Option<ast::NodeID>,
        key: &mut OnceCell<Option<FlowCacheKey>>,
    ) -> FlowTy<'cx> {
        let n = self.flow_node(flow);
        let FlowNodeKind::Cond(cond) = &n.kind else {
            unreachable!()
        };
        let cond_antecedent = cond.antecedent;
        let cond_node = cond.node;
        let assume_true = n.flags.intersects(FlowFlags::TRUE_CONDITION);
        let flow_ty = self.get_ty_at_flow_node(
            cond_antecedent,
            refer,
            shared_flow_start,
            declared_ty,
            init_ty,
            flow_container,
            key,
        );
        let ty = self.get_ty_from_flow_ty(flow_ty);
        if ty.flags.intersects(TypeFlags::NEVER) {
            return flow_ty;
        };
        let non_evolving_ty = self.finalize_evolving_array_ty(ty);
        let narrowed_ty = self.narrow_ty(non_evolving_ty, refer, cond_node, assume_true);
        if narrowed_ty == non_evolving_ty {
            flow_ty
        } else {
            let in_complete = flow_ty.is_incomplete();
            self.create_flow_ty(narrowed_ty, in_complete)
        }
    }

    fn get_ty_from_flow_ty(&self, flow_ty: FlowTy<'cx>) -> &'cx ty::Ty<'cx> {
        match flow_ty {
            FlowTy::Ty(ty) => ty,
            FlowTy::Incomplete { ty, .. } => ty,
        }
    }

    fn finalize_evolving_array_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty
            .get_object_flags()
            .intersects(ObjectFlags::EVOLVING_ARRAY)
        {
            todo!()
        } else {
            ty
        }
    }

    fn is_constant_variable(&self, symbol: SymbolID) -> bool {
        let s = self.symbol(symbol);
        s.flags.contains(SymbolFlags::VARIABLE)
            && s.value_decl.is_some_and(|d| {
                self.node_query(d.module())
                    .get_combined_node_flags(d)
                    .contains(ast::NodeFlags::CONSTANT)
            })
    }

    fn narrow_ty_by_truthiness(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        expr: &'cx ast::Expr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        if self.is_matching_reference(refer, expr.id()) {
            return self.get_adjusted_ty_with_facts(
                ty,
                if assume_true {
                    TypeFacts::TRUTHY
                } else {
                    TypeFacts::FALSE_FACTS
                },
            );
        }
        // TODO: other case
        ty
    }

    fn narrow_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        expr: &'cx ast::Expr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        use bolt_ts_ast::ExprKind::*;
        if let Ident(node) = expr.kind
            && !self.is_matching_reference(refer, node.id)
            && let symbol = self.final_res(expr.id())
            && self.is_constant_variable(symbol)
        {
            //TODO: inline_level < 5
            let value_decl = self.symbol(symbol).value_decl.unwrap();
        }

        match expr.kind {
            Ident(_) => self.narrow_ty_by_truthiness(ty, refer, expr, assume_true),
            Call(node) => self.narrow_ty_by_call_expr(ty, refer, expr, node, assume_true),
            PrefixUnary(node) if node.op == ast::PrefixUnaryOp::Excl => {
                self.narrow_ty(ty, refer, node.expr, !assume_true)
            }
            Bin(node) => self.narrow_ty_by_bin_expr(ty, refer, node, assume_true),
            _ => ty,
        }
    }

    fn narrow_ty_by_equality(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        op: ast::BinOpKind,
        value: &'cx ast::Expr<'cx>,
        mut assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        if ty.flags.contains(TypeFlags::ANY) {
            return ty;
        }

        if matches!(op, ast::BinOpKind::NEqEq | ast::BinOpKind::NEq) {
            assume_true = !assume_true;
        }
        let value_ty = self.get_ty_of_expr(value);
        let double_equal = matches!(op, ast::BinOpKind::EqEq | ast::BinOpKind::NEqEq);
        if value_ty.flags.contains(TypeFlags::NULLABLE) {
            if !self.config.strict_null_checks() {
                return ty;
            }
        }

        if assume_true {
            // TODO
        }

        if value_ty.is_unit() {
            self.filter_type(ty, |this, t| {
                !(this.is_unit_like_ty(t) && this.are_types_comparable(t, value_ty))
            })
        } else {
            ty
        }
    }

    fn narrow_ty_by_bin_expr(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        expr: &'cx ast::BinExpr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        use ast::BinOpKind::*;
        match expr.op.kind {
            Instanceof => self.narrow_ty_by_instanceof_expr(ty, refer, expr, assume_true),
            EqEqEq | NEqEq | EqEq | NEq => {
                let left = self.get_reference_candidate(expr.left);
                let right = self.get_reference_candidate(expr.right);
                if let ast::ExprKind::Typeof(n) = left.kind
                    && right.is_string_lit_like()
                {
                    return self.narrow_ty_by_typeof(
                        ty,
                        refer,
                        n,
                        expr.op.kind,
                        right,
                        assume_true,
                    );
                } else if let ast::ExprKind::Typeof(n) = right.kind
                    && left.is_string_lit_like()
                {
                    return self.narrow_ty_by_typeof(ty, refer, n, expr.op.kind, left, assume_true);
                } else if self.is_matching_reference(refer, left.id()) {
                    return self.narrow_ty_by_equality(ty, expr.op.kind, right, assume_true);
                } else if self.is_matching_reference(refer, right.id()) {
                    return self.narrow_ty_by_equality(ty, expr.op.kind, left, assume_true);
                }

                ty
            }
            _ => ty,
        }
    }

    fn narrow_ty_by_typeof(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        typeof_expr: &'cx ast::TypeofExpr<'cx>,
        op: ast::BinOpKind,
        lit: &'cx ast::Expr<'cx>,
        mut assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        use ast::BinOpKind::*;
        debug_assert!(lit.is_string_lit_like());
        debug_assert!(matches!(op, EqEqEq | NEqEq | EqEq | NEq));
        if matches!(op, NEqEq | NEq) {
            assume_true = !assume_true;
        }
        let target = self.get_reference_candidate(typeof_expr.expr);
        if !self.is_matching_reference(refer, target.id()) {
            // TODO: optional chain
            // TODO: prop access
            ty
        } else {
            self.narrow_ty_by_lit(ty, lit, assume_true)
        }
    }

    fn narrow_ty_by_ty_facts(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        implied_ty: &'cx ty::Ty<'cx>,
        facts: TypeFacts,
    ) -> &'cx ty::Ty<'cx> {
        self.map_ty(
            ty,
            |this, t| {
                if this.is_type_related_to(
                    t,
                    implied_ty,
                    super::relation::RelationKind::StrictSubtype,
                ) {
                    if this.has_type_facts(t, facts) {
                        Some(t)
                    } else {
                        Some(this.never_ty)
                    }
                } else if this.is_ty_sub_type_of(implied_ty, t) {
                    Some(implied_ty)
                } else if this.has_type_facts(t, facts) {
                    Some(this.get_intersection_ty(
                        &[t, implied_ty],
                        super::IntersectionFlags::None,
                        None,
                        None,
                    ))
                } else {
                    Some(this.never_ty)
                }
            },
            false,
        )
        .unwrap()
    }

    fn narrow_ty_by_ty_name(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        lit: &'cx ast::Expr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        match lit.kind {
            ast::ExprKind::StringLit(s) => match s.val {
                keyword::IDENT_STRING => {
                    self.narrow_ty_by_ty_facts(ty, self.string_ty, TypeFacts::TYPEOF_EQ_STRING)
                }
                keyword::IDENT_NUMBER => {
                    self.narrow_ty_by_ty_facts(ty, self.number_ty, TypeFacts::TYPEOF_EQ_NUMBER)
                }
                keyword::IDENT_BIGINT => {
                    self.narrow_ty_by_ty_facts(ty, self.bigint_ty, TypeFacts::TYPEOF_EQ_BIGINT)
                }
                keyword::IDENT_BOOLEAN => {
                    self.narrow_ty_by_ty_facts(ty, self.boolean_ty(), TypeFacts::TYPEOF_EQ_BOOLEAN)
                }
                keyword::IDENT_SYMBOL => {
                    self.narrow_ty_by_ty_facts(ty, self.es_symbol_ty, TypeFacts::TYPEOF_EQ_SYMBOL)
                }
                keyword::IDENT_OBJECT => {
                    if ty.flags.contains(TypeFlags::ANY) {
                        ty
                    } else {
                        let a = self.narrow_ty_by_ty_facts(
                            ty,
                            self.non_primitive_ty,
                            TypeFacts::TYPEOF_EQ_OBJECT,
                        );
                        let b = self.narrow_ty_by_ty_facts(ty, self.null_ty, TypeFacts::EQ_NULL);
                        self.get_union_ty(&[a, b], ty::UnionReduction::Lit, false, None, None)
                    }
                }
                keyword::KW_FUNCTION => {
                    if ty.flags.contains(TypeFlags::ANY) {
                        ty
                    } else {
                        self.narrow_ty_by_ty_facts(
                            ty,
                            self.global_fn_ty(),
                            TypeFacts::TYPEOF_EQ_FUNCTION,
                        )
                    }
                }
                keyword::KW_UNDEFINED => self.narrow_ty_by_ty_facts(
                    ty,
                    self.undefined_ty,
                    TypeFacts::TYPEOF_EQ_HOST_OBJECT,
                ),
                _ => self.narrow_ty_by_ty_facts(
                    ty,
                    self.non_primitive_ty,
                    TypeFacts::TYPEOF_EQ_HOST_OBJECT,
                ),
            },
            _ => unreachable!(),
        }
    }

    fn narrow_ty_by_lit(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        lit: &'cx ast::Expr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        if assume_true {
            self.narrow_ty_by_ty_name(ty, lit)
        } else {
            let facts = match lit.kind {
                ast::ExprKind::StringLit(s) => match s.val {
                    keyword::IDENT_STRING => TypeFacts::TYPEOF_NE_STRING,
                    keyword::IDENT_NUMBER => TypeFacts::TYPEOF_NE_NUMBER,
                    keyword::IDENT_BIGINT => TypeFacts::TYPEOF_NE_BIGINT,
                    keyword::IDENT_BOOLEAN => TypeFacts::TYPEOF_NE_BOOLEAN,
                    keyword::IDENT_SYMBOL => TypeFacts::TYPEOF_NE_SYMBOL,
                    keyword::KW_UNDEFINED => TypeFacts::NE_UNDEFINED,
                    keyword::IDENT_OBJECT => TypeFacts::TYPEOF_NE_OBJECT,
                    keyword::KW_FUNCTION => TypeFacts::TYPEOF_NE_FUNCTION,
                    _ => TypeFacts::TYPEOF_NE_HOST_OBJECT,
                },
                _ => unreachable!(),
            };
            self.get_adjusted_ty_with_facts(ty, facts)
        }
    }

    fn get_reference_candidate(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx ast::Expr<'cx> {
        let n = ast::Expr::skip_parens(expr);
        match n.kind {
            ast::ExprKind::Assign(node) => {
                match node.op {
                    ast::AssignOp::Eq
                    | ast::AssignOp::LogicalOrEq
                    | ast::AssignOp::LogicalAndEq => {
                        // TODO: `??=`
                        self.get_reference_candidate(node.left)
                    }
                    _ => n,
                }
            }
            _ => n,
        }
    }

    fn narrow_ty_by_instanceof_expr(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        expr: &'cx ast::BinExpr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        let left = self.get_reference_candidate(expr.left);
        if !self.is_matching_reference(refer, left.id()) {
            // if assume_true && self.config.strict_null_checks() && self.contain

            return ty;
        }
        let right = expr.right;
        let right_ty = self.get_ty_of_expr(right);
        let global_object_ty = self.global_object_ty();
        if !self.is_ty_derived_from(right_ty, global_object_ty) {
            return ty;
        }
        // TODO: get_effects_sigs
        let global_fn_ty = self.global_fn_ty();
        if !self.is_ty_derived_from(right_ty, global_fn_ty) {
            return ty;
        }
        let instance_ty = self
            .map_ty(
                right_ty,
                |this, t| {
                    // get_instance_ty
                    if let Some(prototype_property_ty) = this.get_ty_of_prop_of_ty(
                        t,
                        bolt_ts_binder::SymbolName::Atom(keyword::IDENT_PROTOTYPE),
                    ) && !this.is_type_any(prototype_property_ty)
                    {
                        return Some(prototype_property_ty);
                    }

                    let ctor_sigs = this.get_signatures_of_type(t, ty::SigKind::Constructor);
                    Some(if ctor_sigs.is_empty() {
                        this.empty_object_ty()
                    } else {
                        let tys = ctor_sigs
                            .iter()
                            .map(|sig| {
                                let s = this.get_erased_sig(sig);
                                this.get_ret_ty_of_sig(s)
                            })
                            .collect::<Vec<_>>();
                        let tys = this.alloc(tys);
                        this.get_union_ty(tys, ty::UnionReduction::Lit, false, None, None)
                    })
                },
                false,
            )
            .unwrap();
        // TODO: don't narrow any
        self.get_narrowed_ty(ty, instance_ty, assume_true, true)
    }

    fn narrow_ty_by_assertion(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let n = ast::Expr::skip_parens(expr);
        match n.kind {
            ast::ExprKind::BoolLit(n) if !n.val => self.unreachable_never_ty,
            ast::ExprKind::Bin(n) => {
                if n.op.kind == ast::BinOpKind::LogicalAnd {
                    let left_ty = self.narrow_ty_by_assertion(ty, refer, n.left);
                    self.narrow_ty_by_assertion(left_ty, refer, n.right)
                } else if n.op.kind == ast::BinOpKind::LogicalOr {
                    let left_ty = self.narrow_ty_by_assertion(ty, refer, n.left);
                    let right_ty = self.narrow_ty_by_assertion(ty, refer, n.right);
                    self.get_union_ty(
                        &[left_ty, right_ty],
                        ty::UnionReduction::Lit,
                        false,
                        None,
                        None,
                    )
                } else {
                    self.narrow_ty(ty, refer, n.left, true)
                }
            }
            _ => self.narrow_ty(ty, refer, expr, true),
        }
    }

    fn narrow_ty_by_call_expr(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        expr: &'cx ast::Expr<'cx>,
        call_expr: &'cx ast::CallExpr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        if self.has_matching_arg(expr, refer) {
            let sig = if assume_true
                || !self
                    .p
                    .node_flags(call_expr.id)
                    .intersects(ast::NodeFlags::OPTIONAL_CHAIN)
            {
                self.get_effects_sig(call_expr.id)
            } else {
                None
            };
            if let Some(pred) = sig.and_then(|sig| self.get_ty_predicate_of_sig(sig))
                && matches!(pred.kind, TyPredKind::Ident(_) | TyPredKind::This(_))
            {
                return self.narrow_ty_by_ty_pred(ty, refer, pred, call_expr, assume_true);
            }
        }
        // TODO: contains_missing_ty
        ty
    }

    fn get_ty_pred_arg(
        &mut self,
        pred: &'cx TyPred<'cx>,
        expr: &'cx ast::CallExpr<'cx>,
    ) -> Option<&'cx ast::Expr<'cx>> {
        match pred.kind {
            TyPredKind::Ident(i) => return Some(expr.args[i.param_index as usize]),
            TyPredKind::AssertsIdent(i) => return Some(expr.args[i.param_index as usize]),
            _ => (),
        }
        let invoked_expr = bolt_ts_ast::Expr::skip_parens(expr.expr);
        use bolt_ts_ast::ExprKind::*;
        if let PropAccess(n) = invoked_expr.kind {
            Some(bolt_ts_ast::Expr::skip_parens(n.expr))
        } else if let EleAccess(n) = invoked_expr.kind {
            Some(bolt_ts_ast::Expr::skip_parens(n.expr))
        } else {
            None
        }
    }

    fn narrow_ty_by_ty_pred(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        pred: &'cx TyPred<'cx>,
        expr: &'cx ast::CallExpr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        if let Some(pred_ty) = pred.ty() {
            if ty.flags.intersects(TypeFlags::ANY)
                && (pred_ty == self.global_object_ty() || pred_ty == self.global_fn_ty())
            {
                return ty;
            }
            if let Some(pred_arg) = self.get_ty_pred_arg(pred, expr)
                && self.is_matching_reference(refer, pred_arg.id())
            {
                return self.get_narrowed_ty(ty, pred_ty, assume_true, false);
            }
        }

        ty
    }

    fn get_narrowed_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        candidate: &'cx ty::Ty<'cx>,
        assume_true: bool,
        check_derived: bool,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: cache
        if !assume_true {
            if ty == candidate {
                return self.never_ty;
            } else if check_derived {
                return self.filter_type(ty, |this, t| !this.is_ty_derived_from(t, candidate));
            }
            let ty = if ty.flags.intersects(TypeFlags::UNKNOWN) {
                todo!()
            } else {
                ty
            };
            let true_ty = self.get_narrowed_ty(ty, candidate, true, false);
            // TODO: self.recombine_unknown_ty()
            return self.filter_type(ty, |this, t| !this.is_ty_sub_type_of(t, true_ty));
        } else if ty.flags.intersects(TypeFlags::ANY_OR_UNKNOWN) || ty == candidate {
            return candidate;
        };

        let is_related = if check_derived {
            Self::is_ty_derived_from
        } else {
            Self::is_ty_sub_type_of
        };

        let key_prop_name = ty.kind.as_union().and_then(|u| self.get_key_prop_name(u));
        let narrowed_ty = self
            .map_ty(
                candidate,
                |this, c| {
                    let discriminant =
                        key_prop_name.and_then(|name| this.get_ty_of_prop_of_ty(c, name));
                    let matching = discriminant.and_then(|key_ty| {
                        let union_ty = ty.kind.expect_union();
                        this.get_constituent_ty_for_key_ty(ty, union_ty, key_ty)
                    });
                    let directly_related = this
                        .map_ty(
                            matching.unwrap_or(ty),
                            |this, t| {
                                Some(if check_derived {
                                    if this.is_ty_derived_from(t, c) {
                                        t
                                    } else if this.is_ty_derived_from(c, t) {
                                        c
                                    } else {
                                        this.never_ty
                                    }
                                } else if this.is_ty_strict_sub_type_of(t, c) {
                                    t
                                } else if this.is_ty_strict_sub_type_of(c, t) {
                                    c
                                } else if this.is_ty_sub_type_of(t, c) {
                                    t
                                } else if this.is_ty_sub_type_of(c, t) {
                                    c
                                } else {
                                    this.never_ty
                                })
                            },
                            false,
                        )
                        .unwrap();
                    if directly_related.flags.intersects(TypeFlags::NEVER) {
                        this.map_ty(
                            ty,
                            |this, t| {
                                if t.maybe_type_of_kind(TypeFlags::INSTANTIABLE) && {
                                    let t = this
                                        .get_base_constraint_of_ty(t)
                                        .unwrap_or(this.unknown_ty);
                                    is_related(this, c, t)
                                } {
                                    Some(this.get_intersection_ty(
                                        &[t, c],
                                        IntersectionFlags::None,
                                        None,
                                        None,
                                    ))
                                } else {
                                    Some(this.never_ty)
                                }
                            },
                            false,
                        )
                    } else {
                        Some(directly_related)
                    }
                },
                false,
            )
            .unwrap();
        if !narrowed_ty.flags.intersects(TypeFlags::NEVER) {
            narrowed_ty
        } else if self.is_ty_sub_type_of(candidate, ty) {
            candidate
        } else if self.is_type_assignable_to(ty, candidate) {
            ty
        } else if self.is_type_assignable_to(candidate, ty) {
            candidate
        } else {
            self.get_intersection_ty(&[ty, candidate], IntersectionFlags::None, None, None)
        }
    }

    fn get_narrow_ty_for_reference(
        &mut self,
        mut ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        check_mode: Option<super::CheckMode>,
    ) -> &'cx ty::Ty<'cx> {
        if ty.is_no_infer_ty() {
            let sub = ty.kind.expect_substitution_ty();
            ty = sub.base_ty;
        }

        ty
        // if check_mode.is_some_and(|check_mode| check_mode.intersects(super::CheckMode::INFERENTIAL))
        //     && false
        // // TODO:
        // {
        //     ty
        // } else {
        //     ty
        // }
    }

    pub(super) fn is_reachable_flow_node(&mut self, id: FlowID) -> bool {
        let result = self._is_reachable_flow_node(id, false);
        self.last_flow_node = Some(id);
        self.last_flow_reachable = result;
        result
    }

    fn _is_reachable_flow_node(&mut self, mut flow: FlowID, mut no_cache_check: bool) -> bool {
        loop {
            if self.last_flow_node.is_some_and(|l| l == flow) {
                return self.last_flow_reachable;
            }
            let f = self.flow_node(flow);
            let flags = f.flags;
            if flags.contains(FlowFlags::SHARED) {
                if !no_cache_check {
                    if let Some(reachable) = self.flow_node_reachable.get(&flow).copied() {
                        return reachable;
                    } else {
                        let reachable = self._is_reachable_flow_node(flow, true);
                        self.flow_node_reachable.insert(flow, reachable);
                        return reachable;
                    }
                }
                no_cache_check = true;
            }

            match &f.kind {
                FlowNodeKind::Assign(n) => flow = n.antecedent,
                FlowNodeKind::Cond(n) => flow = n.antecedent,
                // TODO: handle other kinds
                _ => return !flags.contains(FlowFlags::UNREACHABLE),
            }
        }
    }

    fn get_flow_cache_key(
        &self,
        node: ast::NodeID,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
        flow_container: Option<ast::NodeID>,
    ) -> Option<FlowCacheKey> {
        use ast::Node::*;
        let nq = self.node_query(node.module());
        let n = self.p.node(node);
        match n {
            Ident(_) if !nq.is_this_in_type_query(node) => {
                let symbol = self.final_res(node);
                if symbol != Symbol::ERR {
                    let key = IdentFlowCacheKey {
                        flow_container,
                        declared_ty: declared_ty.id,
                        init_ty: init_ty.id,
                        symbol,
                    };
                    Some(FlowCacheKey::Ident(key))
                } else {
                    None
                }
            }
            Ident(_) | ThisExpr(_) => {
                let key = ThisFlowCacheKey {
                    flow_container,
                    declared_ty: declared_ty.id,
                    init_ty: init_ty.id,
                };
                Some(FlowCacheKey::This(key))
            }
            NonNullExpr(n) => {
                self.get_flow_cache_key(n.expr.id(), declared_ty, init_ty, flow_container)
            }
            ParenExpr(n) => {
                self.get_flow_cache_key(n.expr.id(), declared_ty, init_ty, flow_container)
            }
            QualifiedName(n) => {
                let left =
                    self.get_flow_cache_key(n.left.id(), declared_ty, init_ty, flow_container)?;
                Some(FlowCacheKey::QualifiedName(QualifiedNameFlowCacheKey {
                    left: Box::new(left),
                    right: n.right.name,
                }))
            }
            PropAccessExpr(n) => {
                let prop_name = n.name.name;
                let left =
                    self.get_flow_cache_key(n.expr.id(), declared_ty, init_ty, flow_container)?;
                Some(FlowCacheKey::PropAccess(PropAccessFlowCacheKey {
                    left: Box::new(left),
                    right: prop_name,
                }))
            }
            EleAccessExpr(n) => {
                // TODO: try_get_element_access_name
                None
            }
            ObjectPat(_)
            | ArrayPat(_)
            | FnDecl(_)
            | FnExpr(_)
            | ArrowFnExpr(_)
            | ObjectMethodMember(_)
            | ClassMethodElem(_) => {
                let key = PseudoFlowCacheKey {
                    node,
                    declared_ty: declared_ty.id,
                };
                Some(FlowCacheKey::Pseudo(key))
            }
            _ => None,
        }
    }
}

fn push_flow_loop_ctx<'cx>(
    checker: &mut TyChecker<'cx>,
    flow: FlowID,
    key: FlowCacheKey,
    tys_id: FlowLoopTypesArenaId<'cx>,
) {
    debug_assert!(checker.flow_loop_nodes.len() == checker.flow_loop_keys.len());
    debug_assert!(checker.flow_loop_keys.len() == checker.flow_loop_types.len());
    checker.flow_loop_nodes.push(flow);
    checker.flow_loop_keys.push(key);
    checker.flow_loop_types.push(tys_id);
}

fn pop_flow_loop_ctx<'cx>(checker: &mut TyChecker<'cx>) {
    debug_assert!(checker.flow_loop_nodes.len() == checker.flow_loop_keys.len());
    debug_assert!(checker.flow_loop_keys.len() == checker.flow_loop_types.len());
    checker.flow_loop_types.pop();
    checker.flow_loop_keys.pop();
    checker.flow_loop_nodes.pop();
}

pub(super) fn flow_loop_ctx_len(checker: &TyChecker) -> u32 {
    debug_assert!(checker.flow_loop_nodes.len() == checker.flow_loop_keys.len());
    debug_assert!(checker.flow_loop_keys.len() == checker.flow_loop_types.len());
    checker.flow_loop_nodes.len() as u32
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(super) enum FlowCacheKey {
    Ident(IdentFlowCacheKey),
    This(ThisFlowCacheKey),
    QualifiedName(QualifiedNameFlowCacheKey),
    PropAccess(PropAccessFlowCacheKey),
    Pseudo(PseudoFlowCacheKey),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(super) struct PseudoFlowCacheKey {
    node: ast::NodeID,
    declared_ty: ty::TyID,
}
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(super) struct PropAccessFlowCacheKey {
    left: Box<FlowCacheKey>,
    right: Atom,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(super) struct QualifiedNameFlowCacheKey {
    left: Box<FlowCacheKey>,
    right: Atom,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(super) struct IdentFlowCacheKey {
    flow_container: Option<ast::NodeID>,
    declared_ty: ty::TyID,
    init_ty: ty::TyID,
    symbol: SymbolID,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(super) struct ThisFlowCacheKey {
    flow_container: Option<ast::NodeID>,
    declared_ty: ty::TyID,
    init_ty: ty::TyID,
}

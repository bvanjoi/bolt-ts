use super::TyChecker;
use super::type_predicate::TyPred;
use crate::bind::{FlowFlags, FlowID, FlowNode, FlowNodeKind};
use crate::check::create_ty::IntersectionFlags;
use crate::check::type_predicate::TyPredKind;
use crate::ty::{self, ObjectFlags, TypeFlags};
use bolt_ts_ast as ast;

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
    fn get_flow_node_of_node(&self, node: ast::NodeID) -> Option<FlowID> {
        self.flow_nodes[node.module().as_usize()].get_flow_node_of_node(node)
    }

    fn flow_node(&self, id: FlowID) -> &FlowNode<'cx> {
        self.flow_nodes[id.module().as_usize()].get_flow_node(id)
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

        let init_ty = init_ty.unwrap_or(declared_ty);

        let shared_flow_start = self.shared_flow_info.len();

        let evolved_ty = {
            let flow_ty =
                self.get_ty_at_flow_node(flow_node, refer, shared_flow_start, declared_ty, init_ty);
            self.get_ty_from_flow_ty(flow_ty)
        };

        self.shared_flow_info.truncate(shared_flow_start);

        let result_ty = evolved_ty.get_object_flags();

        evolved_ty
    }

    fn get_ty_at_flow_node(
        &mut self,
        mut flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
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
            if flags.intersects(FlowFlags::CONDITION) {
                ty = self.get_ty_at_flow_cond(flow, refer, shared_flow_start, declared_ty, init_ty);
            } else if flags.intersects(FlowFlags::ASSIGNMENT) {
                let Some(t) = self.get_ty_at_flow_assign(
                    flow,
                    refer,
                    shared_flow_start,
                    declared_ty,
                    init_ty,
                ) else {
                    let FlowNodeKind::Assign(n) = self.flow_node(flow).kind else {
                        unreachable!()
                    };
                    flow = n.antecedent;
                    continue;
                };
                ty = t;
            } else if flags.intersects(FlowFlags::START) {
                let FlowNodeKind::Start(start) = &n.kind else {
                    unreachable!()
                };
                if let Some(start) = start.node {
                    // TODO:
                }
                ty = FlowTy::Ty(init_ty);
            } else {
                ty = FlowTy::Ty(self.convert_auto_to_any(declared_ty));
            }

            if let Some(shared_flow) = shared_flow {
                self.shared_flow_info.push((shared_flow, ty));
            }
            return ty;
        }
    }

    fn get_init_or_assign_ty(&mut self, flow: FlowID, refer: ast::NodeID) -> &'cx ty::Ty<'cx> {
        let FlowNodeKind::Assign(n) = self.flow_node(flow).kind else {
            unreachable!()
        };
        let ty = match self.p.node(n.node) {
            ast::Node::VarDecl(decl) => self.get_init_ty_of_var_decl(decl),
            _ => unreachable!(),
        };
        self.get_narrow_ty_for_reference(ty, refer, None)
    }

    fn get_ty_at_flow_assign(
        &mut self,
        flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
    ) -> Option<FlowTy<'cx>> {
        let FlowNodeKind::Assign(n) = self.flow_node(flow).kind else {
            unreachable!()
        };
        if self.is_matching_reference(refer, n.node) {
            let t = declared_ty;
            let t = if t.kind.is_union() {
                let init_ty = self.get_init_or_assign_ty(flow, refer);
                self.get_assign_reduced_ty(t, init_ty)
            } else {
                t
            };
            return Some(FlowTy::Ty(t));
        }
        None
    }

    fn get_ty_at_flow_cond(
        &mut self,
        flow: FlowID,
        refer: ast::NodeID,
        shared_flow_start: usize,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: &'cx ty::Ty<'cx>,
    ) -> FlowTy<'cx> {
        let n = self.flow_node(flow);
        let FlowNodeKind::Cond(cond) = &n.kind else {
            unreachable!()
        };
        let cond = *cond;
        let assume_true = n.flags.intersects(FlowFlags::TRUE_CONDITION);
        let flow_ty = self.get_ty_at_flow_node(
            cond.antecedent,
            refer,
            shared_flow_start,
            declared_ty,
            init_ty,
        );
        let ty = self.get_ty_from_flow_ty(flow_ty);
        if ty.flags.intersects(TypeFlags::NEVER) {
            return flow_ty;
        };
        let non_evolving_ty = self.finalize_evolving_array_ty(ty);
        let narrowed_ty = self.narrow_ty(non_evolving_ty, refer, cond.node, assume_true);
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

    fn narrow_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        refer: ast::NodeID,
        expr: &'cx ast::Expr<'cx>,
        assume_true: bool,
    ) -> &'cx ty::Ty<'cx> {
        use bolt_ts_ast::ExprKind::*;
        match expr.kind {
            Call(node) => self.narrow_ty_by_call_expr(ty, refer, expr, node, assume_true),
            _ => ty,
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
            let pred = sig.and_then(|sig| self.get_ty_predicate_of_sig(sig));
            if let Some(pred) = pred {
                if matches!(pred.kind, TyPredKind::Ident(_) | TyPredKind::This(_)) {
                    return self.narrow_ty_by_ty_pred(ty, refer, pred, call_expr, assume_true);
                }
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
        if let TyPredKind::Ident(i) = pred.kind {
            return Some(expr.args[i.param_index as usize]);
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
        let pred_ty = match pred.kind {
            TyPredKind::Ident(n) => Some(n.ty),
            TyPredKind::This(n) => Some(n.ty),
            TyPredKind::AssertsThis(n) => n.ty,
            TyPredKind::AssertsIdent(n) => n.ty,
        };
        if let Some(pred_ty) = pred_ty {
            if ty.flags.intersects(TypeFlags::ANY)
                && (pred_ty == self.global_object_ty() || pred_ty == self.global_fn_ty())
            {
                return ty;
            }
            if let Some(pred_arg) = self.get_ty_pred_arg(pred, expr) {
                if self.is_matching_reference(refer, pred_arg.id()) {
                    return self.get_narrowed_ty(ty, pred_ty, assume_true, false);
                }
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
}

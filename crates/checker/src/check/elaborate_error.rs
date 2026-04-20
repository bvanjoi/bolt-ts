use super::Ternary;
use super::TyChecker;
use super::check_type_related_to::NOOP_HEADING_ERROR;
use super::errors;
use super::relation::RelationKind;
use super::ty;
use super::ty::TypeFlags;

use bolt_ts_ast as ast;
use bolt_ts_binder::SymbolName;

struct Elaboration<'cx> {
    error_node: ast::NodeID,
    inner_expr: Option<&'cx ast::Expr<'cx>>,
    name_ty: &'cx ty::Ty<'cx>,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn elaborate_error(
        &mut self,
        node: Option<ast::NodeID>,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
        error_node: Option<ast::NodeID>,
    ) -> bool {
        let Some(node) = node else {
            return false;
        };

        use bolt_ts_ast::Node::*;
        let node = self.p.node(node);
        match node {
            ArrayLit(node) => self.elaborate_array_lit(node, source, target, relation),
            ObjectLit(node) => self.elaborate_object_lit(node, source, target, relation),
            ArrowFnExpr(node) => {
                self.elaborate_arrow_fn_expr(node, source, target, relation, error_node)
            }
            _ => false,
        }
    }

    fn elaborate_arrow_fn_expr(
        &mut self,
        node: &'cx ast::ArrowFnExpr<'cx>,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
        error_output_container: Option<ast::NodeID>,
    ) -> bool {
        let ast::ArrowFnExprBody::Expr(return_expr) = node.body else {
            return false;
        };
        if node.params.iter().any(|p| p.ty.is_some()) {
            return false;
        }
        let Some(source_sig) = self.get_single_call_sig(source) else {
            return false;
        };
        let target_sigs = self.get_signatures_of_type(target, ty::SigKind::Call);
        if target_sigs.is_empty() {
            return false;
        }
        let source_return = self.get_ret_ty_of_sig(source_sig);
        let tys = target_sigs
            .iter()
            .map(|sig| self.get_ret_ty_of_sig(*sig))
            .collect::<Vec<_>>();
        let target_return =
            self.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None);
        if !self.check_type_related_to(
            source_return,
            target_return,
            relation,
            None,
            NOOP_HEADING_ERROR,
        ) {
            if self.elaborate_error(
                Some(return_expr.id()),
                source_return,
                target_return,
                relation,
                error_output_container,
            ) {
                return true;
            }
            self.check_type_related_to(
                source_return,
                target_return,
                relation,
                error_output_container,
                Some(|this: &mut Self| {
                    let span = return_expr.span();
                    let source_return = this.print_ty(source_return, None).to_string();
                    let target_return = this.print_ty(target_return, None).to_string();
                    let error = Box::new(errors::TypeIsNotAssignableToType {
                        span,
                        ty1: source_return,
                        ty2: target_return,
                    });
                    this.push_error(error);
                }),
            );
            return true;
        }
        false
    }

    fn elaborate_object_lit(
        &mut self,
        node: &'cx ast::ObjectLit<'cx>,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        if target
            .flags
            .intersects(TypeFlags::PRIMITIVE.union(TypeFlags::NEVER))
        {
            return false;
        }

        let nodes = node
            .members
            .iter()
            .filter_map(|member| {
                if matches!(member.kind, ast::ObjectMemberKind::SpreadAssignment(_)) {
                    return None;
                }
                let s = self.get_symbol_of_decl(member.id());
                let ty = self.get_literal_ty_from_prop(
                    s,
                    TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE,
                    false,
                );
                use bolt_ts_ast::ObjectMemberKind::*;
                match member.kind {
                    Shorthand(n) => Some(Elaboration {
                        error_node: n.name.id,
                        inner_expr: None,
                        name_ty: ty,
                    }),
                    PropAssignment(n) => Some(Elaboration {
                        error_node: n.name.id(),
                        inner_expr: Some(n.init),
                        name_ty: ty,
                    }),
                    Method(n) => Some(Elaboration {
                        error_node: n.name.id(),
                        inner_expr: None,
                        name_ty: ty,
                    }),
                    SpreadAssignment(_) => unreachable!(),
                    Getter(_) => todo!(),
                    Setter(_) => todo!(),
                }
            })
            .collect::<Vec<_>>();
        self.elaborate_element_wise(&nodes, source, target, relation)
    }

    fn generate_limited_tuple_elements(
        &mut self,
        node: &'cx ast::ArrayLit<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> Vec<Elaboration<'cx>> {
        node.elems
            .iter()
            .enumerate()
            .flat_map(|(i, ele)| {
                if self.is_tuple_like(target)
                    && self
                        .get_prop_of_ty::<false>(target, SymbolName::EleNum((i as f64).into()))
                        .is_none()
                {
                    None
                } else {
                    let name_ty = self.get_number_literal_type_from_number(i as f64);
                    let check_node = self.get_effective_check_node(ele.id());
                    Some(Elaboration {
                        error_node: check_node,
                        inner_expr: Some(*ele),
                        name_ty,
                    })
                }
            })
            .collect()
    }

    fn elaborate_array_lit(
        &mut self,
        node: &'cx ast::ArrayLit<'cx>,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        if target
            .flags
            .intersects(TypeFlags::PRIMITIVE.union(TypeFlags::NEVER))
        {
            return false;
        }
        if self.is_tuple_like(source) {
            let nodes = self.generate_limited_tuple_elements(node, target);
            return self.elaborate_element_wise(&nodes, source, target, relation);
        }
        self.push_type_context(node.id, Some(target), false);
        let tupleized_ty = self.check_array_lit(node, true);
        self.pop_type_context();
        if self.is_tuple_like(tupleized_ty) {
            let nodes = self.generate_limited_tuple_elements(node, target);
            self.elaborate_element_wise(&nodes, tupleized_ty, target, relation)
        } else {
            false
        }
    }

    fn get_best_match_indexed_access_ty_or_undefined(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        name_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(idx) =
            self.get_indexed_access_ty_or_undefined(target, name_ty, None, None, None, None)
        {
            return Some(idx);
        }

        if let Some(target_union) = target.kind.as_union()
            && let Some(best) = self.get_best_matching_ty(source, target_union, |this, s, t| {
                if this.is_type_related_to(s, t, RelationKind::Assignable) {
                    Ternary::TRUE
                } else {
                    Ternary::FALSE
                }
            })
        {
            return self.get_indexed_access_ty_or_undefined(best, name_ty, None, None, None, None);
        }

        None
    }

    fn get_best_matching_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target_union: &'cx ty::UnionTy<'cx>,
        cmp: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> Ternary,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // TODO: findMatchingDiscriminantType
        // TODO: findMatchingTypeReferenceOrTypeAliasReference
        self.find_best_ty_for_object_literal(source, target_union)
            .or_else(|| self.find_best_ty_for_invokable(source, target_union))
            .or_else(|| {
                // find_most_overlappy_ty
                let mut best_match = None;
                if !source
                    .flags
                    .intersects(TypeFlags::PRIMITIVE.union(TypeFlags::INSTANTIABLE_PRIMITIVE))
                {
                    let mut matching_count = 0;
                    for target in target_union.tys {
                        if !target.flags.intersects(
                            TypeFlags::PRIMITIVE.union(TypeFlags::INSTANTIABLE_PRIMITIVE),
                        ) {
                            let tys = &[
                                self.get_index_ty(source, ty::IndexFlags::empty()),
                                self.get_index_ty(target, ty::IndexFlags::empty()),
                            ];
                            let overlap = self.get_intersection_ty(
                                tys,
                                super::IntersectionFlags::None,
                                None,
                                None,
                            );
                            if overlap.flags.contains(TypeFlags::INDEX) {
                                return Some(*target);
                            } else if overlap.is_unit() && 1 >= matching_count {
                                best_match = Some(*target);
                                matching_count = 1;
                            } else if let Some(tys) = overlap.kind.as_union().map(|u| u.tys) {
                                let len = tys.iter().filter(|t| t.is_unit()).count();
                                if len >= matching_count {
                                    best_match = Some(*target);
                                    matching_count = len;
                                }
                            }
                        }
                    }
                }
                best_match
            })
    }

    fn find_best_ty_for_object_literal(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target_union: &'cx ty::UnionTy<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if source
            .get_object_flags()
            .contains(ty::ObjectFlags::OBJECT_LITERAL)
            && target_union.tys.iter().any(|ty| self.is_array_like_ty(ty))
        {
            target_union
                .tys
                .iter()
                .find(|t| self.is_array_like_ty(t))
                .copied()
        } else {
            None
        }
    }

    fn find_best_ty_for_invokable(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target_union: &'cx ty::UnionTy<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let mut kind = ty::SigKind::Call;
        let has_sigs = !self.get_signatures_of_type(source, kind).is_empty() || {
            kind = ty::SigKind::Constructor;
            !self.get_signatures_of_type(source, kind).is_empty()
        };
        if has_sigs {
            target_union
                .tys
                .iter()
                .find(|t| !self.get_signatures_of_type(t, kind).is_empty())
                .copied()
        } else {
            None
        }
    }

    fn elaborate_element_wise(
        &mut self,
        node: &[Elaboration<'cx>],
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        let mut reported_error = false;
        for e in node {
            let Some(target_prop_ty) =
                self.get_best_match_indexed_access_ty_or_undefined(source, target, e.name_ty)
            else {
                continue;
            };
            if target_prop_ty.flags.contains(TypeFlags::INDEXED_ACCESS) {
                continue;
            }
            let error_node = e.error_node;
            let Some(source_prop_ty) =
                self.get_indexed_access_ty_or_undefined(source, e.name_ty, None, None, None, None)
            else {
                continue;
            };
            if !self.check_type_related_to(
                source_prop_ty,
                target_prop_ty,
                relation,
                None,
                NOOP_HEADING_ERROR,
            ) {
                let elaborated = self.elaborate_error(
                    e.inner_expr.map(|e| e.id()),
                    source_prop_ty,
                    target_prop_ty,
                    relation,
                    e.inner_expr.map(|e| e.id()),
                );
                reported_error = true;
                if !elaborated {
                    let specific_source = if let Some(next) = e.inner_expr {
                        self.check_expression_for_mutable_location_with_contextual_type(
                            next,
                            source_prop_ty,
                        )
                    } else {
                        source_prop_ty
                    };
                    self.check_type_related_to(
                        specific_source,
                        target_prop_ty,
                        relation,
                        Some(error_node),
                        Some(|this: &mut Self| {
                            let span = this.p.node(error_node).span();
                            let specific_source = this.print_ty(specific_source, None).to_string();
                            let target_prop_ty = this.print_ty(target_prop_ty, None).to_string();
                            let error = Box::new(errors::TypeIsNotAssignableToType {
                                span,
                                ty1: specific_source,
                                ty2: target_prop_ty,
                            });
                            this.push_error(error);
                        }),
                    );
                }
            }
        }
        reported_error
    }
}

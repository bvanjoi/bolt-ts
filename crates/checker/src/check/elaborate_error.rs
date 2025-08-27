use super::Ternary;
use super::TyChecker;
use super::errors;
use super::relation::RelationKind;
use super::symbol_info::SymbolInfo;

use crate::ty;
use crate::ty::TypeFlags;
use bolt_ts_ast as ast;
use bolt_ts_binder::SymbolName;

struct Elaboration<'cx> {
    error_node: ast::NodeID,
    inner_expr: Option<ast::NodeID>,
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
            _ => false,
        }
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

        let node = node
            .members
            .iter()
            .filter_map(|member| {
                if matches!(member.kind, ast::ObjectMemberKind::SpreadAssignment(_)) {
                    return None;
                }
                let s = self.get_symbol_of_decl(member.id());
                let ty = self.get_lit_ty_from_prop(
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
                        inner_expr: Some(n.init.id()),
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
        self.elaborate_element_wise(&node, source, target, relation)
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
                        .get_prop_of_ty(target, SymbolName::EleNum((i as f64).into()))
                        .is_none()
                {
                    None
                } else {
                    let name_ty = self.get_number_literal_type_from_number(i as f64);
                    let check_node = self.get_effective_check_node(ele.id());
                    Some(Elaboration {
                        error_node: check_node,
                        inner_expr: Some(check_node),
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
            .intersects(TypeFlags::PRIMITIVE | TypeFlags::NEVER)
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
        if let Some(idx) = self.get_indexed_access_ty_or_undefined(target, name_ty, None, None) {
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
            return self.get_indexed_access_ty_or_undefined(best, name_ty, None, None);
        }

        None
    }

    fn get_best_matching_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target_union: &'cx ty::UnionTy<'cx>,
        cmp: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> Ternary,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // TODO:
        None
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
            if target_prop_ty.flags.intersects(TypeFlags::INDEXED_ACCESS) {
                continue;
            }
            let error_node = e.error_node;
            let Some(source_prop_ty) =
                self.get_indexed_access_ty_or_undefined(source, e.name_ty, None, None)
            else {
                continue;
            };
            if !self.check_type_related_to(source_prop_ty, target_prop_ty, relation, None) {
                let elaborated = self.elaborate_error(
                    e.inner_expr,
                    source_prop_ty,
                    target_prop_ty,
                    relation,
                    e.inner_expr,
                );
                reported_error = true;
                if !elaborated {
                    let res = self.check_type_related_to(
                        source_prop_ty,
                        target_prop_ty,
                        relation,
                        Some(error_node),
                    );
                    if !res {
                        let span = self.p.node(error_node).span();
                        let error = errors::TypeIsNotAssignableToType {
                            span,
                            ty1: self.print_ty(source_prop_ty).to_string(),
                            ty2: self.print_ty(target_prop_ty).to_string(),
                        };
                        self.push_error(Box::new(error));
                    }
                }
            }
        }
        reported_error
    }
}

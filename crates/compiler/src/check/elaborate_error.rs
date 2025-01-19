use super::errors;
use super::relation::RelationKind;
use super::Ternary;
use super::TyChecker;

use crate::ast;
use crate::ty;

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

        use ast::Node::*;
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
        if target.kind.is_primitive() || target.kind.is_never() {
            return false;
        }

        let node = node
            .members
            .iter()
            .map(|member| {
                let s = self.get_symbol_of_decl(member.id());
                let ty = self.get_lit_ty_from_prop(s);
                use ast::ObjectMemberKind::*;
                match member.kind {
                    Shorthand(n) => Elaboration {
                        error_node: n.name.id,
                        inner_expr: None,
                        name_ty: ty,
                    },
                    Prop(n) => Elaboration {
                        error_node: n.name.id(),
                        inner_expr: Some(n.value.id()),
                        name_ty: ty,
                    },
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
            .map(|(i, ele)| {
                let name_ty = self.get_number_literal_type(i as f64);
                let check_node = self.get_effective_check_node(ele.id());
                Elaboration {
                    error_node: check_node,
                    inner_expr: Some(check_node),
                    name_ty,
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
        let tuple_ty = self.check_array_lit(node, true);
        if tuple_ty.kind.is_object_tuple() {
            let node = self.generate_limited_tuple_elements(node, target);
            self.elaborate_element_wise(&node, tuple_ty, target, relation)
        } else {
            false
        }
    }

    fn get_best_match_indexed_access_ty_or_undefined(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        name_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let idx = self.get_indexed_access_ty(target, name_ty, None, None);
        idx
    }

    fn elaborate_element_wise(
        &mut self,
        node: &[Elaboration<'cx>],
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        for e in node {
            let target_prop_ty =
                self.get_best_match_indexed_access_ty_or_undefined(source, target, e.name_ty);
            let error_node = e.error_node;
            let source_prop_ty = self.get_indexed_access_ty(source, e.name_ty, None, None);
            if self.check_type_related_to(source_prop_ty, target_prop_ty, relation, None)
                == Ternary::FALSE
            {
                let elaborated = self.elaborate_error(
                    e.inner_expr,
                    source_prop_ty,
                    target_prop_ty,
                    relation,
                    e.inner_expr,
                );
                if !elaborated {
                    let res = self.check_type_related_to(
                        source_prop_ty,
                        target_prop_ty,
                        relation,
                        Some(error_node),
                    );
                    if res == Ternary::FALSE {
                        let span = self.p.node(error_node).span();
                        let error = errors::TypeIsNotAssignableToType {
                            span,
                            ty1: self.print_ty(source_prop_ty).to_string(),
                            ty2: self.print_ty(target_prop_ty).to_string(),
                        };
                        self.push_error(Box::new(error));
                    }
                }
                return true;
            }
        }
        false
    }
}

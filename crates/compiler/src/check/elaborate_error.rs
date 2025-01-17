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
        false
        // if target.kind.is_primitive() || target.kind.is_never() {
        //     return false;
        // }
        // self.elaborate_element_wise(node, source, target, relation)
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

    fn elaborate_element_wise(
        &mut self,
        node: &[Elaboration<'cx>],
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        let target = if target.kind.is_array(self) {
            target
                .kind
                .as_object_reference()
                .map(|refer| refer.resolved_ty_args[0])
                .unwrap()
        } else {
            return false;
        };
        for e in node {
            let error_node = e.error_node;
            let source_item = self.get_indexed_access_ty(source, e.name_ty, None, None);
            if self.check_type_related_to(source_item, target, relation, None) == Ternary::FALSE {
                self.check_type_related_to(source_item, target, relation, Some(error_node));
                return true;
            }
        }
        false
    }
}

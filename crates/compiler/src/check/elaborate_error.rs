use super::relation::RelationKind;
use super::TyChecker;

use crate::ast;
use crate::ty;

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
            _ => false,
        }
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
            self.elaborate_element_wise(node, tuple_ty, target, relation)
        } else {
            false
        }
    }

    fn elaborate_element_wise(
        &mut self,
        node: &'cx ast::ArrayLit<'cx>,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        relation: RelationKind,
    ) -> bool {
        let Some(source) = source.kind.as_object_tuple() else {
            return false;
        };
        let target = if target.kind.is_array(self) {
            target
                .kind
                .as_object_reference()
                .map(|refer| refer.resolved_ty_args[0])
                .unwrap()
        } else {
            return false;
        };
        for (idx, item) in source.tys.iter().enumerate() {
            let error_node = node.elems[idx].id();
            if !self.check_type_related_to(item, target, relation, None) {
                self.check_type_related_to(item, target, relation, Some(error_node));
                return true;
            }
        }
        false
    }
}

use super::Ternary;
use super::TyChecker;
use super::errors;
use super::relation::RelationKind;

use crate::bind::SymbolName;
use crate::ty;
use crate::ty::TypeFlags;
use bolt_ts_ast as ast;

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
        if target.flags.intersects(TypeFlags::PRIMITIVE)
            || target.flags.intersects(TypeFlags::NEVER)
        {
            return false;
        }

        let node = node
            .members
            .iter()
            .map(|member| {
                let s = self.get_symbol_of_decl(member.id());
                let ty = self.get_lit_ty_from_prop(s);
                use bolt_ts_ast::ObjectMemberKind::*;
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
                    Method(n) => Elaboration {
                        error_node: n.name.id(),
                        inner_expr: None,
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
            .flat_map(|(i, ele)| {
                if self.is_tuple_like(target)
                    && self
                        .get_prop_of_ty(target, SymbolName::EleNum((i as f64).into()))
                        .is_none()
                {
                    None
                } else {
                    let name_ty = self.get_number_literal_type(i as f64);
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
        let idx = self.get_indexed_access_ty_or_undefined(target, name_ty, None, None);
        idx
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
                reported_error = true;
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
            }
        }
        reported_error
    }
}

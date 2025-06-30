use super::get_simplified_ty::SimplifiedKind;
use super::symbol_info::SymbolInfo;
use super::{TyChecker, create_ty::IntersectionFlags};
use crate::{
    ir,
    ty::{self, TypeFlags},
};

use bolt_ts_ast as ast;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_effective_base_type_node(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        match self.p.node(id) {
            ast::Node::ClassDecl(c) => c.extends,
            ast::Node::ClassExpr(c) => c.extends,
            _ => None,
        }
    }

    pub(super) fn get_effective_ty_param_decls(&self, id: ast::NodeID) -> ast::TyParams<'cx> {
        let node = self.p.node(id);
        node.ty_params().unwrap_or_default()
    }

    #[inline]
    pub(super) fn get_effective_ret_type_node(&self, id: ast::NodeID) -> Option<&'cx ast::Ty<'cx>> {
        self.p.node(id).ret_ty()
    }

    pub(super) fn get_effective_check_node(&self, id: ast::NodeID) -> ast::NodeID {
        // self.p.skip_outer_expr(id)
        id
    }

    pub(super) fn get_effective_constraint_of_intersection(
        &mut self,
        tys: &[&'cx ty::Ty<'cx>],
        target_is_union: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let mut constraints = Vec::with_capacity(16);
        let mut has_disjoint_domain_ty = false;
        for t in tys {
            if t.flags.intersects(TypeFlags::INSTANTIABLE) {
                let mut constraint = self.get_constraint_of_ty(t);
                while let Some(c) = constraint {
                    if c.flags.intersects(
                        TypeFlags::TYPE_PARAMETER
                            .union(TypeFlags::INDEX)
                            .union(TypeFlags::CONDITIONAL),
                    ) {
                        constraint = self.get_constraint_of_ty(c)
                    } else {
                        break;
                    }
                }
                if let Some(c) = constraint {
                    constraints.push(c);
                    if target_is_union {
                        constraints.push(t);
                    }
                }
            } else if t.flags.intersects(TypeFlags::DISJOINT_DOMAINS)
                || self.is_empty_anonymous_object_ty(t)
            {
                has_disjoint_domain_ty = true;
            }
        }

        if !constraints.is_empty() {
            if has_disjoint_domain_ty {
                for t in tys {
                    if t.flags.intersects(TypeFlags::DISJOINT_DOMAINS)
                        || self.is_empty_anonymous_object_ty(t)
                    {
                        constraints.push(t);
                    }
                }
            }
            if target_is_union || has_disjoint_domain_ty {
                let i = self.get_intersection_ty(
                    &constraints,
                    IntersectionFlags::NoConstraintReduction,
                    None,
                    None,
                );
                return Some(self.get_normalized_ty(i, SimplifiedKind::Reading));
            }
        }

        None
    }

    pub(super) fn get_effective_constraint_of_ty_param(
        &self,
        node: &ast::TyParam<'cx>,
    ) -> Option<&'cx ast::Ty<'cx>> {
        node.constraint
    }

    pub(super) fn get_effective_ty_args(
        &mut self,
        id: ast::NodeID,
        ty_params: ty::Tys<'cx>,
    ) -> Option<ty::Tys<'cx>> {
        let node = self.p.node(id);
        let ty_args = if let Some(ty_args) = node.ty_args() {
            let ty_args = ty_args
                .list
                .iter()
                .map(|ty_arg| self.get_ty_from_type_node(ty_arg))
                .collect::<Vec<_>>();
            let ty_args: ty::Tys<'cx> = self.alloc(ty_args);
            Some(ty_args)
        } else {
            None
        };
        let min_ty_argument_count = self.get_min_ty_arg_count(Some(ty_params));
        self.fill_missing_ty_args(ty_args, Some(ty_params), min_ty_argument_count)
    }

    pub(super) fn get_effective_call_args(
        &mut self,
        expr: &impl ir::CallLike<'cx>,
    ) -> ast::Exprs<'cx> {
        expr.args()
    }
}

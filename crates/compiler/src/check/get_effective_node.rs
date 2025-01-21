use crate::{ast, ty};

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_effective_base_type_node(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        let extends = match self.p.node(id) {
            ast::Node::ClassDecl(c) => c.extends,
            ast::Node::ClassExpr(c) => c.extends,
            _ => None,
        };
        extends
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
        self.p.skip_outer_expr(id)
    }

    pub(super) fn get_effective_constraint_of_ty_param(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::Ty<'cx>> {
        let node = self.p.node(id).expect_ty_param();
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
        let min_ty_argument_count = self.get_min_ty_args_count(ty_params);
        self.fill_missing_ty_args(ty_args, Some(ty_params), min_ty_argument_count)
    }
}

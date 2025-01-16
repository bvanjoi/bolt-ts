use crate::ast;

use super::TyChecker;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_effective_base_type_node(
        &self,
        id: ast::NodeID,
    ) -> Option<&'cx ast::Expr<'cx>> {
        let extends = match self.p.node(id) {
            ast::Node::ClassDecl(c) => c.extends,
            ast::Node::ClassExpr(c) => c.extends,
            _ => None,
        };
        // TODO: type arguments
        extends.map(|extends| extends.expr)
    }

    pub(super) fn get_effective_ty_param_decls(&self, id: ast::NodeID) -> ast::TyParams<'cx> {
        let node = self.p.node(id);
        node.ty_params().unwrap_or_default()
    }

    #[inline]
    pub(super) fn get_effective_ret_type_node(&self, id: ast::NodeID) -> Option<&'cx ast::Ty<'cx>> {
        self.p.node(id).ret_ty()
    }
}

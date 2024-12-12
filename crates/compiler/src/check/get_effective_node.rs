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
        extends.map(|extends| extends.expr)
    }

    pub(super) fn get_effective_ty_param_decls(&self, id: ast::NodeID) -> ast::TyParams<'cx> {
        let node = self.p.node(id);
        if let Some(ty_decl) = node.as_type_decl() {
            if let Some(ty_params) = ty_decl.ty_params {
                return ty_params;
            }
        }
        &[]
    }
}

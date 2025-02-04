use super::TyChecker;
use crate::{ast, ty};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_flow_ty_of_reference(
        &self,
        refer: ast::NodeID,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: Option<&'cx ty::Ty<'cx>>,
        flow_container: Option<ast::NodeID>,
    ) {
        let init_ty = init_ty.unwrap_or(declared_ty);
    }
}

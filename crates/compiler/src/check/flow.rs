use super::TyChecker;
use crate::{ast, ty};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_flow_ty_of_reference(
        &mut self,
        refer: ast::NodeID,
        declared_ty: &'cx ty::Ty<'cx>,
        init_ty: Option<&'cx ty::Ty<'cx>>,
        flow_container: Option<ast::NodeID>,
        flow_node: Option<ast::NodeID>,
    ) -> &'cx ty::Ty<'cx> {
        // let Some(flow_node) = flow_node.or_else(|| {}) else {
        //     return declared_ty;
        // };

        let init_ty = init_ty.unwrap_or(declared_ty);

        let shared_flow_start = self.shared_flow_count;

        self.shared_flow_count = shared_flow_start;

        init_ty
    }

    // fn get_ty_at_flow_node(&self, flow_node: ast::NodeID) -> &'cx ty::Ty<'cx> {
    //     // self.flow_ty_map
    //     //     .get(&flow_node)
    //     //     .unwrap_or(&self.undefined_ty)
    // }
}

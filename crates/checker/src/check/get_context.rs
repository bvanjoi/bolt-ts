use bolt_ts_ast as ast;

use super::InferenceContextId;
use super::TyChecker;
use super::ty;

#[derive(Debug, Clone, Copy)]
pub(super) struct TyContextual<'cx> {
    node: ast::NodeID,
    is_cache: bool,
    pub(super) ty: Option<&'cx ty::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct InferenceContextual {
    node: ast::NodeID,
    pub(super) inference: Option<InferenceContextId>,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn push_type_context(
        &mut self,
        node: ast::NodeID,
        ty: Option<&'cx ty::Ty<'cx>>,
        is_cache: bool,
    ) {
        let ctx = TyContextual { node, ty, is_cache };
        self.type_contextual.push(ctx);
    }

    pub(super) fn push_cached_contextual_type(&mut self, node: ast::NodeID) {
        let ty = self.get_contextual_ty(node, None);
        self.push_type_context(node, ty, true);
    }

    pub(super) fn pop_type_context(&mut self) {
        self.type_contextual.pop().unwrap();
    }

    pub(super) fn push_inference_context(
        &mut self,
        node: ast::NodeID,
        inference: Option<InferenceContextId>,
    ) {
        self.inference_contextual
            .push(InferenceContextual { node, inference });
    }

    pub(super) fn get_inference_context(&self, node: ast::NodeID) -> Option<InferenceContextual> {
        self.inference_contextual
            .iter()
            .rev()
            .find(|ctx| {
                self.node_query(node.module())
                    .is_descendant_of(node, ctx.node)
            })
            .copied()
    }

    pub(super) fn pop_inference_context(&mut self) {
        self.inference_contextual.pop().unwrap();
    }

    pub(super) fn find_context_node(
        &self,
        node: ast::NodeID,
        include_caches: bool,
    ) -> Option<TyContextual<'cx>> {
        self.type_contextual
            .iter()
            .rev()
            .find(|ctx| node == ctx.node && (include_caches || !ctx.is_cache))
            .copied()
    }
}

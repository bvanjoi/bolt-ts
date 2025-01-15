use crate::ast;
use crate::ty;

use super::InferenceContextId;
use super::TyChecker;

#[derive(Debug, Clone, Copy)]
pub(super) struct TyContextual<'cx> {
    node: ast::NodeID,
    is_cache: bool,
    pub(super) ty: &'cx ty::Ty<'cx>,
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
        ty: &'cx ty::Ty<'cx>,
        is_cache: bool,
    ) {
        self.type_contextual
            .push(TyContextual { node, ty, is_cache });
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
            .find(|ctx| self.p.is_descendant_of(node, ctx.node))
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

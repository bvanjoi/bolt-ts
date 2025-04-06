use super::ast;
use super::{Node, NodeID};

#[derive(Debug, Default)]
pub struct Nodes<'cx>(pub(super) Vec<Node<'cx>>);

impl<'cx> Nodes<'cx> {
    pub fn get(&self, id: NodeID) -> Node<'cx> {
        let idx = id.index_as_usize();
        debug_assert!(idx < self.0.len(), "idx: {idx}, len: {}", self.0.len());
        *unsafe { self.0.get_unchecked(idx) }
    }

    pub fn insert(&mut self, id: NodeID, node: Node<'cx>) {
        assert_eq!(id.index_as_usize(), self.0.len());
        self.0.push(node);
    }

    pub(super) fn root(&self) -> &'cx ast::Program<'cx> {
        let idx = self.0.len() - 1;
        let node = unsafe { self.0.get_unchecked(idx) };
        node.expect_program()
    }
}

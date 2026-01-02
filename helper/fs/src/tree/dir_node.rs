use super::{FSNode, FSNodeId};
use crate::PathId;

#[derive(Debug)]
pub struct DirNode {
    path: PathId,
    children: Vec<FSNodeId>,
}

impl DirNode {
    #[inline]
    pub fn new(path: PathId) -> Self {
        Self {
            path,
            children: Vec::with_capacity(8),
        }
    }

    #[inline]
    pub fn children(&self) -> &[FSNodeId] {
        &self.children
    }

    #[inline]
    pub fn path(&self) -> PathId {
        self.path
    }

    pub(super) fn add_child(&mut self, child: FSNodeId) {
        debug_assert!(!self.children.contains(&child));
        self.children.push(child);
    }

    pub(super) fn find_child(&self, path: PathId, nodes: &[FSNode]) -> Option<FSNodeId> {
        self.children
            .iter()
            .find(|child| {
                let child_index = child.as_usize();
                debug_assert!(child_index < nodes.len());
                let child = unsafe { nodes.get_unchecked(child_index) };
                path == child.kind().path()
            })
            .copied()
    }
}

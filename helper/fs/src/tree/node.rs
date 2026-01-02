use bolt_ts_atom::Atom;

use super::FSNodeId;
use super::dir_node::DirNode;
use super::file_node::FileNode;
use crate::PathId;

pub struct FSNode {
    id: FSNodeId,
    kind: FSNodeKind,
}

impl PartialEq for FSNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl FSNode {
    #[inline]
    pub(super) fn new(id: FSNodeId, kind: FSNodeKind) -> Self {
        Self { id, kind }
    }

    #[inline]
    pub fn id(&self) -> FSNodeId {
        self.id
    }

    #[inline]
    pub fn kind(&self) -> &FSNodeKind {
        &self.kind
    }

    #[inline]
    pub(super) fn kind_mut(&mut self) -> &mut FSNodeKind {
        &mut self.kind
    }
}

#[derive(Debug)]
pub enum FSNodeKind {
    File(FileNode),
    Dir(DirNode),
}

impl FSNodeKind {
    pub(super) fn real_file_node(path: PathId, content: Atom) -> Self {
        let node = FileNode::new_real(path, content);
        FSNodeKind::File(node)
    }

    pub(super) fn symlink_file_node(path: PathId, to: PathId) -> Self {
        let node = FileNode::new_symlink(path, to);
        FSNodeKind::File(node)
    }

    pub(super) fn dir_node(path: PathId) -> Self {
        let node = DirNode::new(path);
        FSNodeKind::Dir(node)
    }

    pub fn as_file_node(&self) -> Option<FileNode> {
        let FSNodeKind::File(n) = self else {
            return None;
        };
        Some(*n)
    }

    pub fn as_dir_node(&self) -> Option<&DirNode> {
        let FSNodeKind::Dir(n) = self else {
            return None;
        };
        Some(n)
    }

    pub fn path(&self) -> PathId {
        match self {
            FSNodeKind::File(node) => node.path(),
            FSNodeKind::Dir(node) => node.path(),
        }
    }
}

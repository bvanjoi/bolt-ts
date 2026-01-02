use bolt_ts_atom::Atom;

use crate::PathId;

#[derive(Debug, Clone, Copy)]
pub enum FileNode {
    Real(RealFileNode),
    Symlink(SymlinkFileNode),
}

#[derive(Debug, Clone, Copy)]
pub struct RealFileNode {
    path: PathId,
    content: Atom,
}

#[derive(Debug, Clone, Copy)]
pub struct SymlinkFileNode {
    path: PathId,
    to: PathId,
}

impl FileNode {
    #[inline]
    pub(super) fn new_real(path: PathId, content: Atom) -> Self {
        Self::Real(RealFileNode { path, content })
    }

    pub(super) fn new_symlink(path: PathId, to: PathId) -> Self {
        Self::Symlink(SymlinkFileNode { path, to })
    }

    #[inline]
    pub fn path(&self) -> PathId {
        match self {
            FileNode::Real(n) => n.path,
            FileNode::Symlink(n) => n.path,
        }
    }

    #[inline]
    pub fn content(&self, tree: &super::FSTree) -> Atom {
        match self {
            FileNode::Real(n) => n.content,
            FileNode::Symlink(n) => {
                let node = tree.path_to_node[&n.to];
                match tree.node(node).kind() {
                    super::FSNodeKind::File(f) => f.content(tree),
                    super::FSNodeKind::Dir(_) => {
                        panic!("symlink points to a directory")
                    }
                }
            }
        }
    }

    #[inline]
    pub fn realpath(&self) -> PathId {
        match self {
            FileNode::Real(n) => n.path,
            FileNode::Symlink(n) => n.to,
        }
    }
}

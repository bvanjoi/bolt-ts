use std::{hash::Hash, path::PathBuf};

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_utils::no_hashmap_with_capacity;
use bolt_ts_utils::path::NormalizePath;

use super::errors::{self, FsResult};
use super::has_slash_suffix_and_not_root;
use super::path::PathId;
use crate::FsError;

bolt_ts_utils::index!(FSNodeId);

pub(super) struct FSTree {
    nodes: Vec<FSNode>,
    path_to_node: nohash_hasher::IntMap<PathId, FSNodeId>,
}

impl<'atoms> FSTree {
    pub const ROOT: FSNodeId = FSNodeId(0);
    pub(super) fn new(atoms: &mut AtomMap) -> Self {
        const CAP: usize = 1024;
        let nodes = Vec::with_capacity(CAP);
        let mut this = Self {
            nodes,
            path_to_node: no_hashmap_with_capacity(CAP),
        };
        let root = std::path::Path::new("/");
        let ret = this.add_dir(atoms, root).unwrap();
        assert_eq!(ret, FSTree::ROOT);
        this
    }

    fn target_error(
        &self,
        node: FSNodeId,
        target: PathId,
        is_dir: bool,
    ) -> Option<errors::FsError> {
        match self.node(node).kind {
            FSNodeKind::Dir(_) if !is_dir => Some(errors::FsError::NotAFile(target)),
            FSNodeKind::File(_) if is_dir => Some(errors::FsError::NotADir(target)),
            _ => None,
        }
    }

    fn insert_file_node(
        &mut self,
        parent: FSNodeId,
        path: PathId,
        content: AtomId,
    ) -> FsResult<FSNodeId> {
        let parent_node = self.node(parent);
        let FSNodeKind::Dir(dir) = &parent_node.kind else {
            return Err(errors::FsError::FileExists(parent_node.kind.path()));
        };
        if let Some(old) = dir.find_child(path, &self.nodes) {
            if self.node(old).kind.as_dir_node().is_some() {
                Err(errors::FsError::DirExists(path))
            } else {
                Ok(old)
            }
        } else {
            Ok(self.insert_node(parent, FSNodeKind::file_node(path, content)))
        }
    }

    fn insert_dir_node(
        &mut self,
        parent: Option<FSNodeId>,
        path: PathId,
        atoms: &mut AtomMap,
    ) -> FsResult<FSNodeId> {
        if let Some(parent) = parent {
            let parent_node = self.node(parent);
            let FSNodeKind::Dir(dir) = &parent_node.kind else {
                panic!("parent should must be a directory")
            };
            if let Some(old) = dir.find_child(path, &self.nodes) {
                if self.node(old).kind.as_file_node().is_some() {
                    Err(errors::FsError::FileExists(path))
                } else {
                    Ok(old)
                }
            } else {
                Ok(self.insert_node(parent, FSNodeKind::dir_node(path)))
            }
        } else {
            debug_assert!(atoms.atom("/") == path.into());
            if self.nodes.is_empty() {
                let kind = FSNodeKind::dir_node(path);
                let node = FSNode {
                    id: FSNodeId::root(),
                    kind,
                };
                let prev = self.path_to_node.insert(path, node.id);
                assert!(prev.is_none());
                self.nodes.push(node);
            }
            Ok(FSTree::ROOT)
        }
    }

    fn insert_node(&mut self, parent: FSNodeId, node: FSNodeKind) -> FSNodeId {
        let id = FSNodeId(self.nodes.len() as u32);
        let node = FSNode { id, kind: node };
        let prev = match &node.kind {
            FSNodeKind::File(n) => self.path_to_node.insert(n.path, id),
            FSNodeKind::Dir(n) => self.path_to_node.insert(n.path, id),
        };
        assert!(prev.is_none());
        self.nodes.push(node);
        let FSNodeKind::Dir(dir) = &mut self.mut_node(parent).kind else {
            unreachable!("parent should not be a directory")
        };
        dir.children.push(id);
        id
    }

    pub fn node(&self, id: FSNodeId) -> &FSNode {
        &self.nodes[id.0 as usize]
    }

    fn mut_node(&mut self, id: FSNodeId) -> &mut FSNode {
        &mut self.nodes[id.0 as usize]
    }

    pub(super) fn add_file(
        &mut self,
        atoms: &mut AtomMap,
        path: &std::path::Path,
        content: AtomId,
    ) -> FsResult<FSNodeId> {
        let parent_dir = path.parent().unwrap();
        let parent = self.add_dir(atoms, parent_dir)?;
        let path_id = PathId::new(path, atoms);
        self.insert_file_node(parent, path_id, content)
    }

    pub(super) fn add_dir(
        &mut self,
        atoms: &mut AtomMap,
        path: &std::path::Path,
    ) -> FsResult<FSNodeId> {
        debug_assert!(path.is_normalized());
        if let Some(cache) = self.path_to_node.get(&PathId::get(path, atoms)) {
            return Ok(*cache);
        };

        let mut id = FSTree::ROOT;
        let mut parent = None;
        let mut current_path = PathBuf::with_capacity(path.as_os_str().len() + 8);
        for component in path.components() {
            use std::path::Component::*;
            match component {
                Prefix(_) => todo!("handle prefix path"),
                RootDir => {
                    current_path.push("/");
                    let root = PathId::from(atoms.atom("/"));
                    parent = Some(self.insert_dir_node(parent, root, atoms)?);
                }
                Normal(_) => {
                    current_path.push(component);
                    let path_id = PathId::new(&current_path, atoms);
                    id = self.insert_dir_node(parent, path_id, atoms)?;
                    parent = Some(id)
                }
                CurDir | ParentDir => unreachable!(),
            }
        }
        Ok(id)
    }

    pub(super) fn find_path(
        &self,
        path: &std::path::Path,
        is_dir: bool,
        atoms: &mut AtomMap,
    ) -> errors::FsResult<FSNodeId> {
        if path.as_os_str().as_encoded_bytes() == b"/" {
            return if is_dir {
                Ok(FSTree::ROOT)
            } else {
                let p = atoms.atom("/");
                Err(FsError::NotAFile(p.into()))
            };
        }
        let target = PathId::get(path, atoms);
        let Some(parent) = path.parent() else {
            unreachable!()
        };
        let Some(dir) = self.path_to_node.get(&PathId::get(parent, atoms)) else {
            return Err(errors::FsError::NotFound(target));
        };
        let dir = self.node(*dir).kind.as_dir_node().unwrap();
        match dir.find_child(target, &self.nodes) {
            Some(res) => {
                if let Some(err) = self.target_error(res, target, is_dir) {
                    Err(err)
                } else {
                    Ok(res)
                }
            }
            None => Err(errors::FsError::NotFound(target)),
        }
    }

    pub(super) fn read_file(
        &self,
        path: &std::path::Path,
        atoms: &mut AtomMap,
    ) -> FsResult<AtomId> {
        if has_slash_suffix_and_not_root(path) {
            Err(errors::FsError::NotAFile(PathId::get(path, atoms)))
        } else {
            let id = self.find_path(path, false, atoms)?;
            let Some(file) = self.node(id).kind.as_file_node() else {
                unreachable!("handled been handled by `find_path`");
            };
            Ok(file.content)
        }
    }

    pub(super) fn file_exists(&self, p: &std::path::Path, atoms: &mut AtomMap) -> bool {
        self.find_path(p, super::has_slash_suffix_and_not_root(p), atoms)
            .is_ok_and(|id| self.node(id).kind.as_file_node().is_some())
    }
}

pub(super) struct FSNode {
    id: FSNodeId,
    kind: FSNodeKind,
}

impl PartialEq for FSNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl FSNode {
    pub fn kind(&self) -> &FSNodeKind {
        &self.kind
    }
}

#[derive(Debug)]
pub(super) enum FSNodeKind {
    File(FileNode),
    Dir(DirNode),
}

#[derive(Debug, Clone, Copy)]
pub(super) struct FileNode {
    path: PathId,
    content: AtomId,
}

#[derive(Debug)]
pub(super) struct DirNode {
    path: PathId,
    children: Vec<FSNodeId>,
}

impl DirNode {
    pub fn children(&self) -> &[FSNodeId] {
        &self.children
    }

    fn find_child(&self, path: PathId, nodes: &[FSNode]) -> Option<FSNodeId> {
        self.children
            .iter()
            .find(|child| {
                let child = &nodes[child.as_usize()];
                path == child.kind.path()
            })
            .copied()
    }
}

impl FSNodeKind {
    fn file_node(path: PathId, content: AtomId) -> Self {
        let node = FileNode { path, content };
        FSNodeKind::File(node)
    }

    fn dir_node(path: PathId) -> Self {
        let node = DirNode {
            path,
            children: Vec::with_capacity(32),
        };
        FSNodeKind::Dir(node)
    }

    pub fn as_file_node(&self) -> Option<FileNode> {
        match self {
            FSNodeKind::File(node) => Some(*node),
            FSNodeKind::Dir(_) => None,
        }
    }

    pub fn as_dir_node(&self) -> Option<&DirNode> {
        match self {
            FSNodeKind::File(_) => None,
            FSNodeKind::Dir(node) => Some(node),
        }
    }

    pub fn path(&self) -> PathId {
        match self {
            FSNodeKind::File(node) => node.path,
            FSNodeKind::Dir(node) => node.path,
        }
    }
}

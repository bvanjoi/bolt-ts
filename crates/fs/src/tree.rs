use std::{hash::Hash, path::PathBuf};

use bolt_ts_atom::{AtomId, AtomMap};

use super::has_slash_suffix_and_not_root;
use super::path::PathId;
use crate::errors::{self, FsResult};

bolt_ts_utils::index!(FSNodeId);

pub(super) struct FSTree {
    nodes: Vec<FSNode>,
}

impl<'atoms> FSTree {
    pub const ROOT: FSNodeId = FSNodeId(0);
    pub(super) fn new(atoms: &mut AtomMap<'atoms>) -> Self {
        const CAP: usize = 1024;
        let nodes = Vec::with_capacity(CAP);
        let mut this = Self { nodes };
        let root = std::path::Path::new("/");
        let ret = this.add_dir(atoms, root).unwrap();
        assert_eq!(ret, FSTree::ROOT);
        this
    }

    fn target_error(&self, node: FSNodeId, target: PathId, is_dir: bool) -> Option<errors::Error> {
        match self.node(node).kind {
            FSNodeKind::Dir(_) if !is_dir => Some(errors::Error::NotAFile(target)),
            FSNodeKind::File(_) if is_dir => Some(errors::Error::NotADir(target)),
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
            panic!("parent should not be a directory")
        };
        if let Some(old) = dir.find_child(path, &self.nodes) {
            if self.node(old).kind.as_dir_node().is_some() {
                Err(errors::Error::DirExists(path))
            } else {
                Ok(old)
            }
        } else {
            Ok(self.insert_node(parent, FSNodeKind::file_node(path, content)))
        }
    }

    fn insert_dir_node(&mut self, parent: Option<FSNodeId>, path: PathId) -> FsResult<FSNodeId> {
        if let Some(parent) = parent {
            let parent_node = self.node(parent);
            let FSNodeKind::Dir(dir) = &parent_node.kind else {
                panic!("parent should not be a directory")
            };
            if let Some(old) = dir.find_child(path, &self.nodes) {
                if self.node(old).kind.as_file_node().is_some() {
                    Err(errors::Error::FileExists(path))
                } else {
                    Ok(old)
                }
            } else {
                Ok(self.insert_node(parent, FSNodeKind::dir_node(path)))
            }
        } else {
            assert!(path == PathId::ROOT);
            if self.nodes.is_empty() {
                let kind = FSNodeKind::dir_node(PathId::ROOT);
                let node = FSNode {
                    id: FSNodeId::root(),
                    kind,
                };
                self.nodes.push(node);
            }
            Ok(FSTree::ROOT)
        }
    }

    fn insert_node(&mut self, parent: FSNodeId, node: FSNodeKind) -> FSNodeId {
        let id = FSNodeId(self.nodes.len() as u32);
        let node = FSNode { id, kind: node };
        self.nodes.push(node);
        let FSNodeKind::Dir(dir) = &mut self.mut_node(parent).kind else {
            unreachable!()
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
        atoms: &mut AtomMap<'atoms>,
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
        atoms: &mut AtomMap<'atoms>,
        path: &std::path::Path,
    ) -> FsResult<FSNodeId> {
        let mut id = FSTree::ROOT;
        let mut parent = None;
        let mut current_path = PathBuf::new();
        for component in path.components() {
            use std::path::Component::*;
            match component {
                Prefix(_) => todo!("should handle prefix path"),
                RootDir => {
                    current_path.push("/");
                    parent = Some(self.insert_dir_node(parent, PathId::ROOT)?);
                }
                Normal(_) => {
                    current_path.push(component);
                    let path_id = PathId::new(&current_path, atoms);
                    id = self.insert_dir_node(parent, path_id)?;
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
    ) -> errors::FsResult<FSNodeId> {
        if path == std::path::Path::new("/") && is_dir {
            return Ok(FSTree::ROOT);
        }
        let target = PathId::get(path);

        let mut parent = FSTree::ROOT;
        let mut current_path = PathBuf::new();
        let mut path_id;
        let mut peek = path.components().peekable();

        while let Some(component) = peek.next() {
            use std::path::Component::*;
            match component {
                Prefix(_) => todo!("should handle prefix path"),
                RootDir => {
                    current_path.push("/");
                    continue;
                }
                Normal(_) => {
                    current_path.push(component);
                }
                CurDir | ParentDir => unreachable!(),
            }
            path_id = PathId::get(&current_path);
            let Some(dir) = self.node(parent).kind.as_dir_node() else {
                unreachable!()
            };
            if peek.peek().is_none() {
                // last component

                let res = dir.find_child_by_path(path_id, &self.nodes);
                if res.is_empty() {
                    return Err(errors::Error::NotFound(target));
                } else if res.len() > 1 {
                    return Ok(res
                        .iter()
                        .find(|id| self.target_error(**id, path_id, is_dir).is_none())
                        .copied()
                        .unwrap());
                } else {
                    let res = res[0];
                    if let Some(err) = self.target_error(res, path_id, is_dir) {
                        return Err(err);
                    } else {
                        return Ok(res);
                    }
                }
            } else if let Some(next) = dir.find_child(path_id, &self.nodes) {
                parent = next;
            } else {
                return Err(errors::Error::NotFound(target));
            }
        }

        Err(self.target_error(parent, target, is_dir).unwrap())
    }

    pub(super) fn read_file(&self, path: &std::path::Path) -> FsResult<AtomId> {
        let id = self.find_path(path, false)?;
        if has_slash_suffix_and_not_root(path) {
            Err(errors::Error::NotAFile(self.node(id).kind.path()))
        } else {
            let Some(file) = self.node(id).kind.as_file_node() else {
                unreachable!("handled been handled by `find_path`");
            };
            Ok(file.content)
        }
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

pub(super) enum FSNodeKind {
    File(FileNode),
    Dir(DirNode),
}

#[derive(Debug, Clone, Copy)]
pub(super) struct FileNode {
    path: PathId,
    content: AtomId,
}

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

    fn find_child_by_path(&self, path: PathId, nodes: &[FSNode]) -> Vec<FSNodeId> {
        let array = self
            .children
            .iter()
            .filter(|child| {
                let child = &nodes[child.as_usize()];
                match child.kind {
                    FSNodeKind::Dir(ref dir) => dir.path == path,
                    FSNodeKind::File(ref file) => file.path == path,
                }
            })
            .copied()
            .collect::<Vec<_>>();
        assert!(array.len() <= 2);
        array
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
            children: Vec::with_capacity(16),
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

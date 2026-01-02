use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_utils::path::NormalizePath;

use crate::PathId;

use super::CachedFileSystem;
use super::errors::FsResult;
use super::tree::{FSNodeId, FSTree};

pub struct MemoryFS {
    tree: FSTree,
}

impl MemoryFS {
    pub fn new(
        content_map: impl Iterator<Item = (String, String)>,
        symlink_map: impl Iterator<Item = (String, String)>,
        atoms: &mut AtomIntern,
    ) -> FsResult<Self> {
        let mut tree = FSTree::new(atoms);

        for (path, content) in content_map {
            let path = std::path::Path::new(&path);
            let content = atoms.atom(&content);
            tree.add_file(atoms, path, content)?;
        }

        for (from, to) in symlink_map {
            let path = std::path::Path::new(&from);
            let target = std::path::Path::new(&to);
            tree.add_symlink_file(atoms, path, target)?;
        }

        Ok(Self { tree })
    }

    fn glob_visitor(
        &self,
        result: &mut Vec<std::path::PathBuf>,
        node: FSNodeId,
        atoms: &AtomIntern,
        includes: &[glob::Pattern],
        excludes: &[glob::Pattern],
    ) {
        let n = self.tree.node(node);
        if let Some(dir) = n.kind().as_dir_node() {
            for n in dir.children() {
                self.glob_visitor(result, *n, atoms, includes, excludes);
            }
        } else {
            let path = n.kind().path();
            let path = atoms.get(path.into());
            if !(includes.iter().any(|p| p.matches_path(path.as_ref()))
                && excludes.iter().all(|p| !p.matches_path(path.as_ref())))
            {
                return;
            }
            result.push(std::path::PathBuf::from(path));
        }
    }
}

impl CachedFileSystem for MemoryFS {
    fn read_file(
        &mut self,
        path: &std::path::Path,
        atoms: &mut AtomIntern,
    ) -> FsResult<bolt_ts_atom::Atom> {
        self.tree.read_file(path, atoms)
    }

    fn file_exists(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> bool {
        self.tree.file_exists(p, atoms)
    }

    fn read_dir(
        &mut self,
        path: &std::path::Path,
        atoms: &mut AtomIntern,
    ) -> FsResult<impl Iterator<Item = std::path::PathBuf>> {
        let id = self.tree.find_path(path, true, atoms)?;
        let node = self.tree.node(id);
        node.kind().as_dir_node().map_or_else(
            || {
                let p = node.kind().path();
                Err(crate::errors::FsError::NotADir(p))
            },
            |dir| {
                Ok(dir.children().iter().map(|id| {
                    let id = self.tree.node(*id).kind().path();
                    let atom = atoms.get(id.into());
                    std::path::PathBuf::from(atom)
                }))
            },
        )
    }

    fn glob(
        &mut self,
        base_dir: &std::path::Path,
        includes: &[&str],
        excludes: &[&str],
        atoms: &mut AtomIntern,
    ) -> Vec<std::path::PathBuf> {
        let includes = includes
            .iter()
            .map(|i| glob::Pattern::new(i).unwrap())
            .collect::<Vec<_>>();
        let excludes = excludes
            .iter()
            .map(|e| glob::Pattern::new(e).unwrap())
            .collect::<Vec<_>>();
        let Ok(node) = self.tree.find_path(base_dir, true, atoms) else {
            return vec![];
        };
        let mut results = Vec::new();
        self.glob_visitor(
            &mut results,
            node,
            atoms,
            includes.as_ref(),
            excludes.as_ref(),
        );
        results
    }

    fn dir_exists(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> bool {
        let Ok(id) = self.tree.find_path(p, true, atoms) else {
            return false;
        };
        let node = self.tree.node(id);
        node.kind().as_dir_node().is_some()
    }

    fn add_file(
        &mut self,
        _: &std::path::Path,
        _: String,
        _: Option<Atom>,
        _: &mut AtomIntern,
    ) -> Atom {
        unreachable!("Cannot add file to memory fs")
    }

    fn is_symlink(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> bool {
        self.tree.is_symlink(p, atoms)
    }

    fn realpath(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> FsResult<PathId> {
        debug_assert!(p.is_normalized());
        if !self.is_symlink(p, atoms) {
            let p = PathId::new(p, atoms);
            return Err(crate::errors::FsError::NotASymlink(p));
        }
        self.tree.read_symlink(p, atoms).map(|ret| ret.into())
    }
}

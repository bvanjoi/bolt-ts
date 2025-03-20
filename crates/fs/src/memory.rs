use super::CachedFileSystem;
use bolt_ts_atom::{AtomId, AtomMap};
use indexmap::IndexMap;

use crate::errors::FsResult;
use crate::tree::{FSNodeId, FSTree};

pub struct MemoryFS {
    tree: FSTree,
}

impl MemoryFS {
    pub fn new(input: IndexMap<String, String>, atoms: &mut AtomMap<'_>) -> FsResult<Self> {
        let mut tree = FSTree::new(atoms);

        for (path, content) in input {
            let path = std::path::Path::new(&path);
            let content = atoms.insert_by_str(content.into());
            tree.add_file(atoms, path, content)?;
        }

        Ok(Self { tree })
    }

    fn glob_visitor(
        &self,
        result: &mut Vec<std::path::PathBuf>,
        node: FSNodeId,
        pattern: &[glob::Pattern],
        atoms: &AtomMap<'_>,
    ) {
        let n = self.tree.node(node);
        if let Some(dir) = n.kind().as_dir_node() {
            for n in dir.children() {
                self.glob_visitor(result, *n, pattern, atoms);
            }
        } else {
            let path = n.kind().path();
            let path = atoms.get(path.into());
            if pattern.iter().any(|p| p.matches(path)) {
                result.push(std::path::PathBuf::from(path));
            }
        }
    }
}

impl CachedFileSystem for MemoryFS {
    fn read_file(
        &mut self,
        path: &std::path::Path,
        _atoms: &mut AtomMap<'_>,
    ) -> FsResult<bolt_ts_atom::AtomId> {
        self.tree.read_file(path)
    }

    fn read_dir(
        &mut self,
        path: &std::path::Path,
        atoms: &mut AtomMap<'_>,
    ) -> FsResult<impl Iterator<Item = std::path::PathBuf>> {
        let id = self.tree.find_path(path, true)?;
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
        include: &[&str],
        _exclude: &[&str],
        atoms: &mut AtomMap<'_>,
    ) -> Vec<std::path::PathBuf> {
        let includes = include
            .iter()
            .map(|i| glob::Pattern::new(i).unwrap())
            .collect::<Vec<_>>();
        let Ok(node) = self.tree.find_path(base_dir, true) else {
            return vec![];
        };
        let mut results = Vec::new();
        self.glob_visitor(&mut results, node, includes.as_ref(), atoms);
        results
    }

    fn add_file(&mut self, _: &std::path::Path, _: String, _: &mut AtomMap<'_>) -> AtomId {
        unreachable!("Cannot add file to memory fs")
    }
}

#[test]
fn test_mem_fs() {
    use super::errors;
    let json = serde_json::json!({
      "/a": "/a",
      "/b/c": "/b/c",
    });

    let atoms = &mut AtomMap::new(0);
    let mut fs = MemoryFS::new(serde_json::from_value(json).unwrap(), atoms).unwrap();

    use std::path::Path;

    let read_file =
        |fs: &mut MemoryFS, path: &str, atoms: &mut AtomMap| fs.read_file(Path::new(path), atoms);

    assert_eq!(fs.glob(Path::new("/"), &["a"], &[], atoms).len(), 0);
    assert_eq!(fs.glob(Path::new("/"), &["/a"], &[], atoms).len(), 1);
    assert_eq!(fs.glob(Path::new("/"), &["/b"], &[], atoms).len(), 0);
    assert_eq!(fs.glob(Path::new("/"), &["/b/c"], &[], atoms).len(), 1);
    assert_eq!(fs.glob(Path::new("/"), &["/*/c"], &[], atoms).len(), 1);
    // TODO: should `*/c` match `/b/c`?
    // assert_eq!(fs.glob(Path::new("/"), &["*/c"], &[],atoms).len(), 0);
    assert_eq!(fs.glob(Path::new("/"), &["**/*"], &[], atoms).len(), 2);
    assert_eq!(fs.glob(Path::new("/"), &["**/a"], &[], atoms).len(), 1);
    assert_eq!(fs.glob(Path::new("/"), &["**/b"], &[], atoms).len(), 0);
    assert_eq!(fs.glob(Path::new("/"), &["**/c"], &[], atoms).len(), 1);
    assert_eq!(fs.glob(Path::new("/b"), &["**/c"], &[], atoms).len(), 1);
    assert_eq!(fs.glob(Path::new("/b"), &["**/a"], &[], atoms).len(), 0);
    assert_eq!(fs.glob(Path::new("/a"), &["**/a"], &[], atoms).len(), 0);
    assert_eq!(fs.glob(Path::new("/c"), &["**/a"], &[], atoms).len(), 0);

    let res = read_file(&mut fs, "/a", atoms);
    assert!(res.is_ok_and(|id| atoms.eq_str(id, "/a")));

    let res = read_file(&mut fs, "/b/c", atoms);
    assert!(res.is_ok_and(|id| atoms.eq_str(id, "/b/c")));

    let res = read_file(&mut fs, "/", atoms);
    assert!(res.is_err_and(|err| matches!(err, errors::FsError::NotAFile(_))));

    let res = read_file(&mut fs, "/not-exist", atoms);
    assert!(res.is_err_and(|err| matches!(err, errors::FsError::NotFound(_))));

    let res = read_file(&mut fs, "/b", atoms);
    assert!(res.is_err_and(|err| matches!(err, errors::FsError::NotAFile(_))));

    let res = read_file(&mut fs, "/b/", atoms);
    assert!(res.is_err_and(|err| matches!(err, errors::FsError::NotAFile(_))));

    let res = read_file(&mut fs, "/a/", atoms);
    assert!(res.is_err_and(|err| matches!(err, errors::FsError::NotAFile(_))));

    let is_file =
        |fs: &mut MemoryFS, path: &str, atoms: &mut AtomMap| fs.is_file(Path::new(path), atoms);

    assert!(is_file(&mut fs, "/a", atoms));
    assert!(is_file(&mut fs, "/b/c", atoms));
    assert!(!is_file(&mut fs, "/", atoms));
    assert!(!is_file(&mut fs, "/not-exist", atoms));
    assert!(!is_file(&mut fs, "/a/", atoms));
    assert!(!is_file(&mut fs, "/b", atoms));
    assert!(!is_file(&mut fs, "/b/", atoms));

    let read_dir_failed = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomMap| match fs
        .read_dir(std::path::Path::new(path), atoms)
    {
        Ok(_) => unreachable!(),
        Err(err) => err,
    };

    let res = read_dir_failed(&mut fs, "/a", atoms);
    assert!(matches!(res, errors::FsError::NotADir(_)));

    let read_dir = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomMap| match fs
        .read_dir(std::path::Path::new(path), atoms)
    {
        Ok(iter) => iter
            .map(|p| p.to_string_lossy().to_string())
            .collect::<Vec<_>>(),
        Err(err) => unreachable!("{:?}", err),
    };

    let res = read_dir(&mut fs, "/", atoms);
    assert_eq!(res, vec!["/a", "/b"]);

    let res = read_dir(&mut fs, "/b", atoms);
    assert_eq!(res, vec!["/b/c"]);
}

#[test]
fn test_mem_fs_with_overlap_name_between_dir_and_file() {
    use super::errors;

    let json = serde_json::json!({
      "/a": "/a",
      "/a/b": "/a/b",
    });

    let atoms = &mut AtomMap::new(0);
    let fs = MemoryFS::new(serde_json::from_value(json).unwrap(), atoms);
    assert!(fs.is_err_and(|err| matches!(err, errors::FsError::FileExists(_))));

    let json = serde_json::json!({
        "/a/b": "/a/b",
        "/a": "/a",
    });

    let atoms = &mut AtomMap::new(0);
    let fs = MemoryFS::new(serde_json::from_value(json).unwrap(), atoms);
    assert!(fs.is_err_and(|err| matches!(err, errors::FsError::DirExists(_))));
}

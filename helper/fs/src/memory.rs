use super::CachedFileSystem;
use bolt_ts_atom::{Atom, AtomIntern};

use crate::errors::FsResult;
use crate::tree::{FSNodeId, FSTree};

pub struct MemoryFS {
    tree: FSTree,
}

impl MemoryFS {
    pub fn new(
        input: impl Iterator<Item = (String, String)>,
        atoms: &mut AtomIntern,
    ) -> FsResult<Self> {
        let mut tree = FSTree::new(atoms);

        for (path, content) in input {
            let path = std::path::Path::new(&path);
            let content = atoms.atom(&content);
            tree.add_file(atoms, path, content)?;
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
}

#[test]
fn test_mem_fs() {
    let json = serde_json::json!({
      "/a": "/a",
      "/b/c": "/b/c",
    });

    let atoms = &mut AtomIntern::prefill(&[]);
    let inputs: bolt_ts_utils::FxIndexMap<String, String> = serde_json::from_value(json).unwrap();
    let mut fs = MemoryFS::new(inputs.into_iter(), atoms).unwrap();

    use super::FsError::*;
    use std::path::Path;

    let read_file = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomIntern| {
        fs.read_file(Path::new(path), atoms)
    };

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
    assert!(res.is_ok_and(|id| atoms.get(id) == "/a"));

    let res = read_file(&mut fs, "/b/c", atoms);
    assert!(res.is_ok_and(|id| atoms.get(id) == "/b/c"));

    let res = read_file(&mut fs, "/", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotAFile(_))));

    let res = read_file(&mut fs, "/not-exist", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotFound(_))));

    let res = read_file(&mut fs, "/b", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotAFile(_))));

    let res = read_file(&mut fs, "/b/", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotAFile(_))));

    let res = read_file(&mut fs, "/a/", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotAFile(_))));

    assert!(fs.file_exists(Path::new("/a"), atoms));
    assert!(fs.file_exists(Path::new("/b/c"), atoms));
    assert!(!fs.file_exists(Path::new("/"), atoms));
    assert!(!fs.file_exists(Path::new("/not-exist"), atoms));
    assert!(!fs.file_exists(Path::new("/a/"), atoms));
    assert!(!fs.file_exists(Path::new("/b"), atoms));
    assert!(!fs.file_exists(Path::new("/b/"), atoms));

    let read_dir_failed = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomIntern| match fs
        .read_dir(std::path::Path::new(path), atoms)
    {
        Ok(_) => unreachable!(),
        Err(err) => err,
    };

    let res = read_dir_failed(&mut fs, "/a", atoms);
    assert!(matches!(res, NotADir(_)));

    let read_dir = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomIntern| match fs
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

    assert!(fs.dir_exists(Path::new("/"), atoms));
    assert!(fs.dir_exists(Path::new("/b"), atoms));
    assert!(!fs.dir_exists(Path::new("/a"), atoms));
    assert!(!fs.dir_exists(Path::new("/a/"), atoms));
    assert!(!fs.dir_exists(Path::new("/b/c"), atoms));
    assert!(!fs.dir_exists(Path::new("/not-exist"), atoms));
    assert!(!fs.dir_exists(Path::new("/not-exist/"), atoms));
}

#[test]
fn test_mem_fs_with_overlap_name_between_dir_and_file() {
    use super::errors;

    let json = serde_json::json!({
      "/a": "content",
      "/a/b": "content",
    });

    let atoms = &mut AtomIntern::prefill(&[]);
    let inputs: bolt_ts_utils::FxIndexMap<String, String> = serde_json::from_value(json).unwrap();
    let fs = MemoryFS::new(inputs.into_iter(), atoms);
    assert!(fs.is_err_and(|err| matches!(err, errors::FsError::FileExists(_))));

    let json = serde_json::json!({
        "/a/b": "content",
        "/a": "content",
    });

    let atoms = &mut AtomIntern::prefill(&[]);
    let inputs: bolt_ts_utils::FxIndexMap<String, String> = serde_json::from_value(json).unwrap();
    let fs = MemoryFS::new(inputs.into_iter(), atoms);
    assert!(fs.is_err_and(|err| matches!(err, errors::FsError::DirExists(_))));
}

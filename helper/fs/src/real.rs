use bolt_ts_atom::Atom;
use bolt_ts_utils::fx_hashmap_with_capacity;
use bolt_ts_utils::path::NormalizePath;
use rustc_hash::FxHashMap;

use crate::CachedFileSystem;
use crate::errors::FsResult;
use crate::tree::FSTree;

pub struct LocalFS {
    tree: FSTree,
    file_exists_cache: FxHashMap<Atom, bool>,
    dir_exists_cache: FxHashMap<Atom, bool>,
    metadata_cache: FxHashMap<Atom, Result<std::fs::Metadata, ()>>,
}

impl LocalFS {
    pub fn new(atoms: &mut bolt_ts_atom::AtomIntern) -> Self {
        let tree = FSTree::new(atoms);
        Self {
            tree,
            file_exists_cache: fx_hashmap_with_capacity(1024),
            dir_exists_cache: fx_hashmap_with_capacity(1024),
            metadata_cache: fx_hashmap_with_capacity(1024),
        }
    }

    fn glob_visitor(
        &mut self,
        result: &mut Vec<std::path::PathBuf>,
        dir: &std::path::Path,
        includes: &[glob::Pattern],
        excludes: &[glob::Pattern],
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) {
        // TODO: parallel?
        let matched = self
            .read_dir(dir, atoms)
            .unwrap()
            .filter(|item| {
                includes.iter().any(|p| p.matches_path(item))
                    && excludes.iter().all(|p| !p.matches_path(item))
            })
            .collect::<Vec<_>>();
        for item in matched {
            if item.is_dir() {
                self.glob_visitor(result, &item, includes, excludes, atoms);
            } else {
                result.push(item);
            }
        }
    }

    fn metadata(
        &mut self,
        p: &std::path::Path,
        atom: Option<Atom>,
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) -> Result<std::fs::Metadata, ()> {
        let s = unsafe { std::str::from_utf8_unchecked(p.as_os_str().as_encoded_bytes()) };
        let atom = if let Some(atom) = atom {
            debug_assert!(atoms.atom(s) == atom);
            atom
        } else {
            atoms.atom(s)
        };
        if let Some(metadata) = self.metadata_cache.get(&atom).cloned() {
            return metadata;
        }
        let metadata = std::fs::metadata(p).map_err(|_| ());
        self.metadata_cache.insert(atom, metadata.clone());
        metadata
    }
}

impl CachedFileSystem for LocalFS {
    fn read_file(
        &mut self,
        path: &std::path::Path,
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) -> FsResult<bolt_ts_atom::Atom> {
        if let Ok(atom) = self.tree.read_file(path, atoms) {
            Ok(atom)
        } else {
            let path = path.normalize();
            match read_file_with_encoding(path.as_path()) {
                Ok(content) => {
                    let content = atoms.atom(&content);
                    self.tree.add_file(atoms, path.as_path(), content).unwrap();
                    self.tree.read_file(path.as_path(), atoms)
                }
                Err(err) => match err.kind() {
                    std::io::ErrorKind::NotFound => Err(crate::errors::FsError::NotFound(
                        crate::path::PathId::new(path.as_path(), atoms),
                    )),
                    std::io::ErrorKind::InvalidData => Err(crate::errors::FsError::NotAFile(
                        crate::path::PathId::new(path.as_path(), atoms),
                    )),
                    _ => unreachable!("failed read '{path:#?}': {err}"),
                },
            }
        }
    }

    fn file_exists(&mut self, p: &std::path::Path, atoms: &mut bolt_ts_atom::AtomIntern) -> bool {
        if self.tree.file_exists(p, atoms) {
            return true;
        }
        let s = unsafe { std::str::from_utf8_unchecked(p.as_os_str().as_encoded_bytes()) };
        let id = atoms.atom(s);
        if let Some(exists) = self.file_exists_cache.get(&id).copied() {
            return exists;
        }
        let exists = self.metadata(p, Some(id), atoms).is_ok_and(|m| m.is_file());
        self.file_exists_cache.insert(id, exists);
        exists
    }

    fn read_dir(
        &mut self,
        p: &std::path::Path,
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) -> FsResult<impl Iterator<Item = std::path::PathBuf>> {
        debug_assert!(p.is_dir());
        self.tree.add_dir(atoms, p).map(|_| ())?;
        let entry = std::fs::read_dir(p).unwrap();
        Ok(entry.map(|entry| entry.unwrap().path()))
    }

    fn dir_exists(&mut self, p: &std::path::Path, atoms: &mut bolt_ts_atom::AtomIntern) -> bool {
        if self
            .tree
            .find_path(p, false, atoms)
            .is_ok_and(|id| self.tree.node(id).kind().as_dir_node().is_some())
        {
            return true;
        }
        let s = unsafe { std::str::from_utf8_unchecked(p.as_os_str().as_encoded_bytes()) };
        let id = atoms.atom(s);
        if let Some(exists) = self.dir_exists_cache.get(&id).copied() {
            return exists;
        }
        let exists = self.metadata(p, Some(id), atoms).is_ok_and(|m| m.is_dir());
        self.dir_exists_cache.insert(id, exists);
        exists
    }

    fn glob(
        &mut self,
        base_dir: &std::path::Path,
        include: &[&str],
        exclude: &[&str],
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) -> Vec<std::path::PathBuf> {
        let includes = include
            .iter()
            .map(|i| glob::Pattern::new(i).unwrap())
            .collect::<Vec<_>>();
        let excludes = exclude
            .iter()
            .map(|e| glob::Pattern::new(e).unwrap())
            .collect::<Vec<_>>();
        let mut result = Vec::with_capacity(4096);
        self.glob_visitor(&mut result, base_dir, &includes, &excludes, atoms);
        result
    }

    fn add_file(
        &mut self,
        p: &std::path::Path,
        content: String,
        atom: Option<Atom>,
        atoms: &mut bolt_ts_atom::AtomIntern,
    ) -> Atom {
        let atom = if let Some(atom) = atom {
            debug_assert!(atoms.atom(content.as_str()) == atom);
            atom
        } else {
            atoms.atom(content.as_str())
        };
        self.tree.add_file(atoms, p, atom).unwrap();
        atom
    }
}

pub fn read_file_with_encoding(file: &std::path::Path) -> std::io::Result<String> {
    let file = std::fs::File::open(file)?;
    let size = file.metadata().map(|m| m.len() as usize).ok();
    read_content_with_encoding(file, size)
}

fn read_content_with_encoding(
    mut read: impl std::io::Read,
    size: Option<usize>,
) -> Result<String, std::io::Error> {
    let mut buffer = Vec::new();
    buffer.try_reserve_exact(size.unwrap_or(1024))?;
    read.read_to_end(&mut buffer)?;

    let len = buffer.len();
    if len >= 2 && buffer[0] == 0xFE && buffer[1] == 0xFF {
        // Big endian UTF-16 byte order mark detected. Since big endian is not supported by Rust,
        // flip all byte pairs and treat as little endian.
        let mut i = 0;
        while i + 1 < len {
            buffer.swap(i, i + 1);
            i += 2;
        }
        let utf16_buffer: &[u16] =
            unsafe { std::slice::from_raw_parts(buffer.as_ptr() as *const u16, len / 2) };
        Ok(String::from_utf16_lossy(&utf16_buffer[1..]))
    } else if len >= 2 && buffer[0] == 0xFF && buffer[1] == 0xFE {
        // Little endian UTF-16 byte order mark detected
        let utf16_buffer: &[u16] =
            unsafe { std::slice::from_raw_parts(buffer.as_ptr() as *const u16, (len - 2) / 2) };
        Ok(String::from_utf16_lossy(utf16_buffer))
    } else if len >= 3 && buffer[0] == 0xEF && buffer[1] == 0xBB && buffer[2] == 0xBF {
        // UTF-8 byte order mark detected
        Ok(String::from_utf8_lossy(&buffer[3..]).to_string())
    } else {
        // Default is UTF-8 with no byte order mark
        Ok(String::from_utf8_lossy(&buffer).to_string())
    }
}

mod errors;
mod memory;
mod path;
mod real;
mod tree;

pub use self::errors::{FsError, FsResult};
pub use self::memory::MemoryFS;
pub use self::path::PathId;
pub use self::real::LocalFS;
pub use self::real::read_file_with_encoding;

use bolt_ts_atom::{Atom, AtomIntern};
use std::path::{Path, PathBuf};

pub trait CachedFileSystem: Send + Sync + std::fmt::Debug + Default {
    fn is_vfs(&self) -> bool;
    fn file_exists(&mut self, p: &Path, atoms: &mut AtomIntern) -> bool;
    fn read_file(&mut self, p: &Path, atoms: &mut AtomIntern) -> FsResult<Atom>;

    fn is_symlink(&mut self, p: &Path, atoms: &mut AtomIntern) -> bool;
    fn realpath(&mut self, p: &Path, atoms: &mut AtomIntern) -> FsResult<PathId>;

    fn dir_exists(&mut self, p: &Path, atoms: &mut AtomIntern) -> bool;
    fn read_dir(
        &mut self,
        p: &Path,
        atoms: &mut AtomIntern,
    ) -> FsResult<impl Iterator<Item = PathBuf>>;

    fn add_file(
        &mut self,
        p: &Path,
        content: String,
        atom: Option<Atom>,
        atoms: &mut AtomIntern,
    ) -> Atom;
}

fn has_slash_suffix_and_not_root(p: &Path) -> bool {
    let p = p.as_os_str().as_encoded_bytes();
    p.len() > 1 && p.last() == Some(&b'/')
}

fn glob_visitor(
    fs: &mut impl CachedFileSystem,
    result: &mut Vec<std::path::PathBuf>,
    base_dir: &std::path::Path,
    includes: &glob::Pattern,
    excludes: &[glob::Pattern],
    atoms: &mut AtomIntern,
) {
    let entires = fs.read_dir(base_dir, atoms).unwrap();
    let matched = entires
        .filter(|item| {
            includes.matches_path(item) && excludes.iter().all(|p| !p.matches_path(item))
        })
        .collect::<Vec<_>>();
    for item in matched {
        if fs.dir_exists(&item, atoms) {
            glob_visitor(fs, result, &item, includes, excludes, atoms);
        } else {
            result.push(item);
        }
    }
}

pub fn glob(
    fs: &mut impl CachedFileSystem,
    base_dir: &Path,
    includes: &glob::Pattern,
    excludes: &[glob::Pattern],
    atoms: &mut AtomIntern,
) -> Vec<PathBuf> {
    let mut result = Vec::new();
    glob_visitor(fs, &mut result, base_dir, includes, excludes, atoms);
    result
}

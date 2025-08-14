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

pub trait CachedFileSystem: Send + Sync {
    fn read_file(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> FsResult<Atom>;

    fn file_exists(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> bool;

    fn read_dir(
        &mut self,
        p: &std::path::Path,
        atoms: &mut AtomIntern,
    ) -> FsResult<impl Iterator<Item = std::path::PathBuf>>;

    fn dir_exists(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> bool;

    // TODO: maybe use regexp?
    fn glob(
        &mut self,
        base_dir: &std::path::Path,
        includes: &[&str],
        excludes: &[&str],
        atoms: &mut AtomIntern,
    ) -> Vec<std::path::PathBuf>;

    fn add_file(
        &mut self,
        p: &std::path::Path,
        content: String,
        atom: Option<Atom>,
        atoms: &mut AtomIntern,
    ) -> Atom;
}

pub fn has_slash_suffix_and_not_root(p: &std::path::Path) -> bool {
    let p = p.as_os_str().as_encoded_bytes();
    p.len() > 1 && p.last() == Some(&b'/')
}

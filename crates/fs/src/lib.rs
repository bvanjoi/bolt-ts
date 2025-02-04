mod errors;
mod memory;
mod path;
mod real;
mod tree;

pub use self::errors::{FsError, FsResult};
pub use self::memory::MemoryFS;
pub use self::path::PathId;
pub use self::real::LocalFS;

use bolt_ts_atom::{AtomId, AtomMap};

pub trait CachedFileSystem: Send + Sync {
    fn read_file(&mut self, p: &std::path::Path, atoms: &mut AtomMap<'_>) -> FsResult<AtomId>;

    fn is_file(&mut self, p: &std::path::Path, atoms: &mut AtomMap<'_>) -> bool {
        self.read_file(p, atoms).is_ok()
    }

    fn read_dir(
        &mut self,
        p: &std::path::Path,
        atoms: &mut AtomMap<'_>,
    ) -> FsResult<impl Iterator<Item = std::path::PathBuf>>;
}

pub fn has_slash_suffix_and_not_root(p: &std::path::Path) -> bool {
    let p = p.as_os_str().as_encoded_bytes();
    p.len() > 1 && p.last() == Some(&b'/')
}

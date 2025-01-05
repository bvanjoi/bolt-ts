mod errors;

use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_fs::{CachedFileSystem, PathId};
use normalize_path::NormalizePath;

type RResult<T> = Result<T, ()>;

pub struct Resolver<'atoms, FS: CachedFileSystem> {
    fs: Arc<Mutex<FS>>,
    atoms: Arc<Mutex<AtomMap<'atoms>>>,
}

impl<'atoms, FS: CachedFileSystem> Resolver<'atoms, FS> {
    pub fn new(fs: Arc<Mutex<FS>>, atoms: Arc<Mutex<AtomMap<'atoms>>>) -> Self {
        Self { fs, atoms }
    }

    fn is_file(&self, p: &std::path::Path) -> bool {
        let fs = &mut *self.fs.lock().unwrap();
        let atoms = &mut *self.atoms.lock().unwrap();
        fs.is_file(p, atoms)
    }

    fn join_path(&self, base: PathId, target: AtomId) -> PathBuf {
        let atoms = self.atoms.lock().unwrap();
        let base = std::path::Path::new(atoms.get(base.into()));
        debug_assert!(base.is_normalized());
        let target = std::path::Path::new(atoms.get(target));
        // TODO: use custom `join` for reduced normalized after joined.
        let p = base.join(target);
        p.normalize()
    }

    pub fn resolve(&self, base_dir: PathId, target: AtomId) -> RResult<PathId> {
        self.try_resolve_extension(base_dir, target)
    }

    fn try_resolve_extension(&self, base_dir: PathId, target: AtomId) -> RResult<PathId> {
        const EXTENSIONS: &[&str] = &[".ts", ".js"];
        let mut p = self.join_path(base_dir, target);
        let v = unsafe { &mut *(&mut p as *mut PathBuf as *mut Vec<u8>) };
        for ext in EXTENSIONS {
            v.extend_from_slice(ext.as_bytes());
            if self.is_file(p.as_path()) {
                return Ok(PathId::get(p.as_path()));
            }
            unsafe {
                v.set_len(v.len() - ext.len());
            }
        }
        Err(())
    }
}

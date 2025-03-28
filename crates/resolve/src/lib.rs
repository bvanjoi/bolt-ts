mod errors;

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_config::FLATTENED_ALL_SUPPORTED_EXTENSIONS;
use bolt_ts_fs::{CachedFileSystem, PathId};
use bolt_ts_path::{NormalizePath, path_as_str};
use bolt_ts_utils::fx_hashmap_with_capacity;
pub use errors::ResolveError;
use rustc_hash::FxHashMap;

pub type RResult<T> = Result<T, ResolveError>;
pub const NODE_MODULES_FOLDER: &str = "node_modules";
pub const COMMON_PACKAGE_FOLDERS: &[&str] =
    &[NODE_MODULES_FOLDER, "bower_components", "jspm_packages"];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CacheKey {
    base_dir: PathId,
    target: AtomId,
}

pub struct Resolver<'atoms, FS: CachedFileSystem> {
    fs: Arc<Mutex<FS>>,
    atoms: Arc<Mutex<AtomMap<'atoms>>>,
    cache: Arc<Mutex<FxHashMap<CacheKey, RResult<PathId>>>>,
}

impl<'atoms, FS: CachedFileSystem> Resolver<'atoms, FS> {
    pub fn new(fs: Arc<Mutex<FS>>, atoms: Arc<Mutex<AtomMap<'atoms>>>) -> Self {
        Self {
            fs,
            atoms,
            cache: Arc::new(Mutex::new(fx_hashmap_with_capacity(1024 * 8))),
        }
    }

    fn is_file(&self, p: &Path) -> bool {
        let fs = &mut *self.fs.lock().unwrap();
        let atoms = &mut *self.atoms.lock().unwrap();
        fs.is_file(p, atoms)
    }

    fn join_path(&self, base: PathId, target: AtomId) -> PathBuf {
        let atoms = self.atoms.lock().unwrap();
        let base = Path::new(atoms.get(base.into()));
        let target = Path::new(atoms.get(target));
        normalize_join(base, target)
    }

    pub fn resolve(&self, base_dir: PathId, target: AtomId) -> RResult<PathId> {
        let cache_key = CacheKey { base_dir, target };
        if let Some(cached) = self.cache.lock().unwrap().get(&cache_key) {
            return *cached;
        }
        let result = self.try_resolve_extension(base_dir, target);
        self.cache.lock().unwrap().insert(cache_key, result);
        result
    }

    fn try_resolve_extension(&self, base_dir: PathId, target: AtomId) -> RResult<PathId> {
        let mut p = self.join_path(base_dir, target);
        let v = unsafe { &mut *(&mut p as *mut PathBuf as *mut Vec<u8>) };
        for ext in FLATTENED_ALL_SUPPORTED_EXTENSIONS {
            let ext = ext.as_str_with_dot();
            v.extend_from_slice(ext.as_bytes());
            if self.is_file(p.as_path()) {
                return Ok(PathId::get(p.as_path()));
            }
            unsafe {
                v.set_len(v.len() - ext.len());
            }
        }
        Err(ResolveError::NotFound(PathId::get(p.as_path())))
    }
}

fn normalize_join(base: &Path, target: &Path) -> PathBuf {
    debug_assert!(base.is_normalized());
    let t = if bolt_ts_path::get_root_length(path_as_str(&target)) != 0 {
        target.normalize()
    } else {
        let mut result = base.to_path_buf();
        for comp in target.components() {
            match comp {
                std::path::Component::ParentDir => {
                    if result.components().count() > 1 {
                        // Don't go beyond root
                        result.pop();
                    }
                }
                std::path::Component::CurDir => {}
                _ => {
                    result.push(comp);
                }
            }
        }
        if target.need_trailing_slash() {
            result.as_mut_os_string().push("/");
        }
        result
    };

    debug_assert!(t.is_normalized());
    t
}

#[test]
fn test_normalize_join() {
    let should_eq = |base_dir: &str, target: &str, expected: &str| {
        let base = Path::new(base_dir);
        let target = Path::new(target);
        let res = normalize_join(base, target);
        let res = path_as_str(&res);
        assert_eq!(res, expected);
    };

    should_eq("/a/b/c", "d", "/a/b/c/d");
    should_eq("/a/b/c", "..", "/a/b/");
    should_eq("/a/b/c", "./", "/a/b/c/");
    should_eq("/a/b/c", "./.", "/a/b/c/");
    should_eq("/a/b/c", ".", "/a/b/c/");
    should_eq("/a/b/c", "./..", "/a/b/");
    should_eq("/a/b/c", "./d", "/a/b/c/d");
    should_eq("/a/b/c", "./d/", "/a/b/c/d/");
    should_eq("/a/b/c", "../d", "/a/b/d");
    should_eq("/a/b/c", "../../d", "/a/d");
    should_eq("/a/b/c", "../../../d", "/d");
    should_eq("/a/b/c", "../../../../d", "/d");
    should_eq("/a/b/c", "../../../../d/", "/d/");

    should_eq("/a", "/d", "/d");
    should_eq("/a", "/d/e", "/d/e");
    should_eq("/a", "/d/e/", "/d/e/");
}

mod errors;
mod from_dir;
mod from_spec_node_modules_dir;
mod normalize_join;
mod package_json;
mod parse_package_name;
mod resolution_kind_spec_loader;

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_config::Extension;
use bolt_ts_fs::{CachedFileSystem, PathId};
use bolt_ts_utils::path::{NormalizePath, path_as_str};
use bolt_ts_utils::{fx_hashmap_with_capacity, no_hashmap_with_capacity};
use package_json::PackageJsonInfoContents;
use rustc_hash::FxHashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

pub use self::errors::ResolveError;
use self::normalize_join::normalize_join;
use self::package_json::{PackageJsonInfo, PackageJsonInfoId};

pub type RResult<T> = Result<T, ResolveError>;
pub const NODE_MODULES_FOLDER: &str = "node_modules";
pub const COMMON_PACKAGE_FOLDERS: &[&str] =
    &[NODE_MODULES_FOLDER, "bower_components", "jspm_packages"];

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Extensions: u8 {
        /// `.ts`, `.tsx`, `.mts`, `.cts`
        const TypeScript    = 1 << 0;
        /// `.js`, `.jsx`, `.mjs`, `.cjs`
        const JavaScript    = 1 << 1;
        /// `.d.ts`
        const Declaration   = 1 << 2;
        /// `.json`
        const Json          = 1 << 3;
        const ImplementationFiles = Self::TypeScript.bits() | Self::JavaScript.bits();
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CacheKey {
    base_dir: PathId,
    target: AtomId,
}

pub struct Resolver<'atoms, FS: CachedFileSystem> {
    fs: Arc<Mutex<FS>>,
    atoms: Arc<Mutex<AtomMap<'atoms>>>,
    cache: Arc<Mutex<FxHashMap<CacheKey, RResult<PathId>>>>,
    package_json_arena: Arc<Mutex<Vec<PackageJsonInfo>>>,
    package_json_cache: Arc<Mutex<nohash_hasher::IntMap<PathId, PackageJsonInfoId>>>,
}

impl<'atoms, FS: CachedFileSystem> Resolver<'atoms, FS> {
    pub fn new(fs: Arc<Mutex<FS>>, atoms: Arc<Mutex<AtomMap<'atoms>>>) -> Self {
        Self {
            fs,
            atoms,
            cache: Arc::new(Mutex::new(fx_hashmap_with_capacity(1024 * 8))),
            package_json_arena: Arc::new(Mutex::new(Vec::with_capacity(1024 * 8))),
            package_json_cache: Arc::new(Mutex::new(no_hashmap_with_capacity(1024 * 8))),
        }
    }

    fn is_file(&self, p: &Path) -> bool {
        let fs = &mut *self.fs.lock().unwrap();
        fs.file_exists(p)
    }

    pub fn resolve(&self, base_dir: PathId, target: AtomId) -> RResult<PathId> {
        let cache_key = CacheKey { base_dir, target };
        if let Some(cached) = self.cache.lock().unwrap().get(&cache_key) {
            return *cached;
        }
        let result = self.try_resolve(base_dir, target);
        self.cache.lock().unwrap().insert(cache_key, result);
        result
    }

    fn try_resolve(&self, base_dir: PathId, target: AtomId) -> RResult<PathId> {
        let mut atoms = self.atoms.lock().unwrap();
        let module_name = atoms.get(target);
        if !bolt_ts_path::is_external_module_relative(module_name) {
            let ext = Extensions::TypeScript | Extensions::Declaration;
            drop(atoms);
            self.load_module_from_nearest_node_modules_dir(ext, target, base_dir)
        } else {
            let base_dir = atoms.get(base_dir.into());
            let candidate = {
                let base = Path::new(base_dir);
                debug_assert!(base.is_normalized());
                normalize_join(base, Path::new(module_name))
            };
            atoms.insert_if_not_exist(PathId::get(&candidate).into(), || {
                let bytes = candidate.as_os_str().as_encoded_bytes();
                let s = unsafe { String::from_utf8_unchecked(bytes.to_vec()) };
                std::borrow::Cow::Owned(s)
            });
            drop(atoms);
            self.node_load_module_by_relative_name(
                Extensions::TypeScript.union(Extensions::Declaration),
                candidate,
                false,
            )
        }
    }

    fn load_module_from_nearest_node_modules_dir(
        &self,
        ext: Extensions,
        module_name: AtomId,
        base_dir: PathId,
    ) -> RResult<PathId> {
        self._load_module_from_nearest_node_modules_dir(ext, module_name, base_dir, false)
    }

    fn _load_module_from_nearest_node_modules_dir(
        &self,
        ext: Extensions,
        module_name: AtomId,
        base_dir: PathId,
        types_scope_only: bool,
    ) -> RResult<PathId> {
        fn lookup(
            this: &Resolver<'_, impl CachedFileSystem>,
            ext: Extensions,
            base_dir_id: PathId,
            module_name_id: AtomId,
            types_scope_only: bool,
        ) -> RResult<PathId> {
            let atoms = this.atoms.lock().unwrap();
            let base_dir = Path::new(atoms.get(base_dir_id.into()));
            debug_assert!(base_dir.is_normalized());
            if base_dir
                .file_name()
                .is_none_or(|n| n.as_encoded_bytes() != NODE_MODULES_FOLDER.as_bytes())
            {
                let cache_key = CacheKey {
                    base_dir: base_dir_id,
                    target: module_name_id,
                };
                if let Some(cached) = this.cache.lock().unwrap().get(&cache_key) {
                    return *cached;
                }
                drop(atoms);
                if let Ok(res) = this.load_module_from_immediate_node_modules_dir(
                    ext,
                    base_dir_id,
                    module_name_id,
                    types_scope_only,
                ) {
                    return Ok(res);
                }
            }
            let mut atoms = this.atoms.lock().unwrap();
            let base_dir = Path::new(atoms.get(base_dir_id.into()));
            if let Some(parent) = base_dir.parent() {
                debug_assert!(parent.is_normalized());
                let parent_id = PathId::get(parent);
                if !atoms.contains(parent_id.into()) {
                    let bytes = parent.as_os_str().as_encoded_bytes();
                    let s = unsafe { String::from_utf8_unchecked(bytes.to_vec()) };
                    let s = std::borrow::Cow::Owned(s);
                    atoms.insert(parent_id.into(), s);
                }
                drop(atoms);
                lookup(this, ext, parent_id, module_name_id, types_scope_only)
            } else {
                Err(ResolveError::NotFound(module_name_id.into()))
            }
        }

        lookup(self, ext, base_dir, module_name, types_scope_only)
    }

    fn load_module_from_immediate_node_modules_dir(
        &self,
        ext: Extensions,
        dir_id: PathId,
        module_name: AtomId,
        types_scope_only: bool,
    ) -> RResult<PathId> {
        let mut atoms = self.atoms.lock().unwrap();
        let dir = Path::new(atoms.get(dir_id.into()));
        let node_modules_folder = dir.join(NODE_MODULES_FOLDER);
        debug_assert!(node_modules_folder.is_normalized());
        let node_modules_folder_id = PathId::get(node_modules_folder.as_path());
        atoms.insert_if_not_exist(node_modules_folder_id.into(), || {
            let bytes = node_modules_folder.as_os_str().as_encoded_bytes();
            let s = unsafe { String::from_utf8_unchecked(bytes.to_vec()) };
            std::borrow::Cow::Owned(s)
        });

        let mut fs = self.fs.lock().unwrap();
        let node_module_folder_exists = fs.dir_exists(&node_modules_folder);

        if !types_scope_only {
            drop(atoms);
            drop(fs);
            if let Ok(pkg) = self.load_module_from_spec_node_modules_dir(
                ext,
                node_modules_folder_id,
                module_name,
                node_module_folder_exists,
            ) {
                return Ok(pkg);
            }
        }

        if ext.intersects(Extensions::Declaration) {
            let node_modules_at_types = node_modules_folder.join("@types");
            let node_modules_at_types_id = PathId::get(node_modules_at_types.as_path());
            self.atoms
                .lock()
                .unwrap()
                .insert_if_not_exist(node_modules_at_types_id.into(), || {
                    let bytes = node_modules_at_types.as_os_str().as_encoded_bytes();
                    let s = unsafe { String::from_utf8_unchecked(bytes.to_vec()) };
                    std::borrow::Cow::Owned(s)
                });
            let mut node_modules_at_types_exists = node_module_folder_exists;
            if node_modules_at_types_exists
                && !self.fs.lock().unwrap().dir_exists(&node_modules_at_types)
            {
                node_modules_at_types_exists = false
            }
            // TODO:
            // self.load_module_from_spec_node_modules_dir(
            //     Extensions::Declaration,
            //     node_module_dir,
            //     module_name,
            //     node_modules_at_types_exists,
            // )
        }

        Err(ResolveError::NotFound(module_name.into()))
    }

    fn get_pkg_json_info(
        &self,
        pkg_dir: PathId,
        only_record_failures: bool,
    ) -> Option<PackageJsonInfoId> {
        let mut atoms = self.atoms.lock().unwrap();
        let pkg_dir = Path::new(atoms.get(pkg_dir.into()));
        debug_assert!(pkg_dir.is_normalized());
        let pkg_json_path = normalize_join(pkg_dir, "package.json");
        debug_assert!(pkg_json_path.is_normalized());
        let pkg_json_path_id = PathId::get(&pkg_json_path);
        if only_record_failures {
            debug_assert!(!pkg_json_path.exists());
            return None;
        }
        if let Some(existing) = self
            .package_json_cache
            .lock()
            .unwrap()
            .get(&pkg_json_path_id)
        {
            return Some(*existing);
        }

        let mut fs = self.fs.lock().unwrap();
        let dir_exists = fs.dir_exists(pkg_dir);
        if dir_exists && fs.file_exists(&pkg_json_path) {
            let package_dir = PathId::get(pkg_dir);
            let package_json_content = fs.read_file(&pkg_json_path, &mut atoms).unwrap();
            let c = atoms.get(package_json_content);
            let contents: PackageJsonInfoContents = serde_json::from_str(c).unwrap();
            let package_json = self.new_pkg_json(package_dir, contents);
            let prev = self
                .package_json_cache
                .lock()
                .unwrap()
                .insert(pkg_json_path_id, package_json);
            assert!(prev.is_none());
            Some(package_json)
        } else {
            None
        }
    }

    fn node_load_module_by_relative_name(
        &self,
        ext: Extensions,
        mut candidate: PathBuf,
        mut only_record_failures: bool,
    ) -> RResult<PathId> {
        debug_assert!(candidate.is_normalized());
        let save_len = candidate.as_os_str().len();
        if !bolt_ts_path::has_trailing_directory_separator(path_as_str(&candidate).as_bytes()) {
            if !only_record_failures {
                let parent_of_candidate = candidate.parent().unwrap();
                debug_assert!(parent_of_candidate.is_normalized());
                let fs = &mut self.fs.lock().unwrap();
                if !fs.dir_exists(parent_of_candidate) {
                    only_record_failures = true;
                }
            }

            if let Ok(resolved) =
                self.load_module_from_file(ext, &mut candidate, only_record_failures)
            {
                return Ok(resolved);
            }
        }

        if !only_record_failures {
            let candidate_exists = self.fs.lock().unwrap().dir_exists(&candidate);
            if !candidate_exists {
                only_record_failures = true;
            }
        }

        assert_eq!(
            save_len,
            candidate.as_os_str().len(),
            "The candidate path should not change length when checking for files."
        );
        self.load_node_module_from_dir(ext, candidate, only_record_failures, true)
    }

    fn load_node_module_from_dir(
        &self,
        ext: Extensions,
        mut candidate: PathBuf,
        only_record_failures: bool,
        consider_pkg_json: bool,
    ) -> RResult<PathId> {
        let package_info = if consider_pkg_json {
            self.get_pkg_json_info(PathId::get(&candidate), only_record_failures)
        } else {
            None
        };
        self.load_node_module_from_dir_worker(
            ext,
            &mut candidate,
            only_record_failures,
            package_info,
        )
    }

    fn load_module_from_file(
        &self,
        ext: Extensions,
        candidate: &mut PathBuf,
        only_record_failures: bool,
    ) -> RResult<PathId> {
        // a.js -> a.ts
        if let Some(Ok(resolved)) =
            self.load_module_from_file_no_implicit_ext(candidate, ext, only_record_failures)
        {
            return Ok(resolved);
        }

        // ./foo -> ./foo.{ext}
        self.try_adding_extension(candidate, ext, "", only_record_failures)
    }

    fn load_module_from_file_no_implicit_ext(
        &self,
        candidate: &mut PathBuf,
        ext: Extensions,
        only_record_failures: bool,
    ) -> Option<RResult<PathId>> {
        debug_assert!(candidate.is_normalized());
        let os_ext = candidate.extension()?.to_os_string();
        let save_len = candidate.as_os_str().len();
        candidate.set_extension("");
        let origin_extension = unsafe { std::str::from_utf8_unchecked(os_ext.as_encoded_bytes()) };
        let result =
            self.try_adding_extension(candidate, ext, origin_extension, only_record_failures);
        candidate.set_extension(os_ext);
        assert_eq!(candidate.as_os_str().len(), save_len);
        Some(result)
    }

    fn try_adding_extension(
        &self,
        candidate: &mut PathBuf,
        ext: Extensions,
        origin_extension: &str,
        only_record_failures: bool,
    ) -> RResult<PathId> {
        match origin_extension {
            "ts" | "dts" | "js" | "" => {
                let resolved_using_ts_ext = matches!(origin_extension, "ts" | "dts");
                if ext.intersects(Extensions::TypeScript) {
                    if let Ok(p) = self
                        .try_extension(candidate, Extension::Ts, resolved_using_ts_ext)
                        .or_else(|_| {
                            self.try_extension(candidate, Extension::Tsx, resolved_using_ts_ext)
                        })
                    {
                        return Ok(p);
                    }
                }
                if ext.intersects(Extensions::Declaration) {
                    if let Ok(p) =
                        self.try_extension(candidate, Extension::DTs, resolved_using_ts_ext)
                    {
                        return Ok(p);
                    }
                }
                if ext.intersects(Extensions::JavaScript) {
                    if let Ok(p) = self
                        .try_extension(candidate, Extension::Js, false)
                        .or_else(|_| self.try_extension(candidate, Extension::Jsx, false))
                    {
                        return Ok(p);
                    }
                }
                // TODO: is_config_lookup
                Err(ResolveError::NotFound(PathId::get(candidate)))
            }
            _ => {
                // None means unknown extension, such as `.png`
                // TODO: handle declaration
                Err(ResolveError::NotFound(PathId::get(candidate)))
            }
        }
    }

    fn try_extension(
        &self,
        candidate: &mut PathBuf,
        ext: Extension,
        only_record_failures: bool,
    ) -> RResult<PathId> {
        debug_assert!(candidate.is_normalized());
        let save_len = candidate.as_os_str().len();
        let v = unsafe { &mut *(candidate as *mut PathBuf as *mut Vec<u8>) };
        let ext = ext.as_str_with_dot();
        v.extend_from_slice(ext.as_bytes());
        let result = self.try_file(candidate, only_record_failures);
        unsafe {
            v.set_len(v.len() - ext.len());
        };
        assert_eq!(candidate.as_os_str().len(), save_len);
        result
    }

    fn try_file(&self, p: &std::path::Path, only_record_failures: bool) -> RResult<PathId> {
        // TODO: `config.module_suffix`
        debug_assert!(p.is_normalized());
        let id = PathId::get(p);
        let atoms = &mut self.atoms.lock().unwrap();
        atoms.insert_if_not_exist(id.into(), || {
            let bytes = p.as_os_str().as_encoded_bytes();
            let s = unsafe { String::from_utf8_unchecked(bytes.to_vec()) };
            std::borrow::Cow::Owned(s)
        });
        if self.is_file(p) {
            return Ok(id);
        }
        Err(ResolveError::NotFound(id))
    }
}

mod classic_name_resolver;
mod errors;
mod from_dir;
mod from_spec_node_modules_dir;
mod get_conditions;
mod get_node_resolution_features;
mod node_module_name_resolver;
mod normalize_join;
mod package_json;
mod parse_package_name;
mod resolution_cache;
mod resolution_kind_spec_loader;

use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_config::{Extension, Module, NormalizedModuleResolution};
use bolt_ts_fs::{CachedFileSystem, PathId};
use bolt_ts_path::is_external_module_relative;
use bolt_ts_utils::path::{NormalizePath, path_as_str};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

pub use self::errors::ResolveError;
use self::from_dir::load_node_module_from_directory_worker;
use self::from_spec_node_modules_dir::load_module_from_spec_node_modules_directory;
use self::get_conditions::get_conditions;
use self::node_module_name_resolver::bundler_module_name_resolver;
use self::node_module_name_resolver::node_module_name_resolver;
use self::node_module_name_resolver::node_next_module_name_resolver_worker;
use self::normalize_join::normalize_join;
pub use self::package_json::PackageJsonInfoContents;
use self::package_json::{PackageJsonInfo, PackageJsonPath};
pub use self::resolution_cache::ModuleResolutionCache;

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

    #[derive(Debug, Clone, Copy)]
    pub struct ResolveFlags: u8 {
        const PRESERVE_SYMLINKS     = 1 << 0;
        const NO_DTS_RESOLUTION     = 1 << 1;
        const RESOLVE_JSON_MODULE   = 1 << 2;
    }

    #[derive(Debug, Clone, Copy)]
    struct NodeResolutionFeatures: u8 {
        const IMPORTS                       = 1 << 0;
        const SELF_NAME                     = 1 << 1;
        const EXPORTS                       = 1 << 2;
        const EXPORTS_PATTERN_TRAILERS      = 1 << 3;
        const ESM_MODE                      = 1 << 4;
        const IMPORTS_PATTERN_ROOT          = 1 << 5;

        const ALL_FEATURES          = Self::IMPORTS.bits() | Self::SELF_NAME.bits() | Self::EXPORTS.bits() | Self::EXPORTS_PATTERN_TRAILERS.bits() | Self::IMPORTS_PATTERN_ROOT.bits();
        const NODE16_DEFAULT        = Self::IMPORTS.bits() | Self::SELF_NAME.bits() | Self::EXPORTS.bits() | Self::EXPORTS_PATTERN_TRAILERS.bits();
        const NODE_NEXT_DEFAULT     = Self::ALL_FEATURES.bits();
        const BUNDLER_DEFAULT       = Self::IMPORTS.bits() | Self::SELF_NAME.bits() | Self::EXPORTS.bits() | Self::EXPORTS_PATTERN_TRAILERS.bits() | Self::IMPORTS_PATTERN_ROOT.bits();
    }
}

impl ResolveFlags {
    #[inline]
    pub const fn no_dts_resolution(self) -> bool {
        self.contains(Self::NO_DTS_RESOLUTION)
    }

    #[inline]
    pub const fn preserve_symlinks(self) -> bool {
        self.contains(Self::PRESERVE_SYMLINKS)
    }

    #[inline]
    pub const fn resolve_json_module(self) -> bool {
        self.contains(Self::RESOLVE_JSON_MODULE)
    }
}

pub struct Resolver<FS: CachedFileSystem> {
    fs: Arc<Mutex<FS>>,
    atoms: Arc<Mutex<AtomIntern>>,
    cache: ModuleResolutionCache,
}

#[derive(Debug, Clone, Copy)]
struct Resolved {
    path: PathId,
}

#[derive(Debug, Clone, Copy)]
struct SearchResult {
    resolved: Resolved,
    is_external_library_import: bool,
}

pub struct ContainingFile {
    directory: PathId,
}

impl ContainingFile {
    pub fn new(directory: PathId) -> Self {
        Self { directory }
    }
}

impl<FS: CachedFileSystem> Resolver<FS> {
    pub fn resolve_module_name<'a>(
        module_name: Atom,
        containing_file: ContainingFile,
        options: ResolverOptions<'a>,
        cache: &'a ModuleResolutionCache,
        atoms: &'a Arc<Mutex<AtomIntern>>,
        fs: &'a Arc<Mutex<FS>>,
        resolution_mode: Option<ResolutionMode>,
    ) -> RResult<PathId> {
        if let Some(cached) =
            cache.get_from_directory_cache(module_name, resolution_mode, containing_file.directory)
        {
            return cached;
        }

        let result = match options.module_resolution {
            NormalizedModuleResolution::Node16 => node_next_module_name_resolver_worker(
                NodeResolutionFeatures::NODE16_DEFAULT,
                module_name,
                &containing_file,
                &options,
                atoms,
                fs,
                cache,
                resolution_mode,
                None,
            ),
            NormalizedModuleResolution::NodeNext => node_next_module_name_resolver_worker(
                NodeResolutionFeatures::NODE_NEXT_DEFAULT,
                module_name,
                &containing_file,
                &options,
                atoms,
                fs,
                cache,
                resolution_mode,
                None,
            ),
            NormalizedModuleResolution::Node10 => {
                // TODO: conditions can be empty?
                let conditions = resolution_mode.map(|mode| get_conditions(&options, Some(mode)));
                node_module_name_resolver(
                    module_name,
                    &containing_file,
                    &options,
                    atoms,
                    fs,
                    cache,
                    conditions,
                    false,
                )
            }
            NormalizedModuleResolution::Classic => {
                // TODO: classic
                todo!()
            }
            NormalizedModuleResolution::Bundler => {
                let conditions = resolution_mode.map(|mode| get_conditions(&options, Some(mode)));
                bundler_module_name_resolver(
                    module_name,
                    &containing_file,
                    &options,
                    atoms,
                    fs,
                    cache,
                    conditions,
                )
            }
        };
        if !cache.is_readonly() {
            cache.set_for_directory_cache(
                containing_file.directory,
                module_name,
                resolution_mode,
                result,
            );
            if !is_external_module_relative(atoms.lock().unwrap().get(module_name)) {
                cache.set_for_non_relative_name(
                    containing_file.directory,
                    module_name,
                    resolution_mode,
                    result,
                );
            }
        }
        result
    }
}
fn node_load_module_by_relative_name<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    mut candidate: PathBuf,
    mut only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    debug_assert!(candidate.is_normalized());
    let save_len = candidate.as_os_str().len();
    if !bolt_ts_path::has_trailing_directory_separator(path_as_str(&candidate).as_bytes()) {
        if !only_record_failures {
            let parent_of_candidate = candidate.parent().unwrap();
            debug_assert!(parent_of_candidate.is_normalized());
            let fs = &mut state.fs.lock().unwrap();
            let atoms = &mut state.atoms.lock().unwrap();
            if !fs.dir_exists(parent_of_candidate, atoms) {
                only_record_failures = true;
            }
        }

        if let Ok(resolved) =
            load_module_from_file(ext, &mut candidate, only_record_failures, state)
        {
            return Ok(resolved);
        }
    }

    if !only_record_failures {
        let fs = &mut state.fs.lock().unwrap();
        let atoms = &mut state.atoms.lock().unwrap();
        let candidate_exists = fs.dir_exists(&candidate, atoms);
        if !candidate_exists {
            only_record_failures = true;
        }
    }

    assert_eq!(
        save_len,
        candidate.as_os_str().len(),
        "The candidate path should not change length when checking for files, but actually changed to {:?}",
        candidate.display(),
    );
    load_node_module_from_directory(ext, candidate, only_record_failures, true, state)
}

fn load_node_module_from_directory<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    mut candidate: PathBuf,
    only_record_failures: bool,
    consider_pkg_json: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    let package_json = if consider_pkg_json {
        let pkg_dir = PathId::get(&candidate, state.atoms.lock().as_mut().unwrap());
        get_pkg_json_info(pkg_dir, only_record_failures, state)
    } else {
        None
    };
    load_node_module_from_directory_worker(
        ext,
        &mut candidate,
        only_record_failures,
        state,
        package_json,
    )
}
fn is_file<FS: CachedFileSystem>(
    fs: &Arc<Mutex<FS>>,
    atoms: &Arc<Mutex<AtomIntern>>,
    p: &Path,
) -> bool {
    let fs = &mut fs.lock().unwrap();
    let atoms = &mut atoms.lock().unwrap();
    fs.file_exists(p, atoms)
}

fn try_file_lookup<'a, 'options, FS: CachedFileSystem>(
    p: &std::path::Path,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    debug_assert!(p.is_normalized());
    let is_file = is_file(state.fs, state.atoms, p);
    let atoms = &mut state.atoms.lock().unwrap();
    let id = PathId::get(p, atoms);
    if is_file {
        Ok(id)
    } else {
        Err(ResolveError::NotFound(id))
    }
}
fn try_file<'a, 'options, FS: CachedFileSystem>(
    p: &std::path::Path,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    // TODO: `config.module_suffix`
    debug_assert!(p.is_normalized());
    try_file_lookup(p, only_record_failures, state)
}

fn try_adding_extension<'a, 'options, FS: CachedFileSystem>(
    candidate: &mut PathBuf,
    ext: Extensions,
    origin_extension: &str,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    fn try_extension<'a, 'options, FS: CachedFileSystem>(
        candidate: &mut PathBuf,
        ext: Extension,
        only_record_failures: bool,
        state: &ModuleResolutionState<'a, 'options, FS>,
    ) -> RResult<PathId> {
        debug_assert!(candidate.is_normalized());
        let save_len = candidate.as_os_str().len();
        let v = unsafe { &mut *(candidate as *mut PathBuf as *mut Vec<u8>) };
        let ext = ext.as_str_with_dot();
        v.extend_from_slice(ext.as_bytes());
        let result = try_file(candidate, only_record_failures, state);
        unsafe {
            v.set_len(v.len() - ext.len());
        };
        assert_eq!(candidate.as_os_str().len(), save_len);
        result
    }

    match origin_extension {
        "ts" | "d.ts" | "js" | "" => {
            let resolved_using_ts_ext = matches!(origin_extension, "ts" | "d.ts");
            if ext.contains(Extensions::TypeScript)
                && let Ok(p) = try_extension(candidate, Extension::Ts, resolved_using_ts_ext, state)
                    .or_else(|_| {
                        try_extension(candidate, Extension::Tsx, resolved_using_ts_ext, state)
                    })
            {
                return Ok(p);
            } else if ext.contains(Extensions::Declaration)
                && let Ok(p) =
                    try_extension(candidate, Extension::Dts, resolved_using_ts_ext, state)
            {
                return Ok(p);
            } else if ext.contains(Extensions::JavaScript)
                && let Ok(p) = try_extension(candidate, Extension::Js, false, state)
                    .or_else(|_| try_extension(candidate, Extension::Jsx, false, state))
            {
                return Ok(p);
            }
            // TODO: is_config_lookup
            Err(ResolveError::NotFound(PathId::get(
                candidate,
                state.atoms.lock().as_mut().unwrap(),
            )))
        }
        _ => {
            // None means unknown extension, such as `.png`
            // TODO: handle declaration
            Err(ResolveError::NotFound(PathId::get(
                candidate,
                state.atoms.lock().as_mut().unwrap(),
            )))
        }
    }
}

pub fn remove_extension(p: &mut std::path::PathBuf, ext: Extension) {
    debug_assert!(Extension::file_extension_is(p, ext));
    let v = unsafe { &mut *(p as *mut PathBuf as *mut Vec<u8>) };
    v.truncate(v.len() - ext.as_str_with_dot().len());
}

pub fn add_extension(p: &mut std::path::PathBuf, ext: Extension) {
    let v = unsafe { &mut *(p as *mut PathBuf as *mut Vec<u8>) };
    v.extend_from_slice(ext.as_str_with_dot().as_bytes());
}

fn create_resolved_module_with_failed_lookup_locations_handing_symlink<
    'a,
    'options,
    FS: CachedFileSystem,
>(
    module_name: Atom,
    result: RResult<SearchResult>,
    state: ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    let result = result?;
    Ok(
        if !state.options.flags.preserve_symlinks()
            && result.is_external_library_import
            && !is_external_module_relative(state.atoms.lock().unwrap().get(module_name))
        {
            get_original_and_resolved_file_name(state.fs, state.atoms, result.resolved.path)
        } else {
            result.resolved.path
        },
    )
}

fn get_original_and_resolved_file_name(
    fs: &Arc<Mutex<impl bolt_ts_fs::CachedFileSystem>>,
    atoms: &Arc<Mutex<AtomIntern>>,
    file_name: PathId,
) -> PathId {
    let mut fs = fs.lock().unwrap();
    let p = Path::new(atoms.lock().unwrap().get(file_name.into()));
    fs.realpath(p, atoms.lock().as_mut().unwrap()).unwrap()
}

fn load_module_from_nearest_node_modules_directory<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    module_name: Atom,
    directory: PathId,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    load_module_from_nearest_node_modules_directory_worker(
        ext,
        module_name,
        directory,
        state,
        false,
    )
}

fn load_module_from_nearest_node_modules_directory_worker<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    module_name: Atom,
    base_dir: PathId,
    state: &ModuleResolutionState<'a, 'options, FS>,
    types_scope_only: bool,
) -> RResult<PathId> {
    let mode = if state.features.is_empty() {
        None
    } else if state.features.contains(NodeResolutionFeatures::ESM_MODE)
        || state.conditions.iter().any(|c| c == "import")
    {
        Some(Module::ESNext)
    } else {
        Some(Module::CommonJS)
    };
    fn lookup<'a, 'options, FS: CachedFileSystem>(
        mode: Option<Module>,
        state: &ModuleResolutionState<'a, 'options, FS>,
        ext: Extensions,
        base_dir_id: PathId,
        module_name_id: Atom,
        types_scope_only: bool,
    ) -> Option<PathId> {
        let base_dir = Path::new(state.atoms.lock().unwrap().get(base_dir_id.into()));
        debug_assert!(base_dir.is_normalized());
        if base_dir
            .file_name()
            .is_none_or(|n| n.as_encoded_bytes() != NODE_MODULES_FOLDER.as_bytes())
        {
            // this.cache.get_from_directory_cache(module_name_id, ext, base_dir_id);
            // if let Some(cached) = this.cache.lock().unwrap().get(&cache_key) {
            //     return *cached;
            // }
            if let Ok(res) = load_module_from_immediate_node_modules_directory(
                ext,
                module_name_id,
                base_dir_id,
                state,
                types_scope_only,
            ) {
                return Some(res);
            }
        }
        let base_dir = Path::new(state.atoms.lock().unwrap().get(base_dir_id.into()));
        if let Some(parent) = base_dir.parent() {
            debug_assert!(parent.is_normalized());
            let parent_id = PathId::get(parent, state.atoms.lock().as_mut().unwrap());
            let bytes = parent.as_os_str().as_encoded_bytes();
            let s = unsafe { std::str::from_utf8_unchecked(bytes) };
            state.atoms.lock().unwrap().atom(s);
            lookup(
                mode,
                state,
                ext,
                parent_id,
                module_name_id,
                types_scope_only,
            )
        } else {
            None
        }
    }

    let priority_exts = ext & Extensions::TypeScript.union(Extensions::Declaration);
    if !priority_exts.is_empty()
        && let Some(ret) = lookup(
            mode,
            state,
            priority_exts,
            base_dir,
            module_name,
            types_scope_only,
        )
    {
        return Ok(ret);
    }

    if !types_scope_only
        && let secondary_exts = ext & !Extensions::TypeScript.union(Extensions::Declaration)
        && !secondary_exts.is_empty()
        && let Some(ret) = lookup(
            mode,
            state,
            secondary_exts,
            base_dir,
            module_name,
            types_scope_only,
        )
    {
        return Ok(ret);
    }

    Err(ResolveError::NotFound(module_name.into()))
}

fn load_module_from_immediate_node_modules_directory<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    module_name: Atom,
    dir_id: PathId,
    state: &ModuleResolutionState<'a, 'options, FS>,
    types_scope_only: bool,
) -> RResult<PathId> {
    let dir = Path::new(state.atoms.lock().unwrap().get(dir_id.into()));
    let node_modules_folder = dir.join(NODE_MODULES_FOLDER);
    debug_assert!(node_modules_folder.is_normalized());
    let atom = {
        let bytes = node_modules_folder.as_os_str().as_encoded_bytes();
        unsafe { std::str::from_utf8_unchecked(bytes) }
    };
    let node_modules_folder_id = state.atoms.lock().unwrap().atom(atom).into();
    let node_module_folder_exists = state
        .fs
        .lock()
        .unwrap()
        .dir_exists(&node_modules_folder, state.atoms.lock().as_mut().unwrap());

    if !types_scope_only
        && let Ok(pkg) = load_module_from_spec_node_modules_directory(
            ext,
            module_name,
            node_modules_folder_id,
            node_module_folder_exists,
            state,
        )
    {
        return Ok(pkg);
    }

    if ext.contains(Extensions::Declaration) {
        // let node_modules_at_types = node_modules_folder.join("@types");
        // let bytes = node_modules_at_types.as_os_str().as_encoded_bytes();
        // let s = unsafe { std::str::from_utf8_unchecked(bytes) };
        // let node_modules_at_types_id = state.atoms.lock().unwrap().atom(s);
        // let mut node_modules_at_types_exists = node_module_folder_exists;
        // if node_modules_at_types_exists
        //     && !state.fs.lock().unwrap().dir_exists(&node_modules_at_types)
        // {
        //     node_modules_at_types_exists = false
        // }
        // TODO:
        // load_module_from_spec_node_modules_directory(
        //     Extensions::Declaration,
        //     node_module_dir,
        //     module_name,
        //     node_modules_at_types_exists,
        // )
    }

    Err(ResolveError::NotFound(module_name.into()))
}

fn get_pkg_json_info<'a, 'options, FS: CachedFileSystem>(
    package_directory: PathId,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> Option<PackageJsonPath> {
    let pkg_dir = Path::new(state.atoms.lock().unwrap().get(package_directory.into()));
    debug_assert!(pkg_dir.is_normalized());
    let pkg_json_path = normalize_join(pkg_dir, "package.json");
    debug_assert!(pkg_json_path.is_normalized());
    let pkg_json_path_id = PathId::get(&pkg_json_path, state.atoms.lock().as_mut().unwrap());
    if only_record_failures {
        debug_assert!(!pkg_json_path.exists());
        return None;
    }
    if let Some(existing) = state.cache.package_json_cache().get(pkg_json_path_id) {
        return Some(existing);
    }

    let mut fs = state.fs.lock().unwrap();
    let dir_exists = fs.dir_exists(pkg_dir, state.atoms.lock().as_mut().unwrap());
    if dir_exists && fs.file_exists(&pkg_json_path, state.atoms.lock().as_mut().unwrap()) {
        let package_json_content = fs
            .read_file(&pkg_json_path, state.atoms.lock().as_mut().unwrap())
            .unwrap();
        drop(fs);
        let c = state
            .atoms
            .lock()
            .as_mut()
            .unwrap()
            .get(package_json_content);
        let contents: PackageJsonInfoContents = serde_json::from_str(c).unwrap();
        let package_directory = PathId::get(pkg_dir, state.atoms.lock().as_mut().unwrap());
        let package_json_path = state.cache.package_json_cache().insert_package_json(
            pkg_json_path_id,
            PackageJsonInfo::new(package_directory, contents),
            &state.atoms,
        );
        Some(package_json_path)
    } else {
        None
    }
}

fn load_module_from_file<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    candidate: &mut PathBuf,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> RResult<PathId> {
    // a.js -> a.ts
    if let Some(Ok(resolved)) =
        load_module_from_file_no_implicit_extensions(candidate, ext, only_record_failures, state)
    {
        return Ok(resolved);
    }

    // ./foo -> ./foo.{ext}
    try_adding_extension(candidate, ext, "", only_record_failures, state)
}

fn load_module_from_file_no_implicit_extensions<'a, 'options, FS: CachedFileSystem>(
    candidate: &mut PathBuf,
    ext: Extensions,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> Option<RResult<PathId>> {
    debug_assert!(candidate.is_normalized());
    let filename = candidate.file_name()?;
    if !filename.as_encoded_bytes().contains(&b'.') {
        return None;
    }
    let candidate_ext = Extension::extension_of_file_name(filename);
    debug_assert!(Extension::file_extension_is(candidate, candidate_ext));
    let save_len = candidate.as_os_str().len();
    remove_extension(candidate, candidate_ext);
    let result = try_adding_extension(
        candidate,
        ext,
        candidate_ext.as_str(),
        only_record_failures,
        state,
    );
    add_extension(candidate, candidate_ext);
    debug_assert_eq!(
        candidate.as_os_str().len(),
        save_len,
        "candidate: {:?}",
        candidate.display()
    );
    Some(result)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolutionMode {
    ESNext,
    CommonJS,
}

pub struct ResolverOptions<'options> {
    pub module_resolution: NormalizedModuleResolution,
    pub custom_conditions: &'options [String],
    pub flags: ResolveFlags,
}

struct ModuleResolutionState<'a, 'options, FS: CachedFileSystem> {
    options: &'a ResolverOptions<'options>,
    atoms: &'a Arc<Mutex<AtomIntern>>,
    fs: &'a Arc<Mutex<FS>>,
    cache: &'a ModuleResolutionCache,
    failed_lookup_locations: Vec<PathId>,
    affecting_locations: Vec<PathId>,
    features: NodeResolutionFeatures,
    conditions: Vec<String>,
    request_containing_directory: PathId,
    is_config_lookup: bool,
    candidate_is_from_package_json_field: bool,
    resolved_package_directory: bool,
}

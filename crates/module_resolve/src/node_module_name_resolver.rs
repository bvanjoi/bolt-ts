use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

use bolt_ts_atom::Atom;
use bolt_ts_atom::AtomIntern;
use bolt_ts_config::NormalizedModuleResolution;
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_fs::PathId;
use bolt_ts_utils::path::NormalizePath;

use super::ContainingFile;
use super::Extensions;
use super::ModuleResolutionState;
use super::NodeResolutionFeatures;
use super::RResult;
use super::ResolutionMode;
use super::Resolved;
use super::ResolverOptions;
use super::SearchResult;
use super::create_resolved_module_with_failed_lookup_locations_handing_symlink;
use super::get_conditions::get_conditions;
use super::get_node_resolution_features::get_node_resolution_features;
use super::load_module_from_nearest_node_modules_directory;
use super::node_load_module_by_relative_name;
use super::normalize_join::normalize_join;
use super::resolution_cache::ModuleResolutionCache;

pub(super) fn node_module_name_resolver<'a, 'options, FS: CachedFileSystem>(
    module_name: Atom,
    containing_file: &ContainingFile,
    options: &ResolverOptions<'options>,
    atoms: &'a Arc<Mutex<AtomIntern>>,
    fs: &'a Arc<Mutex<FS>>,
    cache: &'a ModuleResolutionCache,
    conditions: Option<Vec<String>>,
    is_config_lookup: bool,
) -> RResult<PathId> {
    let ext = if is_config_lookup {
        Extensions::Json
    } else if options.flags.no_dts_resolution() {
        let mut ext = Extensions::ImplementationFiles;
        if options.flags.resolve_json_module() {
            ext |= Extensions::Json;
        }
        ext
    } else if options.flags.resolve_json_module() {
        Extensions::TypeScript
            .union(Extensions::JavaScript)
            .union(Extensions::Declaration)
            .union(Extensions::Json)
    } else {
        Extensions::TypeScript
            .union(Extensions::JavaScript)
            .union(Extensions::Declaration)
            .union(Extensions::Json)
    };
    let features = match &conditions {
        Some(_) => NodeResolutionFeatures::ALL_FEATURES,
        None => NodeResolutionFeatures::empty(),
    };
    node_module_name_resolver_worker(
        features,
        module_name,
        containing_file.directory,
        options,
        atoms,
        fs,
        cache,
        ext,
        is_config_lookup,
        conditions,
    )
}

pub(super) fn node_next_module_name_resolver_worker<'a, 'options, FS: CachedFileSystem>(
    features: NodeResolutionFeatures,
    module_name: Atom,
    containing_file: &ContainingFile,
    options: &ResolverOptions<'options>,
    atoms: &'a Arc<Mutex<AtomIntern>>,
    fs: &'a Arc<Mutex<FS>>,
    cache: &'a ModuleResolutionCache,
    resolution_mode: Option<ResolutionMode>,
    conditions: Option<Vec<String>>,
) -> RResult<PathId> {
    let esm_mode = if resolution_mode.is_some_and(|mode| mode == ResolutionMode::ESNext) {
        NodeResolutionFeatures::ESM_MODE
    } else {
        NodeResolutionFeatures::empty()
    };
    let mut ext = if options.flags.no_dts_resolution() {
        Extensions::ImplementationFiles
    } else {
        Extensions::TypeScript
            .union(Extensions::JavaScript)
            .union(Extensions::Declaration)
    };
    if options.flags.resolve_json_module() {
        ext |= Extensions::Json;
    };
    node_module_name_resolver_worker(
        features | esm_mode,
        module_name,
        containing_file.directory,
        options,
        atoms,
        fs,
        cache,
        ext,
        false,
        conditions,
    )
}

pub(super) fn bundler_module_name_resolver<'a, 'options, FS: CachedFileSystem>(
    module_name: Atom,
    containing_file: &ContainingFile,
    options: &ResolverOptions<'options>,
    atoms: &'a Arc<Mutex<AtomIntern>>,
    fs: &'a Arc<Mutex<FS>>,
    cache: &'a ModuleResolutionCache,
    conditions: Option<Vec<String>>,
) -> RResult<PathId> {
    let mut ext = if options.flags.no_dts_resolution() {
        Extensions::ImplementationFiles
    } else {
        Extensions::TypeScript
            .union(Extensions::JavaScript)
            .union(Extensions::Declaration)
    };
    if options.flags.resolve_json_module() {
        ext |= Extensions::Json;
    };
    let features = get_node_resolution_features(options);
    node_module_name_resolver_worker(
        features,
        module_name,
        containing_file.directory,
        options,
        atoms,
        fs,
        cache,
        ext,
        false,
        conditions,
    )
}

fn node_module_name_resolver_worker<'a, 'options, FS: CachedFileSystem>(
    features: NodeResolutionFeatures,
    module_name: Atom,
    containing_directory: PathId,
    options: &'a ResolverOptions<'options>,
    atoms: &'a Arc<Mutex<AtomIntern>>,
    fs: &'a Arc<Mutex<FS>>,
    cache: &'a ModuleResolutionCache,
    ext: Extensions,
    is_config_lookup: bool,
    conditions: Option<Vec<String>>,
) -> RResult<PathId> {
    let conditions = match conditions {
        Some(conditions) => conditions,
        None => {
            let resolution_mode = if matches!(
                options.module_resolution,
                NormalizedModuleResolution::Bundler | NormalizedModuleResolution::Node10
            ) {
                None
            } else if features.contains(NodeResolutionFeatures::ESM_MODE) {
                Some(ResolutionMode::ESNext)
            } else {
                Some(ResolutionMode::CommonJS)
            };
            get_conditions(options, resolution_mode)
        }
    };
    let state = ModuleResolutionState {
        features,
        conditions,
        options,
        atoms,
        fs,
        is_config_lookup,
        candidate_is_from_package_json_field: false,
        resolved_package_directory: std::cell::Cell::new(false),
        failed_lookup_locations: vec![],
        affecting_locations: vec![],
        request_containing_directory: containing_directory,
    };
    let resolved = if options.module_resolution == NormalizedModuleResolution::Node10 {
        try_resolve(module_name, containing_directory, &state, features, cache)
        // TODO:
        // let priority_extensions = ext & Extensions::TypeScript.union(Extensions::Declaration);
        // let secondary_extensions = ext & !Extensions::TypeScript.union(Extensions::Declaration);
        // if !priority_extensions.is_empty() {
        //     try_resolve(module_name, containing_directory)
        // } else if
    } else {
        try_resolve(module_name, containing_directory, &state, features, cache)
    };

    create_resolved_module_with_failed_lookup_locations_handing_symlink(
        module_name,
        resolved,
        state,
    )
}

fn try_resolve<'a, 'options, FS: CachedFileSystem>(
    module_name: Atom,
    containing_directory: PathId,
    state: &ModuleResolutionState<'a, 'options, FS>,
    features: NodeResolutionFeatures,
    cache: &ModuleResolutionCache,
) -> RResult<SearchResult> {
    let atoms = state.atoms.lock().unwrap();
    debug_assert!(Path::new(atoms.get(containing_directory.into())).is_normalized());
    let module_name_str = atoms.get(module_name);
    drop(atoms);
    let is_external_module_relative = bolt_ts_path::is_external_module_relative(module_name_str);
    if !is_external_module_relative {
        if features.contains(NodeResolutionFeatures::SELF_NAME) {
            // TODO:
        }
        if module_name_str.contains(':') {
            // TODO:
        }

        let ext = Extensions::TypeScript.union(Extensions::Declaration);
        let ret = load_module_from_nearest_node_modules_directory(
            ext,
            module_name,
            containing_directory,
            state,
            cache,
        );
        if ext.contains(Extensions::Declaration) {
            // TODO:
        }
        let resolved = Resolved { path: ret? };
        Ok(SearchResult {
            resolved,
            is_external_library_import: true,
        })
    } else {
        let containing_directory = state.atoms.lock().unwrap().get(containing_directory.into());
        let candidate = {
            let base = Path::new(containing_directory);
            debug_assert!(base.is_normalized());
            normalize_join(base, Path::new(module_name_str))
        };
        let bytes = candidate.as_os_str().as_encoded_bytes();
        let s = unsafe { std::str::from_utf8_unchecked(bytes) };
        state.atoms.lock().unwrap().atom(s);
        let ret = node_load_module_by_relative_name(
            Extensions::TypeScript.union(Extensions::Declaration),
            candidate,
            false,
            state,
            cache,
        )?;
        let resolved = Resolved { path: ret };
        Ok(SearchResult {
            resolved,
            is_external_library_import: false,
        })
    }
}

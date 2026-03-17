use bolt_ts_config::SUPPORTED_DECLARATION_EXTENSIONS;
use bolt_ts_config::SUPPORTED_TS_IMPLEMENTATION_EXTENSIONS;
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_fs::PathId;
use bolt_ts_utils::path::NormalizePath;

use super::Extensions;
use super::ModuleResolutionCache;
use super::ModuleResolutionState;
use super::RResult;
use super::load_module_from_file;
use super::load_module_from_file_no_implicit_extensions;
use super::normalize_join::normalize_join;
use super::package_json::PackageJsonPath;
use super::resolution_kind_spec_loader::ResolutionKindSpecLoader;
use super::try_file;

struct Loader {
    package_json: Option<PackageJsonPath>,
}

impl<'a, 'options, FS: bolt_ts_fs::CachedFileSystem> ResolutionKindSpecLoader<'a, 'options, FS>
    for Loader
{
    fn loader(
        &self,
        ext: Extensions,
        candidate: &mut std::path::PathBuf,
        only_record_failures: bool,
        state: &ModuleResolutionState<'a, 'options, FS>,
        _: &ModuleResolutionCache,
    ) -> Option<RResult<PathId>> {
        load_filename_from_package_json_field(
            ext,
            candidate,
            self.package_json,
            only_record_failures,
            state,
        )
    }
}

pub(super) fn load_filename_from_package_json_field<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    candidate: &mut std::path::PathBuf,
    package_json: Option<PackageJsonPath>,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
) -> Option<RResult<PathId>> {
    if ((ext.contains(Extensions::TypeScript)
        && candidate.extension().is_some_and(|e| {
            SUPPORTED_TS_IMPLEMENTATION_EXTENSIONS
                .iter()
                .any(|t| t.as_str().as_bytes() == e.as_encoded_bytes())
        }))
        || (ext.contains(Extensions::Declaration)
            && candidate.extension().is_some_and(|e| {
                SUPPORTED_DECLARATION_EXTENSIONS
                    .iter()
                    .any(|t| t.as_str().as_bytes() == e.as_encoded_bytes())
            })))
        && let Ok(result) = try_file(candidate, only_record_failures, state)
    {
        return Some(Ok(result));
    }
    load_module_from_file_no_implicit_extensions(candidate, ext, only_record_failures, state)
}

pub(super) fn load_node_module_from_directory_worker<'a, 'options, FS: CachedFileSystem>(
    ext: Extensions,
    candidate: &mut std::path::Path,
    only_record_failures: bool,
    state: &ModuleResolutionState<'a, 'options, FS>,
    package_json: Option<PackageJsonPath>,
    cache: &'a ModuleResolutionCache,
) -> RResult<PathId> {
    debug_assert!(candidate.is_normalized());
    let mut package_file: Option<std::path::PathBuf> = None;
    if let Some(package_json) = package_json {
        let package_directory = cache
            .package_json_cache()
            .package_json_directory(package_json);
        if package_directory == PathId::get(candidate, state.atoms.lock().as_mut().unwrap()) {
            // TODO: is_config_lookup
            if ext.contains(Extensions::Declaration)
                && let Some(p) = cache
                    .package_json_cache()
                    .read_package_json_types_filed(package_json, state.atoms)
            {
                package_file = Some(p)
            }
        }
    }
    debug_assert!(package_file.as_ref().is_none_or(|p| p.is_normalized()));

    let only_record_failures_for_package_file = package_file.as_ref().and_then(|p| {
        let p = p.parent()?;
        Some(
            !state
                .fs
                .lock()
                .unwrap()
                .dir_exists(p, state.atoms.lock().as_mut().unwrap()),
        )
    });
    let only_record_failures_for_index = only_record_failures
        || !state
            .fs
            .lock()
            .unwrap()
            .dir_exists(candidate, state.atoms.lock().as_mut().unwrap());
    if let Some(Ok(package_file_result)) = package_file.and_then(|mut package_file| {
        let loader = Loader { package_json };
        loader.loader(
            ext,
            &mut package_file,
            only_record_failures_for_package_file.unwrap(),
            state,
            cache,
        )
    }) {
        return Ok(package_file_result);
    }

    let mut index_path = normalize_join(candidate, "index");
    load_module_from_file(ext, &mut index_path, only_record_failures_for_index, state)
}

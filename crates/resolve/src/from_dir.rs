use bolt_ts_config::SUPPORTED_DECLARATION_EXTENSIONS;
use bolt_ts_config::SUPPORTED_TS_IMPLEMENTATION_EXTENSIONS;
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_fs::PathId;
use bolt_ts_utils::path::NormalizePath;

use crate::package_json::PackageJsonInfoId;

use super::Extensions;
use super::RResult;
use super::normalize_join::normalize_join;
use super::resolution_kind_spec_loader::ResolutionKindSpecLoader;

struct Loader {
    package_json: Option<PackageJsonInfoId>,
}

impl<FS: bolt_ts_fs::CachedFileSystem> ResolutionKindSpecLoader<'_, FS> for Loader {
    fn loader(
        &self,
        resolver: &super::Resolver<FS>,
        ext: Extensions,
        candidate: &mut std::path::PathBuf,
        only_record_failures: bool,
    ) -> RResult<PathId> {
        resolver.load_filename_from_package_json_field(
            ext,
            candidate,
            self.package_json,
            only_record_failures,
        )
    }
}

impl<FS: CachedFileSystem> super::Resolver<FS> {
    fn load_filename_from_package_json_field(
        &self,
        ext: Extensions,
        candidate: &mut std::path::Path,
        package_json: Option<PackageJsonInfoId>,
        only_record_failures: bool,
    ) -> RResult<PathId> {
        if (ext.intersects(Extensions::TypeScript)
            && candidate.extension().is_some_and(|e| {
                SUPPORTED_TS_IMPLEMENTATION_EXTENSIONS
                    .iter()
                    .any(|t| t.as_str().as_bytes() == e.as_encoded_bytes())
            }))
            || (ext.intersects(Extensions::Declaration)
                && candidate.extension().is_some_and(|e| {
                    SUPPORTED_DECLARATION_EXTENSIONS
                        .iter()
                        .any(|t| t.as_str().as_bytes() == e.as_encoded_bytes())
                }))
        {
            if let Ok(result) = self.try_file(candidate, only_record_failures) {
                return Ok(result);
            }
        }
        self.load_node_module_from_dir_worker(ext, candidate, only_record_failures, package_json)
    }

    pub(super) fn load_node_module_from_dir_worker(
        &self,
        ext: Extensions,
        candidate: &mut std::path::Path,
        only_record_failures: bool,
        package_json: Option<PackageJsonInfoId>,
    ) -> RResult<PathId> {
        debug_assert!(candidate.is_normalized());
        let mut package_file: Option<std::path::PathBuf> = None;
        if let Some(package_json) = package_json {
            let arena = self.package_json_arena.lock().unwrap();
            let info = &arena[package_json.as_usize()];
            let dir = info.dir;
            drop(arena);
            if dir == PathId::get(candidate, self.atoms.lock().as_mut().unwrap()) {
                // TODO: is_config_lookup
                if ext.intersects(Extensions::Declaration) {
                    if let Some(p) = self.read_package_json_types_filed(package_json, dir) {
                        package_file = Some(p)
                    }
                }
            }
        }
        debug_assert!(package_file.as_ref().is_none_or(|p| p.is_normalized()));

        let only_record_failures_for_package_file = package_file.as_ref().and_then(|p| {
            let p = p.parent()?;
            Some(
                !self
                    .fs
                    .lock()
                    .unwrap()
                    .dir_exists(p, self.atoms.lock().as_mut().unwrap()),
            )
        });
        let only_record_failures_for_index = only_record_failures
            || !self
                .fs
                .lock()
                .unwrap()
                .dir_exists(candidate, self.atoms.lock().as_mut().unwrap());
        if let Some(Ok(package_file_result)) = package_file.map(|mut package_file| {
            let loader = Loader { package_json };
            loader.loader(
                self,
                ext,
                &mut package_file,
                only_record_failures_for_package_file.unwrap(),
            )
        }) {
            return Ok(package_file_result);
        }

        let mut index_path = normalize_join(candidate, "index");
        self.load_module_from_file(ext, &mut index_path, only_record_failures_for_index)
    }
}

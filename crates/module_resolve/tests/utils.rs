use std::sync::{Arc, Mutex};

use bolt_ts_fs::PathId;
use bolt_ts_module_resolve::ResolveFlags;
use bolt_ts_module_resolve::{ResolveError, Resolver};
use compile_test::TempDir;

#[cfg(test)]
fn build_fs(atoms: &mut bolt_ts_atom::AtomIntern) -> bolt_ts_fs::LocalFS {
    bolt_ts_fs::LocalFS::new(atoms)
}

#[cfg(test)]
fn build_and_resolve(
    file: &std::path::Path,
    target: &str,
    options: &bolt_ts_config::NormalizedCompilerOptions,
) -> Result<String, ResolveError> {
    use bolt_ts_config::Extension;
    use bolt_ts_module_resolve::{ContainingFile, get_resolution_mode_for_usage_location};

    let file_ext = Extension::extension_of_file_name(file.as_os_str().as_encoded_bytes());

    let mut atoms = bolt_ts_atom::AtomIntern::prefill(&[]);
    let fs = build_fs(&mut atoms);
    let fs = Arc::new(Mutex::new(fs));

    let base_dir = file.parent().unwrap();
    let base_dir = PathId::new(base_dir, &mut atoms);
    let target = atoms.atom(target);

    let atoms = Arc::new(Mutex::new(atoms));
    let containing_file = ContainingFile::new(base_dir);
    let resolution_mode = get_resolution_mode_for_usage_location(file_ext, Some(options));
    let options = bolt_ts_module_resolve::ResolverOptions {
        module_resolution: *options.module_resolution(),
        custom_conditions: options.custom_conditions(),
        flags: ResolveFlags::empty(),
    };
    let cache = bolt_ts_module_resolve::ModuleResolutionCache::new();
    let ret = Resolver::resolve_module_name(
        target,
        containing_file,
        options,
        &cache,
        &atoms,
        &fs,
        resolution_mode,
    )?;
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();
    Ok(atoms.get(ret.into()).to_string())
}

#[cfg(test)]
#[allow(dead_code)]
#[track_caller]
fn should_eq_worker(
    from: &std::path::Path,
    target: &str,
    expected: std::path::PathBuf,
    options: &bolt_ts_config::NormalizedCompilerOptions,
) {
    use bolt_ts_utils::path::NormalizePath;

    let ret = build_and_resolve(from, target, options).unwrap();
    assert!(std::path::PathBuf::from(&ret).is_normalized());
    let expected = expected.normalize();
    assert_eq!(ret, expected.to_string_lossy());
}

#[cfg(test)]
#[allow(dead_code)]
pub fn should_eq(from: &std::path::Path, target: &str, expected: std::path::PathBuf) {
    let options = serde_json::json!({
        "compilerOptions": {
            "moduleResolution": "node16"
        }
    });
    let options = serde_json::from_value::<bolt_ts_config::RawCompilerOptions>(options).unwrap();
    let options = options.normalize();
    should_eq_worker(from, target, expected, &options);
}

#[cfg(test)]
#[allow(dead_code)]
pub fn should_not_found(from: &std::path::Path, target: &str) {
    let options = serde_json::json!({
        "compilerOptions": {
            "moduleResolution": "node16"
        }
    });
    let options = serde_json::from_value::<bolt_ts_config::RawCompilerOptions>(options).unwrap();
    let options = options.normalize();
    let res = build_and_resolve(from, target, &options);
    assert!(matches!(res, Err(ResolveError::NotFound(_))))
}

#[cfg(test)]
#[allow(dead_code)]
pub struct Project {
    tsconfig: bolt_ts_config::NormalizedTsConfig,
    dir: TempDir,
}

impl Project {
    #[cfg(test)]
    #[allow(dead_code)]
    pub fn new(tsconfig: bolt_ts_config::NormalizedTsConfig, dir: TempDir) -> Self {
        Self { tsconfig, dir }
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub fn dir_path(&self) -> &std::path::Path {
        self.dir.path()
    }

    #[cfg(test)]
    #[allow(dead_code)]
    #[track_caller]
    pub fn should_eq(&self, from: &std::path::Path, target: &str, expected: std::path::PathBuf) {
        let options = self.tsconfig.compiler_options();
        should_eq_worker(from, target, expected, options);
    }
}

use std::sync::{Arc, Mutex};

use bolt_ts_fs::PathId;
use bolt_ts_middle::Extension;
use bolt_ts_module_resolve::ResolveFlags;
use bolt_ts_module_resolve::{ResolveError, Resolver};

use compile_test::TempDir;

fn build_fs(atoms: &mut bolt_ts_atom::AtomIntern) -> bolt_ts_fs::LocalFS {
    bolt_ts_fs::LocalFS::new(atoms)
}

pub struct ResolveResult {
    pub result: Result<String, ResolveError>,
    pub counter: bolt_ts_fs::Counter,
}

fn build_and_resolve(
    file: &std::path::Path,
    target: &str,
    options: &bolt_ts_config::NormalizedCompilerOptions,
) -> ResolveResult {
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
    );
    #[cfg(debug_assertions)]
    let counter = fs.lock().unwrap().steal_counter();
    #[cfg(not(debug_assertions))]
    let counter = unreachable!();
    match ret {
        Ok(ret) => {
            let atoms = Arc::try_unwrap(atoms).unwrap();
            let atoms = atoms.into_inner().unwrap();
            let result = atoms.get(ret.into()).to_string();
            ResolveResult {
                result: Ok(result),
                counter,
            }
        }
        Err(error) => ResolveResult {
            result: Err(error),
            counter,
        },
    }
}

#[track_caller]
fn should_eq_worker(
    from: &std::path::Path,
    target: &str,
    expected: std::path::PathBuf,
    options: &bolt_ts_config::NormalizedCompilerOptions,
) -> bolt_ts_fs::Counter {
    use bolt_ts_utils::path::NormalizePath;
    let result = build_and_resolve(from, target, options);
    match result.result {
        Ok(ret) => {
            assert!(std::path::PathBuf::from(&ret).is_normalized());
            let expected = expected.normalize();
            assert_eq!(ret, expected.to_string_lossy());
            result.counter
        }
        Err(_) => unreachable!(),
    }
}

pub fn should_eq(
    from: &std::path::Path,
    target: &str,
    expected: std::path::PathBuf,
) -> bolt_ts_fs::Counter {
    let options = serde_json::json!({
        "compilerOptions": {
            "moduleResolution": "node16"
        }
    });
    let options = serde_json::from_value::<bolt_ts_config::RawCompilerOptions>(options).unwrap();
    let options = options.normalize();
    should_eq_worker(from, target, expected, &options)
}

pub fn should_eq_with_counter(
    from: &std::path::Path,
    target: &str,
    expected: std::path::PathBuf,
    expected_counter: expect_test::Expect,
) {
    let counter = should_eq(from, target, expected);
    expected_counter.assert_debug_eq(&counter);
}

pub fn should_not_found(from: &std::path::Path, target: &str) {
    let options = serde_json::json!({
        "compilerOptions": {
            "moduleResolution": "node16"
        }
    });
    let options = serde_json::from_value::<bolt_ts_config::RawCompilerOptions>(options).unwrap();
    let options = options.normalize();
    let res = build_and_resolve(from, target, &options);
    match res.result {
        Ok(ret) => {
            panic!("Expected NotFound, but got {:?}", ret);
        }
        Err(err) => {
            assert!(matches!(err, ResolveError::NotFound(_)));
        }
    }
}

pub struct Project {
    tsconfig: bolt_ts_config::NormalizedTsConfig,
    dir: TempDir,
}

impl Project {
    pub fn new(tsconfig: bolt_ts_config::NormalizedTsConfig, dir: TempDir) -> Self {
        Self { tsconfig, dir }
    }

    pub fn dir_path(&self) -> &std::path::Path {
        self.dir.path()
    }

    #[track_caller]
    pub fn should_eq(&self, from: &std::path::Path, target: &str, expected: std::path::PathBuf) {
        let options = self.tsconfig.compiler_options();
        should_eq_worker(from, target, expected, options);
    }
}

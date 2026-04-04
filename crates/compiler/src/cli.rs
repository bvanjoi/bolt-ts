use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_module_resolve::COMMON_PACKAGE_FOLDERS;
use bolt_ts_utils::path::NormalizePath;

pub(super) struct ConfigFileSpecs {
    include_specs: Vec<String>,
}

impl ConfigFileSpecs {
    pub(super) fn get_config_file_specs(tsconfig: &NormalizedTsConfig) -> ConfigFileSpecs {
        let include_specs = tsconfig.include().to_vec();
        ConfigFileSpecs { include_specs }
    }
}

fn match_files(
    path: &std::path::Path,
    exclude: Option<&[String]>,
    include: Option<&[String]>,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomIntern,
) -> Vec<std::path::PathBuf> {
    debug_assert!(path.is_normalized(), "'{path:#?}' is not normalized");
    let include = include
        .unwrap_or_default()
        .iter()
        .map(|s| {
            // TODO: join means we need to compare more the prefix part
            let p = path.join(s);
            p.normalize()
        })
        .collect::<Vec<_>>();
    let include = include
        .iter()
        .map(|p| p.to_str().unwrap())
        .collect::<Vec<_>>();
    let exclude = exclude
        .unwrap_or_default()
        .iter()
        .map(|s| {
            // TODO: join means we need to compare more the prefix part
            let p = path.join(s);
            p.normalize()
        })
        .collect::<Vec<_>>();
    let exclude = exclude
        .iter()
        .map(|p| p.to_str().unwrap())
        .collect::<Vec<_>>();
    fs.glob(path, &include, &exclude, atoms)
}

fn match_files_with_extensions(
    path: &std::path::Path,
    extensions: &[bolt_ts_middle::Extension],
    exclude: Option<&[String]>,
    include: Option<&[String]>,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomIntern,
) -> Vec<std::path::PathBuf> {
    let files = match_files(path, exclude, include, fs, atoms);
    files
        .into_iter()
        .filter(|p| {
            for ext in extensions {
                let Some(p_ext) = p.extension() else {
                    return false;
                };
                if p_ext == ext.as_str() {
                    return true;
                }
            }
            false
        })
        .collect::<Vec<_>>()
}

fn get_filenames_from_config_specs(
    config_file_specs: &ConfigFileSpecs,
    base_path: &std::path::Path,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomIntern,
) -> Vec<std::path::PathBuf> {
    let supported_extensions = &bolt_ts_middle::FLATTENED_ALL_SUPPORTED_EXTENSIONS;
    let exclude = COMMON_PACKAGE_FOLDERS
        .iter()
        .map(|s| format!("**/{s}/**/*"))
        .collect::<Vec<_>>();

    match_files_with_extensions(
        base_path,
        supported_extensions,
        Some(&exclude),
        Some(&config_file_specs.include_specs),
        fs,
        atoms,
    )
}

pub(super) fn get_filenames(
    config_file_specs: &ConfigFileSpecs,
    base_path: &std::path::Path,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomIntern,
) -> Vec<std::path::PathBuf> {
    get_filenames_from_config_specs(config_file_specs, base_path, fs, atoms)
}

// fn parse_json_config_file_content<'cx>(
//     tsconfig: &'cx NormalizedTsConfig,
//     tsconfig_filename: &std::path::Path,
//     fs: &mut impl CachedFileSystem,
//     atoms: &mut bolt_ts_atom::AtomMap,
// ) {
//     debug_assert!(tsconfig_filename.is_normalized());
//     let base_path_for_filenames = tsconfig_filename.parent().unwrap();
//     assert!(fs.is_dir(base_path_for_filenames, atoms));
//     let config_file_specs = ConfigFileSpecs::get_config_file_specs(&tsconfig, atoms);

//     let filenames = get_filenames(&config_file_specs, base_path_for_filenames, fs, atoms);
// }

#[cfg(test)]
fn assert_included_files(input_map: serde_json::Value, include_glob: &str, expected: &[&str]) {
    use compile_test::build_temp_files;
    let dir = build_temp_files(input_map);
    assert_included_files_under(&dir.path(), include_glob, expected);
}

#[cfg(test)]
fn assert_included_files_under(dir: &std::path::Path, include_glob: &str, expected: &[&str]) {
    let mut atoms = bolt_ts_atom::AtomIntern::prefill(&[]);
    let mut fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let f = match_files(
        &dir,
        None,
        Some(&[include_glob.to_string()]),
        &mut fs,
        &mut atoms,
    );
    assert_eq!(f.len(), expected.len());
    assert!(f.iter().all(|f| f.is_normalized_without_trailing_slash()));
    let files_len = f.len();
    let files = f
        .into_iter()
        .map(|file| file.display().to_string())
        .collect::<Vec<_>>();
    let set = rustc_hash::FxHashSet::from_iter(files.clone());
    assert_eq!(set.len(), files_len);
    for expect in expected {
        let expect = dir.join(expect);
        let expect = expect.normalize();
        let expect = expect.display().to_string();
        assert!(files.contains(&expect));
    }
}

#[test]
fn test_included_match_files() {
    let dir = compile_test::build_temp_files(serde_json::json!(
        {
            "./src/index.ts": "",
            "./src/a.ts": "",
            "./src/b.js": "",
        }
    ));
    assert_included_files_under(
        &dir.path(),
        "./**/*",
        &["./src/index.ts", "./src/a.ts", "./src/b.js"],
    );
    // assert_included_files_under(&dir.path(), "./src/*.ts", &["./src/index.ts", "./src/a.ts"]);
}

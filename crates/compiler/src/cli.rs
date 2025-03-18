use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_resolve::COMMON_PACKAGE_FOLDERS;
use normalize_path::NormalizePath;

pub(super) struct ConfigFileSpecs {
    include_specs: Vec<String>,
}

impl ConfigFileSpecs {
    pub(super) fn get_config_file_specs<'cx>(tsconfig: &NormalizedTsConfig) -> ConfigFileSpecs {
        let include_specs = tsconfig
            .include()
            .iter()
            .map(|s| s.clone())
            .collect::<Vec<_>>();
        let specs = ConfigFileSpecs { include_specs };
        specs
    }
}

fn match_files<'cx>(
    path: &std::path::Path,
    extensions: &[bolt_ts_config::Extension],
    exclude: Option<&[String]>,
    include: Option<&[String]>,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomMap<'cx>,
) -> Vec<std::path::PathBuf> {
    debug_assert!(path.is_normalized(), "{:?}", path);
    let include = include
        .unwrap_or_default()
        .iter()
        .map(|s| {
            // TODO: join means we need to compare more the prefix part
            path.join(s)
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

            path.join(s)
        })
        .collect::<Vec<_>>();
    let exclude = exclude
        .iter()
        .map(|p| p.to_str().unwrap())
        .collect::<Vec<_>>();
    fs.glob(path, &include, &exclude, atoms)
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
        .collect()
}

fn get_filenames_from_config_specs<'cx>(
    config_file_specs: &ConfigFileSpecs,
    base_path: &std::path::Path,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomMap<'cx>,
) -> Vec<std::path::PathBuf> {
    let supported_extensions = &bolt_ts_config::FLATTENED_ALL_SUPPORTED_EXTENSIONS;
    let exclude = COMMON_PACKAGE_FOLDERS
        .iter()
        .map(|s| format!("**/{}/**/*", s))
        .collect::<Vec<_>>();

    match_files(
        base_path,
        supported_extensions,
        Some(&exclude),
        Some(&config_file_specs.include_specs),
        fs,
        atoms,
    )
}

pub(super) fn get_filenames<'cx>(
    config_file_specs: &ConfigFileSpecs,
    base_path: &std::path::Path,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomMap<'cx>,
) -> Vec<std::path::PathBuf> {
    get_filenames_from_config_specs(config_file_specs, base_path, fs, atoms)
}

// fn parse_json_config_file_content<'cx>(
//     tsconfig: &'cx NormalizedTsConfig,
//     tsconfig_filename: &std::path::Path,
//     fs: &mut impl CachedFileSystem,
//     atoms: &mut bolt_ts_atom::AtomMap<'cx>,
// ) {
//     debug_assert!(tsconfig_filename.is_normalized());
//     let base_path_for_filenames = tsconfig_filename.parent().unwrap();
//     assert!(fs.is_dir(base_path_for_filenames, atoms));
//     let config_file_specs = ConfigFileSpecs::get_config_file_specs(&tsconfig, atoms);

//     let filenames = get_filenames(&config_file_specs, base_path_for_filenames, fs, atoms);
// }

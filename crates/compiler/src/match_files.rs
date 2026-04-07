use bolt_ts_fs::CachedFileSystem;
use bolt_ts_utils::path::NormalizePath;

fn match_files(
    path: &std::path::Path,
    exclude: Option<&[String]>,
    include: Option<&[String]>,
    fs: &mut impl CachedFileSystem,
    atoms: &mut bolt_ts_atom::AtomIntern,
) -> Vec<std::path::PathBuf> {
    debug_assert!(path.is_normalized(), "'{path:#?}' is not normalized");
    let Some(include) = include else {
        return vec![];
    };
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
        .map(|p| glob::Pattern::new(p.to_str().unwrap()).unwrap())
        .collect::<Vec<_>>();

    let mut matched = vec![];
    for item in include {
        let base_dir = bolt_ts_path::get_base_path(path, item);
        let include = path.join(item).normalize();
        let include = glob::Pattern::new(include.to_str().unwrap()).unwrap();
        matched.extend(bolt_ts_fs::glob(fs, &base_dir, &include, &&exclude, atoms));
    }
    matched
}

pub(super) fn match_files_with_extensions(
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

#[cfg(test)]
fn assert_included_files(input_map: serde_json::Value, include_glob: &str, expected: &[&str]) {
    {
        // real file system
        use compile_test::build_temp_files;
        let dir = build_temp_files(input_map.clone());
        let mut atoms = bolt_ts_atom::AtomIntern::prefill(&[]);
        let mut fs = bolt_ts_fs::LocalFS::new(&mut atoms);
        let f = match_files(
            &dir.path(),
            None,
            Some(&[include_glob.to_string()]),
            &mut fs,
            &mut atoms,
        );
        assert_included_files_worker(dir.path(), f, expected);
    }

    {
        // memory file system
        let serde_json::Value::Object(map) = input_map else {
            panic!("input_map should be an object");
        };
        let content_map = map.into_iter().map(|(mut k, v)| {
            let serde_json::Value::String(v) = v else {
                panic!("input_map should be an object");
            };
            assert!(k.starts_with("./"));
            k.remove(0);
            (k, v)
        });
        let mut atoms = bolt_ts_atom::AtomIntern::prefill(&[]);
        let mut fs = bolt_ts_fs::MemoryFS::new(content_map, &mut atoms).unwrap();
        let dir = std::path::PathBuf::from("/");
        let f = match_files(
            &dir,
            None,
            Some(&[include_glob.to_string()]),
            &mut fs,
            &mut atoms,
        );
        assert_included_files_worker(&dir, f, expected);
    }
}

#[cfg(test)]
fn assert_included_files_worker(
    dir: &std::path::Path,
    files: Vec<std::path::PathBuf>,
    expected: &[&str],
) {
    let f = files;
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
    let input_map = serde_json::json!(
        {
            "./src/index.ts": "",
            "./src/a.ts": "",
            "./src/b.js": "",
        }
    );

    assert_included_files(
        input_map.clone(),
        "./**/*",
        &["./src/index.ts", "./src/a.ts", "./src/b.js"],
    );
    assert_included_files(
        input_map.clone(),
        "**/*",
        &["./src/index.ts", "./src/a.ts", "./src/b.js"],
    );
    assert_included_files(
        input_map.clone(),
        "./src/*.ts",
        &["./src/index.ts", "./src/a.ts"],
    );
}

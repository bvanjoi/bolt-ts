use bolt_ts_atom::AtomIntern;
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_fs::{
    FsError::{self, *},
    MemoryFS,
};

use std::path::Path;

#[test]
fn test_mem_fs() {
    let json = serde_json::json!({
      "/a": "/a",
      "/b/c": "/b/c",
    });

    let atoms = &mut AtomIntern::prefill(&[]);
    let inputs: bolt_ts_utils::FxIndexMap<String, String> = serde_json::from_value(json).unwrap();
    let mut fs = MemoryFS::new(inputs.into_iter(), atoms).unwrap();

    let read_file = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomIntern| {
        fs.read_file(Path::new(path), atoms)
    };

    let res = read_file(&mut fs, "/a", atoms);
    assert!(res.is_ok_and(|id| atoms.get(id) == "/a"));

    let res = read_file(&mut fs, "/b/c", atoms);
    assert!(res.is_ok_and(|id| atoms.get(id) == "/b/c"));

    let res = read_file(&mut fs, "/", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotAFile(_))));

    let res = read_file(&mut fs, "/not-exist", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotFound(_))));

    let res = read_file(&mut fs, "/b", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotAFile(_))));

    let res = read_file(&mut fs, "/b/", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotFound(_))));

    let res = read_file(&mut fs, "/a/", atoms);
    assert!(res.is_err_and(|err| matches!(err, NotFound(_))));

    assert!(fs.file_exists(Path::new("/a"), atoms));
    assert!(fs.file_exists(Path::new("/b/c"), atoms));
    assert!(!fs.file_exists(Path::new("/"), atoms));
    assert!(!fs.file_exists(Path::new("/not-exist"), atoms));
    assert!(!fs.file_exists(Path::new("/a/"), atoms));
    assert!(!fs.file_exists(Path::new("/b"), atoms));
    assert!(!fs.file_exists(Path::new("/b/"), atoms));

    let read_dir_failed = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomIntern| match fs
        .read_dir(std::path::Path::new(path), atoms)
    {
        Ok(_) => unreachable!(),
        Err(err) => err,
    };

    let res = read_dir_failed(&mut fs, "/a", atoms);
    assert!(matches!(res, NotADir(_)));

    let read_dir = |fs: &mut MemoryFS, path: &str, atoms: &mut AtomIntern| match fs
        .read_dir(std::path::Path::new(path), atoms)
    {
        Ok(iter) => iter
            .map(|p| p.to_string_lossy().to_string())
            .collect::<Vec<_>>(),
        Err(err) => unreachable!("{:?}", err),
    };

    let res = read_dir(&mut fs, "/", atoms);
    assert_eq!(res.len(), 2);
    assert!(res.iter().find(|p| *p == "/a").is_some());
    assert!(res.iter().find(|p| *p == "/b").is_some());

    let res = read_dir(&mut fs, "/b", atoms);
    assert_eq!(res, vec!["/b/c"]);

    assert!(fs.dir_exists(Path::new("/"), atoms));
    assert!(fs.dir_exists(Path::new("/b"), atoms));
    assert!(!fs.dir_exists(Path::new("/a"), atoms));
    assert!(!fs.dir_exists(Path::new("/a/"), atoms));
    assert!(!fs.dir_exists(Path::new("/b/c"), atoms));
    assert!(!fs.dir_exists(Path::new("/not-exist"), atoms));
    assert!(!fs.dir_exists(Path::new("/not-exist/"), atoms));
}

#[test]
fn test_mem_fs_with_overlap_name_between_dir_and_file() {
    let json = serde_json::json!({
      "/a": "",
      "/a/b": "",
    });

    let atoms = &mut AtomIntern::prefill(&[]);
    let inputs: bolt_ts_utils::FxIndexMap<String, String> = serde_json::from_value(json).unwrap();
    let fs = MemoryFS::new(inputs.into_iter(), atoms);
    assert!(fs.is_err_and(|err| matches!(err, FsError::FileExists(_))));

    let json = serde_json::json!({
        "/a/b": "",
        "/a": "",
    });

    let atoms = &mut AtomIntern::prefill(&[]);
    let inputs: bolt_ts_utils::FxIndexMap<String, String> = serde_json::from_value(json).unwrap();
    let fs = MemoryFS::new(inputs.into_iter(), atoms);
    assert!(fs.is_err_and(|err| matches!(err, FsError::DirExists(_))));
}

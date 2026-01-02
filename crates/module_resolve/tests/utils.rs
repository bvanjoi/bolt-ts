use std::sync::{Arc, Mutex};

use bolt_ts_fs::{MemoryFS, PathId};
use bolt_ts_module_resolve::PackageJsonInfoContents;
use bolt_ts_module_resolve::ResolveFlags;
use bolt_ts_module_resolve::{ResolveError, Resolver};

#[cfg(test)]
fn build_memory_fs(atoms: &mut bolt_ts_atom::AtomIntern, map: serde_json::Value) -> MemoryFS {
    let mut content_map = bolt_ts_utils::FxIndexMap::default();
    let mut symlink_map = bolt_ts_utils::FxIndexMap::default();

    let map: bolt_ts_utils::FxIndexMap<String, String> = serde_json::from_value(map).unwrap();
    for (k, v) in map {
        const SYMLINK_PREFIX: &str = "symlink:";
        if v.starts_with(SYMLINK_PREFIX) {
            let target = &v[SYMLINK_PREFIX.len()..];
            assert!(target.starts_with('/'));
            assert!(std::path::Path::new(target).is_absolute());
            symlink_map.insert(k, target.to_string());
        } else {
            assert!(v.is_empty() || serde_json::from_str::<PackageJsonInfoContents>(&v).is_ok());
            content_map.insert(k, v);
        }
    }
    bolt_ts_fs::MemoryFS::new(content_map.into_iter(), symlink_map.into_iter(), atoms).unwrap()
}

#[cfg(test)]
fn build_and_resolve(
    map: serde_json::Value,
    base_dir: &str,
    target: &str,
) -> Result<String, ResolveError> {
    let mut atoms = bolt_ts_atom::AtomIntern::prefill(&[]);
    let fs = build_memory_fs(&mut atoms, map);
    let fs = Arc::new(Mutex::new(fs));

    let base_dir = PathId::new(std::path::Path::new(base_dir), &mut atoms);
    let target = atoms.atom(target);

    let atoms = Arc::new(Mutex::new(atoms));
    let resolver = Resolver::new(fs.clone(), atoms.clone(), ResolveFlags::empty());
    let ret = resolver.resolve(base_dir, target)?;
    drop(resolver);
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();
    Ok(atoms.get(ret.into()).to_string())
}

#[cfg(test)]
#[allow(dead_code)]
pub fn should_eq(map: serde_json::Value, base_dir: &str, target: &str, expected: &str) {
    let ret = build_and_resolve(map, base_dir, target).unwrap();
    assert_eq!(ret, expected);
}

#[cfg(test)]
#[allow(dead_code)]
pub fn should_not_found(map: serde_json::Value, base_dir: &str, target: &str) {
    let res = build_and_resolve(map, base_dir, target);
    assert!(matches!(res, Err(ResolveError::NotFound(_))))
}

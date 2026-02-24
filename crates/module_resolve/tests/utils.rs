use std::sync::{Arc, Mutex};

use bolt_ts_fs::PathId;
use bolt_ts_module_resolve::ResolveFlags;
use bolt_ts_module_resolve::{ResolveError, Resolver};
use compile_test::build_temp_files;

#[cfg(test)]
fn build_fs(atoms: &mut bolt_ts_atom::AtomIntern) -> bolt_ts_fs::LocalFS {
    bolt_ts_fs::LocalFS::new(atoms)
}

#[cfg(test)]
fn build_and_resolve(base_dir: &std::path::Path, target: &str) -> Result<String, ResolveError> {
    let mut atoms = bolt_ts_atom::AtomIntern::prefill(&[]);
    let fs = build_fs(&mut atoms);
    let fs = Arc::new(Mutex::new(fs));

    let base_dir = PathId::new(std::path::Path::new(base_dir), &mut atoms);
    let target = atoms.atom(target);

    let atoms = Arc::new(Mutex::new(atoms));
    let resolver = Resolver::new(fs.clone(), atoms.clone(), ResolveFlags::empty());
    let ret = resolver.resolve_module_name(base_dir, target, bolt_ts_config::ModuleKind::Node16)?;
    drop(resolver);
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();
    Ok(atoms.get(ret.into()).to_string())
}

#[cfg(test)]
#[allow(dead_code)]
pub fn should_eq(map: serde_json::Value, target: &str, expected: &str) {
    use bolt_ts_utils::path::NormalizePath;

    let temp = build_temp_files(map);
    let base_dir = temp.path();
    let ret = build_and_resolve(base_dir, target).unwrap();
    let expect = base_dir.join(expected).normalize();
    assert_eq!(ret, expect.to_string_lossy());
}

#[cfg(test)]
#[allow(dead_code)]
pub fn should_not_found(map: serde_json::Value, target: &str) {
    let temp = build_temp_files(map);
    let base_dir = temp.path();
    let res = build_and_resolve(base_dir, target);
    assert!(matches!(res, Err(ResolveError::NotFound(_))))
}

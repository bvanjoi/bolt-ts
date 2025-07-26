use std::sync::{Arc, Mutex};

use bolt_ts_fs::PathId;
use bolt_ts_resolve::{ResolveError, Resolver};

#[cfg(test)]
fn build_and_resolve(
    map: serde_json::Value,
    base_dir: &str,
    target: &str,
) -> Result<String, ResolveError> {
    let input = serde_json::from_value(map).unwrap();
    let mut atoms = bolt_ts_atom::AtomMap::prefill(&[]);
    let base_dir = PathId::new(std::path::Path::new(base_dir), &mut atoms);
    let target = atoms.atom(target);

    let fs = bolt_ts_fs::MemoryFS::new(input, &mut atoms).unwrap();

    let fs = Arc::new(Mutex::new(fs));
    let atoms = Arc::new(Mutex::new(atoms));
    let resolver = Resolver::new(fs.clone(), atoms.clone());
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

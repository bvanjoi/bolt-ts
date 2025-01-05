use std::sync::{Arc, Mutex};

use bolt_ts_fs::PathId;
use bolt_ts_resolve::Resolver;

pub fn should_eq(map: serde_json::Value, base_dir: &str, target: &str, expected: &str) {
    let input = serde_json::from_value(map).unwrap();
    let mut atoms = bolt_ts_atom::AtomMap::new(0);
    let base_dir = PathId::new(std::path::Path::new(base_dir), &mut atoms);
    let target = atoms.insert_by_str(target.into());

    let fs = bolt_ts_fs::MemoryFS::new(input, &mut atoms).unwrap();

    let fs = Arc::new(Mutex::new(fs));
    let atoms = Arc::new(Mutex::new(atoms));
    let resolver = Resolver::new(fs.clone(), atoms.clone());
    let ret = resolver.resolve(base_dir, target).unwrap();
    drop(resolver);
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();
    let ret = atoms.get(ret.into());
    assert_eq!(ret, expected);
}

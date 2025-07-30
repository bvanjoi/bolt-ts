use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_utils::path::NormalizePath;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct PathId(AtomId);
impl nohash_hasher::IsEnabled for PathId {}

impl From<PathId> for AtomId {
    fn from(val: PathId) -> Self {
        val.0
    }
}

impl From<AtomId> for PathId {
    fn from(val: AtomId) -> Self {
        PathId(val)
    }
}

impl PathId {
    fn _new(id: AtomId, atoms: &mut AtomMap) -> Self {
        debug_assert!(std::path::Path::new(&atoms.get(id)).is_normalized());
        Self(id)
    }

    pub fn new(p: &std::path::Path, atoms: &mut AtomMap) -> Self {
        if cfg!(target_arch = "wasm32") {
            assert!(p.has_root());
        } else {
            assert!(p.is_absolute(), "Path should be absolute, but got {p:?}");
        }
        let p = p.normalize();
        let slice = unsafe { std::str::from_utf8_unchecked(p.as_os_str().as_encoded_bytes()) };
        let atom = atoms.atom(slice);
        Self::_new(atom, atoms)
    }

    pub fn get(p: &std::path::Path, atoms: &mut AtomMap) -> Self {
        let slice = unsafe { std::str::from_utf8_unchecked(p.as_os_str().as_encoded_bytes()) };
        let atom = atoms.atom(slice);
        Self(atom)
    }
}

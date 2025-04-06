use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_path::NormalizePath;

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

impl<'a> PathId {
    pub const ROOT: Self = Self(AtomId::from_str("/"));

    fn _new(id: AtomId, atoms: &mut AtomMap<'a>) -> Self {
        debug_assert!(std::path::Path::new(&atoms.get(id)).is_normalized());
        Self(id)
    }

    pub fn new(p: &std::path::Path, atoms: &mut AtomMap<'a>) -> Self {
        let insert_by_slice = |slice: &[u8], atoms: &mut AtomMap| {
            let atom = AtomId::from_bytes(slice);
            atoms.insert_if_not_exist(atom, || unsafe {
                std::borrow::Cow::Owned(String::from_utf8_unchecked(slice.to_vec()))
            });
            Self::_new(atom, atoms)
        };
        if cfg!(target_arch = "wasm32") {
            assert!(p.has_root());
        } else {
            assert!(p.is_absolute(), "Path should be absolute, but got {p:?}");
        }
        let p = p.normalize();
        let slice = p.as_os_str().as_encoded_bytes();
        insert_by_slice(slice, atoms)
    }

    pub fn get(p: &std::path::Path) -> Self {
        let slice = p.as_os_str().as_encoded_bytes();
        let atom = AtomId::from_bytes(slice);
        Self(atom)
    }
}

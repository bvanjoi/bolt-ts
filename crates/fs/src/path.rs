use std::path::Path;

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

impl Into<PathId> for AtomId {
    fn into(self) -> PathId {
        PathId(self)
    }
}

impl<'a> PathId {
    pub const ROOT: Self = Self(AtomId::from_str("/"));

    fn is_regular_path(atom: AtomId, atoms: &AtomMap<'a>) -> bool {
        let p = atoms.get(atom);
        if p != "/" && p.ends_with("/") {
            // Path should not ends with slash
            false
        } else if !Path::new(p).is_normalized() {
            // "Path is not normalized"
            false
        } else {
            true
        }
    }

    fn _new(id: AtomId, atoms: &mut AtomMap<'a>) -> Self {
        debug_assert!(Self::is_regular_path(id, atoms));
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
        if p.is_normalized() {
            let mut slice = p.as_os_str().as_encoded_bytes();
            if slice == [b'/'] {
                insert_by_slice(slice, atoms)
            } else if let Some(b'/') = slice.last() {
                slice = &slice[..slice.len() - 1];
                insert_by_slice(slice, atoms)
            } else {
                insert_by_slice(slice, atoms)
            }
        } else {
            let p = p.normalize();
            let slice = p.as_os_str().as_encoded_bytes();
            insert_by_slice(slice, atoms)
        }
    }

    pub fn get(p: &std::path::Path) -> Self {
        let slice = p.as_os_str().as_encoded_bytes();
        let atom = AtomId::from_bytes(slice);
        // TODO: debug_assert!(Self::is_regular_path(atom, atoms));
        Self(atom)
    }
}

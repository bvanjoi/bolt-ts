use std::borrow::Cow;

use rustc_hash::FxHashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct AtomId(u64);

impl AtomId {
    pub const fn from_str(s: &str) -> Self {
        Self::from_bytes(s.as_bytes())
    }

    pub const fn from_bytes(bytes: &[u8]) -> Self {
        use xxhash_rust::const_xxh3::xxh3_64;
        Self(xxh3_64(bytes))
    }
}

#[derive(Debug, Default)]
pub struct AtomMap(FxHashMap<AtomId, String>);

impl AtomMap {
    pub fn insert_by_str(&mut self, value: String) -> AtomId {
        let id = AtomId::from_bytes(value.as_bytes());
        if self.0.get(&id).is_none() {
            self.insert(id, value)
        }
        id
    }

    pub fn insert_by_vec(&mut self, value: Vec<u8>) -> AtomId {
        self.insert_by_str(unsafe { String::from_utf8_unchecked(value) })
    }

    pub fn insert_if_not_exist(&mut self, atom: AtomId, lazy: impl FnOnce() -> String) {
        if self.0.get(&atom).is_none() {
            self.insert(atom, lazy());
        }
    }

    pub fn insert(&mut self, atom: AtomId, value: String) {
        let prev = self.0.insert(atom, value);
        assert!(prev.is_none());
    }

    pub fn get(&self, atom: AtomId) -> &str {
        self.0.get(&atom).unwrap()
    }

    pub fn eq_str(&self, atom: AtomId, s: &str) -> bool {
        self.get(atom) == s
    }
}

use std::borrow::Cow;

use rustc_hash::FxHashMap;

use crate::utils::fx_hashmap_with_capacity;

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

#[derive(Debug)]
pub struct AtomMap<'cx>(FxHashMap<AtomId, Cow<'cx, str>>);

impl<'cx> AtomMap<'cx> {
    pub fn new() -> Self {
        Self(fx_hashmap_with_capacity(1024 * 128))
    }

    pub fn insert_by_str(&mut self, value: Cow<'cx, str>) -> AtomId {
        let id = AtomId::from_bytes(value.as_bytes());
        if self.0.get(&id).is_none() {
            self.insert(id, value)
        }
        id
    }

    pub fn insert_by_vec(&mut self, value: Vec<u8>) -> AtomId {
        self.insert_by_str(unsafe { Cow::Owned(String::from_utf8_unchecked(value)) })
    }

    pub fn insert_if_not_exist(&mut self, atom: AtomId, lazy: impl FnOnce() -> Cow<'cx, str>) {
        if self.0.get(&atom).is_none() {
            self.insert(atom, lazy());
        }
    }

    pub fn insert(&mut self, atom: AtomId, value: Cow<'cx, str>) {
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

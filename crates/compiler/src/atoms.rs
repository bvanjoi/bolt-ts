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
pub struct AtomMap<'cx>(FxHashMap<AtomId, Cow<'cx, str>>);

impl<'cx> AtomMap<'cx> {
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

    pub fn insert(&mut self, atom: AtomId, value: Cow<'cx, str>) {
        let prev = self.0.insert(atom, value);
        assert!(prev.is_none());
    }

    pub fn get(&'cx self, atom: AtomId) -> &'cx str {
        self.0.get(&atom).unwrap()
    }

    pub fn eq_str(&self, atom: AtomId, s: &str) -> bool {
        self.get(atom) == s
    }
}

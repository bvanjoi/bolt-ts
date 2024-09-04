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
    pub fn insert(&mut self, atom: AtomId, value: String) {
        let prev = self.0.insert(atom, value);
        assert!(prev.is_none());
    }

    pub fn get(&self, atom: AtomId) -> Option<&String> {
        self.0.get(&atom)
    }
}

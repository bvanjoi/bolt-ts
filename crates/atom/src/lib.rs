use bolt_ts_utils::no_hashmap_with_capacity;
pub use paste;
use std::borrow::Cow;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
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

impl nohash_hasher::IsEnabled for AtomId {}

#[derive(Debug)]
pub struct AtomMap<'a>(nohash_hasher::IntMap<AtomId, Cow<'a, str>>);

impl<'a> AtomMap<'a> {
    pub fn new(capacity: usize) -> Self {
        let map = no_hashmap_with_capacity(capacity);
        Self(map)
    }

    pub fn insert_by_str(&mut self, value: Cow<'a, str>) -> AtomId {
        let id = AtomId::from_bytes(value.as_bytes());
        if !self.0.contains_key(&id) {
            self.insert(id, value)
        }
        id
    }

    pub fn insert_by_vec(&mut self, value: Vec<u8>) -> AtomId {
        self.insert_by_str(unsafe { Cow::Owned(String::from_utf8_unchecked(value)) })
    }

    pub fn insert_if_not_exist(&mut self, atom: AtomId, lazy: impl FnOnce() -> Cow<'a, str>) {
        if !self.0.contains_key(&atom) {
            self.insert(atom, lazy());
        }
    }

    pub fn insert(&mut self, atom: AtomId, value: Cow<'a, str>) {
        let prev = self.0.insert(atom, value);
        assert!(prev.is_none());
    }

    pub fn contains(&self, atom: AtomId) -> bool {
        self.0.contains_key(&atom)
    }

    #[track_caller]
    pub fn get(&self, atom: AtomId) -> &str {
        self.0
            .get(&atom)
            .unwrap_or_else(|| panic!("atom not found: {:?}", atom))
    }

    pub fn eq_str(&self, atom: AtomId, s: &str) -> bool {
        self.get(atom) == s
    }
}

/// Generate atoms and their corresponding AtomId.
///
/// ```
/// use bolt_ts_atom::{gen_atoms, paste, AtomId};
///
/// gen_atoms!(
///     ATOMS,
///     (IDENT_A, "a"),
/// );
/// ```
#[macro_export]
macro_rules! gen_atoms {
    ($owner: ident, $(($name:ident, $lit:literal)),* $(,)?) => {
        gen_atoms!($(($name, $lit)),*);
        pub const $owner: &[(&str, AtomId)] = &[$(($lit, $name),)*];
    };
    ($(($name:ident, $lit:literal)),* $(,)?) => {
        paste::paste! {
            $(pub const [<$name _STR>]: &str = $lit;)*
            $(pub const $name: AtomId = AtomId::from_str([<$name _STR>]);)*
        }
    };
}

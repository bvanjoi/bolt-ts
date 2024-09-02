use xxhash_rust::const_xxh3::xxh3_64;

use super::AtomId;

const fn h(s: &str) -> AtomId {
    AtomId(xxh3_64(s.as_bytes()))
}

pub static KEYWORD: &[(&str, AtomId)] = &[("false", h("false"))];

pub static IDENTIFIER: &[(&str, AtomId)] = &[];

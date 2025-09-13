#[macro_use]
mod index;

pub mod path;

pub type FxIndexMap<K, V> = indexmap::IndexMap<K, V, rustc_hash::FxBuildHasher>;
pub type FxIndexSet<V> = indexmap::IndexSet<V, rustc_hash::FxBuildHasher>;

pub type NoHashIndexMap<K, V> = indexmap::IndexMap<K, V, nohash_hasher::BuildNoHashHasher<K>>;
pub type NoHashIndexSet<V> = indexmap::IndexSet<V, nohash_hasher::BuildNoHashHasher<V>>;

pub fn fx_hashmap_with_capacity<K, V>(capacity: usize) -> rustc_hash::FxHashMap<K, V> {
    let hasher = rustc_hash::FxBuildHasher;
    rustc_hash::FxHashMap::with_capacity_and_hasher(capacity, hasher)
}

pub fn fx_hashset_with_capacity<V>(capacity: usize) -> rustc_hash::FxHashSet<V> {
    let hasher = rustc_hash::FxBuildHasher;
    rustc_hash::FxHashSet::with_capacity_and_hasher(capacity, hasher)
}

pub fn no_hashmap_with_capacity<K, V>(capacity: usize) -> nohash_hasher::IntMap<K, V> {
    let hasher = nohash_hasher::BuildNoHashHasher::default();
    nohash_hasher::IntMap::with_capacity_and_hasher(capacity, hasher)
}

pub fn no_hashset_with_capacity<V>(capacity: usize) -> nohash_hasher::IntSet<V> {
    let hasher = nohash_hasher::BuildNoHashHasher::default();
    nohash_hasher::IntSet::with_capacity_and_hasher(capacity, hasher)
}

pub fn fx_indexmap_with_capacity<K, V>(capacity: usize) -> FxIndexMap<K, V> {
    let hasher = rustc_hash::FxBuildHasher;
    indexmap::IndexMap::with_capacity_and_hasher(capacity, hasher)
}

pub fn fx_indexset_with_capacity<V>(capacity: usize) -> FxIndexSet<V> {
    let hasher = rustc_hash::FxBuildHasher;
    indexmap::IndexSet::with_capacity_and_hasher(capacity, hasher)
}

pub fn ensure_sufficient_stack<R, F: FnOnce() -> R>(f: F) -> R {
    const RED_ZONE: usize = 100 * 1024; // 100k
    const STACK_PER_RECURSION: usize = 1024 * 1024; // 1MB
    stacker::maybe_grow(RED_ZONE, STACK_PER_RECURSION, f)
}

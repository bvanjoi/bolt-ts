#[macro_use]
mod index;

pub mod path;

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

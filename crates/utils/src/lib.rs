#[macro_use]
mod index;

pub fn fx_hashmap_with_capacity<K, V>(capacity: usize) -> rustc_hash::FxHashMap<K, V> {
    let hasher = rustc_hash::FxBuildHasher;
    rustc_hash::FxHashMap::with_capacity_and_hasher(capacity, hasher)
}

pub fn fx_hashset_with_capacity<V>(capacity: usize) -> rustc_hash::FxHashSet<V> {
    let hasher = rustc_hash::FxBuildHasher;
    rustc_hash::FxHashSet::with_capacity_and_hasher(capacity, hasher)
}

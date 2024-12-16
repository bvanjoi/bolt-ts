pub fn fx_hashmap_with_capacity<K, V>(capacity: usize) -> rustc_hash::FxHashMap<K, V> {
    let hasher = rustc_hash::FxBuildHasher::default();
    rustc_hash::FxHashMap::with_capacity_and_hasher(capacity, hasher)
}

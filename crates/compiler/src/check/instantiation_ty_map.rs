use std::hash::Hasher;

use bolt_ts_utils::{fx_hashmap_with_capacity, no_hashmap_with_capacity};
use rustc_hash::FxHashMap;

use crate::ty;

fn _hash_ty_args(hasher: &mut rustc_hash::FxHasher, ty_args: &[&ty::Ty]) {
    ty_args
        .iter()
        .for_each(|ty| hasher.write_u32(ty.id.as_u32()));
}

pub(super) fn hash_ty_args(ty_args: &[&ty::Ty]) -> TyKey {
    let mut hasher = rustc_hash::FxHasher::default();
    _hash_ty_args(&mut hasher, ty_args);
    let id = hasher.finish();
    TyKey(id)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct TyKey(u64);

impl nohash_hasher::IsEnabled for TyKey {}

struct TyCache<'cx> {
    inner: nohash_hasher::IntMap<TyKey, &'cx ty::Ty<'cx>>,
}

impl<'cx> TyCache<'cx> {
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: no_hashmap_with_capacity(capacity),
        }
    }

    pub fn get(&self, key: TyKey) -> Option<&'cx ty::Ty<'cx>> {
        self.inner.get(&key).copied()
    }

    pub fn contain(&self, key: TyKey) -> bool {
        self.inner.contains_key(&key)
    }

    pub fn insert(&mut self, key: TyKey, ty: &'cx ty::Ty<'cx>) {
        let prev = self.inner.insert(key, ty);
        assert!(prev.is_none());
    }
}

pub(super) struct UnionOrIntersectionMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> UnionOrIntersectionMap<'cx> {
    #[inline]
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }

    pub fn create_id(ty_args: &[&'cx ty::Ty<'cx>]) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        _hash_ty_args(&mut hasher, ty_args);
        let id = hasher.finish();
        TyKey(id)
    }

    #[inline]
    pub fn get(&self, key: TyKey) -> Option<&'cx ty::Ty<'cx>> {
        self.inner.get(key)
    }

    #[inline]
    pub fn contain(&self, key: TyKey) -> bool {
        self.inner.contain(key)
    }

    #[inline]
    pub fn insert(&mut self, key: TyKey, ty: &'cx ty::Ty<'cx>) {
        self.inner.insert(key, ty);
    }
}

pub(super) struct InstantiationTyMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> InstantiationTyMap<'cx> {
    #[inline]
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }

    pub fn create_id(target_ty_id: ty::TyID, ty_args: &[&'cx ty::Ty<'cx>]) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u32(target_ty_id.as_u32());
        _hash_ty_args(&mut hasher, ty_args);
        let id = hasher.finish();
        TyKey(id)
    }

    #[inline]
    pub fn get(&self, key: TyKey) -> Option<&'cx ty::Ty<'cx>> {
        self.inner.get(key)
    }

    #[inline]
    pub fn contain(&self, key: TyKey) -> bool {
        self.inner.contain(key)
    }

    #[inline]
    pub fn insert(&mut self, key: TyKey, ty: &'cx ty::Ty<'cx>) {
        self.inner.insert(key, ty);
    }
}

pub(super) struct IndexedAccessTyMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> IndexedAccessTyMap<'cx> {
    #[inline]
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }

    pub fn create_id(
        flags: ty::AccessFlags,
        object: &'cx ty::Ty<'cx>,
        index: &'cx ty::Ty<'cx>,
    ) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u16(flags.bits());
        hasher.write_u32(object.id.as_u32());
        hasher.write_u32(index.id.as_u32());
        let id = hasher.finish();
        TyKey(id)
    }

    #[inline]
    pub fn get(&self, key: TyKey) -> Option<&'cx ty::Ty<'cx>> {
        self.inner.get(key)
    }

    #[inline]
    pub fn contain(&self, key: TyKey) -> bool {
        self.inner.contain(key)
    }

    #[inline]
    pub fn insert(&mut self, key: TyKey, ty: &'cx ty::Ty<'cx>) {
        self.inner.insert(key, ty);
    }
}

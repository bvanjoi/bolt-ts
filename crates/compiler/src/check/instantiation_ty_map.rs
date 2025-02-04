use std::hash::Hasher;

use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use crate::ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct InstantiationTyKey(u64);

pub(super) struct InstantiationTyMap<'cx> {
    inner: FxHashMap<InstantiationTyKey, &'cx ty::Ty<'cx>>,
}

impl<'cx> InstantiationTyMap<'cx> {
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: fx_hashmap_with_capacity(capacity),
        }
    }

    pub fn create_id(target_ty_id: ty::TyID, ty_args: &[&'cx ty::Ty<'cx>]) -> InstantiationTyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u32(target_ty_id.as_u32());
        ty_args
            .iter()
            .for_each(|ty| hasher.write_u32(ty.id.as_u32()));
        let id = hasher.finish();
        InstantiationTyKey(id)
    }

    pub fn get(&self, key: InstantiationTyKey) -> Option<&'cx ty::Ty<'cx>> {
        self.inner.get(&key).copied()
    }

    pub fn contain(&self, key: InstantiationTyKey) -> bool {
        self.inner.contains_key(&key)
    }

    pub fn insert(&mut self, key: InstantiationTyKey, ty: &'cx ty::Ty<'cx>) {
        let prev = self.inner.insert(key, ty);
        assert!(prev.is_none());
    }
}

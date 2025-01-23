use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

use crate::ty::{self, ObjectFlags};

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
        let id = ty_args
            .iter()
            .flat_map(|ty| ty.id.as_u32().to_le_bytes())
            .chain(target_ty_id.as_u32().to_le_bytes())
            .collect::<Vec<u8>>();
        let id = xxhash_rust::xxh3::xxh3_64(id.as_slice());
        InstantiationTyKey(id)
    }

    pub fn get(&self, key: InstantiationTyKey) -> Option<&'cx ty::Ty<'cx>> {
        self.inner.get(&key).copied()
    }

    pub fn insert(&mut self, key: InstantiationTyKey, ty: &'cx ty::Ty<'cx>) {
        assert!(ty.get_object_flags().intersects(ObjectFlags::REFERENCE));
        let prev = self.inner.insert(key, ty);
        assert!(prev.is_none());
    }
}

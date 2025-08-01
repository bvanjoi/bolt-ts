use std::hash::Hasher;

use bolt_ts_utils::no_hashmap_with_capacity;

use super::bind::SymbolID;
use crate::ty;

fn hash_ty_args_with_hasher(hasher: &mut rustc_hash::FxHasher, ty_args: &[&ty::Ty]) {
    ty_args
        .iter()
        .for_each(|ty| hasher.write_u32(ty.id.as_u32()));
}

pub(super) fn hash_ty_args(ty_args: &[&ty::Ty]) -> TyKey {
    let mut hasher = rustc_hash::FxHasher::default();
    hash_ty_args_with_hasher(&mut hasher, ty_args);
    let id = hasher.finish();
    TyKey(id)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct TyKey(u64);

impl nohash_hasher::IsEnabled for TyKey {}

pub(super) struct TyCache<'cx> {
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

    #[track_caller]
    pub fn insert(&mut self, key: TyKey, ty: &'cx ty::Ty<'cx>) {
        let prev = self.inner.insert(key, ty);
        assert!(prev.is_none());
    }
}

pub(super) trait TyCacheTrait<'cx> {
    type Input: ?Sized;
    fn new(capacity: usize) -> Self;
    fn create_ty_key(input: &Self::Input) -> TyKey;
    fn inner(&self) -> &TyCache<'cx>;
    fn inner_mut(&mut self) -> &mut TyCache<'cx>;
    fn get(&self, key: TyKey) -> Option<&'cx ty::Ty<'cx>> {
        self.inner().get(key)
    }
    fn contain(&self, key: TyKey) -> bool {
        self.inner().contain(key)
    }
    #[track_caller]
    fn insert(&mut self, key: TyKey, ty: &'cx ty::Ty<'cx>) {
        self.inner_mut().insert(key, ty);
    }
    fn r#override(&mut self, key: TyKey, ty: &'cx ty::Ty<'cx>) {
        let prev = self.inner_mut().inner.insert(key, ty);
        assert!(prev.is_some());
    }
}

pub struct IntersectionMap<'cx> {
    inner: TyCache<'cx>,
}
impl<'cx> TyCacheTrait<'cx> for IntersectionMap<'cx> {
    type Input = (Vec<&'cx ty::Ty<'cx>>, bool);
    fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }
    fn create_ty_key(input: &Self::Input) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hash_ty_args_with_hasher(&mut hasher, input.0.as_ref());
        if input.1 {
            hasher.write_u8(1);
        }
        let id = hasher.finish();
        TyKey(id)
    }
    fn inner(&self) -> &TyCache<'cx> {
        &self.inner
    }
    fn inner_mut(&mut self) -> &mut TyCache<'cx> {
        &mut self.inner
    }
}

pub(super) struct UnionMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> TyCacheTrait<'cx> for UnionMap<'cx> {
    type Input = [&'cx ty::Ty<'cx>];
    fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }
    fn create_ty_key(input: &Self::Input) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hash_ty_args_with_hasher(&mut hasher, input);
        let id = hasher.finish();
        TyKey(id)
    }
    fn inner(&self) -> &TyCache<'cx> {
        &self.inner
    }
    fn inner_mut(&mut self) -> &mut TyCache<'cx> {
        &mut self.inner
    }
}

pub(super) struct InstantiationTyMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> TyCacheTrait<'cx> for InstantiationTyMap<'cx> {
    type Input = (ty::TyID, ty::Tys<'cx>);
    fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }
    fn create_ty_key(_: &Self::Input) -> TyKey {
        unreachable!("use InstantiationTyMap::create_id instead")
    }
    fn inner(&self) -> &TyCache<'cx> {
        &self.inner
    }
    fn inner_mut(&mut self) -> &mut TyCache<'cx> {
        &mut self.inner
    }
}

impl<'cx> InstantiationTyMap<'cx> {
    pub fn create_id(target_ty_id: ty::TyID, ty_args: &[&'cx ty::Ty<'cx>]) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u32(target_ty_id.as_u32());
        hash_ty_args_with_hasher(&mut hasher, ty_args);
        let id = hasher.finish();
        TyKey(id)
    }
}

pub(super) struct ConditionalTyInstantiationTyMap;

impl<'cx> ConditionalTyInstantiationTyMap {
    pub fn create_id(
        root_node_id: bolt_ts_ast::NodeID,
        ty_args: &[&'cx ty::Ty<'cx>],
        alias_symbol: Option<SymbolID>,
        alias_ty_arguments: Option<ty::Tys<'cx>>,
    ) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u32(root_node_id.module().as_u32());
        hasher.write_u32(root_node_id.index_as_u32());
        hash_ty_args_with_hasher(&mut hasher, ty_args);
        if let Some(alias_symbol) = alias_symbol {
            hasher.write_u32(alias_symbol.module().as_u32());
            hasher.write_u32(alias_symbol.index_as_u32());
        }
        if let Some(alias_ty_arguments) = alias_ty_arguments {
            hash_ty_args_with_hasher(&mut hasher, alias_ty_arguments);
        }
        let id = hasher.finish();
        TyKey(id)
    }
}

pub(super) struct IndexedAccessTyMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> TyCacheTrait<'cx> for IndexedAccessTyMap<'cx> {
    type Input = (ty::AccessFlags, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>);
    fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }
    fn create_ty_key(input: &Self::Input) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u16(input.0.bits());
        hasher.write_u32(input.1.id.as_u32());
        hasher.write_u32(input.2.id.as_u32());
        let id = hasher.finish();
        TyKey(id)
    }
    fn inner(&self) -> &TyCache<'cx> {
        &self.inner
    }
    fn inner_mut(&mut self) -> &mut TyCache<'cx> {
        &mut self.inner
    }
}

pub(super) struct StringMappingTyMap<'cx> {
    inner: TyCache<'cx>,
}
impl<'cx> TyCacheTrait<'cx> for StringMappingTyMap<'cx> {
    type Input = (SymbolID, &'cx ty::Ty<'cx>);
    fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }
    fn create_ty_key(input: &Self::Input) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u32(input.0.module().as_u32());
        hasher.write_u32(input.0.index_as_u32());
        hasher.write_u32(input.1.id.as_u32());
        let id = hasher.finish();
        TyKey(id)
    }
    fn inner(&self) -> &TyCache<'cx> {
        &self.inner
    }
    fn inner_mut(&mut self) -> &mut TyCache<'cx> {
        &mut self.inner
    }
}

pub(super) fn create_iteration_tys_key<'cx>(
    yield_ty: &'cx ty::Ty<'cx>,
    return_ty: &'cx ty::Ty<'cx>,
    next_ty: &'cx ty::Ty<'cx>,
) -> TyKey {
    let mut hasher = rustc_hash::FxHasher::default();
    hasher.write_u32(yield_ty.id.as_u32());
    hasher.write_u32(return_ty.id.as_u32());
    hasher.write_u32(next_ty.id.as_u32());
    let id = hasher.finish();
    TyKey(id)
}

pub(super) struct TyAliasInstantiationMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> TyCacheTrait<'cx> for TyAliasInstantiationMap<'cx> {
    type Input = (
        SymbolID,
        ty::Tys<'cx>,
        Option<SymbolID>,
        Option<ty::Tys<'cx>>,
    );
    fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }
    fn create_ty_key(input: &Self::Input) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u32(input.0.module().as_u32());
        hasher.write_u32(input.0.index_as_u32());
        hash_ty_args_with_hasher(&mut hasher, input.1);
        if let Some(alias_symbol) = input.2 {
            hasher.write_u32(alias_symbol.module().as_u32());
            hasher.write_u32(alias_symbol.index_as_u32());
        }
        if let Some(alias_ty_arguments) = input.3 {
            hash_ty_args_with_hasher(&mut hasher, alias_ty_arguments);
        }
        let id = hasher.finish();
        TyKey(id)
    }
    fn inner(&self) -> &TyCache<'cx> {
        &self.inner
    }
    fn inner_mut(&mut self) -> &mut TyCache<'cx> {
        &mut self.inner
    }
}

pub(super) struct TyInstantiationMap<'cx> {
    inner: TyCache<'cx>,
}

impl<'cx> TyCacheTrait<'cx> for TyInstantiationMap<'cx> {
    type Input = (ty::TyID, Option<SymbolID>, Option<ty::Tys<'cx>>);
    fn new(capacity: usize) -> Self {
        Self {
            inner: TyCache::new(capacity),
        }
    }
    fn create_ty_key(input: &Self::Input) -> TyKey {
        let mut hasher = rustc_hash::FxHasher::default();
        hasher.write_u32(input.0.as_u32());
        if let Some(alias_symbol) = input.1 {
            hasher.write_u32(alias_symbol.module().as_u32());
            hasher.write_u32(alias_symbol.index_as_u32());
        }
        if let Some(alias_ty_arguments) = input.2 {
            hash_ty_args_with_hasher(&mut hasher, alias_ty_arguments);
        }
        let id = hasher.finish();
        TyKey(id)
    }
    fn inner(&self) -> &TyCache<'cx> {
        &self.inner
    }
    fn inner_mut(&mut self) -> &mut TyCache<'cx> {
        &mut self.inner
    }
}

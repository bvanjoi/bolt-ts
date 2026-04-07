use bolt_ts_atom::Atom;
use bolt_ts_fs::PathId;
use dashmap::DashMap;

use super::RResult;
use super::ResolutionMode;
use super::package_json::PackageJsonInfoCache;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct RedirectCacheKey {
    module_name: Atom,
    resolution_mode: Option<ResolutionMode>,
}

pub struct ModeAwareCache<V>(DashMap<RedirectCacheKey, V>);
impl<V: Copy> ModeAwareCache<V> {
    pub fn new() -> Self {
        Self(DashMap::new())
    }
    pub fn get(&self, module_name: Atom, resolution_mode: Option<ResolutionMode>) -> Option<V> {
        let key = RedirectCacheKey {
            module_name,
            resolution_mode,
        };
        self.0.get(&key).map(|v| *v)
    }
    pub fn set(&self, module_name: Atom, resolution_mode: Option<ResolutionMode>, value: V) {
        let key = RedirectCacheKey {
            module_name,
            resolution_mode,
        };
        let prev = self.0.insert(key, value);
        debug_assert!(prev.is_none())
    }
}

pub struct CacheWithRedirects<K, V> {
    own_map: DashMap<K, V>,
}

impl<K: PartialEq + Eq + std::hash::Hash, V> CacheWithRedirects<K, V> {
    pub fn new() -> Self {
        Self {
            own_map: DashMap::new(),
        }
    }

    pub fn get_map_of_cache_redirects(&self) -> &DashMap<K, V> {
        // TODO: command line options
        &self.own_map
    }
}

pub struct PerNonRelativeNameCache<V> {
    _is_readonly: bool,
    directory_path_map: DashMap<PathId, V>,
}

impl<V: Copy> PerNonRelativeNameCache<V> {
    pub fn new() -> Self {
        Self {
            _is_readonly: false,
            directory_path_map: DashMap::new(),
        }
    }

    pub fn set(&self, directory: PathId, value: V) {
        let prev = self.directory_path_map.insert(directory, value);
        debug_assert!(prev.is_none())
    }
}

pub struct ModuleResolutionCache {
    is_readonly: bool,
    directory_to_module_name_map: CacheWithRedirects<PathId, ModeAwareCache<RResult<PathId>>>,
    module_name_to_directory_map:
        CacheWithRedirects<RedirectCacheKey, PerNonRelativeNameCache<RResult<PathId>>>,
    package_json_cache: PackageJsonInfoCache,
}

impl Default for ModuleResolutionCache {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleResolutionCache {
    pub fn new() -> ModuleResolutionCache {
        Self {
            is_readonly: false,
            directory_to_module_name_map: CacheWithRedirects::new(),
            module_name_to_directory_map: CacheWithRedirects::new(),
            package_json_cache: PackageJsonInfoCache::new(),
        }
    }

    pub fn is_readonly(&self) -> bool {
        self.is_readonly
    }

    pub fn get_from_directory_cache(
        &self,
        module_name: Atom,
        resolution_mode: Option<ResolutionMode>,
        directory: PathId,
    ) -> Option<RResult<PathId>> {
        self.directory_to_module_name_map
            .get_map_of_cache_redirects()
            .get(&directory)
            .and_then(|map| map.get(module_name, resolution_mode))
    }

    pub fn set_for_directory_cache(
        &self,
        directory: PathId,
        module_name: Atom,
        resolution_mode: Option<ResolutionMode>,
        result: RResult<PathId>,
    ) {
        // TODO: get_or_create
        let map = self
            .directory_to_module_name_map
            .get_map_of_cache_redirects();
        let entry = map.entry(directory).or_insert_with(ModeAwareCache::new);
        entry.set(module_name, resolution_mode, result);
    }

    pub fn set_for_non_relative_name(
        &self,
        directory: PathId,
        module_name: Atom,
        resolution_mode: Option<ResolutionMode>,
        result: RResult<PathId>,
    ) {
        // TODO: get_or_create
        let map = self
            .module_name_to_directory_map
            .get_map_of_cache_redirects();
        let entry = map
            .entry(RedirectCacheKey {
                module_name,
                resolution_mode,
            })
            .or_insert_with(PerNonRelativeNameCache::new);
        entry.set(directory, result);
    }

    pub fn package_json_cache(&self) -> &PackageJsonInfoCache {
        &self.package_json_cache
    }
}

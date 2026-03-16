use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use bolt_ts_atom::AtomIntern;
use bolt_ts_fs::PathId;
use bolt_ts_utils::path::NormalizePath;
use dashmap::DashMap;

use super::normalize_join;

pub struct PackageJsonInfo {
    package_directory: PathId,
    contents: Arc<PackageJsonInfoContents>,
}

impl PackageJsonInfo {
    pub fn package_directory(&self) -> PathId {
        self.package_directory
    }
    pub fn contents(&self) -> &PackageJsonInfoContents {
        &self.contents
    }
    pub fn new(package_directory: PathId, contents: PackageJsonInfoContents) -> Self {
        Self {
            package_directory,
            contents: Arc::new(contents),
        }
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct PackageJsonInfoContents {
    name: Option<String>,
    types: Option<String>,
    typings: Option<String>,
    exports: Option<serde_json::Value>,
}

impl PackageJsonInfoContents {
    pub fn exports(&self) -> Option<&serde_json::Value> {
        self.exports.as_ref()
    }
}

pub struct PackageJsonInfoCache {
    map: DashMap<PathId, PackageJsonInfo>,
}

impl PackageJsonInfoCache {
    pub fn new() -> Self {
        Self {
            map: DashMap::new(),
        }
    }

    pub fn get(&self, package_json_path: PathId) -> Option<PackageJsonPath> {
        self.map
            .get(&package_json_path)
            .map(|_| PackageJsonPath(package_json_path))
    }

    pub fn insert_package_json(
        &self,
        package_json_path: PathId,
        info: PackageJsonInfo,
        atoms: &Arc<Mutex<AtomIntern>>,
    ) -> PackageJsonPath {
        debug_assert!({
            let str = atoms.lock().unwrap().get(package_json_path.into());
            str.ends_with("package.json") && Path::new(str).is_normalized()
        });
        let prev = self.map.insert(package_json_path, info);
        debug_assert!(prev.is_none());
        PackageJsonPath(package_json_path)
    }

    fn read_package_json_path_field(
        &self,
        package_json: PackageJsonPath,
        filed: Field,
        atoms: &Arc<Mutex<AtomIntern>>,
    ) -> Option<PathBuf> {
        let Some(json) = self.map.get(&package_json.0) else {
            unreachable!()
        };
        let filename = match filed {
            Field::Types => json.contents.types.as_ref(),
            Field::Typings => json.contents.typings.as_ref(),
        }?;
        let filename = filename.trim();
        if filename.is_empty() {
            return None;
        }
        let mut atoms = atoms.lock().unwrap();
        let base = atoms.get(json.package_directory.into());
        let base = std::path::Path::new(base);
        let p = normalize_join(base, filename);
        drop(json);
        let bytes = p.as_os_str().as_encoded_bytes();
        let s = unsafe { std::str::from_utf8_unchecked(bytes) };
        atoms.atom(s);
        Some(p)
    }

    pub fn read_package_json_types_filed(
        &self,
        package_json: PackageJsonPath,
        atoms: &Arc<Mutex<AtomIntern>>,
    ) -> Option<PathBuf> {
        self.read_package_json_path_field(package_json, Field::Typings, atoms)
            .or_else(|| self.read_package_json_path_field(package_json, Field::Types, atoms))
    }

    pub fn package_json_directory(&self, package_json: PackageJsonPath) -> PathId {
        self.map
            .get(&package_json.0)
            .map(|info| info.package_directory())
            .unwrap()
    }

    pub fn read_package_json(&self, package_json: PackageJsonPath) -> Arc<PackageJsonInfoContents> {
        let Some(json) = self.map.get(&package_json.0) else {
            unreachable!()
        };
        json.contents.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PackageJsonPath(PathId);

impl PackageJsonPath {
    pub fn path(&self) -> PathId {
        self.0
    }
}

enum Field {
    Types,
    Typings,
    // Main,
    // TsConfig,
}

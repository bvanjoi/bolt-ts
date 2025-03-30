use std::path::PathBuf;

use bolt_ts_fs::PathId;

use crate::normalize_join;

pub struct PackageJsonInfo {
    pub dir: PathId,
    pub contents: PackageJsonInfoContents,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct PackageJsonInfoContents {
    name: Option<String>,
    types: Option<String>,
    typings: Option<String>,
}

enum Field {
    Types,
    Typings,
    Main,
    TsConfig,
}

bolt_ts_utils::index!(PackageJsonInfoId);

impl<'atoms, FS: super::CachedFileSystem> super::Resolver<'atoms, FS> {
    pub fn new_pkg_json(
        &self,
        dir: PathId,
        contents: PackageJsonInfoContents,
    ) -> PackageJsonInfoId {
        let info = PackageJsonInfo { dir, contents };
        let mut arena = self.package_json_arena.lock().unwrap();
        let id = arena.len();
        arena.push(info);
        PackageJsonInfoId(id as u32)
    }

    fn read_package_json_path_field(
        &self,
        package_json: PackageJsonInfoId,
        filed: Field,
        base_dir: PathId,
    ) -> Option<PathBuf> {
        let p = &self.package_json_arena.lock().unwrap()[package_json.as_usize()].contents;
        let filename = match filed {
            Field::Types => p.types.as_ref(),
            Field::Typings => p.typings.as_ref(),
            Field::Main => todo!(),
            Field::TsConfig => todo!(),
        }?;
        let filename = filename.trim();
        if filename.is_empty() {
            return None;
        }
        let mut atoms = self.atoms.lock().unwrap();
        let base = atoms.get(base_dir.into());
        let p = normalize_join(std::path::Path::new(base), filename);
        atoms.insert_if_not_exist(PathId::get(&p).into(), || unsafe {
            std::borrow::Cow::Owned(String::from_utf8_unchecked(
                p.as_os_str().as_encoded_bytes().to_vec(),
            ))
        });
        Some(p)
    }

    pub fn read_package_json_types_filed(
        &self,
        package_json: PackageJsonInfoId,
        base_dir: PathId,
    ) -> Option<PathBuf> {
        self.read_package_json_path_field(package_json, Field::Typings, base_dir)
            .or_else(|| self.read_package_json_path_field(package_json, Field::Types, base_dir))
    }
}

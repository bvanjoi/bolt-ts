use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_utils::path::NormalizePath;

use super::CachedFileSystem;
use super::PathId;
use super::errors;
use super::errors::FsResult;

use std::io::Read;
use std::io::Write;
use vfs::FileSystem;
use vfs::MemoryFS as VFSMemoryFS;

#[derive(Default)]
pub struct MemoryFS {
    fs: VFSMemoryFS,
}

impl std::fmt::Debug for MemoryFS {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MemoryFS").finish()
    }
}

impl MemoryFS {
    pub fn new(
        content_map: impl Iterator<Item = (String, String)>,
        atoms: &mut AtomIntern,
    ) -> FsResult<Self> {
        let fs = VFSMemoryFS::new();
        for (path, content) in content_map {
            let path = std::path::PathBuf::from(path);
            let path = path.normalize();
            let mut current = std::path::PathBuf::new();
            let path_count = path.components().count();
            for (i, c) in path.components().enumerate() {
                if i == path_count - 1 {
                    break;
                }
                match c {
                    std::path::Component::Prefix(_) => unreachable!(),
                    std::path::Component::CurDir => unreachable!(),
                    std::path::Component::ParentDir => unreachable!(),
                    std::path::Component::RootDir => {
                        current.push(c.as_os_str());
                    }
                    std::path::Component::Normal(os_str) => {
                        current.push(os_str);
                    }
                }
                let current_str = current.to_str().unwrap();
                if let Ok(metadata) = fs.metadata(current_str) {
                    if metadata.file_type == vfs::VfsFileType::File {
                        return Err(errors::FsError::FileExists(PathId::new(&current, atoms)));
                    } else if metadata.file_type == vfs::VfsFileType::Directory {
                        continue;
                    }
                }
                fs.create_dir(current_str).unwrap();
            }
            let path_str = path.to_str().unwrap();
            if let Ok(metadata) = fs.metadata(path_str) {
                if metadata.file_type == vfs::VfsFileType::Directory {
                    return Err(errors::FsError::DirExists(PathId::new(&path, atoms)));
                }
            }
            let mut file = fs.create_file(path_str).unwrap();
            file.write_all(content.as_bytes()).unwrap();
        }
        Ok(Self { fs })
    }
}

impl CachedFileSystem for MemoryFS {
    fn is_vfs(&self) -> bool {
        true
    }

    fn read_file(
        &mut self,
        path: &std::path::Path,
        atoms: &mut AtomIntern,
    ) -> FsResult<bolt_ts_atom::Atom> {
        match self.fs.open_file(path.to_str().unwrap()) {
            Ok(mut file) => {
                let mut content = String::new();
                file.read_to_string(&mut content).unwrap();
                Ok(atoms.atom(&content))
            }
            Err(error) => {
                let path_id = PathId::new(path, atoms);
                Err(match error.kind() {
                    vfs::error::VfsErrorKind::FileNotFound => errors::FsError::NotFound(path_id),
                    vfs::error::VfsErrorKind::DirectoryExists => errors::FsError::NotAFile(path_id),
                    vfs::error::VfsErrorKind::Other(_) => errors::FsError::NotAFile(path_id),
                    vfs::error::VfsErrorKind::IoError(_) => todo!(),
                    vfs::error::VfsErrorKind::InvalidPath => todo!(),
                    vfs::error::VfsErrorKind::FileExists => todo!(),
                    vfs::error::VfsErrorKind::NotSupported => todo!(),
                })
            }
        }
    }

    fn file_exists(&mut self, p: &std::path::Path, _: &mut AtomIntern) -> bool {
        let path_str = p.to_str().unwrap();
        self.fs
            .metadata(path_str)
            .map(|m| m.file_type == vfs::VfsFileType::File)
            .unwrap_or_default()
    }

    fn read_dir(
        &mut self,
        path: &std::path::Path,
        atoms: &mut AtomIntern,
    ) -> FsResult<impl Iterator<Item = std::path::PathBuf>> {
        let path_str = path.to_str().unwrap();
        let path_str = if path_str.ends_with('/') {
            &path_str[..path_str.len() - 1]
        } else {
            path_str
        };
        if let Ok(metadata) = self.fs.metadata(path_str) {
            if metadata.file_type == vfs::VfsFileType::File {
                return Err(errors::FsError::NotADir(PathId::new(path, atoms)));
            }
        } else {
            return Err(errors::FsError::NotFound(PathId::new(path, atoms)));
        }

        self.fs
            .read_dir(path_str)
            .map_err(|_| errors::FsError::NotFound(PathId::new(path, atoms)))
            .map(|iter| {
                iter.flat_map(|entry| {
                    if entry.is_empty() {
                        None
                    } else {
                        let res = path.join(entry);
                        debug_assert!(res.is_normalized());
                        Some(res)
                    }
                })
            })
    }

    fn dir_exists(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> bool {
        let path_str = p.to_str().unwrap();
        self.fs
            .metadata(path_str)
            .map(|m| m.file_type == vfs::VfsFileType::Directory)
            .unwrap_or_default()
    }

    fn add_file(
        &mut self,
        _: &std::path::Path,
        _: String,
        _: Option<Atom>,
        _: &mut AtomIntern,
    ) -> Atom {
        unreachable!("Cannot add file to memory fs")
    }

    fn is_symlink(&mut self, _: &std::path::Path, _: &mut AtomIntern) -> bool {
        unreachable!()
    }

    fn realpath(&mut self, p: &std::path::Path, atoms: &mut AtomIntern) -> FsResult<PathId> {
        debug_assert!(p.is_normalized());
        Ok(PathId::get(p, atoms))
    }
}

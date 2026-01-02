use crate::path::PathId;

pub type FsResult<T> = Result<T, FsError>;

#[derive(Debug)]
pub enum FsError {
    NotFound(PathId),
    NotAFile(PathId),
    NotADir(PathId),
    FileExists(PathId),
    DirExists(PathId),
    NotASymlink(PathId),
}

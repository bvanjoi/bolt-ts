use crate::path::PathId;

pub type FsResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    NotFound(PathId),
    NotAFile(PathId),
    NotADir(PathId),
    FileExists(PathId),
    DirExists(PathId),
}

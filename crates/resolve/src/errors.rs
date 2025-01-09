use bolt_ts_fs::PathId;

#[derive(Debug)]
pub enum ResolveError {
    NotFound(PathId),
}

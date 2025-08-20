use bolt_ts_fs::PathId;

#[derive(Debug, Clone, Copy)]
pub enum ResolveError {
    NotFound(PathId),
}

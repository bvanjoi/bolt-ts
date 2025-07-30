use bolt_ts_fs::PathId;

use crate::{Extensions, RResult};

pub trait ResolutionKindSpecLoader<FS: bolt_ts_fs::CachedFileSystem> {
    fn loader(
        &self,
        resolver: &super::Resolver<FS>,
        ext: Extensions,
        candidate: &mut std::path::PathBuf,
        only_record_failures: bool,
    ) -> RResult<PathId>;
}

use bolt_ts_fs::PathId;

use super::{Extensions, ModuleResolutionState, RResult};

pub trait ResolutionKindSpecLoader<'a, 'options, FS: bolt_ts_fs::CachedFileSystem> {
    fn loader(
        &self,
        ext: Extensions,
        candidate: &mut std::path::PathBuf,
        only_record_failures: bool,
        state: &ModuleResolutionState<'a, 'options, FS>,
    ) -> RResult<PathId>;
}

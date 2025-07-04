mod normalized;
mod options;
mod raw;

pub use normalized::{CompilerOptionFlags, NormalizedCompilerOptions, NormalizedTsConfig};
pub use options::Extension;
pub use options::OutDir;
pub use options::{
    ALL_SUPPORTED_EXTENSIONS, FLATTENED_ALL_SUPPORTED_EXTENSIONS,
    FLATTENED_SUPPORTED_TS_EXTENSIONS, SUPPORTED_DECLARATION_EXTENSIONS, SUPPORTED_TS_EXTENSIONS,
    SUPPORTED_TS_IMPLEMENTATION_EXTENSIONS,
};
pub use options::{RawTarget, Target};
pub use raw::{RawCompilerOptions, RawTsConfig};

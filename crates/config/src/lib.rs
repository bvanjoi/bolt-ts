mod normalized;
mod options;
mod raw;

pub use self::normalized::AllowUnusedLabels;
pub use self::normalized::{CompilerOptionFlags, NormalizedCompilerOptions, NormalizedTsConfig};
pub use self::options::Extension;
pub use self::options::OutDir;
pub use self::options::{ALL_SUPPORTED_EXTENSIONS, FLATTENED_ALL_SUPPORTED_EXTENSIONS};
pub use self::options::{FLATTENED_SUPPORTED_TS_EXTENSIONS, SUPPORTED_DECLARATION_EXTENSIONS};
pub use self::options::{RawTarget, Target};
pub use self::options::{SUPPORTED_TS_EXTENSIONS, SUPPORTED_TS_IMPLEMENTATION_EXTENSIONS};
pub use self::raw::{RawCompilerOptions, RawTsConfig};

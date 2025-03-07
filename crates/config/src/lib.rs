mod normalized;
mod options;
mod raw;

pub use normalized::{NormalizedCompilerOptions, NormalizedTsConfig};
pub use options::OutDir;
pub use options::{RawTarget, Target};
pub use raw::{RawCompilerOptions, RawTsConfig};

mod normalized;
mod options;
mod raw;

pub use self::normalized::AllowUnreachableCode;
pub use self::normalized::AllowUnusedLabels;
pub use self::normalized::{CompilerOptionFlags, NormalizedCompilerOptions, NormalizedTsConfig};
pub use self::options::NormalizedModuleResolution;
pub use self::options::OutDir;
pub use self::options::{Module, Target};
pub use self::raw::{RawCompilerOptions, RawTsConfig};
pub use self::raw::{RawModule, RawModuleResolution, RawTarget};

pub fn parse_tsconfig(input: &str) -> Result<RawTsConfig, jsonc_parser::errors::ParseError> {
    jsonc_parser::parse_to_serde_value(
        input,
        &jsonc_parser::ParseOptions {
            allow_comments: true,
            allow_loose_object_property_names: false,
            allow_trailing_commas: true,
            allow_missing_commas: false,
            allow_single_quoted_strings: false,
            allow_hexadecimal_numbers: false,
            allow_unary_plus_numbers: false,
        },
    )
}

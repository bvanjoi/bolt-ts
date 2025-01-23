use crate::options::OutDir;

macro_rules! normalized_option {
    ($s: ident, $(($option: ident, $ty: ty)),* $(,)?) => {
        pub struct $s {
            $(pub(super) $option: $ty,)*
        }
        impl $s {
            $(pub fn $option(&self) -> & $ty {
                &self.$option
            })*
        }
    };
}

normalized_option!(
    NormalizedCompilerOptions,
    (out_dir, OutDir),
    (no_emit, bool),
    (strict, bool),
    (declaration, bool),
);
normalized_option!(
    NormalizedTsConfig,
    (include, Vec<String>),
    (compiler_options, NormalizedCompilerOptions),
);

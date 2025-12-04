use crate::options::{OutDir, Target};

macro_rules! normalized_option {
    ($s: ident, $(($option: ident, $ty: ty)),* $(,)?) => {
        #[derive(Debug)]
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

bitflags::bitflags! {
    #[derive(Debug)]
    pub struct CompilerOptionFlags: u16 {
        const NO_EMIT                                   = 1 << 0;
        const DECLARATION                               = 1 << 1;
        const STRICT                                    = 1 << 2;
        const STRICT_NULL_CHECKS                        = 1 << 3;
        const STRICT_FUNCTION_TYPES                     = 1 << 4;
        const NO_IMPLICIT_ANY                           = 1 << 5;
        const NO_UNCHECKED_INDEXED_ACCESS               = 1 << 6;
        const ALWAYS_STRICT                             = 1 << 7;
        const EXACT_OPTIONAL_PROPERTY_TYPES             = 1 << 8;
        const NO_IMPLICIT_RETURNS                       = 1 << 9;
    }
}

#[derive(Debug)]
pub enum AllowUnusedLabels {
    Allow,
    Deny,
    Warning,
}

normalized_option!(
    NormalizedCompilerOptions,
    (out_dir, OutDir),
    (target, Target),
    (allow_unused_labels, AllowUnusedLabels),
    (flags, CompilerOptionFlags),
);

normalized_option!(
    NormalizedTsConfig,
    (include, Vec<String>),
    (compiler_options, NormalizedCompilerOptions),
);

impl NormalizedCompilerOptions {
    #[inline(always)]
    pub const fn strict(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::STRICT)
    }

    #[inline(always)]
    pub const fn always_strict(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::ALWAYS_STRICT)
    }

    #[inline(always)]
    pub const fn strict_null_checks(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::STRICT_NULL_CHECKS)
    }

    #[inline(always)]
    pub const fn no_unchecked_indexed_access(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::NO_UNCHECKED_INDEXED_ACCESS)
    }

    #[inline(always)]
    pub const fn exact_optional_property_types(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::EXACT_OPTIONAL_PROPERTY_TYPES)
    }

    #[inline(always)]
    pub const fn strict_function_types(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::STRICT_FUNCTION_TYPES)
    }
}

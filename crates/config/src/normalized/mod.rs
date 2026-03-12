use super::options::{NormalizedModuleResolution, OutDir, Target};

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
    pub struct CompilerOptionFlags: u32 {
        const NO_EMIT                                   = 1 << 0;
        const DECLARATION                               = 1 << 1;
        const STRICT                                    = 1 << 2;
        const STRICT_NULL_CHECKS                        = 1 << 3;
        const STRICT_FUNCTION_TYPES                     = 1 << 4;
        const NO_IMPLICIT_ANY                           = 1 << 5;
        const NO_IMPLICIT_THIS                          = 1 << 6;
        const NO_UNCHECKED_INDEXED_ACCESS               = 1 << 7;
        const ALWAYS_STRICT                             = 1 << 8;
        const EXACT_OPTIONAL_PROPERTY_TYPES             = 1 << 9;
        const NO_IMPLICIT_RETURNS                       = 1 << 10;
        const PRESERVE_SYMLINKS                         = 1 << 11;
        const USE_DEFINE_FOR_CLASS_FIELDS               = 1 << 12;
        const STRICT_PROPERTY_INITIALIZATION            = 1 << 13;
        const STRICT_BIND_CALL_APPLY                    = 1 << 14;
        const NO_DTS_RESOLUTION                         = 1 << 15;
        const STRICT_BUILTIN_ITERATION_RETURN           = 1 << 16;
        const NO_UNUSED_LOCALS                          = 1 << 17;
        const NO_UNUSED_PARAMETERS                      = 1 << 18;
    }
}

#[derive(Debug)]
pub enum AllowUnusedLabels {
    Allow,
    Deny,
    Warning,
}

#[derive(Debug)]
pub enum AllowUnreachableCode {
    Allow,
    Deny,
    Warning,
}

normalized_option!(
    NormalizedCompilerOptions,
    (out_dir, OutDir),
    (target, Target),
    (allow_unused_labels, AllowUnusedLabels),
    (allow_unreachable_code, AllowUnreachableCode),
    (flags, CompilerOptionFlags),
    (module_resolution, NormalizedModuleResolution),
    (custom_conditions, Vec<String>)
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
    pub const fn no_implicit_any(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::NO_IMPLICIT_ANY)
    }

    #[inline(always)]
    pub const fn no_implicit_this(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::NO_IMPLICIT_THIS)
    }

    #[inline(always)]
    pub const fn no_implicit_returns(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::NO_IMPLICIT_RETURNS)
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

    #[inline(always)]
    pub const fn preserve_symlinks(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::PRESERVE_SYMLINKS)
    }

    #[inline(always)]
    pub const fn use_define_for_class_fields(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::USE_DEFINE_FOR_CLASS_FIELDS)
    }

    #[inline(always)]
    pub const fn strict_property_initialization(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::STRICT_PROPERTY_INITIALIZATION)
    }

    #[inline(always)]
    pub const fn strict_bind_call_apply(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::STRICT_BIND_CALL_APPLY)
    }

    #[inline(always)]
    pub const fn no_dts_resolution(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::NO_DTS_RESOLUTION)
    }

    #[inline(always)]
    pub const fn strict_builtin_iteration_return(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::STRICT_BUILTIN_ITERATION_RETURN)
    }

    #[inline(always)]
    pub const fn no_unused_locals(&self) -> bool {
        self.flags.contains(CompilerOptionFlags::NO_UNUSED_LOCALS)
    }

    #[inline(always)]
    pub const fn no_unused_parameters(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::NO_UNUSED_PARAMETERS)
    }
}

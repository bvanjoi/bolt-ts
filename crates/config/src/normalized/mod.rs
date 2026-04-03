use super::RawTarget;
use super::options::Module;
use super::options::{NormalizedModuleResolution, OutDir, Target};
use super::{RawModule, RawModuleResolution};

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
        const NO_IMPLICIT_ANY                           = 1 << 1;
        const NO_IMPLICIT_THIS                          = 1 << 2;
        const NO_UNCHECKED_INDEXED_ACCESS               = 1 << 3;
        const NO_IMPLICIT_RETURNS                       = 1 << 4;
        const NO_DTS_RESOLUTION                         = 1 << 5;
        const NO_UNUSED_LOCALS                          = 1 << 6;
        const NO_UNUSED_PARAMETERS                      = 1 << 7;

        const STRICT                                    = 1 << 8;
        const STRICT_NULL_CHECKS                        = 1 << 9;
        const STRICT_PROPERTY_INITIALIZATION            = 1 << 10;
        const STRICT_FUNCTION_TYPES                     = 1 << 11;
        const STRICT_BIND_CALL_APPLY                    = 1 << 12;
        const STRICT_BUILTIN_ITERATION_RETURN           = 1 << 13;

        const DECLARATION                               = 1 << 14;
        const ALWAYS_STRICT                             = 1 << 15;
        const EXACT_OPTIONAL_PROPERTY_TYPES             = 1 << 16;
        const PRESERVE_SYMLINKS                         = 1 << 17;
        const USE_DEFINE_FOR_CLASS_FIELDS               = 1 << 18;

        const RESOLVE_JSON_MODULE                       = 1 << 19;
        const RESOLVE_PACKAGE_JSON_EXPORTS              = 1 << 20;
        const RESOLVE_PACKAGE_JSON_IMPORTS              = 1 << 21;
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
    (module, Module),
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

    #[inline(always)]
    pub const fn resolve_json_module(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::RESOLVE_JSON_MODULE)
    }

    #[inline(always)]
    pub const fn resolve_package_json_exports(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::RESOLVE_PACKAGE_JSON_EXPORTS)
    }

    #[inline(always)]
    pub const fn resolve_package_json_imports(&self) -> bool {
        self.flags
            .contains(CompilerOptionFlags::RESOLVE_PACKAGE_JSON_IMPORTS)
    }

    pub fn import_syntax_affects_module_resolution(&self) -> bool {
        let module_resolution = self.module_resolution();
        (NormalizedModuleResolution::Node16 <= *module_resolution
            && *module_resolution <= NormalizedModuleResolution::NodeNext)
            || self.resolve_package_json_exports()
            || self.resolve_package_json_imports()
    }
}

pub(super) fn get_target(target: Option<RawTarget>) -> Target {
    let Some(target) = target else {
        return Target::ES5;
        // return Target::latest_standard();
    };
    match target {
        RawTarget::ES3 => Target::ESNext,
        RawTarget::ES5 => Target::ES5,
        RawTarget::ES6 => Target::ES2015,
        RawTarget::ES2015 => Target::ES2015,
        RawTarget::ES2016 => Target::ES2016,
        RawTarget::ES2017 => Target::ES2017,
        RawTarget::ES2018 => Target::ES2018,
        RawTarget::ES2019 => Target::ES2019,
        RawTarget::ES2020 => Target::ES2020,
        RawTarget::ES2021 => Target::ES2021,
        RawTarget::ES2022 => Target::ES2022,
        RawTarget::ES2023 => Target::ES2023,
        RawTarget::ES2024 => Target::ES2024,
        RawTarget::ES2025 => Target::ES2025,
        RawTarget::ESNext => Target::ESNext,
        RawTarget::JSON => Target::JSON,
    }
}

pub(super) fn get_module(module: Option<RawModule>, target: Target) -> Module {
    if let Some(module) = module {
        match module {
            RawModule::CommonJS => Module::CommonJS,
            RawModule::AMD => Module::AMD,
            RawModule::UMD => Module::UMD,
            RawModule::System => Module::System,
            RawModule::ES2015 | RawModule::ES6 => Module::ES2015,
            RawModule::ES2020 => Module::ES2020,
            RawModule::ES2022 => Module::ES2022,
            RawModule::ESNext => Module::ESNext,
            RawModule::None => Module::None,
            RawModule::Node16 => Module::Node16,
            RawModule::Node18 => Module::Node18,
            RawModule::Node20 => Module::Node20,
            RawModule::NodeNext => Module::NodeNext,
            RawModule::Preserve => Module::Preserve,
        }
    } else if target == Target::ESNext {
        Module::ESNext
    } else if target >= Target::ES2022 {
        Module::ES2022
    } else if target >= Target::ES2020 {
        Module::ES2020
    } else if target >= Target::ES2015 {
        Module::ES2015
    } else {
        Module::CommonJS
    }
}

pub(super) fn get_module_resolution(
    module_resolution: Option<RawModuleResolution>,
    module: Module,
) -> NormalizedModuleResolution {
    if let Some(module_resolution) = module_resolution {
        match module_resolution {
            RawModuleResolution::Node => NormalizedModuleResolution::Node10,
            RawModuleResolution::Node10 => NormalizedModuleResolution::Node10,
            RawModuleResolution::Node16 => NormalizedModuleResolution::Node16,
            RawModuleResolution::NodeNext => NormalizedModuleResolution::NodeNext,
            RawModuleResolution::Bundler => NormalizedModuleResolution::Bundler,
            RawModuleResolution::Classic => NormalizedModuleResolution::Classic,
        }
    } else if matches!(
        module,
        Module::None | Module::AMD | Module::UMD | Module::System
    ) {
        NormalizedModuleResolution::Classic
    } else if module == Module::NodeNext {
        NormalizedModuleResolution::NodeNext
    } else if Module::Node16 <= module && module <= Module::NodeNext {
        NormalizedModuleResolution::Node16
    } else {
        NormalizedModuleResolution::Bundler
    }
}

pub(super) fn get_resolve_json_module(
    resolve_json_module: Option<bool>,
    module: Module,
    module_resolution: NormalizedModuleResolution,
) -> bool {
    if let Some(resolve_json_module) = resolve_json_module {
        resolve_json_module
    } else if matches!(module, Module::Node20 | Module::NodeNext) {
        true
    } else {
        module_resolution == NormalizedModuleResolution::Bundler
    }
}

pub(super) fn get_resolve_package_json_exports(
    resolve_package_json_exports: Option<bool>,
    module_resolution: NormalizedModuleResolution,
) -> bool {
    if !module_resolution.supports_package_json_exports_and_imports() {
        return false;
    }
    if let Some(custom) = resolve_package_json_exports {
        return custom;
    }
    matches!(
        module_resolution,
        NormalizedModuleResolution::Node16
            | NormalizedModuleResolution::NodeNext
            | NormalizedModuleResolution::Bundler
    )
}

pub(super) fn get_resolve_package_json_imports(
    resolve_package_json_imports: Option<bool>,
    module_resolution: NormalizedModuleResolution,
) -> bool {
    if !module_resolution.supports_package_json_exports_and_imports() {
        return false;
    }
    if let Some(custom) = resolve_package_json_imports {
        return custom;
    }
    matches!(
        module_resolution,
        NormalizedModuleResolution::Node16
            | NormalizedModuleResolution::NodeNext
            | NormalizedModuleResolution::Bundler
    )
}

#[test]
fn test_resolve_json_module() {
    assert!(get_resolve_json_module(
        Some(true),
        Module::None,
        NormalizedModuleResolution::Classic
    ));
    assert!(!get_resolve_json_module(
        Some(false),
        Module::None,
        NormalizedModuleResolution::Classic
    ));
    assert!(get_resolve_json_module(
        None,
        Module::Node20,
        NormalizedModuleResolution::Classic
    ));
    assert!(get_resolve_json_module(
        None,
        Module::NodeNext,
        NormalizedModuleResolution::Classic
    ));
    assert!(!get_resolve_json_module(
        None,
        Module::None,
        NormalizedModuleResolution::Classic
    ));
    assert!(get_resolve_json_module(
        None,
        Module::ES2015,
        NormalizedModuleResolution::Bundler
    ));
}

#[test]
fn test_get_target() {
    // assert_eq!(get_target(None), Target::latest_standard());
    assert_eq!(get_target(Some(RawTarget::ES3)), Target::ESNext);
    assert_eq!(get_target(Some(RawTarget::ES5)), Target::ES5);
    assert_eq!(get_target(Some(RawTarget::ES6)), Target::ES2015);
    assert_eq!(get_target(Some(RawTarget::ES2015)), Target::ES2015);
    assert_eq!(get_target(Some(RawTarget::ES2016)), Target::ES2016);
    assert_eq!(get_target(Some(RawTarget::ES2017)), Target::ES2017);
    assert_eq!(get_target(Some(RawTarget::ES2018)), Target::ES2018);
    assert_eq!(get_target(Some(RawTarget::ES2019)), Target::ES2019);
    assert_eq!(get_target(Some(RawTarget::ES2020)), Target::ES2020);
    assert_eq!(get_target(Some(RawTarget::ES2021)), Target::ES2021);
    assert_eq!(get_target(Some(RawTarget::ES2022)), Target::ES2022);
    assert_eq!(get_target(Some(RawTarget::ES2023)), Target::ES2023);
    assert_eq!(get_target(Some(RawTarget::ES2024)), Target::ES2024);
    assert_eq!(get_target(Some(RawTarget::ES2025)), Target::ES2025);
    assert_eq!(get_target(Some(RawTarget::ESNext)), Target::ESNext);
    assert_eq!(get_target(Some(RawTarget::JSON)), Target::JSON);
}

#[test]
fn test_get_module() {
    fn ensure_get_module(module: RawModule, expected: Module) {
        assert_eq!(get_module(Some(module), Target::ESNext), expected);
        assert_eq!(get_module(Some(module), Target::ES2022), expected);
        assert_eq!(get_module(Some(module), Target::ES2020), expected);
        assert_eq!(get_module(Some(module), Target::ES2015), expected);
        assert_eq!(get_module(Some(module), Target::ES5), expected);
    }
    ensure_get_module(RawModule::CommonJS, Module::CommonJS);
    ensure_get_module(RawModule::AMD, Module::AMD);
    ensure_get_module(RawModule::UMD, Module::UMD);
    ensure_get_module(RawModule::System, Module::System);
    ensure_get_module(RawModule::ES2015, Module::ES2015);
    ensure_get_module(RawModule::ES6, Module::ES2015);
    ensure_get_module(RawModule::ES2020, Module::ES2020);
    ensure_get_module(RawModule::ES2022, Module::ES2022);
    ensure_get_module(RawModule::ESNext, Module::ESNext);
    ensure_get_module(RawModule::None, Module::None);
    ensure_get_module(RawModule::Node16, Module::Node16);
    ensure_get_module(RawModule::Node18, Module::Node18);
    ensure_get_module(RawModule::Node20, Module::Node20);
    ensure_get_module(RawModule::NodeNext, Module::NodeNext);
    ensure_get_module(RawModule::Preserve, Module::Preserve);
    assert_eq!(get_module(None, Target::latest()), Module::ESNext);
    assert_eq!(get_module(None, Target::latest_standard()), Module::ES2022);
    assert_eq!(get_module(None, Target::JSON), Module::ES2022);
    assert_eq!(get_module(None, Target::ESNext), Module::ESNext);
    assert_eq!(get_module(None, Target::ES2025), Module::ES2022);
    assert_eq!(get_module(None, Target::ES2024), Module::ES2022);
    assert_eq!(get_module(None, Target::ES2023), Module::ES2022);
    assert_eq!(get_module(None, Target::ES2022), Module::ES2022);
    assert_eq!(get_module(None, Target::ES2021), Module::ES2020);
    assert_eq!(get_module(None, Target::ES2020), Module::ES2020);
    assert_eq!(get_module(None, Target::ES2019), Module::ES2015);
    assert_eq!(get_module(None, Target::ES2018), Module::ES2015);
    assert_eq!(get_module(None, Target::ES2017), Module::ES2015);
    assert_eq!(get_module(None, Target::ES2016), Module::ES2015);
    assert_eq!(get_module(None, Target::ES2015), Module::ES2015);
    assert_eq!(get_module(None, Target::ES5), Module::CommonJS);
    assert_eq!(get_module(None, Target::ES3), Module::CommonJS);
}

#[test]
fn test_get_module_resolution() {
    fn ensure(m: RawModuleResolution, e: NormalizedModuleResolution) {
        assert_eq!(get_module_resolution(Some(m), Module::None), e);
        assert_eq!(get_module_resolution(Some(m), Module::CommonJS), e);
        assert_eq!(get_module_resolution(Some(m), Module::AMD), e);
        assert_eq!(get_module_resolution(Some(m), Module::UMD), e);
        assert_eq!(get_module_resolution(Some(m), Module::System), e);
        assert_eq!(get_module_resolution(Some(m), Module::ES2015), e);
        assert_eq!(get_module_resolution(Some(m), Module::ES2020), e);
        assert_eq!(get_module_resolution(Some(m), Module::ES2022), e);
        assert_eq!(get_module_resolution(Some(m), Module::ESNext), e);
        assert_eq!(get_module_resolution(Some(m), Module::ESNext), e);
        assert_eq!(get_module_resolution(Some(m), Module::Node16), e);
        assert_eq!(get_module_resolution(Some(m), Module::Node18), e);
        assert_eq!(get_module_resolution(Some(m), Module::Node20), e);
        assert_eq!(get_module_resolution(Some(m), Module::NodeNext), e);
        assert_eq!(get_module_resolution(Some(m), Module::Preserve), e);
    }
    ensure(
        RawModuleResolution::Node,
        NormalizedModuleResolution::Node10,
    );
    ensure(
        RawModuleResolution::Node10,
        NormalizedModuleResolution::Node10,
    );
    ensure(
        RawModuleResolution::Node16,
        NormalizedModuleResolution::Node16,
    );
    ensure(
        RawModuleResolution::NodeNext,
        NormalizedModuleResolution::NodeNext,
    );
    ensure(
        RawModuleResolution::Bundler,
        NormalizedModuleResolution::Bundler,
    );
    ensure(
        RawModuleResolution::Classic,
        NormalizedModuleResolution::Classic,
    );
    assert_eq!(
        get_module_resolution(None, Module::None),
        NormalizedModuleResolution::Classic
    );
    assert_eq!(
        get_module_resolution(None, Module::CommonJS),
        NormalizedModuleResolution::Bundler
    );
    assert_eq!(
        get_module_resolution(None, Module::AMD),
        NormalizedModuleResolution::Classic
    );
    assert_eq!(
        get_module_resolution(None, Module::UMD),
        NormalizedModuleResolution::Classic
    );
    assert_eq!(
        get_module_resolution(None, Module::System),
        NormalizedModuleResolution::Classic
    );
    assert_eq!(
        get_module_resolution(None, Module::ES2015),
        NormalizedModuleResolution::Bundler
    );
    assert_eq!(
        get_module_resolution(None, Module::ES2020),
        NormalizedModuleResolution::Bundler
    );
    assert_eq!(
        get_module_resolution(None, Module::ES2022),
        NormalizedModuleResolution::Bundler
    );
    assert_eq!(
        get_module_resolution(None, Module::ESNext),
        NormalizedModuleResolution::Bundler
    );
    assert_eq!(
        get_module_resolution(None, Module::Node16),
        NormalizedModuleResolution::Node16
    );
    assert_eq!(
        get_module_resolution(None, Module::Node18),
        NormalizedModuleResolution::Node16
    );
    assert_eq!(
        get_module_resolution(None, Module::Node20),
        NormalizedModuleResolution::Node16
    );
    assert_eq!(
        get_module_resolution(None, Module::NodeNext),
        NormalizedModuleResolution::NodeNext
    );
    assert_eq!(
        get_module_resolution(None, Module::Preserve),
        NormalizedModuleResolution::Bundler
    );
}

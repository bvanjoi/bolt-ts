use super::OutDir;
use super::normalized::get_module;
use super::normalized::get_module_resolution;
use super::normalized::get_resolve_json_module;
use super::normalized::get_resolve_package_json_exports;
use super::normalized::get_resolve_package_json_imports;
use super::normalized::get_target;

macro_rules! with_option {
    ($s: ident, $(($option: ident, $ty: ty)),* $(,)?) => {
        #[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
        #[serde(rename_all = "camelCase")]
        pub struct $s {
            $(pub(super) $option: Option<$ty>,)*
        }
        impl $s {
            paste::paste! {
                $(
                    pub fn [<with_ $option>](mut self, $option: $ty) -> Self {
                        self.$option = Some($option);
                        self
                    }
                    pub fn [<with_ $option _if_none>](mut self, $option: $ty) -> Self {
                        if self.$option.is_none() {
                            self.$option = Some($option);
                        }
                        self
                    }
                    pub fn [<config_ $option>](mut self, f: impl FnOnce($ty) -> $ty) -> Self {
                        self.$option = match self.$option {
                            Some(c) => Some(f(c)),
                            None => unreachable!(),
                        };
                        self
                    }
                )*
            }
        }
    };
}

with_option!(
    RawCompilerOptions,
    (out_dir, String),
    (no_emit, bool),
    (declaration, bool),
    (strict, bool),
    (strict_null_checks, bool),
    (strict_function_types, bool),
    (strict_bind_call_apply, bool),
    (no_implicit_any, bool),
    (no_implicit_this, bool),
    (no_implicit_returns, bool),
    (no_unchecked_indexed_access, bool),
    (no_strict_generic_checks, bool),
    (no_fallthrough_cases_in_switch, bool),
    (always_strict, bool),
    (exact_optional_property_types, bool),
    (allow_unused_labels, bool),
    (allow_unreachable_code, bool),
    (preserve_symlinks, bool),
    (use_define_for_class_fields, bool),
    (use_unknown_in_catch_variables, bool),
    (strict_property_initialization, bool),
    (no_unused_locals, bool),
    (no_unused_parameters, bool),
    (resolve_json_module, bool),
    (resolve_package_json_exports, bool),
    (resolve_package_json_imports, bool),
    (es_module_interop, bool),
    (target, RawTarget),
    (module, RawModule),
    (module_resolution, RawModuleResolution),
    (lib, Vec<Lib>),
    (custom_conditions, Vec<String>)
);

impl RawCompilerOptions {
    pub fn normalize(self) -> super::NormalizedCompilerOptions {
        let mut flags = super::CompilerOptionFlags::empty();
        let target = get_target(self.target);
        if self.no_emit.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_EMIT);
        }
        if self.declaration.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::DECLARATION);
        }
        let strict = self.strict.unwrap_or_default();
        if strict {
            flags.insert(super::CompilerOptionFlags::STRICT);
        }
        let get_strict_option_value = |v: Option<bool>| v.unwrap_or(strict);
        if get_strict_option_value(self.strict_null_checks) {
            flags.insert(super::CompilerOptionFlags::STRICT_NULL_CHECKS);
        }
        if get_strict_option_value(self.strict_property_initialization) {
            flags.insert(super::CompilerOptionFlags::STRICT_PROPERTY_INITIALIZATION);
        }
        if get_strict_option_value(self.no_implicit_any) {
            flags.insert(super::CompilerOptionFlags::NO_IMPLICIT_ANY);
        }
        if get_strict_option_value(self.no_implicit_this) {
            flags.insert(super::CompilerOptionFlags::NO_IMPLICIT_THIS);
        }
        if get_strict_option_value(self.strict_bind_call_apply) {
            flags.insert(super::CompilerOptionFlags::STRICT_BIND_CALL_APPLY);
        }
        if self.no_unchecked_indexed_access.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_UNCHECKED_INDEXED_ACCESS);
        }
        if self.always_strict != Some(false) {
            flags.insert(super::CompilerOptionFlags::ALWAYS_STRICT);
        }
        if self.exact_optional_property_types.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::EXACT_OPTIONAL_PROPERTY_TYPES);
        }
        if self.no_strict_generic_checks.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_STRICT_GENERIC_CHECKS);
        }
        if self.no_fallthrough_cases_in_switch.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_FALLTHROUGH_CASES_IN_SWITCH);
        }
        if get_strict_option_value(self.strict_function_types) {
            flags.insert(super::CompilerOptionFlags::STRICT_FUNCTION_TYPES);
        }
        if self.preserve_symlinks.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::PRESERVE_SYMLINKS);
        }
        if self.no_implicit_returns.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_IMPLICIT_RETURNS);
        }
        if self.no_unused_locals.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_UNUSED_LOCALS);
        }
        if self.no_unused_parameters.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_UNUSED_PARAMETERS);
        }
        if get_strict_option_value(self.use_unknown_in_catch_variables) {
            flags.insert(super::CompilerOptionFlags::USE_UNKNOWN_IN_CATCH_VARIABLES);
        }
        match self.use_define_for_class_fields {
            Some(true) => flags.insert(super::CompilerOptionFlags::USE_DEFINE_FOR_CLASS_FIELDS),
            None if target >= super::Target::ES2022 => {
                flags.insert(super::CompilerOptionFlags::USE_DEFINE_FOR_CLASS_FIELDS);
            }
            _ => {}
        }

        let allow_unused_labels = match self.allow_unused_labels {
            Some(true) => super::AllowUnusedLabels::Warning,
            Some(false) => super::AllowUnusedLabels::Deny,
            None => super::AllowUnusedLabels::Allow,
        };

        let allow_unreachable_code = match self.allow_unreachable_code {
            Some(true) => super::AllowUnreachableCode::Allow,
            Some(false) => super::AllowUnreachableCode::Deny,
            None => super::AllowUnreachableCode::Warning,
        };

        let custom_conditions = self.custom_conditions.unwrap_or_default();
        let out_dir = self.out_dir.map_or(OutDir::default(), OutDir::Custom);
        let module = get_module(self.module, target);
        let module_resolution = get_module_resolution(self.module_resolution, module);

        if get_resolve_json_module(self.resolve_json_module, module, module_resolution) {
            flags.insert(super::CompilerOptionFlags::RESOLVE_JSON_MODULE);
        }

        if get_resolve_package_json_exports(self.resolve_package_json_exports, module_resolution) {
            flags.insert(super::CompilerOptionFlags::RESOLVE_PACKAGE_JSON_EXPORTS);
        }

        if get_resolve_package_json_imports(self.resolve_package_json_imports, module_resolution) {
            flags.insert(super::CompilerOptionFlags::RESOLVE_PACKAGE_JSON_IMPORTS);
        }

        super::NormalizedCompilerOptions {
            out_dir,
            target,
            flags,
            allow_unused_labels,
            allow_unreachable_code,
            module,
            module_resolution,
            custom_conditions,
            lib: self.lib,
        }
    }
}

with_option!(
    RawTsConfig,
    (include, Vec<String>),
    (compiler_options, RawCompilerOptions)
);

impl RawTsConfig {
    pub fn normalize(self) -> super::NormalizedTsConfig {
        const DEFAULT_INCLUDE: &[&str] = &["**/*"];
        let include = self
            .include
            .unwrap_or_else(|| DEFAULT_INCLUDE.iter().map(|&s| s.to_string()).collect());
        let compiler_options = self.compiler_options.unwrap_or_default().normalize();
        super::NormalizedTsConfig {
            include,
            compiler_options,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Deserialize, serde::Serialize, Default)]
pub enum RawModuleResolution {
    #[default]
    #[serde(alias = "node")]
    Node,
    #[serde(alias = "node10")]
    Node10,
    #[serde(alias = "node16")]
    Node16,
    #[serde(alias = "nodeNext")]
    NodeNext,
    #[serde(alias = "bundler")]
    Bundler,
    #[serde(alias = "classic")]
    Classic,
}

#[derive(Debug, Clone, Default, PartialEq, serde::Deserialize, serde::Serialize)]
pub enum RawTarget {
    #[serde(alias = "es3")]
    ES3,
    #[default]
    #[serde(alias = "es5")]
    ES5,
    #[serde(alias = "es6")]
    ES6,
    #[serde(alias = "es2015")]
    ES2015,
    #[serde(alias = "es2016")]
    ES2016,
    #[serde(alias = "es2017")]
    ES2017,
    #[serde(alias = "es2018")]
    ES2018,
    #[serde(alias = "es2019")]
    ES2019,
    #[serde(alias = "es2020")]
    ES2020,
    #[serde(alias = "es2021")]
    ES2021,
    #[serde(alias = "es2022")]
    ES2022,
    #[serde(alias = "es2023")]
    ES2023,
    #[serde(alias = "es2024")]
    ES2024,
    #[serde(alias = "es2025")]
    ES2025,
    #[serde(alias = "esnext")]
    ESNext,
    #[serde(alias = "json")]
    JSON,
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Deserialize, serde::Serialize)]
pub enum RawModule {
    #[serde(alias = "none")]
    None,
    #[serde(alias = "commonjs")]
    CommonJS,
    #[serde(alias = "amd")]
    AMD,
    #[serde(alias = "umd")]
    UMD,
    #[serde(alias = "system")]
    System,
    #[serde(alias = "es6")]
    ES6,
    #[serde(alias = "es2015")]
    ES2015,
    #[serde(alias = "es2020")]
    ES2020,
    #[serde(alias = "es2022")]
    ES2022,
    #[serde(alias = "esnext")]
    ESNext,
    #[serde(alias = "node16")]
    Node16,
    #[serde(alias = "node18")]
    Node18,
    #[serde(alias = "node20")]
    Node20,
    #[serde(alias = "nodenext")]
    NodeNext,
    #[serde(alias = "preserve")]
    Preserve,
}

#[derive(Debug, Clone, Default, PartialEq, serde::Deserialize, serde::Serialize)]
pub enum Lib {
    #[default]
    #[serde(alias = "es5")]
    ES5,
    #[serde(alias = "es2015")]
    ES2015,
    #[serde(alias = "es6")]
    ES6,
    #[serde(alias = "es2016")]
    ES2016,
    #[serde(alias = "es7")]
    ES7,
    #[serde(alias = "es2017")]
    ES2017,
    #[serde(alias = "es2018")]
    ES2018,
    #[serde(alias = "es2019")]
    ES2019,
    #[serde(alias = "es2020")]
    ES2020,
    #[serde(alias = "es2021")]
    ES2021,
    #[serde(alias = "es2022")]
    ES2022,
    #[serde(alias = "es2023")]
    ES2023,
    #[serde(alias = "esnext")]
    ESNext,
    #[serde(alias = "dom")]
    DOM,
    #[serde(alias = "webworker")]
    WebWorker,
    #[serde(alias = "scripthost")]
    ScriptHost,
    #[serde(alias = "dom.iterable")]
    DOMIterable,
    #[serde(alias = "es2015.core")]
    ES2015Core,
    #[serde(alias = "es2015.collection")]
    ES2015Collection,
    #[serde(alias = "es2015.generator")]
    ES2015Generator,
    #[serde(alias = "es2015.iterable")]
    ES2015Iterable,
    #[serde(alias = "es2015.promise")]
    ES2015Promise,
    #[serde(alias = "es2015.proxy")]
    ES2015Proxy,
    #[serde(alias = "es2015.reflect")]
    ES2015Reflect,
    #[serde(alias = "es2015.symbol")]
    ES2015Symbol,
    #[serde(alias = "es2015.symbol.wellknown")]
    ES2015SymbolWellKnown,
    #[serde(alias = "es2016.array.include")]
    ES2016ArrayInclude,
    #[serde(alias = "es2017.object")]
    ES2017Object,
    #[serde(alias = "es2017.intl")]
    ES2017Intl,
    #[serde(alias = "es2017.sharedmemory")]
    ES2017SharedMemory,
    #[serde(alias = "es2017.string")]
    ES2017String,
    #[serde(alias = "es2017.typedarrays")]
    ES2017TypedArrays,
    #[serde(alias = "es2018.intl")]
    ES2018Intl,
    #[serde(alias = "es2018.promise")]
    ES2018Promise,
    #[serde(alias = "es2018.regexp")]
    ES2018RegExp,
    #[serde(alias = "es2019.array")]
    ES2019Array,
    #[serde(alias = "es2019.object")]
    ES2019Object,
    #[serde(alias = "es2019.string")]
    ES2019String,
    #[serde(alias = "es2019.symbol")]
    ES2019Symbol,
    #[serde(alias = "es2020.string")]
    ES2020String,
    #[serde(alias = "es2020.symbol.wellknown")]
    ES2020SymbolWellKnown,
    #[serde(alias = "es2020.intl")]
    ES2021Promise,
    #[serde(alias = "es2021.promise")]
    ES2021String,
    #[serde(alias = "es2021.weakref")]
    ES2021WeakRef,
    #[serde(alias = "esnext.asynciterable")]
    ESNextAsyncIterable,
    #[serde(alias = "esnext.array")]
    ESNextArray,
    #[serde(alias = "esnext.intl")]
    ESNextIntl,
    #[serde(alias = "esnext.symbol")]
    ESNextSymbol,
}

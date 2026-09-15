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
    (declaration, bool),
    (custom_conditions, Vec<String>),
    (strict, bool),
    (strict_null_checks, bool),
    (strict_function_types, bool),
    (strict_bind_call_apply, bool),
    (strict_property_initialization, bool),
    (no_emit, bool),
    (no_lib, bool),
    (no_implicit_any, bool),
    (no_implicit_this, bool),
    (no_implicit_returns, bool),
    (no_unchecked_indexed_access, bool),
    (no_strict_generic_checks, bool),
    (no_fallthrough_cases_in_switch, bool),
    (no_error_truncation, bool),
    (no_unused_locals, bool),
    (no_unused_parameters, bool),
    (always_strict, bool),
    (allow_unused_labels, bool),
    (allow_unreachable_code, bool),
    (es_module_interop, bool),
    (exact_optional_property_types, bool),
    (preserve_symlinks, bool),
    (use_define_for_class_fields, bool),
    (use_unknown_in_catch_variables, bool),
    (resolve_json_module, bool),
    (resolve_package_json_exports, bool),
    (resolve_package_json_imports, bool),
    (remove_comments, bool),
    (target, RawTarget),
    (module, RawModule),
    (module_resolution, RawModuleResolution),
    (lib, Vec<Lib>),
    (jsx, JSX),
    (check_js, bool),
);

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub enum JSX {
    #[serde(alias = "react")]
    React,
    #[serde(alias = "react-jsx")]
    ReactJSX,
    #[serde(alias = "react-jsxdev")]
    ReactJSXDev,
    #[serde(alias = "react-native")]
    ReactNative,
    #[serde(alias = "preserve")]
    Preserve,
}

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
        let strict = self.strict.unwrap_or(true);
        if strict {
            flags.insert(super::CompilerOptionFlags::STRICT);
        }
        if self.check_js.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::CHECK_JS);
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
        if self.no_lib.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_LIB);
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
        if self.no_error_truncation.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::NO_ERROR_TRUNCATION);
        }
        if self.remove_comments.unwrap_or_default() {
            flags.insert(super::CompilerOptionFlags::REMOVE_COMMENTS);
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
            jsx: self.jsx,
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
    // JavaScript only
    #[default]
    #[serde(alias = "es5")]
    ES5,
    #[serde(alias = "es6")]
    ES6,
    #[serde(alias = "es2015")]
    ES2015,
    #[serde(alias = "es7")]
    ES7,
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
    // Host only
    #[serde(alias = "dom")]
    Dom,
    #[serde(alias = "dom.iterable")]
    DOMIterable,
    #[serde(alias = "dom.asynciterable")]
    DOMAsyncIterable,
    #[serde(alias = "webworker")]
    WebWorker,
    #[serde(alias = "webworker.importscripts")]
    WebWorkerImportScripts,
    #[serde(alias = "webworker.iterable")]
    WebWorkerIterable,
    #[serde(alias = "webworker.asynciterable")]
    WebWorkerAsyncIterable,
    #[serde(alias = "scripthost")]
    ScriptHost,
    // ES2015 and later By-feature options
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
    #[serde(alias = "es2016.intl")]
    ES2016Intl,
    #[serde(alias = "es2017.arraybuffer")]
    ES2017ArrayBuffer,
    #[serde(alias = "es2017.date")]
    ES2017Date,
    #[serde(alias = "es2017.object")]
    ES2017Object,
    #[serde(alias = "es2017.sharedmemory")]
    ES2017SharedMemory,
    #[serde(alias = "es2017.string")]
    ES2017String,
    #[serde(alias = "es2017.intl")]
    ES2017Intl,
    #[serde(alias = "es2017.typedarrays")]
    ES2017TypedArrays,
    #[serde(alias = "es2018.asyncgenerator")]
    ES2018AsyncGenerator,
    #[serde(alias = "es2018.asynciterable")]
    ES2018AsyncIterable,
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
    #[serde(alias = "es2019.intl")]
    ES2019Intl,
    #[serde(alias = "es2020.bigint")]
    ES2020BigInt,
    #[serde(alias = "es2020.date")]
    ES2020Date,
    #[serde(alias = "es2020.promise")]
    ES2020Promise,
    #[serde(alias = "es2020.sharedmemory")]
    ES2020SharedMemory,
    #[serde(alias = "es2020.string")]
    ES2020String,
    #[serde(alias = "es2020.symbol.wellknown")]
    ES2020SymbolWellKnown,
    #[serde(alias = "es2020.intl")]
    ES2020Intl,
    #[serde(alias = "es2020.number")]
    ES2020Number,
    #[serde(alias = "es2021.promise")]
    ES2021Promise,
    #[serde(alias = "es2021.string")]
    ES2021String,
    #[serde(alias = "es2021.weakref")]
    ES2021WeakRef,
    #[serde(alias = "es2021.intl")]
    ES2021Intl,
    #[serde(alias = "es2022.array")]
    ES2022Array,
    #[serde(alias = "es2022.error")]
    ES2022Error,
    #[serde(alias = "es2022.intl")]
    ES2022Intl,
    #[serde(alias = "es2022.object")]
    ES2022Object,
    #[serde(alias = "es2022.string")]
    ES2022String,
    #[serde(alias = "es2022.regexp")]
    ES2022RegExp,
    #[serde(alias = "es2023.array")]
    ES2023Array,
    #[serde(alias = "es2023.collection")]
    ES2023Collection,
    #[serde(alias = "es2023.intl")]
    ES2023Intl,
    #[serde(alias = "es2024.arraybuffer")]
    ES2024ArrayBuffer,
    #[serde(alias = "es2024.collection")]
    ES2024Collection,
    #[serde(alias = "es2024.object")]
    ES2024Object,
    #[serde(alias = "es2024.promise")]
    ES2024Promise,
    #[serde(alias = "es2024.regexp")]
    ES2024RegExp,
    #[serde(alias = "es2024.sharedmemory")]
    ES2024SharedMemory,
    #[serde(alias = "es2024.string")]
    ES2024String,
    #[serde(alias = "es2025.collection")]
    ES2025Collection,
    #[serde(alias = "es2025.float16")]
    ES2025Float16,
    #[serde(alias = "es2025.intl")]
    ES2025Intl,
    #[serde(alias = "es2025.iterator")]
    ES2025Iterator,
    #[serde(alias = "es2025.promise")]
    ES2025Promise,
    #[serde(alias = "es2025.regexp")]
    ES2025RegExp,
    // Fallback for backward compatibility
    #[serde(alias = "esnext.asynciterable")]
    ESNextAsyncIterable,
    #[serde(alias = "esnext.symbol")]
    ESNextSymbol,
    #[serde(alias = "esnext.bigint")]
    ESNextBigInt,
    #[serde(alias = "esnext.weakref")]
    ESNextWeakRef,
    #[serde(alias = "esnext.object")]
    ESNextObject,
    #[serde(alias = "esnext.regexp")]
    ESNextRegExp,
    #[serde(alias = "esnext.string")]
    ESNextString,
    #[serde(alias = "esnext.float16")]
    ESNextFloat16,
    #[serde(alias = "esnext.iterator")]
    ESNextIterator,
    #[serde(alias = "esnext.promise")]
    ESNextPromise,
    // ESNext By-feature options
    #[serde(alias = "esnext.array")]
    ESNextArray,
    #[serde(alias = "esnext.collection")]
    ESNextCollection,
    #[serde(alias = "esnext.date")]
    ESNextDate,
    #[serde(alias = "esnext.decorators")]
    ESNextDecorators,
    #[serde(alias = "esnext.disposable")]
    ESNextDisposable,
    #[serde(alias = "esnext.error")]
    ESNextError,
    #[serde(alias = "esnext.intl")]
    ESNextIntl,
    #[serde(alias = "esnext.sharedmemory")]
    ESNextSharedMemory,
    #[serde(alias = "esnext.temporal")]
    ESNextTemporal,
    #[serde(alias = "esnext.typedarrays")]
    ESNextTypedArrays,
    // Decorators
    #[serde(alias = "decorators")]
    Decorators,
    #[serde(alias = "decorators.legacy")]
    DecoratorsLegacy,
}

impl Lib {
    pub const fn entry(&self) -> &'static str {
        match *self {
            Lib::ES5 => "lib.es5.d.ts",
            Lib::ES6 => "lib.es2015.d.ts",
            Lib::ES2015 => "lib.es2015.d.ts",
            Lib::ES7 => "lib.es2016.d.ts",
            Lib::ES2016 => "lib.es2016.d.ts",
            Lib::ES2017 => "lib.es2017.d.ts",
            Lib::ES2018 => "lib.es2018.d.ts",
            Lib::ES2019 => "lib.es2019.d.ts",
            Lib::ES2020 => "lib.es2020.d.ts",
            Lib::ES2021 => "lib.es2021.d.ts",
            Lib::ES2022 => "lib.es2022.d.ts",
            Lib::ES2023 => "lib.es2023.d.ts",
            Lib::ES2024 => "lib.es2024.d.ts",
            Lib::ES2025 => "lib.es2025.d.ts",
            Lib::ESNext => "lib.esnext.d.ts",
            Lib::Dom => "lib.dom.d.ts",
            Lib::DOMIterable => "lib.dom.iterable.d.ts",
            Lib::DOMAsyncIterable => "lib.dom.asynciterable.d.ts",
            Lib::WebWorker => "lib.webworker.d.ts",
            Lib::WebWorkerImportScripts => "lib.webworker.importscripts.d.ts",
            Lib::WebWorkerIterable => "lib.webworker.iterable.d.ts",
            Lib::WebWorkerAsyncIterable => "lib.webworker.asynciterable.d.ts",
            Lib::ScriptHost => "lib.scripthost.d.ts",
            Lib::ES2015Core => "lib.es2015.core.d.ts",
            Lib::ES2015Collection => "lib.es2015.collection.d.ts",
            Lib::ES2015Generator => "lib.es2015.generator.d.ts",
            Lib::ES2015Iterable => "lib.es2015.iterable.d.ts",
            Lib::ES2015Promise => "lib.es2015.promise.d.ts",
            Lib::ES2015Proxy => "lib.es2015.proxy.d.ts",
            Lib::ES2015Reflect => "lib.es2015.reflect.d.ts",
            Lib::ES2015Symbol => "lib.es2015.symbol.d.ts",
            Lib::ES2015SymbolWellKnown => "lib.es2015.symbol.wellknown.d.ts",
            Lib::ES2016ArrayInclude => "lib.es2016.array.include.d.ts",
            Lib::ES2016Intl => "lib.es2016.intl.d.ts",
            Lib::ES2017ArrayBuffer => "lib.es2017.arraybuffer.d.ts",
            Lib::ES2017Date => "lib.es2017.date.d.ts",
            Lib::ES2017Object => "lib.es2017.object.d.ts",
            Lib::ES2017SharedMemory => "lib.es2017.sharedmemory.d.ts",
            Lib::ES2017String => "lib.es2017.string.d.ts",
            Lib::ES2017Intl => "lib.es2017.intl.d.ts",
            Lib::ES2017TypedArrays => "lib.es2017.typedarrays.d.ts",
            Lib::ES2018AsyncGenerator => "lib.es2018.asyncgenerator.d.ts",
            Lib::ES2018AsyncIterable => "lib.es2018.asynciterable.d.ts",
            Lib::ES2018Intl => "lib.es2018.intl.d.ts",
            Lib::ES2018Promise => "lib.es2018.promise.d.ts",
            Lib::ES2018RegExp => "lib.es2018.regexp.d.ts",
            Lib::ES2019Array => "lib.es2019.array.d.ts",
            Lib::ES2019Object => "lib.es2019.object.d.ts",
            Lib::ES2019String => "lib.es2019.string.d.ts",
            Lib::ES2019Symbol => "lib.es2019.symbol.d.ts",
            Lib::ES2019Intl => "lib.es2019.intl.d.ts",
            Lib::ES2020BigInt => "lib.es2020.bigint.d.ts",
            Lib::ES2020Date => "lib.es2020.date.d.ts",
            Lib::ES2020Promise => "lib.es2020.promise.d.ts",
            Lib::ES2020SharedMemory => "lib.es2020.sharedmemory.d.ts",
            Lib::ES2020String => "lib.es2020.string.d.ts",
            Lib::ES2020SymbolWellKnown => "lib.es2020.symbol.wellknown.d.ts",
            Lib::ES2020Intl => "lib.es2020.intl.d.ts",
            Lib::ES2020Number => "lib.es2020.number.d.ts",
            Lib::ES2021Promise => "lib.es2021.promise.d.ts",
            Lib::ES2021String => "lib.es2021.string.d.ts",
            Lib::ES2021WeakRef => "lib.es2021.weakref.d.ts",
            Lib::ES2021Intl => "lib.es2021.intl.d.ts",
            Lib::ES2022Array => "lib.es2022.array.d.ts",
            Lib::ES2022Error => "lib.es2022.error.d.ts",
            Lib::ES2022Intl => "lib.es2022.intl.d.ts",
            Lib::ES2022Object => "lib.es2022.object.d.ts",
            Lib::ES2022String => "lib.es2022.string.d.ts",
            Lib::ES2022RegExp => "lib.es2022.regexp.d.ts",
            Lib::ES2023Array => "lib.es2023.array.d.ts",
            Lib::ES2023Collection => "lib.es2023.collection.d.ts",
            Lib::ES2023Intl => "lib.es2023.intl.d.ts",
            Lib::ES2024ArrayBuffer => "lib.es2024.arraybuffer.d.ts",
            Lib::ES2024Collection => "lib.es2024.collection.d.ts",
            Lib::ES2024Object => "lib.es2024.object.d.ts",
            Lib::ES2024Promise => "lib.es2024.promise.d.ts",
            Lib::ES2024RegExp => "lib.es2024.regexp.d.ts",
            Lib::ES2024SharedMemory => "lib.es2024.sharedmemory.d.ts",
            Lib::ES2024String => "lib.es2024.string.d.ts",
            Lib::ES2025Collection => "lib.es2025.collection.d.ts",
            Lib::ES2025Float16 => "lib.es2025.float16.d.ts",
            Lib::ES2025Intl => "lib.es2025.intl.d.ts",
            Lib::ES2025Iterator => "lib.es2025.iterator.d.ts",
            Lib::ES2025Promise => "lib.es2025.promise.d.ts",
            Lib::ES2025RegExp => "lib.es2025.regexp.d.ts",
            Lib::ESNextAsyncIterable => "lib.es2018.asynciterable.d.ts",
            Lib::ESNextSymbol => "lib.es2019.symbol.d.ts",
            Lib::ESNextBigInt => "lib.es2020.bigint.d.ts",
            Lib::ESNextWeakRef => "lib.es2021.weakref.d.ts",
            Lib::ESNextObject => "lib.es2024.object.d.ts",
            Lib::ESNextRegExp => "lib.es2024.regexp.d.ts",
            Lib::ESNextString => "lib.es2024.string.d.ts",
            Lib::ESNextFloat16 => "lib.es2025.float16.d.ts",
            Lib::ESNextIterator => "lib.es2025.iterator.d.ts",
            Lib::ESNextPromise => "lib.es2025.promise.d.ts",
            Lib::ESNextArray => "lib.esnext.array.d.ts",
            Lib::ESNextCollection => "lib.esnext.collection.d.ts",
            Lib::ESNextDate => "lib.esnext.date.d.ts",
            Lib::ESNextDecorators => "lib.esnext.decorators.d.ts",
            Lib::ESNextDisposable => "lib.esnext.disposable.d.ts",
            Lib::ESNextError => "lib.esnext.error.d.ts",
            Lib::ESNextIntl => "lib.esnext.intl.d.ts",
            Lib::ESNextSharedMemory => "lib.esnext.sharedmemory.d.ts",
            Lib::ESNextTemporal => "lib.esnext.temporal.d.ts",
            Lib::ESNextTypedArrays => "lib.esnext.typedarrays.d.ts",
            Lib::Decorators => "lib.decorators.d.ts",
            Lib::DecoratorsLegacy => "lib.decorators.legacy.d.ts",
        }
    }
}

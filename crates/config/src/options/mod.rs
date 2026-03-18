#[derive(Debug, Clone, Default, PartialEq)]
pub enum OutDir {
    #[default]
    OwnRoot,
    Custom(String),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Target {
    ES3 = 0,
    ES5 = 1,
    ES2015 = 2,
    ES2016 = 3,
    ES2017 = 4,
    ES2018 = 5,
    ES2019 = 6,
    ES2020 = 7,
    ES2021 = 8,
    ES2022 = 9,
    ES2023 = 10,
    ES2024 = 11,
    ES2025 = 12,
    ESNext = 99,
    JSON = 100,
}

impl Target {
    pub const fn latest() -> Self {
        Self::ESNext
    }
    pub const fn latest_standard() -> Self {
        Self::ES2025
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, PartialOrd)]
pub enum Module {
    #[default]
    None = 0,
    CommonJS = 1,
    AMD = 2,
    UMD = 3,
    System = 4,
    ES2015 = 5,
    ES2020 = 6,
    ES2022 = 7,
    ESNext = 99,
    Node16 = 100,
    Node18 = 101,
    Node20 = 102,
    NodeNext = 199,
    Preserve = 200,
}
impl Module {
    pub fn is_non_node_esm(self) -> bool {
        Self::ES2015 <= self && self <= Self::ES2022 || self == Self::ESNext
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Extension {
    Ts,
    Tsx,
    Dts,
    Js,
    Jsx,
    Json,
    Mjs,
    Mts,
    Dmts,
    Cjs,
    Cts,
    Dcts,
    Empty,
}

impl Extension {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Extension::Ts => "ts",
            Extension::Tsx => "tsx",
            Extension::Dts => "d.ts",
            Extension::Js => "js",
            Extension::Jsx => "jsx",
            Extension::Json => "json",
            Extension::Mjs => "mjs",
            Extension::Mts => "mts",
            Extension::Dmts => "d.mts",
            Extension::Cjs => "cjs",
            Extension::Cts => "cts",
            Extension::Dcts => "d.cts",
            Extension::Empty => "",
        }
    }

    pub const fn as_str_with_dot(&self) -> &'static str {
        match self {
            Extension::Ts => ".ts",
            Extension::Tsx => ".tsx",
            Extension::Dts => ".d.ts",
            Extension::Js => ".js",
            Extension::Jsx => ".jsx",
            Extension::Json => ".json",
            Extension::Mjs => ".mjs",
            Extension::Mts => ".mts",
            Extension::Dmts => ".d.mts",
            Extension::Cjs => ".cjs",
            Extension::Cts => ".cts",
            Extension::Dcts => ".d.cts",
            Extension::Empty => "",
        }
    }

    pub fn extension_of_file_name(filename: &[u8]) -> Extension {
        for extension in &EXTENSIONS_TO_REMOVE {
            if filename.ends_with(extension.as_str_with_dot().as_bytes()) {
                return *extension;
            }
        }
        Extension::Empty
    }

    pub fn file_extension_is(path: &std::path::Path, extension: Extension) -> bool {
        let bytes = path.as_os_str().as_encoded_bytes();
        bytes.ends_with(extension.as_str_with_dot().as_bytes())
    }

    pub fn is_one_of(&self, extensions: &[Extension]) -> bool {
        extensions.contains(self)
    }
}

pub const EXTENSIONS_TO_REMOVE: [Extension; 12] = [
    Extension::Dts,
    Extension::Dmts,
    Extension::Dcts,
    Extension::Mjs,
    Extension::Mts,
    Extension::Cjs,
    Extension::Cts,
    Extension::Ts,
    Extension::Js,
    Extension::Tsx,
    Extension::Jsx,
    Extension::Json,
];

pub const SUPPORTED_TS_EXTENSIONS: &[&[Extension]] = &[
    &[Extension::Ts, Extension::Tsx, Extension::Dts],
    &[Extension::Cts, Extension::Dcts],
    &[Extension::Mts, Extension::Dmts],
];

pub const SUPPORTED_TS_IMPLEMENTATION_EXTENSIONS: &[Extension] = &[
    Extension::Ts,
    Extension::Cts,
    Extension::Mts,
    Extension::Tsx,
];

pub const SUPPORTED_DECLARATION_EXTENSIONS: &[Extension] =
    &[Extension::Dts, Extension::Dcts, Extension::Dmts];

pub const FLATTENED_SUPPORTED_TS_EXTENSIONS: &[Extension] = &[
    Extension::Ts,
    Extension::Tsx,
    Extension::Dts,
    Extension::Cts,
    Extension::Dcts,
    Extension::Mts,
    Extension::Dmts,
];

pub const ALL_SUPPORTED_EXTENSIONS: &[&[Extension]] = &[
    &[
        Extension::Ts,
        Extension::Tsx,
        Extension::Dts,
        Extension::Js,
        Extension::Jsx,
    ],
    &[Extension::Cts, Extension::Dcts, Extension::Cjs],
    &[Extension::Mts, Extension::Dmts, Extension::Mjs],
];

pub const FLATTENED_ALL_SUPPORTED_EXTENSIONS: &[Extension] = &[
    Extension::Ts,
    Extension::Tsx,
    Extension::Dts,
    Extension::Js,
    Extension::Jsx,
    Extension::Cts,
    Extension::Dcts,
    Extension::Cjs,
    Extension::Mts,
    Extension::Dmts,
    Extension::Mjs,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum NormalizedModuleResolution {
    Classic = 1,
    Node10 = 2,
    Node16 = 3,
    NodeNext = 99,
    Bundler = 100,
}

impl NormalizedModuleResolution {
    pub fn supports_package_json_exports_and_imports(self) -> bool {
        (self >= NormalizedModuleResolution::Node16 && self <= NormalizedModuleResolution::NodeNext)
            || self == NormalizedModuleResolution::Bundler
    }
}

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

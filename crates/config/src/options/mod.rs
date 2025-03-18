#[derive(Debug, Clone, Default, PartialEq)]
pub enum OutDir {
    #[default]
    OwnRoot,
    Custom(String),
}

#[derive(Debug, Clone, Default, PartialEq, serde::Deserialize, serde::Serialize)]
pub enum RawTarget {
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
    #[serde(alias = "esnext")]
    ESNext,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Target {
    #[default]
    ES5,
    ES2015,
    ES2016,
    ES2017,
    ES2018,
    ES2019,
    ES2020,
    ES2021,
    ES2022,
    ES2023,
    ES2024,
    ESNext,
}

impl From<RawTarget> for Target {
    fn from(val: RawTarget) -> Self {
        match val {
            RawTarget::ES5 => Target::ES5,
            RawTarget::ES2015 | RawTarget::ES6 => Target::ES2015,
            _ => unsafe { std::mem::transmute::<u8, Target>(val as u8 - 1) },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Extension {
    Ts,
    Tsx,
    DTs,
    Js,
    Jsx,
    Json,
    Mjs,
    Mts,
    Dmts,
    Cjs,
    Cts,
    Dcts,
}

impl Extension {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Extension::Ts => "ts",
            Extension::Tsx => "tsx",
            Extension::DTs => "d.ts",
            Extension::Js => "js",
            Extension::Jsx => "jsx",
            Extension::Json => "json",
            Extension::Mjs => "mjs",
            Extension::Mts => "mts",
            Extension::Dmts => "d.mts",
            Extension::Cjs => "cjs",
            Extension::Cts => "cts",
            Extension::Dcts => "d.cts",
        }
    }
}

pub const SUPPORTED_TS_EXTENSIONS: &[&[Extension]] = &[
    &[Extension::Ts, Extension::Tsx, Extension::DTs],
    &[Extension::Cts, Extension::Dcts],
    &[Extension::Mts, Extension::Dmts],
];

pub const FLATTENED_SUPPORTED_TS_EXTENSIONS: &[Extension] = &[
    Extension::Ts,
    Extension::Tsx,
    Extension::DTs,
    Extension::Cts,
    Extension::Dcts,
    Extension::Mts,
    Extension::Dmts,
];

pub const ALL_SUPPORTED_EXTENSIONS: &[&[Extension]] = &[
    &[
        Extension::Ts,
        Extension::Tsx,
        Extension::DTs,
        Extension::Js,
        Extension::Jsx,
    ],
    &[Extension::Cts, Extension::Dcts, Extension::Cjs],
    &[Extension::Mts, Extension::Dmts, Extension::Mjs],
];

pub const FLATTENED_ALL_SUPPORTED_EXTENSIONS: &[Extension] = &[
    Extension::Ts,
    Extension::Tsx,
    Extension::DTs,
    Extension::Js,
    Extension::Jsx,
    Extension::Cts,
    Extension::Dcts,
    Extension::Cjs,
    Extension::Mts,
    Extension::Dmts,
    Extension::Mjs,
];

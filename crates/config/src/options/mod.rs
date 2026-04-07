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

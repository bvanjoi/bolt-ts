// #[derive(Debug, Clone, Copy, Default)]
// enum Target {
//     #[default]
//     ES3,
//     ES5,
//     ES6,
// }

// #[derive(Debug, Clone, Copy, Default)]
// struct CompilerOptions {
//     target: Target,
// }

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct TsConfig {
    include: Option<Vec<String>>,
}

impl TsConfig {
    pub fn with_include(mut self, include: Vec<String>) -> Self {
        self.include = Some(include);
        self
    }

    pub fn normalize(self) -> NormalizedTsConfig {
        const DEFAULT_INCLUDE: &[&str] = &["**/*"];
        let include = self
            .include
            .unwrap_or_else(|| DEFAULT_INCLUDE.iter().map(|&s| s.to_string()).collect());
        NormalizedTsConfig { include }
    }
}

pub struct NormalizedTsConfig {
    include: Vec<String>,
}

impl NormalizedTsConfig {
    pub fn include(&self) -> &[String] {
        &self.include
    }
}

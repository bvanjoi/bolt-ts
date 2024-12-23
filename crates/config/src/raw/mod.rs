use super::OutDir;

macro_rules! with_option {
    ($s: ident, $option: ident, $ty: ty) => {
        impl $s {
            paste::paste! {
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
            }
        }
    };
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct RawCompilerOptions {
    out_dir: Option<String>,
}

impl RawCompilerOptions {
    pub fn normalize(self) -> super::NormalizedCompilerOptions {
        let out_dir = self.out_dir.map_or(OutDir::default(), OutDir::Custom);
        super::NormalizedCompilerOptions { out_dir }
    }
}

with_option!(RawCompilerOptions, out_dir, String);

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct RawTsConfig {
    include: Option<Vec<String>>,
    compiler_options: Option<RawCompilerOptions>,
}

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

with_option!(RawTsConfig, include, Vec<String>);
with_option!(RawTsConfig, compiler_options, RawCompilerOptions);

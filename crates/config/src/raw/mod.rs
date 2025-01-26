use super::OutDir;

macro_rules! with_option {
    ($s: ident, $(($option: ident, $ty: ty)),* $(,)?) => {
        #[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
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
                            None => Some(f(Default::default())),
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
    (strict_null_checks, bool)
);

impl RawCompilerOptions {
    pub fn normalize(self) -> super::NormalizedCompilerOptions {
        let out_dir = self.out_dir.map_or(OutDir::default(), OutDir::Custom);
        let no_emit = self.no_emit.unwrap_or_default();
        let declaration = self.declaration.unwrap_or_default();
        let strict = self.strict.unwrap_or_default();
        let strict_null_checks = strict || self.strict_null_checks.unwrap_or_default();
        super::NormalizedCompilerOptions {
            out_dir,
            no_emit,
            declaration,
            strict,
            strict_null_checks,
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

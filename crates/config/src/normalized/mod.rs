use crate::options::OutDir;

pub struct NormalizedCompilerOptions {
    pub(super) out_dir: OutDir,
}

pub struct NormalizedTsConfig {
    pub(super) include: Vec<String>,
    pub(super) compiler_options: NormalizedCompilerOptions,
}

impl NormalizedTsConfig {
    pub fn include(&self) -> &[String] {
        &self.include
    }
    pub fn compiler_options(&self) -> &NormalizedCompilerOptions {
        &self.compiler_options
    }
}
